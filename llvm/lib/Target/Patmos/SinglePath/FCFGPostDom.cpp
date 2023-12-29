
#include "FCFGPostDom.h"
#include "Patmos.h"
#include "SinglePath/PatmosSPReduce.h"
#include "SinglePath/ConstantLoopDominatorAnalysis.h" // for "get_intersection"
#include "TargetInfo/PatmosTargetInfo.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/IR/Dominators.h"

#include <deque>

using namespace llvm;

MachineBasicBlock* FCFGPostDom::outermost_inner_loop_header(MachineLoop *Inner) {
	assert(Inner && "Inner loop was null");

	auto OuterDepth = loop? loop->getLoopDepth():0;

	assert(Inner->getLoopDepth() > (OuterDepth));
	while(Inner->getLoopDepth() != (OuterDepth+1)) {
		Inner = Inner->getParentLoop();
	}
	assert(loop == Inner->getParentLoop());
	return Inner->getHeader();
}

Optional<MachineBasicBlock*> FCFGPostDom::fcfg_predecessor(MachineBasicBlock *Pred) {
	auto *PredLoop = LI.getLoopFor(Pred);

	if(PredLoop == loop) {
		return Pred;
	} 
	if(!loop || loop->contains(Pred)) {
		/// Predecessor is in nested loop, use header of outermost inner loop
		return outermost_inner_loop_header(PredLoop);
	}  
	/// Predecessor is in outer loop
	return None;

}

Optional<MachineBasicBlock*> FCFGPostDom::fcfg_successor(MachineBasicBlock *Succ) {
	auto *SuccLoop = LI.getLoopFor(Succ);

	if(SuccLoop == loop) {
		return Succ;
	} else if(!loop || loop->contains(Succ)) {
		/// Successor is in nested loop, use header of outermost inner loop
		return outermost_inner_loop_header(SuccLoop);
	} else {
		/// Successor is in outer loop
		return None;
	}
}

void FCFGPostDom::calculate(std::set<MachineBasicBlock*> Roots) {
	assert(std::all_of(Roots.begin(), Roots.end(), [&](auto root){
		return
			// Root is a node in the loop
			LI.getLoopFor(root) == loop ||
			// Root is a header of an inner loop
			(LI.getLoopFor(root)->getParentLoop() == loop && LI.getLoopFor(root)->getHeader() == root);
	}) && "All roots not in the same loop");

	std::deque<MachineBasicBlock*> Worklist;

	auto AddPredsToWorklist = [&](auto Current){
		std::for_each(Current->pred_begin(), Current->pred_end(), [&](auto Pred){
			auto FcfgPred = fcfg_predecessor(Pred);
			if(FcfgPred) {
				Worklist.push_back(*FcfgPred);
			}
		});
	};

	// Initial post dominators: roots are only post dominated by themselves
	for(auto *Root: Roots) {
		post_doms[Root] = {Root};
		AddPredsToWorklist(Root);
	}

	while(!Worklist.empty()) {
		auto *Current = Worklist.front();
		Worklist.pop_front();
		if(post_doms.count(Current)) {
			// already did it
			continue;
		}

		std::set<MachineBasicBlock*> Succs;
		if(LI.isLoopHeader(Current) && (!loop || loop->getHeader() != Current)) {
			// Current is header of inner loop
			auto *CurrentsLoop = LI.getLoopFor(Current);
			assert(CurrentsLoop);
			assert(CurrentsLoop->getParentLoop() == loop);

			SmallVector<std::pair<MachineBasicBlock*, MachineBasicBlock*>> Exits;
			CurrentsLoop->getExitEdges(Exits);
			for(auto Edge: Exits) {
				if(!loop || loop->contains(Edge.second)) {
					Succs.insert(Edge.second);
				}
			}
		} else {
			std::for_each(Current->succ_begin(), Current->succ_end(), [&](auto Succ){
				return Succs.insert(*fcfg_successor(Succ));
			});
		}

		auto AllSuccsDone = std::all_of(Succs.begin(), Succs.end(), [&](auto Succ){
			return post_doms.count(Succ);
		});
		if(AllSuccsDone) {
			auto Doms = post_doms[*Succs.begin()];
			std::for_each(Succs.begin(), Succs.end(), [&](auto Succ){
				Doms = get_intersection(Doms, post_doms[Succ]);
			});
			Doms.insert(Current);
			post_doms[Current] = Doms;
			AddPredsToWorklist(Current);
		} else {
			// Wait for all successors to be done
			Worklist.push_back(Current);
		}
	}
}

FCFGPostDom::FCFGPostDom(MachineLoop *l, MachineLoopInfo &LI): loop(l), LI(LI){
	assert(loop);
	SmallVector<std::pair<MachineBasicBlock*, MachineBasicBlock*>> Exits;
	loop->getExitEdges(Exits);

	std::set<MachineBasicBlock*> Roots;
	for(auto Exit: Exits) {
		auto *Source = Exit.first;
		auto *SourceLoop = LI.getLoopFor(Source);
		if(SourceLoop != loop) {
			Roots.insert(outermost_inner_loop_header(SourceLoop));
		} else {
			Roots.insert(Source);
		}
	}

	// add the latches to the roots
	SmallVector<MachineBasicBlock*> Latches;
	loop->getLoopLatches(Latches);
	for(auto *Latch: Latches) {
		Roots.insert(Latch);
	}

	calculate(Roots);
	for(auto *Inner: *loop) {
		inner_doms.push_back(FCFGPostDom(Inner, LI));
	}
}

void FCFGPostDom::get_post_dominees(MachineBasicBlock *Dominator, std::set<MachineBasicBlock*> &Dominees) {
	for(auto Entry: post_doms) {
		if(Entry.second.count(Dominator)){
			Dominees.insert(Entry.first);
		}
	}

	for(auto Inner: inner_doms) {
		Inner.get_post_dominees(Dominator, Dominees);
	}
}

FCFGPostDom::FCFGPostDom(MachineFunction &MF, MachineLoopInfo &LI) : loop(nullptr), LI(LI){
	auto IsEnd = [&](auto &Block){return Block.succ_size() == 0;};
	assert(std::count_if(MF.begin(), MF.end(), IsEnd) == 1 && "Found multiple end blocks");
	auto End = std::find_if(MF.begin(), MF.end(), IsEnd);

	calculate({&*End});
	for(auto *Inner: LI) {
		inner_doms.push_back(FCFGPostDom(Inner, LI));
	}
}

void FCFGPostDom::print(raw_ostream &O, unsigned Indent) {
	if(Indent == 0) O << "Post Dominators:\n";
	for(auto Entry: post_doms) {
		auto *Block = Entry.first;
		auto Dominees = Entry.second;

		for(int I = 0; I<Indent; I++) O << "\t";
		O << "bb." << Block->getNumber() << ": [";
		for(auto *Dominee: Dominees) {
			O << "bb." << Dominee->getNumber() << ", ";
		}
		O << "]\n";
	}
	for(auto Inner: inner_doms) {
		Inner.print(O, Indent+1);
	}
}

bool FCFGPostDom::post_dominates(MachineBasicBlock *dominator, MachineBasicBlock *dominee) {
	return post_doms[dominee].count(dominator) ||
		std::any_of(inner_doms.begin(), inner_doms.end(),
			[&](auto Inner){ return Inner.post_dominates(dominator, dominee);});
}

void FCFGPostDom::get_control_dependencies(std::map<
		// X
		MachineBasicBlock*,
		// Set of {Y->Z} control dependencies of X
		std::set<std::pair<Optional<MachineBasicBlock*>,MachineBasicBlock*>>
	> &Deps) {
	for(auto Entry: post_doms) {
		auto *Block = Entry.first;

		std::set<MachineBasicBlock*> Dominees;
		get_post_dominees(Block, Dominees);
		// If mbb post-dominates a block, any of its predecessors that mbb does not dominate must therefore
		// be a control dependency of mbb
		for(auto *Dominee: Dominees){
			auto *DomineeLoop = LI.getLoopFor(Dominee);

			if(Dominee->pred_size() == 0 || (loop && loop->getHeader() == Dominee)) {
				// dominee is the entry to the loop (header).
				// If you post dominate the entry, you are control dependent on the entry edge
				Deps[Block].insert(std::make_pair(None, Dominee));
			} else {
				std::for_each(Dominee->pred_begin(), Dominee->pred_end(), [&](auto Pred){
					auto FcfgPred = fcfg_predecessor(Pred);
					assert(FcfgPred && "Predecessor is header or entry");

					if (!post_dominates(Block, *FcfgPred)) {
						auto FcfgPredecessorLoop = LI.getLoopFor(*FcfgPred);

						if(FcfgPredecessorLoop != loop) {
							// the predecessor is a loop header. Use the loop's exit edges as the dep edge
							SmallVector<std::pair<MachineBasicBlock*, MachineBasicBlock*>> Exits;
							FcfgPredecessorLoop->getExitEdges(Exits);

							for(auto Exit: Exits) {
								if(Exit.second == Dominee) {
									Deps[Block].insert(std::make_pair(Exit.first, Dominee));
								}
							}
						} else {
							assert(FcfgPred);
							Deps[Block].insert(std::make_pair(FcfgPred, Dominee));
						}
					}
				});
			}
		}
	}
	for(auto Inner: inner_doms) {
		Inner.get_control_dependencies(Deps);
	}

	assert(
		std::all_of(Deps.begin(), Deps.end(), [&](auto entry){
			return std::all_of(entry.second.begin(), entry.second.end(), [&](auto edge){
				if(edge.first) {
					// Edge exists
					return std::any_of((*edge.first)->succ_begin(), (*edge.first)->succ_end(), [&](auto succ){
						return succ == edge.second;
					});
				} else {
					// Non-edges use only headers
					return LI.isLoopHeader(edge.second) || edge.second->pred_size() == 0;
				}
			});
		})
		&& "Not all dependencies are valid"
	);
}







