#ifndef TARGET_PATMOS_SINGLEPATH_CONSTANTLOOPDOMINATORANALYSIS_H_
#define TARGET_PATMOS_SINGLEPATH_CONSTANTLOOPDOMINATORANALYSIS_H_

#include "llvm/CodeGen/MachineBasicBlock.h"
#include <set>
#include <map>
#include <queue>

#define DEBUG_TYPE "patmos-singlepath"

// Uncomment to always print debug info
//#define LLVM_DEBUG(...) __VA_ARGS__

namespace llvm {
/// A map of dominators.
/// The values dominate the keys
template<typename MachineBasicBlock>
using DomMap = std::map<const MachineBasicBlock*, std::set<const MachineBasicBlock*>>;

/// A map of reachability.
/// The keys are reachable from the values
template<typename MachineBasicBlock>
using ReachMap = std::map<const MachineBasicBlock*, std::set<const MachineBasicBlock*>>;

/// A map for loop headers.
/// The keys are loop headers, the values reflect something about the header's loop.
template<typename MachineBasicBlock, typename Value>
using LoopMap = std::map<const MachineBasicBlock*, Value>;

/// Computes the intersection of two sets, returning the result without
/// changing the given sets
template<typename T>
static std::set<T> get_intersection(std::set<T> &x, std::set<T> &y) {
	std::set<T> intersected;
	std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
			std::inserter(intersected, intersected.begin()));
	return intersected;
}

/// Removes all elements in the given set that return true when passed to the given predicate.
/// Roughly equivalent to std::remove_if (but that one is not available in C++ 14).
template<typename T, typename Pred>
static void remove_if(std::set<T> &remove_from, Pred pred) {
	std::set<T> to_remove;
	for(auto val: remove_from) {
		if(pred(val)) {
			to_remove.insert(val);
		}
	}
	for(auto val: to_remove) {
		remove_from.erase(val);
	}
}

/// Pretty-prints the given list of MBBs to the given stream (doesn't end with space or newline)
template<
	typename OUT, typename T
>
static OUT& dump_mbb_list(OUT &out, T &list) {
	out << "{";
	for (auto x : list) {
		out << x->getName() << ", ";
	}
	out << "}";
	return out;
}

/// Returns the FCFG successors of "current" assuming start_mbb is the root of the FCFG
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static std::set<MachineBasicBlock*> fcfg_successors(
		const MachineBasicBlock *current, const MachineBasicBlock *start_mbb,
		const MachineLoopInfo *LI)
{
	std::set<MachineBasicBlock*> successors;

	// Collect possible successors
	if (current != start_mbb && LI->isLoopHeader(current)) {
		LLVM_DEBUG(dbgs() << "header block.\n");

		auto loop = LI->getLoopFor(current);

		// Use its exits to continue exploring the current loop
		SmallVector<std::pair<MachineBasicBlock*, MachineBasicBlock*>> exits;
		loop->getExitEdges(exits);
		for (auto exit : exits) {
			if ( 	// Exit to this loop
					LI->getLoopDepth(exit.second) == LI->getLoopDepth(start_mbb) ||
					// Exit to header of next loop
					LI->getLoopDepth(exit.second)
							== (LI->getLoopDepth(start_mbb) + 1)
			){
				successors.insert(exit.second);
				LLVM_DEBUG(dbgs()	<< "Exit to current loop: "
									<< exit.first->getName() << " -> "
									<< exit.second->getName() << "\n");
			} else {
				LLVM_DEBUG(dbgs()	<< "Exit to outer loop: "
									<< exit.first->getName() << " -> "
									<< exit.second->getName() << "\n");
			}
		}
	} else {
		successors.insert(current->succ_begin(), current->succ_end());
	}

	// Remove any successors not in the FCFG
	auto iter = successors.begin();
	while (iter != successors.end()) {
		auto *succ = *iter;
		if (// 'succ' is a block in the outer loop
			LI->getLoopDepth(succ) < LI->getLoopDepth(start_mbb) ||
			// 'succ' is a header of a loop in the outer loop
			(LI->getLoopDepth(succ) == LI->getLoopDepth(start_mbb)
				&& LI->getLoopFor(succ) != LI->getLoopFor(start_mbb))
		){
			LLVM_DEBUG(dbgs() << "Ignore exit target " << succ->getName() << "\n");
			successors.erase(succ);
			iter = successors.begin();
		} else if (succ == start_mbb) {
			LLVM_DEBUG( dbgs() << "Ignore back edge to " << succ->getName()	<< "\n");
			successors.erase(succ);
			iter = successors.begin();
		} else {
			iter++;
		}
	}
	return successors;
}

/// Returns the header of the outermost loop, containing the given loop ("inner"),
/// within the current loop ("limit").
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static MachineBasicBlock* outermost_inner_loop_header(
		const MachineBasicBlock *inner, const MachineBasicBlock *limit,
		const MachineLoopInfo *LI
) {
	assert(LI->isLoopHeader(inner));
	assert(LI->isLoopHeader(limit) || limit->pred_size() == 0);
	auto inner_loop = LI->getLoopFor(inner);
	auto limit_loop = LI->getLoopFor(limit);
	assert(inner && "Inner loop was null");

	auto outer_depth = limit_loop ? limit_loop->getLoopDepth() : 0;

	assert(inner_loop->getLoopDepth() > (outer_depth));
	while (inner_loop->getLoopDepth() != (outer_depth + 1)) {
		inner_loop = inner_loop->getParentLoop();
	}
	assert(limit_loop == inner_loop->getParentLoop());
	return inner_loop->getHeader();
}

/// Returns the FCFG predecessors of "current" assuming start_mbb is the root of the FCFG
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static std::set<MachineBasicBlock*> fcfg_predecessors(
		const MachineBasicBlock *current, const MachineBasicBlock *start_mbb,
		const MachineLoopInfo *LI
) {
	std::set<MachineBasicBlock*> predecessors;
	auto loop = LI->getLoopFor(start_mbb);

	for (auto pred_iter = current->pred_begin();
			current != start_mbb && pred_iter != current->pred_end();
			pred_iter++
	) {
		auto pred = *pred_iter;
		auto *pred_loop = LI->getLoopFor(pred);

		if (pred == current) {
			continue;
		}

		if (pred_loop == loop) {
			predecessors.insert(pred);
		} else if (!loop || loop->contains(pred)) {
			auto header = outermost_inner_loop_header(pred_loop->getHeader(),start_mbb, LI);
			if (header != current) {
				/// Predecessor is in nested loop, use header of outermost inner loop
				predecessors.insert(header);
			}
		} else {
			/// Predecessor is in outer loop
		}
	}

	return predecessors;
}

/// If the given block is in a nested loop to the given parent loop,
/// returns the outermost inner loop, i.e., the header of the outermost
/// loop that is still nested in the given parent loop.
/// If the block is not part of a nested loop, returns none.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static Optional<const MachineBasicBlock*> get_outermost_header_in_parent(
		const MachineBasicBlock *block,
		const MachineBasicBlock *parent_loop_header,
		const MachineLoopInfo &LI
){
	auto parent_loop = LI->getLoopFor(parent_loop_header);
	auto loop = LI->getLoopFor(block);
	if(loop == parent_loop || !loop || loop->contains(parent_loop_header)) {
		return None;
	}
	assert(!parent_loop || parent_loop->contains(block));

	auto temp_parent = loop;
	while(temp_parent != parent_loop) {
		loop = temp_parent;
		temp_parent = loop->getParentLoop();
	}
	return loop->getHeader();
}

/// Returns the set of exits from the given header's loop
/// that may reach the given target according to the given reachmap.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static std::set<const MachineBasicBlock*> get_reaching_exits(
		const MachineBasicBlock* header,
		const MachineBasicBlock* target,
		ReachMap<MachineBasicBlock> &reachable_from,
		const MachineLoopInfo &LI
) {
	auto loop = LI->getLoopFor(header);
	assert(loop);
	std::set<const MachineBasicBlock*> result;
	SmallVector<std::pair<MachineBasicBlock*, MachineBasicBlock*>> exits;
	loop->getExitEdges(exits);

	for(auto exit: exits) {
		if(reachable_from[target].count(exit.second)) {
			result.insert(exit.first);
		}
	}

	return result;
}

/// Returns whether the given block is between the two given targets
/// according to the given reachmap.
template<
	typename MachineBasicBlock
>
static bool is_between(
		const MachineBasicBlock* block,
		const MachineBasicBlock* target1,
		const MachineBasicBlock* target2,
		ReachMap<MachineBasicBlock> &reachable_from
) {
	return (block != target1 && reachable_from[block].count(target1) && reachable_from[target2].count(block)) ||
		(block != target2 && reachable_from[block].count(target2) && reachable_from[target1].count(block));
}

/// Returns whether the given dominator should be excluded from
/// dominating the given dominee in its given loop.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static bool should_exclude_from_dom(
		const MachineBasicBlock *dominator,
		const MachineBasicBlock *dominee,
		const MachineBasicBlock *dominee_loop,
		ReachMap<MachineBasicBlock> &reachable_from,
		LoopMap<MachineBasicBlock, ReachMap<MachineBasicBlock>> &loop_reachable_from,
		const MachineLoopInfo &LI
){
	auto dom_header = get_outermost_header_in_parent(dominator, dominee_loop, LI);

	if(dom_header) {
		auto dom_loop = LI->getLoopFor(*dom_header);
		auto exits = get_reaching_exits(*dom_header, dominee, reachable_from, LI);
		for(auto exit1: exits) {
			for(auto exit2: exits) {
				if(exit1 != exit2) {
					if(is_between(dominator, exit1, exit2, loop_reachable_from[dom_loop->getHeader()])) {
						return true;
					}
				}
			}
		}
	}
	return false;
}

/// Recursive implementation of fcfg_topo_sort.
/// "current" is the current block of the sort to explore.
/// "start_mbb" is the fcfg header node.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static void fcfg_topo_sort_rec(
		const MachineBasicBlock *current,
		const MachineBasicBlock *start_mbb,
		const MachineLoopInfo *LI,
		std::deque<const MachineBasicBlock*> &stack
) {
	if (std::find(stack.begin(), stack.end(), current) == stack.end()) {
		auto succs = fcfg_successors(current, start_mbb, LI);
		if (!succs.empty()) {
			for (auto succ : succs) {
				fcfg_topo_sort_rec(succ, start_mbb, LI, stack);
			}
		}
		stack.push_back(current);
	}
}

/// Returns the reverse topological sort of the fcfg with the given start node.
/// The first element in the sort if the last of the given list.
/// Use ".pop_back" to get the correct ordering of the sort.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static std::deque<const MachineBasicBlock*> fcfg_topo_sort(
		const MachineBasicBlock *start_mbb, const MachineLoopInfo *LI
) {
	std::deque<const MachineBasicBlock*> stack;
	fcfg_topo_sort_rec(start_mbb, start_mbb, LI, stack);
	return stack;
}

/// Recursive version of 'constantLoopDominatorsAnalysis'.
///
/// Returns the cl-dominators of each block (i.e. x -> y, means y cl-dom y)
/// in the FCFG of the given loop header.
/// Second, returns the dominators of the header in a potential parent loop,
/// assuming the given star_mbb was a nested loop header.
/// Lastly, returns the reachability map of the FCFG.
///
/// The second and third results are not usefull if the given start_mbb
/// is the function entry.
///
/// Inputs:
/// * start_mbb: The header of the loop to be analyzed (or the function entry initially)
/// * LI: MachineLoopInfo of the block's function
/// * constantBounds: function that given a loop header block returns whether the loop
///                   is constant (lower and upper bound are equal)
///
/// This function is templated such that it can be tested. Normal use should just use LLVM's
/// MachineBasicBlock and MachineLoopInfo.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
static std::tuple<
	DomMap<MachineBasicBlock>,
	std::set<const MachineBasicBlock*>, // Dominators of all latches
	ReachMap<MachineBasicBlock> // key Reachable from value
>
constantLoopDominatorsAnalysisImpl(
		const MachineBasicBlock *StartMbb,
		const MachineLoopInfo *LI,
		bool (*ConstantBounds)(const MachineBasicBlock *Mbb)
) {
	LLVM_DEBUG(dbgs() << "\nStarting Bounded Dominator Analysis: "<< StartMbb->getName() << "\n");
	DomMap<MachineBasicBlock> Dominators;
	// Set of nodes in each loop that dominates the loop header
	DomMap<MachineBasicBlock> LoopDominators;
	ReachMap<MachineBasicBlock> ReachableFrom;
	LoopMap<MachineBasicBlock, ReachMap<MachineBasicBlock>> LoopReachableFrom;
	auto CurrentLoop = LI->getLoopFor(StartMbb);


	// Returns whether the given block is a header of an inner loop (assuming it is in the current loop).
	auto IsFcfgHeader = [&](const MachineBasicBlock *Block) {
		assert(!CurrentLoop || CurrentLoop->contains(Block));
		return Block != StartMbb && LI->isLoopHeader(Block);
	};
	auto RevTopo = fcfg_topo_sort(StartMbb, LI);

	while (!RevTopo.empty()) {
		auto *current = RevTopo.back();
		RevTopo.pop_back();

		LLVM_DEBUG(
			dbgs() << "\nCurrent: " << current->getName() << "\n";
			for(auto end: Dominators){
				dbgs() << end.first->getName() << ": ";
				dump_mbb_list(dbgs(), end.second) << "\n";
			}
		);

		// If loop header, analyze recursively
		if (IsFcfgHeader(current)) {
			assert(!LoopDominators.count(current) && "Loop already recursively handled");
			LLVM_DEBUG(dbgs() << "Header block: " << current->getName() << "\n");
			auto LoopResult = constantLoopDominatorsAnalysisImpl(current, LI,ConstantBounds);

			// If dominates header, dominates all
			// If doesn't dominate header, dominates none
			LoopDominators[current] =  std::get<1>(LoopResult);
			LoopReachableFrom[current] = std::get<2>(LoopResult);

			LLVM_DEBUG(
				dbgs() << "Loop dominators for " << current->getName() << ": ";
				for(auto dom: LoopDominators[current]) {
					dbgs() << dom->getName() << ", ";
				}
				dbgs() << "\n";
			);
		}

		LLVM_DEBUG(
			dbgs() << "\nNext: " << current->getName() <<"\n";
			dbgs() << "FCFG pred: [";
			for(auto pred: fcfg_predecessors(current, StartMbb, LI)){
				dbgs() << pred->getName() <<", ";
			}
			dbgs() << "]\n"
		);

		Optional<std::set<const MachineBasicBlock*>> PredDomsIntersect;
		for (auto Pred : fcfg_predecessors(current, StartMbb, LI)) {
			assert(Dominators.count(Pred) && "Predecessor hasn't been handled");

			if (PredDomsIntersect) {
				PredDomsIntersect = get_intersection(
					*PredDomsIntersect,
					Dominators[Pred]
				);
			} else {
				PredDomsIntersect = Dominators[Pred];
			}
		}

		if (PredDomsIntersect) {
			Dominators[current] = *PredDomsIntersect;
		}

		// Self dominant if const header (or not header)
		if(!IsFcfgHeader(current) || ConstantBounds(current)) {
			if(IsFcfgHeader(current)) {
				Dominators[current].insert(LoopDominators[current].begin(), LoopDominators[current].end());
			}
			Dominators[current].insert(current);
		}

		// Update reachability
		ReachableFrom[current].insert(current);
		for (auto pred : fcfg_predecessors(current, StartMbb, LI)) {
			if (IsFcfgHeader(pred)) {
				assert(LoopReachableFrom.count(pred) && "Predecessor loop not handled");
				auto pred_loop = LI->getLoopFor(pred);
				assert(pred_loop);
				SmallVector<std::pair<MachineBasicBlock*, MachineBasicBlock*>> exits;
				pred_loop->getExitEdges(exits);
				for (auto exit : exits) {
					if(exit.second == current) {
						ReachableFrom[current].insert(LoopReachableFrom[pred][exit.first].begin(), LoopReachableFrom[pred][exit.first].end());
					}
				}
			}
			ReachableFrom[current].insert(ReachableFrom[pred].begin(), ReachableFrom[pred].end());
		}

		// Remove any dominators between exits
		remove_if(Dominators[current], [&](auto dom){
			return should_exclude_from_dom(dom, current, StartMbb, ReachableFrom, LoopReachableFrom, LI);
		});
	}

	// Extract dominators of block within loops into result
	for (auto loop_doms : LoopDominators) {
		auto header = loop_doms.first;
		auto header_loop = LI->getLoopFor(header);
		assert(header_loop);

		std::for_each(header_loop->block_begin(),header_loop->block_end(), [&](auto block){
			Dominators[block] = Dominators[header];
			ReachableFrom[block] = ReachableFrom[header];
			ReachableFrom[block].insert(
				LoopReachableFrom[header][block].begin(),
				LoopReachableFrom[header][block].end()
			);
		});
	}

	Optional<std::set<const MachineBasicBlock*>> unilatch_doms;
	if(CurrentLoop){
		SmallVector<MachineBasicBlock*> latches;
		CurrentLoop->getLoopLatches(latches);
		for(auto latch: latches) {
			if(unilatch_doms) {
				unilatch_doms = get_intersection(*unilatch_doms, Dominators[latch]);
			} else {
				unilatch_doms = Dominators[latch];
			}
		}
		assert(unilatch_doms);

		// Use nullptr to signify the unilatch and add it to the reachability sets.
		// All block can reach the unilatch by definition.
		ReachableFrom[nullptr].insert(nullptr);
		for(auto block: ReachableFrom) {
			ReachableFrom[nullptr].insert(block.first);
		}

		// Remove any dominators between nested exits
		remove_if(*unilatch_doms, [&](auto dom){
			return should_exclude_from_dom(dom, (const MachineBasicBlock*) nullptr,
					StartMbb, ReachableFrom, LoopReachableFrom, LI);
		});

		// Remove nullptr from reachbility set such that it can't be seen in parent loop
		ReachableFrom.erase(nullptr);
	}

	LLVM_DEBUG(
		dbgs() << "\nConstant-Loop Dominator Analysis Results: " << StartMbb->getName() << "\n";
		for(auto end: Dominators){
			dbgs() << end.first->getName() << ": ";
			dump_mbb_list(dbgs(), end.second) << "\n";
		}
		if(unilatch_doms){
			dbgs() << "Unilatch doms:\n";
			for(auto d: *unilatch_doms) {
				dbgs() << d->getName() <<", ";
			}
			dbgs() << "\n";
		}
		dbgs() << "Path map:\n";
		for(auto entry: ReachableFrom) {
			dbgs() << entry.first->getName() << ": ";
			dump_mbb_list(dbgs(), entry.second) << "\n";
		}
	);
	if(!unilatch_doms) {
		unilatch_doms = std::set<const MachineBasicBlock*>();
	}

	return std::make_tuple(Dominators, *unilatch_doms, ReachableFrom);
}

/// Performs a constant-loop dominator analysis, where the bounds of loops are taken into account.
/// Definition:
///   A node 'x' constant-loop dominates a node 'y' (y cl-dom x) if every path from the entry to
///   the last possible visit of 'y' visits 'x' a fixed number of non-zero times.
///
/// Returns the cl-dominators of each block (i.e. x -> y, means y cl-dom y).
///
/// The entry block of the function is assumed to not be the header of a loop.
///
/// Inputs:
/// * start_mbb: The entry block to start analysis from
/// * LI: MachineLoopInfo of the block's function
/// * constantBounds: function that given a loop header block returns whether the loop
///                   is constant (lower and upper bound are equal)
/// * end_doms_only: Whether resulting map should only contain the end blocks of the
///                  given function
///
/// This function is templated such that it can be tested. Normal use should just use LLVM's
/// MachineBasicBlock and MachineLoopInfo.
template<
	typename MachineBasicBlock,
	typename MachineLoopInfo
>
DomMap<MachineBasicBlock>
constantLoopDominatorsAnalysis(
		const MachineBasicBlock *StartMBB,
		const MachineLoopInfo *LI,
		bool (*constantBounds)(const MachineBasicBlock *mbb),
		bool EndDomsOnly = true
) {
	assert(StartMBB->pred_size() == 0
							&& "Should only be used on function entry");
	auto Dominators =
		std::get<0>(constantLoopDominatorsAnalysisImpl(StartMBB, LI,constantBounds));

	if (EndDomsOnly) {

		LLVM_DEBUG(dbgs() << "End dominators only\n");
		DomMap<MachineBasicBlock> end_doms;
		for (auto Entry : Dominators) {
			auto Block = Entry.first;
			if (Block->succ_size() == 0) {
				end_doms[Block] = Dominators[Block];
			}
		}
		Dominators = end_doms;
	}
	return Dominators;
}

}
#endif /* TARGET_PATMOS_SINGLEPATH_CONSTANTLOOPDOMINATORANALYSIS_H_ */
