#include "Linearizer.h"
#include "Patmos.h"
#include "SinglePath/PatmosSPReduce.h"
#include "TargetInfo/PatmosTargetInfo.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/Statistic.h"

using namespace llvm;

#define DEBUG_TYPE "patmos-singlepath"

char Linearizer::ID = 0;

FunctionPass *llvm::createSinglePathLinearizer(const PatmosTargetMachine &tm) {
  return new Linearizer(tm);
}

bool Linearizer::runOnMachineFunction(MachineFunction &MF) {
	bool Changed = false;
	// only convert function if marked
	if ( MF.getInfo<PatmosMachineFunctionInfo>()->isSinglePath()) {
		auto *RootScope = getAnalysis<PatmosSinglePathInfo>().getRootScope();
		LLVM_DEBUG(
			dbgs() << "[Single-Path] Linearizing " << MF.getFunction().getName() << "\n";
		);

		auto &LI = getAnalysis<MachineLoopInfo>();
		// Update unilatch to use the counter as condition (instead of unconditionally branching
		for(auto &HeaderMbb: MF){
			if(!LI.isLoopHeader(&HeaderMbb)) continue;
			DebugLoc DL;
			auto *Loop = LI.getLoopFor(&HeaderMbb);

			if(PatmosSinglePathInfo::needsCounter(Loop)) {
				auto *Unilatch = PatmosSinglePathInfo::getPreHeaderUnilatch(Loop).second;

				// Insert counter check in unilatch
				auto CounterDecrementer = PatmosSinglePathInfo::getUnilatchCounterDecrementer(Unilatch);
				auto DecrementedCountReg = CounterDecrementer->getOperand(3).getReg();

				auto CounterCheckVreg = createVirtualRegisterWithHint(MF.getRegInfo(), Patmos::P1);
				BuildMI(*Unilatch, Unilatch->getFirstInstrTerminator(), DL,
					TII->get(Patmos::CMPLT), CounterCheckVreg)
					.addReg(Patmos::P0).addImm(0)
					.addReg(Patmos::R0).addReg(DecrementedCountReg);

				// Replace branch with back edge
				Unilatch->remove(&*Unilatch->getFirstInstrTerminator());
				BuildMI(*Unilatch, Unilatch->getFirstInstrTerminator(), DL,
					TII->get(Patmos::BR))
					.addReg(CounterCheckVreg).addImm(0)
					.addMBB(&HeaderMbb);

				// Replace any PSEUDO_POSTLOOP_RELOAD with reload predicated on check condition
				for(auto Iter = Unilatch->begin(); Iter != Unilatch->end(); ) {
					if(Iter->getOpcode() == Patmos::PSEUDO_POSTLOOP_RELOAD) {
						auto Reg = Iter->getOperand(0).getReg();
						assert(Patmos::RRegsRegClass.contains(Reg));
						auto FrameIdx = Iter->getOperand(1).getImm();
						TII->loadRegFromStackSlot(*Unilatch,Unilatch->getFirstTerminator(), Reg,
								FrameIdx, &Patmos::RRegsRegClass, TRI);
						std::prev(Unilatch->getFirstTerminator())->getOperand(1).setReg(CounterCheckVreg);
						std::prev(Unilatch->getFirstTerminator())->getOperand(2).setImm(1);
						Unilatch->erase(Iter);
						Iter = Unilatch->begin();
					} else {
						Iter++;
					}
				}
			} else {
				// Loops that don't use a counter just need to jump out when the unilatch is disabled.
				auto *Unilatch = PatmosSinglePathInfo::getPreHeaderUnilatch(Loop).second;
				auto BackBranch = Unilatch->getFirstInstrTerminator();
				assert(BackBranch->getOpcode() == Patmos::BRu);
				assert(BackBranch->getOperand(0).getReg() == Patmos::NoRegister);
				assert(BackBranch->getOperand(1).isImm());
				assert(BackBranch->getOperand(2).getMBB() == &HeaderMbb);

				// We find PSEUDO_UNILATCH_EXIT_PRED to figure out which predicate should be used.
				auto FoundPseudoPred = std::find_if(Unilatch->instr_begin(), Unilatch->instr_end(), [&](auto &Instr){
					return Instr.getOpcode() == Patmos::PSEUDO_UNILATCH_EXIT_PRED;
				});
				assert(FoundPseudoPred != Unilatch->instr_end());
				auto ExitPred = FoundPseudoPred->getOperand(0).getReg();
				assert(ExitPred.isVirtual() && MF.getRegInfo().getRegClass(ExitPred) == &Patmos::PRegsRegClass);
				// Erase pseudo-instruction so it doesn't need to be handled elsewhere
				Unilatch->erase(FoundPseudoPred);

				// Replace branch with predicated version on the unilatch's predicate
				Unilatch->remove(&*Unilatch->getFirstInstrTerminator());
				BuildMI(*Unilatch, Unilatch->getFirstInstrTerminator(), DL,
					TII->get(Patmos::BR))
					.addReg(ExitPred).addImm(0)
					.addMBB(&HeaderMbb);

				// Remove the PSEUDO_COUNTLESS_SPLOOP instruction, since it is no longer needed
				// and so we don't need to handle it in other passes
				auto FoundPsuedo = std::find_if(HeaderMbb.instr_begin(), HeaderMbb.instr_end(), [&](auto &Instr){
					return Instr.getOpcode() == Patmos::PSEUDO_COUNTLESS_SPLOOP;
				});
				assert(FoundPsuedo != HeaderMbb.instr_end());
				HeaderMbb.erase(FoundPsuedo);
			}
		}

		linearizeScope(getAnalysis<PatmosSinglePathInfo>().getRootScope());
		mergeMBBs(MF);

		// Clear all flags from general purpose register operands since they might not be correct any more.
		// E.g. if a register is killed on one path, it might not be so on the other paths. Since we are now
		// in single-path code, the kill would interfere with a live register from other paths.
		// The only flag we keep is the "implicit", since we need it for calls etc.
		// We also set the "undef" for all registers to ensure we don't get "using undefined register" errors.
		std::for_each(MF.begin(), MF.end(), [&](auto &Mbb){
			std::for_each(Mbb.begin(), Mbb.end(), [&](MachineInstr &Instr){
				for(auto &Op: Instr.operands()) {
					if(Op.isReg() && Op.isUse() && Patmos::RRegsRegClass.contains(Op.getReg())) {
						Op.ChangeToRegister(Op.getReg(), false, Op.isImplicit(), false, false, true, false);
					}
				}
			});
		});

		assert(MF.verify());

		LLVM_DEBUG(
			dbgs() << "[Single-Path] Linearizing end " << MF.getFunction().getName() << "\n";
		);
		Changed |= true;
	}
	return Changed;
}


MachineBasicBlock* Linearizer::linearizeScope(SPScope *S, MachineBasicBlock* LastBlock)
{
	auto Blocks = S->getBlocksTopoOrd();

	for(auto *Block: Blocks){
	    auto *MBB = Block->getMBB();
	    if (S->isSubheader(Block)) {
			LastBlock = linearizeScope(S->findScopeOf(Block), LastBlock);


	    } else {
			if(MBB->succ_size() == 1 && *MBB->succ_begin() == S->getHeader()->getMBB()) {
				// This is the unilatch block, don't mess with branches
			} else {
				// remove all successors
				for ( MachineBasicBlock::succ_iterator SI = MBB->succ_begin();
								SI != MBB->succ_end();
								SI = MBB->removeSuccessor(SI) )
								; // no body

				// remove the branch at the end of MBB
				TII->removeBranch(*MBB);
			}

			if (LastBlock) {
				// add to the last MBB as successor
				LastBlock->addSuccessor(MBB);
				// move in the code layout
				MBB->moveAfter(LastBlock);
			}
			// keep track of tail
			LastBlock = MBB;
	    }
	}
	return LastBlock;
}

void Linearizer::mergeMBBs(MachineFunction &MF) {
	LLVM_DEBUG( dbgs() << "Function before block merge:\n"; MF.dump() );
	// first, obtain the sequence of MBBs in DF order (as copy!)
	// NB: have to use the version below, as some version of libcxx will not
	// compile it (similar to
	//    http://lists.cs.uiuc.edu/pipermail/cfe-commits/Week-of-Mon-20130325/076850.html)
	//std::vector<MachineBasicBlock*> order(df_begin(&MF.front()),
	//                                      df_end(  &MF.front()));
	std::vector<MachineBasicBlock*> Order;
	for (auto I = df_begin(&MF.front()), E = df_end(&MF.front()); I != E; ++I) {
		Order.push_back(*I);
	}

	auto I = Order.begin(), E = Order.end();

	MachineBasicBlock *BaseMBB = *I;
	LLVM_DEBUG( dbgs() << "Base MBB#" << BaseMBB->getNumber() << "\n" );
	// iterate through order of MBBs
	while (++I != E) {
		// get MBB of iterator
		MachineBasicBlock *MBB = *I;

		if (BaseMBB->succ_size() > 1) {
			// we have encountered a backedge

			// Create an unconditional branch to the next block to exit the loop
			DebugLoc DL;
			BuildMI(*BaseMBB, BaseMBB->end(), DL,
				TII->get(Patmos::BRu))
				.addReg(Patmos::P0).addImm(0)
				.addMBB(MBB);

			BaseMBB = MBB;
			LLVM_DEBUG( dbgs() << "Base MBB#" << BaseMBB->getNumber() << "\n" );
		} else if (MBB->pred_size() == 1) {
			LLVM_DEBUG( dbgs() << "  Merge MBB#" << MBB->getNumber() << "\n" );
			// transfer the instructions
			BaseMBB->splice(BaseMBB->end(), MBB, MBB->begin(), MBB->end());
			// Any PHIs in MBB's successors are redirected to base
			std::for_each(MBB->succ_begin(), MBB->succ_end(), [&](auto Succ){
				Succ->replacePhiUsesWith(MBB, BaseMBB);
			});
			// remove the edge between BaseMBB and MBB
			BaseMBB->removeSuccessor(MBB);
			// BaseMBB gets the successors of MBB instead
			BaseMBB->transferSuccessors(MBB);

			// remove MBB from MachineFunction
			MF.erase(MBB);
		} else {
			BaseMBB = MBB;
			LLVM_DEBUG( dbgs() << "Base MBB#" << BaseMBB->getNumber() << "\n" );
		}
	}
	// invalidate order
	Order.clear();
}

