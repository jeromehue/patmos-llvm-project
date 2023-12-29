//===-- PatmosTargetInfo.cpp - Patmos Target Implementation ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosTargetInfo.h"
#include "PatmosRegisterInfo.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/Compiler.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/ADT/Optional.h"
#include "llvm/CodeGen/MachineModuleInfo.h"

using namespace llvm;


void llvm::getMBBIRName(const MachineBasicBlock *MBB,
                         SmallString<128> &result) {
  const BasicBlock *BB = MBB->getBasicBlock();
  const Twine bbname = (BB && BB->hasName()) ? BB->getName()
                                             : "<anonymous>";
  const Twine bb_ir_label = "#" + MBB->getParent()->getFunction().getName() +
                            "#" + bbname +
                            "#" + Twine(MBB->getNumber());
  bb_ir_label.toVector(result);
}

Optional<std::pair<uint64_t, uint64_t>> llvm::getLoopBounds(const MachineBasicBlock * MBB) {
  if(MBB && MBB->getBasicBlock()) {
    auto bound_instr = std::find_if(MBB->begin(), MBB->end(), [&](auto &instr){
      return instr.getOpcode() == Patmos::PSEUDO_LOOPBOUND;
    });
    if(bound_instr != MBB->end()) {
      assert(bound_instr->getOperand(0).isImm());
      assert(bound_instr->getOperand(1).isImm());
      assert(bound_instr->getOperand(0).isImm() >= 0);
      assert(bound_instr->getOperand(1).isImm() >= 0);

      auto min = bound_instr->getOperand(0).getImm() + 1;
      auto max = min + bound_instr->getOperand(1).getImm();

      // Check for overflows
      if(min < 0 || max < min) {
        report_fatal_error("Invalid loop bounds");
      }

      return std::make_pair((uint64_t) min, (uint64_t) max);
    }
  }
  return None;
}

Optional<Register> llvm::getVLoopBounds(const MachineBasicBlock * MBB) {
  if(MBB && MBB->getBasicBlock()) {
    auto LoopBoundInstruction = std::find_if(MBB->begin(), MBB->end(), [&](auto &Instr){
      return Instr.getOpcode() == Patmos::V_PSEUDO_LOOPBOUND;
    });
    if(LoopBoundInstruction != MBB->end()) {
      assert(LoopBoundInstruction->getOperand(0).isImm());
      assert(LoopBoundInstruction->getOperand(1).isReg());

      llvm::dbgs() << "Loopbound is register : " << LoopBoundInstruction->getOperand(1) << "\n";
      auto Max = LoopBoundInstruction->getOperand(1).getReg();
      return Max;
    }  
  }
  return None;
}

Target &llvm::getThePatmosTarget() {
  static Target ThePatmosTarget;
  return ThePatmosTarget;
}

const Function *llvm::getCallTarget(const MachineInstr *MI) {
  const Module *M = MI->getParent()->getParent()->getFunction().getParent();
  const MachineOperand &MO = MI->getOperand(2);
  const Function *Target = NULL;
  llvm::StringRef TargetName = "";
  if (MO.isGlobal()) {
    Target = dyn_cast<Function>(MO.getGlobal());
    if(!Target) {
      TargetName = MO.getGlobal()->getName();
    }
  } else if (MO.isSymbol()) {
    TargetName = MO.getSymbolName();
    Target = M->getFunction(TargetName);
  }

  if(!Target) {
    if(auto *alias = M->getNamedAlias(TargetName)) {
      Target = M->getFunction(alias->getAliasee()->getName());
    }
  }

  return Target;
}

MachineFunction *llvm::getCallTargetMF(const MachineInstr *MI) {
  auto *F = getCallTarget(MI);
  MachineFunction *MF;
  if (F && (MF = &MI->getParent()->getParent()->getMMI().getOrCreateMachineFunction((Function&)*F))) {
    return MF;
  }
  return NULL;
}

/// Returns true if the given opcode represents a load instruction.
/// Pseudo-instructions aren't considered.
bool llvm::isLoadInst(unsigned opcode) {
  switch( opcode ){
    default:
      return false;
    case Patmos::LBC:
    case Patmos::LBL:
    case Patmos::LBM:
    case Patmos::LBS:
    case Patmos::LBUC:
    case Patmos::LBUL:
    case Patmos::LBUM:
    case Patmos::LBUS:
    case Patmos::LHC:
    case Patmos::LHL:
    case Patmos::LHM:
    case Patmos::LHS:
    case Patmos::LHUC:
    case Patmos::LHUL:
    case Patmos::LHUM:
    case Patmos::LHUS:
    case Patmos::LWC:
    case Patmos::LWL:
    case Patmos::LWM:
    case Patmos::LWS:
      return true;
  }
}

/// Returns true if the given opcode represents a store instruction.
/// Pseudo-instructions aren't considered.
bool llvm::isStoreInst(unsigned opcode) {
  switch( opcode ){
    default:
      return false;
    case Patmos::SBC:
    case Patmos::SBL:
    case Patmos::SBM:
    case Patmos::SBS:
    case Patmos::SHC:
    case Patmos::SHL:
    case Patmos::SHM:
    case Patmos::SHS:
    case Patmos::SWC:
    case Patmos::SWL:
    case Patmos::SWM:
    case Patmos::SWS:
      return true;
  }
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializePatmosTargetInfo() {
  RegisterTarget<Triple::patmos> X(getThePatmosTarget(), "patmos",
                                   "Patmos [experimental]", "Patmos");
}
