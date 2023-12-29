//===-- PatmosDelaySlotKiller.cpp - Patmos delay slot killer --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a simple dummy pass
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

#define DEBUG_TYPE "patmos-vlb-replace"


namespace {

  class PatmosMFReplace : public MachineFunctionPass {
  private:
    static char ID;
  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    PatmosTargetMachine &TM;
    const PatmosInstrInfo *TII;

    PatmosMFReplace(PatmosTargetMachine &TM)
      : MachineFunctionPass(ID), TM(TM),
        TII(static_cast<const PatmosInstrInfo*>(TM.getInstrInfo())) { }

    StringRef getPassName() const override {
      return "Patmos Replace MF";
    }

    bool runOnMachineFunction(MachineFunction &F) {
      bool Changed = false;
      LLVM_DEBUG(dbgs() << "running on machine function" << F.getName() << "\n");
      return Changed;
    }

  protected:

  };

  char PatmosMFReplace::ID = 0;
} // end of anonymous namespace

FunctionPass *llvm::createPatmosMFReplace(PatmosTargetMachine &TM) {
  return new PatmosMFReplace(TM);
}
