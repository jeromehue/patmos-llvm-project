//===-- PatmosTargetInfo.h - Patmos Target Implementation -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_PATMOS_TARGETINFO_PATMOSTARGETINFO_H
#define LLVM_LIB_TARGET_PATMOS_TARGETINFO_PATMOSTARGETINFO_H

#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/ADT/SmallString.h"

namespace llvm {

class Target;

Target &getThePatmosTarget();

/// getMBBIRName - Get a name for the MBB that reflects its identity in the
/// LLVM IR. If there is no mapping from IR code, an <anonymous> is used.
/// For uniqueness, the function name and current MBB number are included.
/// Format: #FunctionName#BasicBlockName#MBBNumber
void getMBBIRName(const MachineBasicBlock *MBB,
                         SmallString<128> &result);

/// Extracts loop bound information from the PSEUDO_LOOPBOUND instruction
/// if available in the given block
///
/// The first element is the minimum iteration count.
/// The second element is the maximum iteration count.
Optional<std::pair<uint64_t, uint64_t>> getLoopBounds(const MachineBasicBlock * MBB);

/// Return the register holding the maximum iteration count if V_PSEUDO_LOOPBOUND instruction
/// is available in the given block.
Optional<Register> getVLoopBounds(const MachineBasicBlock * MBB);

Optional<Register> getMininumIterationCountRegister(const MachineBasicBlock * MBB);

Optional<Register> getMaximumIterationCountRegister(const MachineBasicBlock * MBB);

/// Extracts loop bound information from the (V|F)_PSEUDO_LOOPBOUND instruction
/// if available in the given block
Optional<std::pair<Optional<Register>, Register>> getVariableLoopBounds(const MachineBasicBlock* MBB);

const Function *getCallTarget(const MachineInstr *MI);

MachineFunction *getCallTargetMF(const MachineInstr *MI);

/// Returns true if the given opcode represents a load instruction.
/// Pseudo-instructions aren't considered.
bool isLoadInst(unsigned opcode);

/// Returns true if the given opcode represents a store instruction.
/// Pseudo-instructions aren't considered.
bool isStoreInst(unsigned opcode);

} // namespace llvm

#endif // LLVM_LIB_TARGET_PATMOS_TARGETINFO_PATMOSTARGETINFO_H
