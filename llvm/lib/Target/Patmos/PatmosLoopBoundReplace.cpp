//===-- PatmosLoopBoundPropagation.cpp - Propagate the loop bound. -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// It is important to distinguish between inside and outside loop bounds.
// Thus at the beginning of the algorithm we must for each function, determine
// if the loop bound is inside or outside.
//
// When this is done, we look at every function call.
// If the loopbound is outside defined and values are not immediate, wer report
// and error. If loop is defined and value is immediate, then we can propagate
// normallu
//
// If the loopbound is defined inside the function, then we must still analyse
// it
//
//===----------------------------------------------------------------------===//

#include "PatmosCallGraphBuilder.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosStackCacheAnalysis2.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

#include <fstream>
#include <map>
#include <set>
#include <sstream>

#define DEBUG_TYPE "patmos-loop-bound-propagation"

// INITIALIZE_PASS(PatmosStackCacheAnalysisInfo2, "scainfo2",
//                 "Stack Cache Analysis Info 2", false, true)
// namespace llvm {
// char PatmosStackCacheAnalysisInfo2::ID = 0;
//
// ModulePass *createPatmosStackCacheAnalysisInfo2(const PatmosTargetMachine
// &tm) {
//   return new PatmosStackCacheAnalysisInfo2(tm);
// }
// } // namespace llvm

namespace llvm {

/// Link context-sensitive information on the spill costs at a call graph
/// nodes to calling contexts.
/// Context-sensitive information on the spill costs at a call graph node.
/// A graph representing context-sensitive information on the spill costs at
/// call graph nodes -- aka SCA graph or SC-SCA graph.
/// Information concerning a specific SCC.
/// Map function names to SCC infos.

/// Class to parse bounds specifications constraining paths through SCCs in
/// the call graph during ILP solving.
/// Pass to analyze the occupancy and displacement of Patmos' stack cache.
class PatmosLoopBoundReplace : public ModulePass {
private:
public:
  /// Pass ID
  static char ID;

  PatmosLoopBoundReplace() : ModulePass(ID) {}

  /// getAnalysisUsage - Inform the pass manager that nothing is modified.
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {}

  /// runOnModule - determine the state of the stack cache for each call site.
  bool runOnModule(Module &M) override;

  /// getPassName - Return the pass' name.
  StringRef getPassName() const override { return "Patmos Loop Bound Replace"; }

  llvm::Optional<CallInst *> findVarLoopBoundCall(llvm::Function &F);
  std::pair<bool, int> isLoopBoundInternal(Function *F, Value *Arg);
  bool instructionIsLoadGlobalInternal(Instruction *Inst);
  int valueIsInArguments(Value *V, Function *F);
};

char PatmosLoopBoundReplace::ID = 0;
} // namespace llvm

ModulePass *llvm::createPatmosLoopBoundReplace() {
  return new PatmosLoopBoundReplace();
}

bool isVarBoundCall(llvm::Function *CalledFunction) {
  return (CalledFunction && CalledFunction->getName() == "llvm.loop.varbound");
}

llvm::Optional<CallInst *>
PatmosLoopBoundReplace::findVarLoopBoundCall(llvm::Function &Function) {
  llvm::dbgs() << "Finding variable loop bound call in function "
               << Function.getName() << "\n";

  CallInst *CallInstruction;
  bool AlreadyFound = false;

  for (BasicBlock &BasicBlock : Function) {
    for (Instruction &I : BasicBlock) {
      if (CallInst *CI = dyn_cast<CallInst>(&I)) {
        llvm::Function *CalledFunction = CI->getCalledFunction();
        if (isVarBoundCall(CalledFunction)) {
          if (AlreadyFound) {
            report_fatal_error("More than one llvm.loop.varbound in function");
          }
          CallInstruction = CI;
          AlreadyFound = true;
        }
      }
    }
  }

  if (AlreadyFound) {
    return CallInstruction;
  }

  return None;
}

bool PatmosLoopBoundReplace::instructionIsLoadGlobalInternal(
    Instruction *Inst) {
  /// First check if this is a load inst
  if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
    llvm::dbgs() << "This is a load instruction\n";
    Value *LoadedValue = LI->getPointerOperand();
    if (GlobalVariable *GV = dyn_cast<GlobalVariable>(LoadedValue)) {
      llvm::dbgs() << "This is a load instruction, loading a global variable\n";
      /// The two last conditions are used to determine if we are loading from
      /// an internal global value or not. An external value would be, for
      /// instance, the "%x" in `define i32 main(i32 @x)`. This does not qualify
      /// as a global variable as it can change between different program
      /// executions.
      // llvm::dbgs() << "cond 1" << (LI->getType() == GV->getValueType()) <<
      // '\n'; llvm::dbgs() << "cond 2" << GV->hasExternalLinkage() << '\n';
      // llvm::dbgs() << "cond 3" << GV->hasInitializer() << '\n';
      //  TODO: Might be broken with external global
      if (LI->getType() == GV->getValueType() and GV->hasInitializer()) {
        // TODO: Check that GV is inde
        llvm::dbgs() << "Loading from global internal constant" << *GV << '\n';
        /// If this is the case return true;
        return true;
      }
    }
  }

  return false;
}

// NOTE: At least change the name, it looks like a boolean function here
int PatmosLoopBoundReplace::valueIsInArguments(Value *V, Function *F) {
  llvm::dbgs() << "Searching for value " << *V << " in arguments of "
               << F->getName() << "\n";
  auto Index = 0;
  for (auto &Arg : F->args()) {
    if (&Arg == V)
      break;
    ++Index;
  }
  if (Index < F->arg_size()) {
    llvm::dbgs() << " returning index " << Index << '\n';
    return Index;
  }
  return -1;
}

std::pair<bool, int> PatmosLoopBoundReplace::isLoopBoundInternal(Function *F,
                                                                 Value *Arg) {

  // TODO: Unclear
  /// Start from the instruction determining loopbound, then go up by queuing
  /// every operand of this instructio
  llvm::dbgs() << "Checking if the loopbound in " << F->getName()
               << " is defined internally or externally \n";
  std::queue<Value *> Worklist;
  Worklist.push(Arg);

  /// Check if there is an instruction assignment for this function
  while (not Worklist.empty()) {
    /// Pop the first element
    Value *V = Worklist.front();
    Worklist.pop();
    llvm::dbgs() << " Inspecting value : " << *V << "\n";

    /// Is there any instruction defining it ?
    // TODO: Not sure about this, it seems too hacky
    if (Instruction *Inst = dyn_cast<Instruction>(V)) {

      /// If the instruction is loading an internal global value, then it is
      /// defined internally
      if (instructionIsLoadGlobalInternal(Inst)) {
        return std::make_pair(true, -1);
      }

      /// Handling PHI instructions
      /// This means that the LB definition could be internal, but ambiguous,
      /// thus we will track phi
      if (isa<PHINode>(Inst)) {
        llvm::dbgs() << "Loopbound defined by a phi instruction !\n";
        assert(Inst->getNumOperands() == 2);

        llvm::dbgs() << "Op 1 : " << *(Inst->getOperand(0));
        llvm::dbgs() << "Op 2 : " << *(Inst->getOperand(1));
      }

      /// Iterate through each operand, if it is an immediate value do nothing,
      /// otherwise add it to the worklist
      bool Pushed = false;
      for (Value *Op : Inst->operand_values()) {
        if (ConstantInt *ConstInt = dyn_cast<ConstantInt>(Op)) {
          continue;
        }
        /// If the operand is not a ConstantInt, push to worklist
        dbgs() << " Pushing " << *Op << " to worklist\n";
        if (not Pushed) {
          Worklist.push(Op);
          Pushed = true;
        } else {

          report_fatal_error(
              "Loopbound defined with more than one variable not handled yet");
        }
      }

    } else {
      dbgs() << "  Value not defined by an instruction\n";
      /// Try to find it in argument
      auto Index = valueIsInArguments(V, F);
      if (Index >= 0) {
        /// We have one of the variable used to computer our loopbound variable
        /// in the argument list -> Loopbound is external
        dbgs() << "  Value found in the argument list " << *V << " at index "
               << Index << '\n';
        return std::make_pair(false, Index);
      }
      /// This should never happen
      report_fatal_error("Loopbound is not defined by any instruction, and "
                         "is not part of the function arguments");
    }
  }

  report_fatal_error("Should have returned from worklist");
}

// TODO: It could be a machine function
bool PatmosLoopBoundReplace::runOnModule(Module &M) {

  std::vector<CallInst*> toReplace;
  std::vector<CallInst*> toReplaceWith;

  for (auto &F : M) {
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (auto *CI = dyn_cast<CallInst>(&I)) {
          if (auto *Callee = CI->getCalledFunction()) {
            if (Callee->getName() == "llvm.loop.varbound") {
              if (auto *Arg = dyn_cast<ConstantInt>(CI->getArgOperand(1))) {
                printf("Call to llvm.loop.varbound with const int as second argument");
                llvm::dbgs() << " call to llvm.loop.varbound with const int as "
                                "second argument";
                CI->dump();
                llvm::dbgs() << " call to llvm.loop.varbound with const int dbgs 2 ";
                Function *NewCallee = M.getFunction("llvm.loop.bound");
                if (NewCallee) {
                  llvm::outs() << " already exists";
                  CallInst *NewCI =
                      CallInst::Create(NewCallee, {CI->getArgOperand(0), CI->getArgOperand(1)}, "", CI);
                  toReplace.push_back(CI);
                  toReplaceWith.push_back(NewCI);
                } else {
                  llvm::dbgs()
                      << " llvm.loop.bound function was not declared\n";
                  std::vector<llvm::Type *> BoundTypes(
                      2, llvm::Type::getInt32Ty(M.getContext()));

                  auto *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(M.getContext()), BoundTypes, false);
                  NewCallee = llvm::Function::Create(
                      FT, llvm::Function::ExternalLinkage, "llvm.loop.bound",
                      F.getParent());
                  // We add attributes that ensure optimizations don't mess with
                  // the bounds
                  NewCallee->addFnAttr(llvm::Attribute::Convergent);
                  NewCallee->addFnAttr(llvm::Attribute::NoDuplicate);
                  NewCallee->addFnAttr(llvm::Attribute::NoInline);
                  NewCallee->addFnAttr(llvm::Attribute::NoRecurse);
                  NewCallee->addFnAttr(llvm::Attribute::NoMerge);
                  NewCallee->addFnAttr(llvm::Attribute::OptimizeNone);


                  llvm::Value *MinVal = llvm::ConstantInt::get(BoundTypes.at(0), 0);
                  llvm::Value *MaxVal = llvm::ConstantInt::get(BoundTypes.at(1), 20);
                  //auto *call_inst = llvm::CallInst::Create(FT, NewCallee, {MinVal, MaxVal});
                  auto *call_inst = CallInst::Create(NewCallee, {MinVal, MaxVal}, "", CI);
                  toReplace.push_back(CI);
                  toReplaceWith.push_back(call_inst);
                  continue;
                }
              }
            }
          }
        }
      }
    }
  }

  for (size_t i = 0; i < toReplace.size(); ++i) {
    toReplace[i]->replaceAllUsesWith(toReplaceWith[i]);
    toReplace[i]->eraseFromParent();
  }
  return true;

  /// Dumping Module
  //LLVM_DEBUG(dbgs() << "PatmosLoopBoundReplace : Dumping module\n");
  //M.dump();

  ///// Creating the llvm.var.bound function
  ///// TODO: Check if it already exists in module
  ///// if it is declared but not used, this do not work
  //FunctionType *FTy = FunctionType::get(
  //    Type::getVoidTy(M.getContext()),
  //    {Type::getInt32Ty(M.getContext()), Type::getInt32Ty(M.getContext())},
  //    false);
  //llvm::Function *NewFunction = Function::Create(
  //    FTy, GlobalValue::ExternalLinkage, "llvm.loop.bound", &M);

  //LLVM_DEBUG(dbgs() << "Listing all functions \n");
  //for (llvm::Function &Function : M.getFunctionList()) {
  //  llvm::dbgs() << Function.getName() << "\n";

  //  /// --- Proof of Concept
  //  if (Function.getName() == "add_to") {
  //    auto Result = this->findVarLoopBoundCall(Function);
  //    if (Result.hasValue()) {
  //      CallInst *CallInst = Result.getValue();
  //      CallInst->dump();
  //      CallInst->setCalledFunction(NewFunction);
  //      llvm::ConstantInt *NewArg =
  //          ConstantInt::get(CallInst->getContext(), APInt(32, 10));
  //      CallInst->setArgOperand(1, NewArg);
  //    }
  //  }
  //  /// --- End (Proof of Concept)

  //  auto CallInst = findVarLoopBoundCall(Function);
  //  if (CallInst.hasValue()) {
  //    llvm::dbgs() << "Function : " << Function.getName()
  //                 << " has  a call to llvm.var.loopbound\n";
  //    Value *LoopboundArg = CallInst.getValue()->getArgOperand(1);
  //    auto Result = isLoopBoundInternal(&Function, LoopboundArg);
  //  } else {
  //    llvm::dbgs() << " Not found !\n";
  //  }
  //  /// -- Finding llvm.loop.varbound calls :
  //  ///
  //  ///
  //  ///
  //}

  ///// Dumping Module
  //LLVM_DEBUG(
  //    dbgs() << "PatmosLoopBoundReplace : Dumping module at end of pass\n");
  //M.dump();

  //return false;
}

namespace llvm {} // end namespace llvm
