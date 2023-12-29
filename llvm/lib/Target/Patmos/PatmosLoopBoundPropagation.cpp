//===--- PatmosLoopBoundPropagation.cpp - Propagate loop bound. -----------===//
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
#include "SinglePath/ConstantLoopDominators.h"
#include "SinglePath/PatmosSinglePathInfo.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

#include <fstream>
#include <map>
#include <set>
#include <sstream>

#define DEBUG_TYPE "patmos-loop-bound-propagation"

namespace llvm {

class PatmosLoopBoundPropagation : public ModulePass {
private:
  const TargetInstrInfo &TII;
  const PatmosTargetMachine &TM;
  MachineModuleInfo *MMI; // contains map Function -> MachineFunction
  Module *M;
  // MCallGraph MCG;

  // TODO: private functions should be declared here,
  // there should be no "static" function, convert them all !

  // Returns true is a function should not be checked, for instance
  // llvm.loop.(var)bound or *_sp_ functions
  bool functionIsDisabled(Function &F);

  // Returns true is the Instruction is a load to an internal global variable
  bool instructionIsLoadGlobalInternal(Instruction *Inst);

  // Returns the index of value if it is in argument, -1 otherwise
  int valueIsInArguments(Value *V, Function *F);
      
  StoreInst *findMatchingStore(LoadInst *LI);

  bool isArithmeticOperation(Instruction *Instr);

  uint8_t getNConstantOperands(BinaryOperator *BinOp);

  std::pair<bool, int> computeLoopBoundIndex(Function *F, Value *Arg);

  bool isCallToVarLoopBound(Instruction &Instruction);

  std::vector<CallInst *> findVarLoopBoundCalls(Function &F);

  std::vector<CallInst *> determineFunctionCalls(Module &M,Function *TargetFunction);
 
  // determineFunctionCalls    
 // argIsImmediate            
 // getMaxLoopBoundValue      
 // endsWith                  
 // PatmosLoopBoundPropagation::rewriteLoopBoundInstruction
 // isInlineArgument          
 
public:
  /// Pass ID
  static char ID;

  PatmosLoopBoundPropagation(const PatmosTargetMachine &TM)
      : ModulePass(ID), TII(*TM.getInstrInfo()), TM(TM) {
    initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
  }

  /// getAnalysisUsage - Inform the pass manager that nothing is modified.
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesCFG();
    AU.addRequired<PatmosCallGraphBuilder>();
    AU.addRequired<MachineModuleInfoWrapperPass>();

    ModulePass::getAnalysisUsage(AU);
  }

  void buildPseudoLoopboundInstruction(MachineBasicBlock &MBB,
                                       MachineInstr *OldInstr,
                                       int64_t NewLoopBound);

  void findRegisterDefInFunction(MachineFunction &MF, Register &Register);

  /// Try to extract value from register
  int64_t extractValueFromRegister(MachineFunction &MF, Register &Register);

  std::int64_t getValueFromGlobalVariable(const GlobalVariable *GV);

  void handlePhiInstr(MachineInstr &MI, MachineFunction &MF);

  void rewriteLoopBoundInstruction(MachineFunction *MF,
                                   const int64_t NewLoopBound);

  /// runOnModule
  bool runOnModule(Module &M) override;

  /// getPassName - Return the pass name.
  StringRef getPassName() const override {
    return "Patmos TMP LoopBound Propagation";
  }
};

char PatmosLoopBoundPropagation::ID = 0;
} // namespace llvm


ModulePass *
llvm::createPatmosLoopBoundPropagation(const PatmosTargetMachine &TM) {
  return new PatmosLoopBoundPropagation(TM);
}


bool PatmosLoopBoundPropagation::functionIsDisabled(Function &F) {
  return (F.getName() == "llvm.loop.bound" or
          F.getName() == "llvm.loop.varbound" or F.getName() == "_start");
}


// TODO: It seems insane that this is not built in, do we really have to
// dyn_cast the entire world ?
bool PatmosLoopBoundPropagation::instructionIsLoadGlobalInternal(Instruction *Inst) {

  /// First check if this is a load inst
  if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
    // llvm::dbgs() << "  This is a load instruction\n";
    Value *LoadedValue = LI->getPointerOperand();
    if (GlobalVariable *GV = dyn_cast<GlobalVariable>(LoadedValue)) {
      // llvm::dbgs() << "  This is a load instruction, loading a global
      // variable\n";
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
        llvm::dbgs() << "  Loading from global internal constant" << *GV
                     << '\n';
        /// If this is the case return true;
        return true;
      }
    }
  }

  return false;
}


// TODO: At least change the name, it looks like a boolean function here
auto PatmosLoopBoundPropagation::valueIsInArguments(Value *V, Function *F) -> int {
  llvm::dbgs() << "    Searching for value " << *V << " in arguments\n";
  auto Index = 0;
  for (auto &Arg : F->args()) {
    llvm::dbgs() << "    Index : " << Index << "; Comparing to arg : " << Arg
                 << "\n";
    if (&Arg == V) {
      break;
    }
    ++Index;
  }
  if (Index < F->arg_size()) {
    llvm::dbgs() << "    Returning index " << Index << '\n';
    return Index;
  }
  return -1;
}


StoreInst * PatmosLoopBoundPropagation::findMatchingStore(LoadInst *LI) {
  llvm::dbgs() << "  Finding matching store to load instruciton : " << *LI
               << "\n";
  Value *LoadedValue = LI->getOperand(0);

  for (User *User : LoadedValue->users()) {
    if (Instruction *UserInst = dyn_cast<Instruction>(User)) {
      /// Check if the user instruction is a store
      if (StoreInst *SI = dyn_cast<StoreInst>(UserInst)) {
        /// Check if the store is storing to the same memory location
        if (SI->getPointerOperand() == LoadedValue) {
          /// Found the corresponding store instruction
          return SI;
        }
      }
    }
  }
}


// TODO: Does this really have to be a function
bool PatmosLoopBoundPropagation::isArithmeticOperation(Instruction *Instr) {
  /// Check if the instruction is a BinaryOperator
  if (BinaryOperator *BinOp = dyn_cast<BinaryOperator>(Instr)) {
    /// Check the opcode to determine the type of operation
    switch (BinOp->getOpcode()) {
    // TODO: add fallthrough comments
    case Instruction::Add:
    case Instruction::Sub:
    case Instruction::Mul:
    case Instruction::UDiv:
    case Instruction::SDiv:
      return true;
    default:
      /// Logical operators, Comparaison, Floating point operations, Bitwise
      /// Operation
      return false;
    }
  }

  // The instruction is not a BinaryOperator
  return false;
}


// TODO: We only need three values, since we only have three possibilities
// TODO: Isn't there already a way to do this built in llvm
uint8_t PatmosLoopBoundPropagation::getNConstantOperands(BinaryOperator *BinOp) {
  uint8_t NConstantOperands = 0;
  for (auto *Op : BinOp->operand_values()) {
    llvm::dbgs() << "  Determining if " << *Op << " is a constant value\n";
    if (ConstantInt *ConstInt = dyn_cast<ConstantInt>(Op)) {
      NConstantOperands += 1;
    }
  }
  return NConstantOperands;
}


std::pair<bool, int> PatmosLoopBoundPropagation::computeLoopBoundIndex(Function *F, Value *Arg) {
  LLVM_DEBUG(llvm::dbgs() << "Call to isLoopBoundInternalNew for function "
                          << F->getName() << "\n");

  /// Create and initialize queue
  std::queue<Value *> Worklist;
  Worklist.push(Arg);

  while (not Worklist.empty()) {

    /// Poping first element
    Value *V = Worklist.front();
    Worklist.pop();

    llvm::dbgs() << "\n  Inspecting value : " << *V << "\n";

    /// Getting the instruction, easier to determine its type afterwards
    Instruction *Instr = dyn_cast<Instruction>(V);
    if (Instr) {
      llvm::dbgs() << "  Value is defined by an instruction.\n";

      if (isArithmeticOperation(Instr)) {
        llvm::dbgs() << "  Instruction is arithmetic operation.\n";
        BinaryOperator *BinOp = dyn_cast<BinaryOperator>(Instr);
        int32_t NConstantArguments = getNConstantOperands(BinOp);
        llvm::dbgs() << "  Number of constant operands : " << NConstantArguments
                     << "\n";

        bool Op0IsConst = false;
        bool Op1IsConst = false;
        // TODO: This has to be completely reworked
        if (ConstantInt *ConstInt =
                dyn_cast<ConstantInt>(Instr->getOperand(0))) {
          llvm::dbgs() << "  Operand 0 is constant\n";
          Op0IsConst = true;
        }

        if (ConstantInt *ConstInt =
                dyn_cast<ConstantInt>(Instr->getOperand(1))) {
          llvm::dbgs() << "  Operand 1 is constant\n";
          Op1IsConst = true;
        }

        if (not Op0IsConst and Op1IsConst) {
          llvm::dbgs() << "  Second Operand is const, first is not.\n";
          Worklist.push(Instr->getOperand(0));
        }
      }

      /// Early test to determine if definition is internal
      if (instructionIsLoadGlobalInternal(Instr)) {
        return std::make_pair(true, -1);
      }

      if (isa<LoadInst>(Instr)) {
        auto *LI = dyn_cast<LoadInst>(Instr);
        StoreInst *SI = findMatchingStore(LI);
        if (SI) {
          llvm::dbgs() << "  Corresponding store : " << *SI << ", aborting\n";
          /// We should push the value in the worklist
          llvm::dbgs() << "  Pushing " << *SI->getOperand(0);
          Worklist.push(SI->getOperand(0));
        } else {
          llvm::dbgs()
              << "  Could not ffind corresponding matching store to our load\n";
        }
      }

      // TODO: Handle this case, it shouldn't be too hard,
      // as it was handled before refactoring.
      if (isa<PHINode>(Instr)) {
        report_fatal_error("PHI nodes are not handled yet.");
      }

      /// Preview of operands
      // llvm::dbgs() << "  Preview of Operands : \n";
      // for (Value *Op : Inst->operand_values()) {
      //   llvm::dbgs() << "  " << *Op << "\n";
      // }

      // TODO: Don't we need to split between unary and ternary operators

      /// Real iteration loop
      /// That is where we should update our worklist
      // for (auto *Op : Inst->operand_values()) {
      //   auto Pushed = 0;

      //  if (isa<AllocaInst>(Op)) {
      //    llvm::dbgs() << "Op is alloc, finding corresponding store
      //    operation";
      //  }

      //  /// If nothing was pushed
      //  if (Pushed == 0) {
      //    report_fatal_error("Nothing was pushed, that is a bit weird!");
      //  }
      //}

    } else {
      llvm::dbgs() << "  Value not defined by an instruction !\n";

      /// Try to find it in argument
      auto Index = valueIsInArguments(V, F);

      if (Index >= 0) {
        dbgs() << "    Value found in the argument list " << *V << " at index "
               << Index << '\n';
        return std::make_pair(false, Index);
      }
      /// This should never happen
      report_fatal_error("Loopbound is not part of the function arguments");
    }
  }
  report_fatal_error("Not implemented yet");
}


bool PatmosLoopBoundPropagation::isCallToVarLoopBound(Instruction &Instruction) {
  if (CallInst *CI = dyn_cast<CallInst>(&Instruction)) {
    Function *CalledFunction = CI->getCalledFunction();
    if (CalledFunction && CalledFunction->getName() == "llvm.loop.varbound") {
      return true;
    }
  }
  return false;
}


std::vector<CallInst *> PatmosLoopBoundPropagation::findVarLoopBoundCalls(Function &F) {
  std::vector<CallInst *> Calls;

  for (BasicBlock &BB : F) {
    for (Instruction &I : BB) {
      if (isCallToVarLoopBound(I)) {
        Calls.push_back(dyn_cast<CallInst>(&I));
      }
    }
  }

  if (Calls.size() > 1) {
    dbgs() << F.getName() << " has " << Calls.size()
           << " calls to loopbound, printing them all.\n";
    for (auto *Instruction : Calls) {
      Instruction->dump();
    }
  }

  return Calls;
}


std::vector<CallInst *> PatmosLoopBoundPropagation::determineFunctionCalls(Module &M,
                                               Function *TargetFunction) {
  std::vector<CallInst *> CallInstructions;

  // TODO: We should print the machine function here.
  // Can we not use callgraphs

  for (Function &F : M) {
    if (&F == TargetFunction) {
      // Skip the target function itself
      continue;
    }

    for (BasicBlock &BB : F) {
      for (Instruction &I : BB) {
        if (auto *CI = dyn_cast<CallInst>(&I)) {
          Function *CalledFunction = CI->getCalledFunction();
          if (CalledFunction == TargetFunction) {
            // Function F calls the TargetFunction
            llvm::dbgs() << "Pushing into call instructions : " << F.getName()
                         << "\n";
            CallInstructions.push_back(CI);
          }
        }
      }
    }
  }

  return CallInstructions;
}


bool argIsImmediate(int Index, CallInst *CI) {
  /// Useless  sanity check here
  if (Index < CI->getNumArgOperands()) {
    Value *Arg = CI->getArgOperand(Index);
    if (ConstantInt *CIArg = dyn_cast<ConstantInt>(Arg)) {
      // The argument at the specified index is an immediate constant
      llvm::dbgs() << "Argument specified at index " << Index
                   << " of instruction " << *CI
                   << " is an immedidate constant.\n";
      return true;
    }
  }
  return false;
}


// Simply find the maximum value of a loopbound, given all the loopbound calls
int64_t getMaxLoopBoundValue(int Index,
                             const std::vector<CallInst *> CIVector) {
  auto MaxValue = 0;
  for (auto *CallInstruction : CIVector) {
    auto *ConstantInt =
        dyn_cast<llvm::ConstantInt>(CallInstruction->getArgOperand(Index));
    assert(ConstantInt);
    int64_t LoopBoundValue = ConstantInt->getSExtValue();
    if (LoopBoundValue > MaxValue) {
      llvm::dbgs() << "    Updating maximum LoopBound with new value "
                   << LoopBoundValue << "\n";
      MaxValue = LoopBoundValue;
    }
  }
  return MaxValue;
}


bool endsWith(const std::string &FullString, const std::string &Ending) {
  if (FullString.length() >= Ending.length()) {
    return (0 == FullString.compare(FullString.length() - Ending.length(),
                                    Ending.length(), Ending));
  }
  return false;
}


// TODO: I am sure this can be optimized, the control-flow looks wrong
void PatmosLoopBoundPropagation::rewriteLoopBoundInstruction(
    MachineFunction *MF, int64_t NewLoopBound) {
  MachineInstr *ToBeRemoved = nullptr;
  for (auto &MBB : *MF) {
    for (auto &MI : MBB) {
      if (MI.getOpcode() == Patmos::V_PSEUDO_LOOPBOUND) {
        dbgs() << " Found a Var loopbound call\n";
        MI.print(dbgs());
        ToBeRemoved = &MI;

        // TODO: It should not be 30 here
        // FIXME: Use the real value here
        DebugLoc DL = MI.getDebugLoc();
        BuildMI(MBB, MI, DL, TII.get(Patmos::PSEUDO_LOOPBOUND))
            .addImm(0)
            .addImm(NewLoopBound);
      }
    }
    if (ToBeRemoved) {
      MBB.remove_instr(ToBeRemoved);
      break;
    }
  }
}


bool isInlineArgument(int Index, CallInst *CI) {
  if (Index < CI->getNumArgOperands()) {
    Value *Arg = CI->getArgOperand(Index);
    if (ConstantInt *CIArg = dyn_cast<ConstantInt>(Arg)) {
      int Value = CIArg->getValue().getSExtValue();
      return true;
    }
  }
  return false;
}


bool PatmosLoopBoundPropagation::runOnModule(Module &M) {

  // Measure time now
  std::chrono::time_point<std::chrono::system_clock> StartTime = std::chrono::system_clock::now();

  LLVM_DEBUG(dbgs() << "PatmosLoopBoundPropagation : Start\n");

  MMI = &getAnalysis<MachineModuleInfoWrapperPass>().getMMI();

  // Here F is a simple function, and not a MachineFunction
  for (Function &F : M) {
    LLVM_DEBUG(dbgs() << "Iterating over function " << F.getName() << '\n');

    // Only work on "true" functions, not *sp_ or *pseudo_sp_
    if (functionIsDisabled(F) or endsWith(F.getName().str(), "_sp_")) {
      continue;
    }

    // Get all calls to llvm.loop.varbound in our function
    auto Calls = findVarLoopBoundCalls(F);
    LLVM_DEBUG(dbgs() << Calls.size() << " calls to llvm.loop.varbound\n");

    // If the function does not have any loopbound, we have nothing to do
    if (Calls.empty()) {
      continue;
    }

    // Getting all functions calls in our Module
    // TODO: Good that it is here, but it is not used yet
    std::vector<CallInst *> CIVector = determineFunctionCalls(M, &F);

    for (auto *VLBCallInst : Calls) {

      // Determine if call to llvm.loop.varbound is internal or external
      // bool LoopBoundIsInternal;
      // int Index;
      // std::tie(LoopBoundIsInternal, Index) =
      auto [LoopBoundIsInternal, Index] =
          computeLoopBoundIndex(&F, VLBCallInst->getArgOperand(1));

      // Debug print
      LLVM_DEBUG(dbgs() << F.getName()
                        << (LoopBoundIsInternal ? " have" : " do not have")
                        << " internal loopbound definition\n");

      // If it is internal, nothing to do
      // Everything has already been handled by isLoopBoundInternalNew
      if (LoopBoundIsInternal) {
        continue;
      }

      // If external and function is main, then it is an error,
      // Since main arguments are not known at compile time
      if (F.getName() == "main") {
        report_fatal_error(
            "A loopbound is defined using one of main() argument.");
      }

      // Check if function is called in the module
      // If not, then nothing to do
      if (CIVector.size() == 0) {
        assert(endsWith(F.getName().str(), "sp_") &&
               "Function called zero times");
        continue;
      }

      // Checking if loopbound is `immediate` in function call,
      // for each function call in our module
      auto IsImmediate = [&](auto *CallInst) {
        return argIsImmediate(Index, CallInst);
      };
      int CountImm = count_if(CIVector.begin(), CIVector.end(), IsImmediate);

      LLVM_DEBUG(dbgs() << "Number of CI with immediate arg : " << CountImm
                        << "\n");

      if (CountImm == CIVector.size()) {

        // Find maximum value for our loopbound
        int64_t MaxValue = getMaxLoopBoundValue(Index, CIVector);

        // Debug print
        llvm::MachineFunction *MF = MMI->getMachineFunction(F);
        LLVM_DEBUG(dbgs() << "Replacing with loopbounds " << MaxValue
                          << " in function " << F.getName() << "\n");

        // Replace V_PSEUDO_LOOPBOUND by PSEUDO_LOOPBOUND with a real value
        rewriteLoopBoundInstruction(MF, MaxValue);

        // Rewrite sp variant, later on the call to our function
        // will be rewriten to call the sp function
        StringRef SPVariant = StringRef((F.getName() + Twine("_sp_")).str());
        if (auto *SPF = M.getFunction(SPVariant)) {
          if (auto *SPMF = MMI->getMachineFunction(*SPF)) {
            rewriteLoopBoundInstruction(SPMF, MaxValue);
          }
        } else {
          LLVM_DEBUG(dbgs() << "sp variant of function not found\n");
        }

        dbgs() << "Printing function after PSEUDO_LOOPBOUND replacement\n";
        MF->dump();

      } else {
        report_fatal_error(
            "Some loopbound calls are not immediate values, which "
            "is not handled as of now");
      }
    }

    LLVM_DEBUG(dbgs() << "Dumping at end of LoopBoundPropagation\n");
    F.dump();
  }


  std::chrono::time_point<std::chrono::system_clock> EndTime = std::chrono::system_clock::now();

  auto MS = std::chrono::duration_cast<std::chrono::milliseconds>(EndTime - StartTime).count();
  llvm::outs() << "Total time taken by our pass : " << "\n"; 

  // TODO: return something meaningful here, or just return void 
  return false;
}

namespace llvm {} // end namespace llvm
