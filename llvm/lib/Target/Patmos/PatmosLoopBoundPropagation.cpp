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
#include "TargetInfo/PatmosTargetInfo.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/ReachingDefAnalysis.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

#include "DFA.h"

#define DEBUG_TYPE "patmos-loop-bound-propagation"

namespace llvm {

class ReachingInfo : public Info {
private:
  std::set<unsigned> InfoSet;

public:
  ReachingInfo() {}

  ReachingInfo(std::set<unsigned> S) { InfoSet = S; }

  // getter and setter
  std::set<unsigned> &getInfoSet() { return InfoSet; }

  void setInfoSet(std::set<unsigned> Set) { InfoSet = Set; }

  void print() {
    for (auto it = InfoSet.begin(); it != InfoSet.end(); ++it) {
      llvm::dbgs() << *it << '|';
    }

    llvm::dbgs() << '\n';
  }

  static bool equals(ReachingInfo *info1, ReachingInfo *info2) {
    return info1->getInfoSet() == info2->getInfoSet();
  }

  static ReachingInfo *join(ReachingInfo *info1, ReachingInfo *info2,
                            ReachingInfo *result) {
    std::set<unsigned> retSet = info1->getInfoSet();
    std::set<unsigned> info2Set = info2->getInfoSet();

    for (auto it = info2Set.begin(); it != info2Set.end(); ++it) {
      retSet.insert(*it);
    }

    result->setInfoSet(retSet);

    return nullptr;
  }
};

// Templated on <Info Info, bool direction>
class ReachingDefinitionAnalysis : public DataFlowAnalysis<ReachingInfo, true> {
private:
  typedef std::pair<unsigned, unsigned> Edge;
  std::map<Edge, ReachingInfo *> EdgeToInfo;
  // Maping each to category
  std::map<std::string, int> opToCategory = {
      {"alloca", 1}, {"load", 1},   {"select", 1},
      {"icmp", 1},   {"fcmp", 1},   {"getelementptr", 1},
      {"br", 2},     {"switch", 2}, {"store", 2},
      {"phi", 3}};

public:
  ReachingDefinitionAnalysis(ReachingInfo &bottom, ReachingInfo &initialState)
      : DataFlowAnalysis(bottom, initialState) {}

  void flowfunction(Instruction *I, std::vector<unsigned> &IncomingEdges,
                    std::vector<unsigned> &OutgoingEdges,
                    std::vector<ReachingInfo *> &Infos) {

    std::string OpName = I->getOpcodeName();

    // Get category of this instruction
    int Category = opToCategory.count(OpName) ? opToCategory[OpName] : 2;
    Category = I->isBinaryOp() ? 1 : Category;
    unsigned instrIdx = getInstrToIndex()[I];

    std::map<Edge, ReachingInfo *> edgeToInfo = getEdgeToInfo();
    ReachingInfo *OutInfo = new ReachingInfo();

    // Join all incoming infos
    for (size_t i = 0; i < IncomingEdges.size(); ++i) {
      Edge inEdge = std::make_pair(IncomingEdges[i], instrIdx);
      ReachingInfo::join(OutInfo, edgeToInfo[inEdge], OutInfo);
    }

    // Join index/indices for category 1 and 3
    if (Category == 1) {
      std::set<unsigned> Current = {instrIdx};
      ReachingInfo::join(OutInfo, new ReachingInfo(Current), OutInfo);
    }

    else if (Category == 3) {
      Instruction *firstNonPhi = I->getParent()->getFirstNonPHI();
      unsigned firstNonPhiIdx = getInstrToIndex()[firstNonPhi];
      std::set<unsigned> current;

      for (unsigned i = instrIdx; i < firstNonPhiIdx; ++i) {
        current.insert(i);
      }

      ReachingInfo::join(OutInfo, new ReachingInfo(current), OutInfo);
    }

    // Distribute result to outgoing edges
    for (size_t i = 0; i < OutgoingEdges.size(); ++i) {
      Infos.push_back(OutInfo);
    }
  }
};

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

  std::vector<CallInst *> determineFunctionCalls(Module &M,
                                                 Function *TargetFunction);

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

// NOTE: There should be something better
bool PatmosLoopBoundPropagation::functionIsDisabled(Function &F) {
  return (F.getName() == "llvm.loop.bound" or
          F.getName() == "llvm.loop.varbound" or F.getName() == "_start");
}

// TODO: It seems insane that this is not built in, do we really have to
// dyn_cast the entire world ?
bool PatmosLoopBoundPropagation::instructionIsLoadGlobalInternal(
    Instruction *Inst) {

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
auto PatmosLoopBoundPropagation::valueIsInArguments(Value *V, Function *F)
    -> int {
  LLVM_DEBUG(dbgs() << "    Searching for value " << *V << " in arguments\n");
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

StoreInst *PatmosLoopBoundPropagation::findMatchingStore(LoadInst *LI) {
  // llvm::dbgs() << "  Finding matching store to load instruciton : " << *LI
  //              << "\n";
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
uint8_t
PatmosLoopBoundPropagation::getNConstantOperands(BinaryOperator *BinOp) {
  uint8_t NConstantOperands = 0;
  for (auto *Op : BinOp->operand_values()) {
    // llvm::dbgs() << "  Determining if " << *Op << " is a constant value\n";
    if (ConstantInt *ConstInt = dyn_cast<ConstantInt>(Op)) {
      NConstantOperands += 1;
    }
  }
  return NConstantOperands;
}

void PatmosLBPropagate(Function *F, Value *LB) {
  // Where LB is the value that is used as a loopbound in function F.
  LLVM_DEBUG(dbgs() << "debugging");
}

std::pair<bool, int>
PatmosLoopBoundPropagation::computeLoopBoundIndex(Function *F, Value *Arg) {
  LLVM_DEBUG(llvm::dbgs() << "Call to isLoopBoundInternalNew for function "
                          << F->getName() << "\n");

  LLVM_DEBUG(dbgs() << " Value specified as loop bound : " << Arg->getName()
                    << '\n');

  auto Index = valueIsInArguments(Arg, F);
  if (Index >= 0) {
    LLVM_DEBUG(dbgs() << "Loopbound is part of the function arguments\n");
    return std::make_pair(false, Index);
  }
  //if (auto *arg = dyn_cast<llvm::Argument>(Arg)) {
  //  LLVM_DEBUG(dbgs() << "Loopboundis part of the function arguments ??? \n");
  //  for (auto &argInFunc : F->args()) {
  //    if (arg == &argInFunc) {
  //      LLVM_DEBUG(dbgs() << "Loopboundis part of the function arguments\n");
  //    }
  //  }
  //}

  // Run reaching definition analysis
  ReachingInfo Bottom;
  ReachingInfo InitialState;
  ReachingDefinitionAnalysis *RD =
      new ReachingDefinitionAnalysis(Bottom, InitialState);
  RD->runWorklistAlgorithm(F);

  // NOTE: For debugging purposes, we can do bette than this print, for instance
  // by printing every instruction with their number before.
  LLVM_DEBUG(llvm::dbgs() << "Done running worklist algorithm\n");
  RD->print();

  // Get the instruction defining Arg
  LLVM_DEBUG(dbgs() << "While loop for iterating over instructions of function F\n");

  Instruction *InstructionB;

  // What is this
  for (auto &BB : *F) {
    for (auto &Instr : BB) {
      if (&Instr == Arg) {
        InstructionB = &Instr;
        break;
      }
    }
  }

  LLVM_DEBUG(dbgs() << " Matching, at index \n"
                    << RD->getInstrToIndex()[InstructionB] << '\n');

  // List of instructions that our variable is depending on.
  std::queue<llvm::Instruction *> WL;
  WL.push(InstructionB);

  std::map<std::string, uint8_t> OpToCategory = {
      {"alloca", 1}, {"load", 1},   {"select", 1},
      {"icmp", 1},   {"fcmp", 1},   {"getelementptr", 1},
      {"br", 2},     {"switch", 2}, {"store", 2},
      {"phi", 3}};


  // TODO: Handle PHI nodes
  while (not WL.empty()) {
    assert(not WL.empty());
    llvm::Instruction *I = WL.front();
    WL.pop();


    //auto Index = valueIsInArguments(V, F);
    //if (Index >= 0) {
    //  LLVM_DEBUG(dbgs() << "Loopbound is part of the function arguments\n");
    //  return std::make_pair(false, Index);
    //}

    LLVM_DEBUG(dbgs() << "Inspecting Intruction : "
                      << *I
                      << '\n');

    // Categorize instructions
    int Category = OpToCategory.count(I->getOpcodeName())
                       ? OpToCategory[I->getOpcodeName()]
                       : 2;
    if (I->isBinaryOp()) {
      Category = 1;
    }

    // We are only handling categories that return a value
    if (Category != 1) {
      
      if(isa<StoreInst>(I)) {
        if(isa<ConstantInt>(I->getOperand(0))) {
          LLVM_DEBUG(dbgs() << " Is a store instruction of const int detected\n");
          return {/*isInternal*/ true, /*Index*/ -1};
        } else {
          if(not isa<Instruction>(I->getOperand(0))) {
            // Let us hope it is in argument
            LLVM_DEBUG(dbgs() << F->getName() << '\n');
            if(valueIsInArguments(I->getOperand(0), F) >= 0) {
              return {true, Index};
              report_fatal_error("better, actually"); 
            }
            report_fatal_error("Not in arg list, not an instruction"); 
          }
          LLVM_DEBUG(dbgs() << "Pushing on worklist\n");
          WL.push(dyn_cast<Instruction>(I->getOperand(0)));
          continue;
        }
      }
      
      
      //report_fatal_error("Loopbound is defined by an instruction that is not "
      //                   "returning a value.");
    }

    if (isa<LoadInst>(I)) {
      if (instructionIsLoadGlobalInternal(I)) {
        return {/*isInternal*/ true, /*Index*/ -1};
      }

      // dyn_cast is sure to succeed
      StoreInst *SI = findMatchingStore(dyn_cast<LoadInst>(I));
      WL.push(SI);
      if (auto *StoredInstr = dyn_cast<Instruction>(SI->getOperand(0))) {
        LLVM_DEBUG(dbgs() << "Storing another variable : "
                          << RD->getInstrToIndex()[StoredInstr]
                          << '\n');
      }

      // Print the value being loaded

    } else if (isa<BinaryOperator>(I)) {
      LLVM_DEBUG(dbgs() << "Encountered a binary operator.\n");

      //LLVM_DEBUG(dbgs() << "Operand 0 : " << I->getOperand(0)->getName()
      //                  << '\n');
      if (not isa<Constant>(I->getOperand(0))) {
        LLVM_DEBUG(dbgs() << "Pushing to worklist\n");
        WL.push(dyn_cast<Instruction>(I->getOperand(0)));
      }

      LLVM_DEBUG(dbgs() << "Operand 1 : " << I->getOperand(1)->getName()
                        << '\n');
      if (not isa<Constant>(I->getOperand(1))) {
        LLVM_DEBUG(dbgs() << "Pushing to worklist\n");
        WL.push(dyn_cast<Instruction>(I->getOperand(1)));
      }
    } else {
      report_fatal_error(
          "Instruction defining loop bound is of unknown category");
    }
  }

  // if (instructionIsLoadGlobalInternal(Instr)) {
  //       return std::make_pair(true, -1);
  //     }

  /// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  /// Everything before this is line testing for debug purposes.

  report_fatal_error("returning from here is not implemented yet");
}

bool PatmosLoopBoundPropagation::isCallToVarLoopBound(
    Instruction &Instruction) {
  if (CallInst *CI = dyn_cast<CallInst>(&Instruction)) {
    Function *CalledFunction = CI->getCalledFunction();
    if (CalledFunction && CalledFunction->getName() == "llvm.loop.varbound") {
      return true;
    }
  }
  return false;
}

std::vector<CallInst *>
PatmosLoopBoundPropagation::findVarLoopBoundCalls(Function &F) {
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

std::vector<CallInst *>
PatmosLoopBoundPropagation::determineFunctionCalls(Module &M,
                                                   Function *TargetFunction) {
  std::vector<CallInst *> CallInstructions;

  // TODO: We should print the machine function here.
  // Can we not use callgraphs
  llvm::outs() << "Lookinfg for call of functions : "
               << TargetFunction->getName();

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
            // llvm::dbgs() << "Pushing into call instructions : " <<
            // F.getName()
            //             << "\n";
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
      // llvm::dbgs() << "Argument specified at index " << Index
      //             << " of instruction " << *CI
      //             << " is an immedidate constant.\n";
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
// ------------------------------------------------
// ------------------------------------------------
// Global Definitions
using MBBRegUnitDefs = TinyPtrVector<ReachingDef>;
using MBBDefsInfo = std::vector<MBBRegUnitDefs>;
using MBBReachingDefsInfo = SmallVector<MBBDefsInfo, 4>;

MBBReachingDefsInfo MBBReachingDefs;

/// Instruction that defined each register, relative to the beginning of the
/// current basic block.  When a LiveRegsDefInfo is used to represent a
/// live-out register, this value is relative to the end of the basic block,
/// so it will be a negative number.
using LiveRegsDefInfo = std::vector<int>;
LiveRegsDefInfo LiveRegs;

LoopTraversal::TraversalOrder TraversedMBBOrder;

/// Keeps clearance information for all registers. Note that this
/// is different from the usual definition notion of liveness. The CPU
/// doesn't care whether or not we consider a register killed.
using OutRegsInfoMap = SmallVector<LiveRegsDefInfo, 4>;
OutRegsInfoMap MBBOutRegsInfos;

unsigned NumRegUnits;

const int ReachingDefDefaultVal = -(1 << 20);

/// Current instruction number.
/// The first instruction in each basic block is 0.
int CurInstr;

const TargetRegisterInfo *TRI;

DenseMap<MachineInstr *, int> InstIds;

static bool isValidReg(const MachineOperand &MO) {
  return MO.isReg() && MO.getReg();
}

static bool isValidRegUse(const MachineOperand &MO) {
  return isValidReg(MO) && MO.isUse();
}

static bool isValidRegUseOf(const MachineOperand &MO, MCRegister PhysReg) {
  return isValidRegUse(MO) && MO.getReg() == PhysReg;
}

static bool isValidRegDef(const MachineOperand &MO) {
  return isValidReg(MO) && MO.isDef();
}

static bool isValidRegDefOf(const MachineOperand &MO, MCRegister PhysReg) {
  return isValidRegDef(MO) && MO.getReg() == PhysReg;
}


void enterBasicBlock(MachineBasicBlock *MBB) {
  // Special debug value
  LLVM_DEBUG(dbgs() << "Entering basic block");

  unsigned MBBNumber = MBB->getNumber();
  assert(MBBNumber < MBBReachingDefs.size() &&
         "Unexpected basic block number.");
  MBBReachingDefs[MBBNumber].resize(NumRegUnits);

  // Reset instruction counter in each basic block.
  CurInstr = 0;

  // Set up LiveRegs to represent registers entering MBB.
  // Default values are 'nothing happened a long time ago'.
  if (LiveRegs.empty())
    LiveRegs.assign(NumRegUnits, ReachingDefDefaultVal);

  // This is the entry block.
  if (MBB->pred_empty()) {
    for (const auto &LI : MBB->liveins()) {
      for (MCRegUnitIterator Unit(LI.PhysReg, TRI); Unit.isValid(); ++Unit) {
        // Treat function live-ins as if they were defined just before the first
        // instruction.  Usually, function arguments are set up immediately
        // before the call.
        if (LiveRegs[*Unit] != -1) {
          LiveRegs[*Unit] = -1;
          MBBReachingDefs[MBBNumber][*Unit].push_back(-1);
        }
      }
    }
    LLVM_DEBUG(dbgs() << printMBBReference(*MBB) << ": entry\n");
    return;
  }

  // Try to coalesce live-out registers from predecessors.
  for (MachineBasicBlock *Pred : MBB->predecessors()) {
    assert(unsigned(Pred->getNumber()) < MBBOutRegsInfos.size() &&
           "Should have pre-allocated MBBInfos for all MBBs");
    const LiveRegsDefInfo &Incoming = MBBOutRegsInfos[Pred->getNumber()];
    // Incoming is null if this is a backedge from a BB
    // we haven't processed yet
    if (Incoming.empty())
      continue;

    // Find the most recent reaching definition from a predecessor.
    for (unsigned Unit = 0; Unit != NumRegUnits; ++Unit)
      LiveRegs[Unit] = std::max(LiveRegs[Unit], Incoming[Unit]);
  }

  // Insert the most recent reaching definition we found.
  for (unsigned Unit = 0; Unit != NumRegUnits; ++Unit)
    if (LiveRegs[Unit] != ReachingDefDefaultVal)
      MBBReachingDefs[MBBNumber][Unit].push_back(LiveRegs[Unit]);
}

void processDefs(MachineInstr *MI) {
  // Debug statement
  LLVM_DEBUG(dbgs() << "  Call to processDefs\n");

  assert(!MI->isDebugInstr() && "Won't process debug instructions");

  unsigned MBBNumber = MI->getParent()->getNumber();
  assert(MBBNumber < MBBReachingDefs.size() &&
         "Unexpected basic block number.");

  for (auto &MO : MI->operands()) {
    if (!isValidRegDef(MO))
      continue;

    // This loop is a problem
    MO.dump();
    for (MCRegUnitIterator Unit(MO.getReg().asMCReg(), TRI); Unit.isValid();
         ++Unit) {
      // This instruction explicitly defines the current reg unit.
      LLVM_DEBUG(dbgs() << printReg(*Unit, TRI) << ":\t" << CurInstr << '\t'
                        << *MI);

      // How many instructions since this reg unit was last written?
      if (LiveRegs[*Unit] != CurInstr) {
        LiveRegs[*Unit] = CurInstr;
        MBBReachingDefs[MBBNumber][*Unit].push_back(CurInstr);
      }
    }
  }
  InstIds[MI] = CurInstr;
  ++CurInstr;
}

void reprocessBasicBlock(MachineBasicBlock *MBB) {
  unsigned MBBNumber = MBB->getNumber();
  assert(MBBNumber < MBBReachingDefs.size() &&
         "Unexpected basic block number.");

  // Count number of non-debug instructions for end of block adjustment.
  auto NonDbgInsts =
      instructionsWithoutDebug(MBB->instr_begin(), MBB->instr_end());
  int NumInsts = std::distance(NonDbgInsts.begin(), NonDbgInsts.end());

  // When reprocessing a block, the only thing we need to do is check whether
  // there is now a more recent incoming reaching definition from a predecessor.
  for (MachineBasicBlock *Pred : MBB->predecessors()) {
    assert(unsigned(Pred->getNumber()) < MBBOutRegsInfos.size() &&
           "Should have pre-allocated MBBInfos for all MBBs");
    const LiveRegsDefInfo &Incoming = MBBOutRegsInfos[Pred->getNumber()];
    // Incoming may be empty for dead predecessors.
    if (Incoming.empty())
      continue;

    for (unsigned Unit = 0; Unit != NumRegUnits; ++Unit) {
      int Def = Incoming[Unit];
      if (Def == ReachingDefDefaultVal)
        continue;

      auto Start = MBBReachingDefs[MBBNumber][Unit].begin();
      if (Start != MBBReachingDefs[MBBNumber][Unit].end() && *Start < 0) {
        if (*Start >= Def)
          continue;

        // Update existing reaching def from predecessor to a more recent one.
        *Start = Def;
      } else {
        // Insert new reaching def from predecessor.
        MBBReachingDefs[MBBNumber][Unit].insert(Start, Def);
      }

      // Update reaching def at end of of BB. Keep in mind that these are
      // adjusted relative to the end of the basic block.
      if (MBBOutRegsInfos[MBBNumber][Unit] < Def - NumInsts)
        MBBOutRegsInfos[MBBNumber][Unit] = Def - NumInsts;
    }
  }
}

void leaveBasicBlock(MachineBasicBlock *MBB) {
  LLVM_DEBUG(dbgs() << "Call to leaveBasicBlock\n");
  assert(!LiveRegs.empty() && "Must enter basic block first.");

  unsigned MBBNumber = MBB->getNumber();
  assert(MBBNumber < MBBOutRegsInfos.size() &&
         "Unexpected basic block number.");
  // Save register clearances at end of MBB - used by enterBasicBlock().
  MBBOutRegsInfos[MBBNumber] = LiveRegs;

  // While processing the basic block, we kept `Def` relative to the start
  // of the basic block for convenience. However, future use of this information
  // only cares about the clearance from the end of the block, so adjust
  // everything to be relative to the end of the basic block.
  for (int &OutLiveReg : MBBOutRegsInfos[MBBNumber])
    if (OutLiveReg != ReachingDefDefaultVal)
      OutLiveReg -= CurInstr;
  LiveRegs.clear();
}

void processBasicBlock(const LoopTraversal::TraversedMBBInfo &TraversedMBB) {
  // Debug
  LLVM_DEBUG(dbgs() << "Processing bassic block\n");
  TraversedMBB.MBB->dump();
  // End Debug
  //
  MachineBasicBlock *MBB = TraversedMBB.MBB;
  LLVM_DEBUG(dbgs() << printMBBReference(*MBB)
                    << (!TraversedMBB.IsDone ? ": incomplete\n"
                                             : ": all preds known\n"));

  if (!TraversedMBB.PrimaryPass) {
    // Reprocess MBB that is part of a loop.
    reprocessBasicBlock(MBB);
    return;
  }

  LLVM_DEBUG(dbgs() << "Primary Pass\n");
  enterBasicBlock(MBB);
  for (MachineInstr &MI :
       instructionsWithoutDebug(MBB->instr_begin(), MBB->instr_end()))
    processDefs(&MI);
  leaveBasicBlock(MBB);
}

// New function
void newProcessBasicBlock(const LoopTraversal::TraversedMBBInfo &TraversedMBB) {
  MachineBasicBlock *MBB = TraversedMBB.MBB;
  LLVM_DEBUG(dbgs() << " New process basic block");

  LLVM_DEBUG(dbgs() << printMBBReference(*MBB)
                    << (!TraversedMBB.IsDone ? ": incomplete\n"
                                             : ": all preds known\n"));
  if (!TraversedMBB.PrimaryPass) {
    // Reprocess MBB that is part of a loop.
    LLVM_DEBUG(llvm::dbgs() << "Reprocess MBB");
    reprocessBasicBlock(MBB);
    return;
  }

  std::vector<llvm::MachineOperand *> MBBDefs;
  // To rewrite
  for (MachineInstr &MI :
       instructionsWithoutDebug(MBB->instr_begin(), MBB->instr_end())) {
    MI.dump();
    MI.getOperand(0).dump();
    MBBDefs.push_back(&MI.getOperand(0));
  }

  LLVM_DEBUG(dbgs() << "At the end of BB, defined registers were :\n");

  for (auto &operand : MBBDefs) {
    operand->dump();
  }

  LLVM_DEBUG(dbgs() << "And that is all");

  return;
}

bool reachindDef(llvm::MachineFunction *MF) {

  // TRI, MF already initialized in MF.
  TRI = MF->getSubtarget().getRegisterInfo();
  // TODO: Arrete de crier
  LLVM_DEBUG(
      dbgs() << "********** CUSTOM REACHING DEFINITION ANALYSIS **********\n");
  MF->dump();
  NumRegUnits = TRI->getNumRegUnits();
  MBBReachingDefs.resize(MF->getNumBlockIDs());
  MBBOutRegsInfos.resize(MF->getNumBlockIDs());
  LoopTraversal Traversal;
  TraversedMBBOrder = Traversal.traverse(*MF);
  // --- Done with initialization

  for (LoopTraversal::TraversedMBBInfo TraversedMBB : TraversedMBBOrder)
    newProcessBasicBlock(TraversedMBB);

  LLVM_DEBUG(
      dbgs()
      << "********** CUSTOM REACHING DEFINITION ANALYSIS - Done **********\n");
  return false;
}

// NOTE: In the end, maybe a FunctionPass would be better
bool PatmosLoopBoundPropagation::runOnModule(Module &M) {

  LLVM_DEBUG(dbgs() << "PatmosLoopBoundPropagation : Start\n");

  MMI = &getAnalysis<MachineModuleInfoWrapperPass>().getMMI();

  for (Function &F : M) {

    if (functionIsDisabled(F) or endsWith(F.getName().str(), "_sp_")) {
      continue;
    }

    auto Calls = findVarLoopBoundCalls(F);

    if (Calls.empty()) {
      continue;
    }

    // Reaching definition Analysis
    ReachingInfo Bottom;
    ReachingInfo InitialState;
    ReachingDefinitionAnalysis *RD =
        new ReachingDefinitionAnalysis(Bottom, InitialState);

    RD->runWorklistAlgorithm(&F);
    // RD->print();

    std::vector<CallInst *> CIVector = determineFunctionCalls(M, &F);

    for (auto *VLBCallInst : Calls) {

      // Testing new stuff

      // auto* LB = VLBCallInst->getArgOperand(1);
      // LB->dump();
      // if(isa<ConstantInt>(LB)) {
      //   llvm::outs() << " LB is a constant int when called";
      // }
      
      auto* MaxIterReg = VLBCallInst->getArgOperand(1);

      PatmosLBPropagate(&F, MaxIterReg);
      // TODO: Maybe this could be separated in two different functions.
      auto [LoopBoundIsInternal, Index] = computeLoopBoundIndex(&F, MaxIterReg);

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
        llvm::dbgs() << " Debugging error\n";
        report_fatal_error("LoopBound variable cannot be proven constant");
      }
    }
    F.dump();
  }

  return false;
}

namespace llvm {} // end namespace llvm
