//===-- PatmosIntrinsicElimination.cpp - Remove unused function declarations ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass makes the single-pat code utilitize Patmos' dual issue pipeline.
// TODO: more description
//
//===----------------------------------------------------------------------===//

#include "PatmosIntrinsicElimination.h"
#include "SinglePath/PatmosSinglePathInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "patmos-intrinsic-elimination"

char PatmosIntrinsicElimination::ID = 0;

FunctionPass *llvm::createPatmosIntrinsicEliminationPass() {
  return new PatmosIntrinsicElimination();
}

/// Performs the elimination of the given call to a memory intrinsic (llvm.memset/memcpy).
/// Must be provided with lambdas to control the produced substitution code.
/// The structure of the substitution is a loop.
/// It starts in an entry block which jumps to the loop condition.
/// The condition either branches to the loop body or the end block (epilogue).
/// The loop body jumps to the condition, while the end block
/// Jumps to the instruction after the eliminated call.
/// The given lambda can be used to add code to the various blocks
/// The lambdas to the entry and condition blocks must return
/// any object that is needed for the production of the other blocks.
/// the blocks are created in the same order as given in the argument list.
template <
  typename RetEntry,
  typename RetCondition,
  typename InsertEntry,
  typename InsertCondition,
  typename InsertBody,
  typename InsertEpilogue
>
static void eliminate(
    Function &F, BasicBlock &BB, BasicBlock::iterator instr_iter, llvm::Value* lenVal, uint32_t increment,
    InsertEntry InsertAtEntry,
    InsertCondition InsertAtCondition,
    InsertBody InsertAtBody,
    InsertEpilogue InsertAtEpilogue,
    StringRef LabelPrefix
) {

  // Create IR Builder
  IRBuilder<> Builder(F.getContext());

  uint64_t len;
  bool LenIsConst = false;

  // Check if length argument is a constant
  if(auto* memcpy_len = dyn_cast<ConstantInt>(lenVal)) {
    len = memcpy_len->getValue().getLimitedValue(std::numeric_limits<uint32_t>::max());
    LenIsConst = true;
  }

  // Assert length of memcpy is in limites
  if(len == std::numeric_limits<uint32_t>::max())
    report_fatal_error(LabelPrefix + " length argument is too large");

  // Create Basic Blocks for the loop
  auto *MemsetEntry = BasicBlock::Create(F.getContext(), LabelPrefix + ".entry", &F);
  auto *MemsetLoopCond = BasicBlock::Create(F.getContext(), LabelPrefix + ".loop.cond", &F);
  auto *MemsetLoopBody = BasicBlock::Create(F.getContext(), LabelPrefix + ".loop.body", &F);
  auto *MemsetLoopEnd = BasicBlock::Create(F.getContext(), LabelPrefix + ".loop.end", &F);

  Builder.SetInsertPoint(MemsetEntry);
  RetEntry EntryRet = InsertAtEntry(Builder, MemsetEntry);

  BranchInst::Create(MemsetLoopCond, MemsetEntry);

  Builder.SetInsertPoint(MemsetLoopCond);



  // Compute loop bound
  auto LoopBound = len/increment;
  auto EpilogueLen = len % increment;

  auto *IPhi = Builder.CreatePHI(Builder.getInt32Ty(), 2, LabelPrefix + ".i");
  if(LenIsConst) {
    IPhi->addIncoming(Builder.getInt32(LoopBound), MemsetEntry);
  } else {
    IPhi->addIncoming(lenVal, MemsetEntry);
  }

  RetCondition ConditionRet = InsertAtCondition(Builder, MemsetEntry, EntryRet, MemsetLoopCond);

  auto *ICmp = Builder.CreateICmpEQ(IPhi, ConstantInt::get(Builder.getInt32Ty(), 0), LabelPrefix + ".loop.finished");

  if(LenIsConst) {
    auto *LoopBoundFn = F.getParent()->getFunction("llvm.loop.bound");
    if(!LoopBoundFn) {
      // Loop bound function not declared yet. Declare it.
      std::vector<Type*> BoundTypes(2, Type::getInt32Ty(F.getContext()));
      FunctionType *FT = FunctionType::get(Type::getVoidTy(F.getContext()), BoundTypes, false);
      LoopBoundFn = Function::Create(FT, Function::ExternalLinkage, "llvm.loop.bound", F.getParent());
    }
    Builder.CreateCall(LoopBoundFn, {Builder.getInt32(LoopBound), Builder.getInt32(LoopBound)});

  } else {
    // Set loop bound for generated loop
    auto *VarLoopBoundFn = F.getParent()->getFunction("llvm.loop.varbound");

    if(!VarLoopBoundFn) {
      std::vector<Type*> BoundTypes(2, Type::getInt32Ty(F.getContext()));
      FunctionType *FT = FunctionType::get(Type::getVoidTy(F.getContext()), BoundTypes, false);
      VarLoopBoundFn = Function::Create(FT, Function::ExternalLinkage, "llvm.loop.varbound", F.getParent());
    }

    Builder.CreateCall(VarLoopBoundFn, {Builder.getInt32(0), lenVal});
  }
  



  auto *CondBr = BranchInst::Create(MemsetLoopEnd, MemsetLoopBody, ICmp, MemsetLoopCond);

  Builder.SetInsertPoint(MemsetLoopBody);

  auto *IDec = Builder.CreateSub(IPhi, Builder.getInt32(1), LabelPrefix + ".i.decremented");
  IPhi->addIncoming(IDec, MemsetLoopBody);

  InsertAtBody(Builder, MemsetEntry, EntryRet, MemsetLoopCond, ConditionRet, MemsetLoopBody);

  BranchInst::Create(MemsetLoopCond, MemsetLoopBody);

  if(EpilogueLen) {
    Builder.SetInsertPoint(MemsetLoopEnd);
    // Need epilogue
    InsertAtEpilogue(Builder,
        MemsetEntry, EntryRet,
        MemsetLoopCond, ConditionRet,
        MemsetLoopBody, MemsetLoopEnd, EpilogueLen);
  }
  Builder.SetInsertPoint((BasicBlock*)NULL);

  // Replace llvm.memset
  auto *Successor = BB.splitBasicBlock(instr_iter, "llvm.memset" + BB.getName() + ".continued");

  // Point the first half of the original block to the memset blocks
  cast<BranchInst>(BB.back()).setSuccessor(0, MemsetEntry);

  // If the original block has a loop bound instruction, ensure it is put in the previous half
  for(auto InstrIter = Successor->begin(); InstrIter != Successor->end(); InstrIter++){
    if(InstrIter->getOpcode() == Instruction::Call || InstrIter->getOpcode() == Instruction::CallBr){
      if (CallInst *II = dyn_cast<CallInst>(&*InstrIter)) {
        auto *called = II->getCalledFunction();
        if(called && called->getName() == "llvm.loop.bound"){
          Builder.SetInsertPoint(&*std::prev(BB.end()));
          Builder.CreateCall(called, {II->getArgOperand(0), II->getArgOperand(1)});
          Builder.SetInsertPoint((BasicBlock*)NULL);
          Successor->getInstList().erase(InstrIter);
          break;
        }
      }
    }
  }

  assert(isa<IntrinsicInst>(Successor->begin())); // This should be the call to intrinsic
  Successor->getInstList().pop_front(); // remove the intrinsic call

  BranchInst::Create(Successor, MemsetLoopEnd);
}









/// Checks that the given llvm.memset/memcpy is valid and should be eliminated.
/// If so, calls the given lambda (which is assumed to then call 'eliminate').
/// Returns true if the intrinsic was eliminated, false otherwise.
template<typename L>
static bool eliminate_mem_intrinsic(Function &F, IntrinsicInst *II, StringRef name, L should_eliminate_call) {
  assert(II->arg_size() >= 3); // We don't care about the volatile flag (4th arg)
  auto arg0 = II->getArgOperand(0);
  auto arg2 = II->getArgOperand(2);

  assert(cast<PointerType>(arg0->getType())->getAddressSpace() == 0);
  assert(arg0->getType()->getContainedType(0)->isIntegerTy(8));
  assert(arg2->getType()->isIntegerTy(32) || arg2->getType()->isIntegerTy(64));

  if(auto* memcpy_len = dyn_cast<ConstantInt>(arg2)) {
    auto len = memcpy_len->getValue().getLimitedValue(std::numeric_limits<uint32_t>::max());

    if(len <= 12) return false; // Too small to be worth it

    should_eliminate_call(arg0, arg2, arg2);
    return true;
  } else {
    if (PatmosSinglePathInfo::isEnabled(F)) {
      should_eliminate_call(arg0, arg2, arg2);
      //report_fatal_error("LoopBound variable cannot be proven constant");
      return true;
    }
  }
  return false;
}

/// Tries to eliminate 1 intrinsic from the given block.
/// If it finds one and successfully eliminates it, returns true.
/// An elimination results in changes to both the given block and the function.
/// If no intrinsic is found, or none was eliminated even if present, returns false.
static bool eliminateIntrinsic(Function &F, BasicBlock &BB) {
  for(auto instr_iter = BB.begin(), instr_iter_end = BB.end(); instr_iter != instr_iter_end; ++instr_iter){
    auto &instr = *instr_iter;

    if(instr.getOpcode() == Instruction::Call || instr.getOpcode() == Instruction::CallBr) {
      if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(&instr)) {
        switch (II->getIntrinsicID()) {
        case Intrinsic::memcpy: {
          llvm::errs() << " eliminating memcpy\n";
          auto arg1 = II->getArgOperand(1);

          assert(cast<PointerType>(arg1->getType())->getAddressSpace() == 0);
          assert(arg1->getType()->getContainedType(0)->isIntegerTy(8));

          if(eliminate_mem_intrinsic(F, II, "llvm.memcpy",
            [&](auto *arg0, auto *arg2, auto len){
              eliminate<
                int,                          // Not used
                std::pair<PHINode*,PHINode*>  // Returned by condition lambda
              >(
                  F, BB, instr_iter, len, 1,
                  [&](auto &builder, auto entry_block){
                    return 0;
                  },
                  [&](auto &builder, auto entry_block, auto entry_ret, auto condition_block){
                    auto *dest_phi = builder.CreatePHI(PointerType::get(builder.getInt8Ty(),0), 2, "llvm.memcpy.dest");
                    auto *src_phi = builder.CreatePHI(PointerType::get(builder.getInt8Ty(),0), 2, "llvm.memcpy.src");
                    dest_phi->addIncoming(arg0, entry_block);
                    src_phi->addIncoming(arg1, entry_block);
                    return std::make_pair(dest_phi, src_phi);
                  },
                  [&](auto &builder, auto entry_block, auto entry_ret, auto condition_block, auto cond_ret, auto body_block){
                    auto *dest_phi = std::get<0>(cond_ret);
                    auto *src_phi = std::get<1>(cond_ret);

                    auto *dest_inc = builder.CreateGEP(dest_phi, builder.getInt32(1), "llvm.memcpy.dest.incremented");
                    auto *src_inc = builder.CreateGEP(src_phi, builder.getInt32(1), "llvm.memcpy.src.incremented");
                    dest_phi->addIncoming(dest_inc, body_block);
                    src_phi->addIncoming(src_inc, body_block);

                    auto *to_cpy = builder.CreateAlignedLoad(builder.getInt8Ty(), src_phi, MaybeAlign(1), "llvm.memcpy.tmp");
                    builder.CreateAlignedStore(to_cpy, dest_phi, MaybeAlign(1));
                  },
                  [&](auto &builder,
                      auto entry_block, auto entry_ret,
                      auto condition_block, auto dest_phi,
                      auto body_block, auto end_block, auto epilogue_len
                  ){

                  },
                  "llvm.memcpy"
              );
            }
          )) {
            return true;
          }
          break;
        }
        case Intrinsic::memset: {
          auto arg1 = II->getArgOperand(1);

          assert(arg1->getType()->isIntegerTy(8));

          if(eliminate_mem_intrinsic(F, II, "llvm.memset",
            [&](auto *arg0, auto *arg2, auto len){
              eliminate<
                std::pair<Value*, Value*>,  // Returned by entry lambda
                PHINode*                    // Returned by condition lambda
              >(
                  F, BB, instr_iter, len, 4,
                  [&](auto &builder, auto entry_block){
                    // Prepare i32 version of value
                    Value *val_i32_done;
                    if(auto* set_to_const = dyn_cast<ConstantInt>(arg1)) {
                      auto set_to_val = set_to_const->getValue().getLimitedValue(std::numeric_limits<uint16_t>::max());
                      assert(set_to_val <= std::numeric_limits<uint8_t>::max() &&
                          "llvm.memset value to set to is out of range");

                      auto set_to_shl8 = set_to_val << 8;
                      auto set_to_half = set_to_shl8 + set_to_val;
                      auto set_to_upper = set_to_half << 16;
                      val_i32_done = builder.getInt32(set_to_upper + set_to_half);
                    } else {
                      auto* val_i32 = builder.CreateZExt(arg1, Type::getInt32Ty(F.getContext()));
                      auto* val_shl8 = builder.CreateShl(val_i32, builder.getInt32(8));
                      auto* val_halfword = builder.CreateAdd(val_shl8, val_i32);
                      auto* val_upper = builder.CreateShl(val_halfword, builder.getInt32(16));
                      auto *val_i32_done_instr = builder.CreateAdd(val_halfword, val_upper, "llvm.memset.set.to.word");
                      val_i32_done = val_i32_done_instr;
                    }
                    auto *dest_i32 = builder.CreateBitCast(arg0, PointerType::get(builder.getInt32Ty(),0), "llvm.memset.dest.i32");
                    return std::make_pair(dest_i32, val_i32_done);
                  },
                  [&](auto &builder, auto entry_block, auto entry_ret, auto condition_block){
                    auto *dest_phi = builder.CreatePHI(PointerType::get(builder.getInt32Ty(),0), 2, "llvm.memset.dest");
                    dest_phi->addIncoming(std::get<0>(entry_ret), entry_block);
                    return dest_phi;
                  },
                  [&](auto &builder, auto entry_block, auto entry_ret, auto condition_block, auto *dest_phi, auto body_block){
                    auto *dest_inc = builder.CreateGEP(dest_phi, builder.getInt32(1), "llvm.memset.dest.incremented");
                    dest_phi->addIncoming(dest_inc, body_block);
                    auto align = II->paramHasAttr(0, Attribute::Alignment) ?
                       II->getParamAttr(0, Attribute::Alignment).getAlignment()
                       : MaybeAlign(1);
                    // Note: Technically, an 'align' attribute without 'noundef' is undefined behaviour.
                    // However, we instead just assume its there. (since undefined behaviour allows us
                    // to do anything, we choose to treat it as if 'noundef' is present)
                    builder.CreateAlignedStore(std::get<1>(entry_ret), dest_phi, align);
                  },
                  [&](auto &builder,
                      auto entry_block, auto entry_ret,
                      auto condition_block, auto *dest_phi,
                      auto body_block, auto end_block, auto epilogue_len
                  ){
                    // Just call llvm.memset which would automatically be expanded by LLVM
                    auto *dest_phi_i8 = builder.CreateBitCast(dest_phi, PointerType::get(builder.getInt8Ty(),0), "llvm.memset.dest.i32.i8");

                    builder.CreateCall(II->getCalledFunction(),
                        {dest_phi_i8, arg1, ConstantInt::get(arg2->getType(), epilogue_len), builder.getFalse()}) ;

                  },
                  "llvm.memset"
              );
            })){
            return true;
          }
          break;
        }
        default:
          break;
        }
      }

    }
  }
  return false;
}

bool PatmosIntrinsicElimination::runOnFunction(Function &F) {

  for(auto BB_iter = F.begin(), BB_iter_end = F.end(); BB_iter != BB_iter_end; ++BB_iter){
    if(eliminateIntrinsic(F, *BB_iter)) {
      // Blocks have been created, the iterator is therefore no longer valid.
      // Restart.
      BB_iter = F.begin();
    }
  }

  return true;
}
