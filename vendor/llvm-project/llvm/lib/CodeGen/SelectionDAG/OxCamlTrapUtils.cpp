//===- OxCamlTrapUtils.cpp - OxCaml trap recovery helpers -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "OxCamlTrapUtils.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IntrinsicsAArch64.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

static const IntrinsicInst *
getUniqueOxCamlTrapPublishForRecoveryPad(const Function &F,
                                         const BasicBlock &RecoveryBB) {
  const IntrinsicInst *Publish = nullptr;
  for (const BasicBlock &BB : F) {
    for (const Instruction &I : BB) {
      if (!isAArch64OxCamlTrapPublish(&I))
        continue;
      const auto *II = cast<IntrinsicInst>(&I);
      const Value *RecoveryTarget = II->getArgOperand(2)->stripPointerCasts();
      const auto *BA = dyn_cast<BlockAddress>(RecoveryTarget);
      if (!BA || BA->getFunction() != &F ||
          BA->getBasicBlock() != &RecoveryBB)
        continue;
      if (Publish)
        return nullptr;
      Publish = II;
    }
  }
  return Publish;
}

static bool instructionDominates(const Instruction &Dom,
                                 const Instruction &User,
                                 const DominatorTree &DT) {
  return DT.dominates(&Dom, &User);
}

static bool hasCallBrPredecessor(const BasicBlock &BB) {
  for (const BasicBlock *Pred : predecessors(&BB))
    if (isa<CallBrInst>(Pred->getTerminator()))
      return true;
  return false;
}

static bool isMaterializableTrapRecoveryOperand(const Function &F,
                                                const Value *V,
                                                unsigned Depth = 0) {
  if (Depth > 8)
    return false;

  if (isa<Constant>(V) || isa<BasicBlock>(V) || isa<InlineAsm>(V) ||
      isa<MetadataAsValue>(V))
    return true;

  const auto *I = dyn_cast<Instruction>(V);
  if (!I)
    return false;

  if (isa<AllocaInst>(I))
    return I->getParent() == &F.getEntryBlock();

  if (I->getParent() != &F.getEntryBlock())
    return false;

  switch (I->getOpcode()) {
  case Instruction::Add:
  case Instruction::Sub:
  case Instruction::And:
  case Instruction::Or:
  case Instruction::Xor:
  case Instruction::Shl:
  case Instruction::LShr:
  case Instruction::AShr:
  case Instruction::GetElementPtr:
  case Instruction::PtrToInt:
  case Instruction::IntToPtr:
  case Instruction::BitCast:
  case Instruction::AddrSpaceCast:
  case Instruction::Trunc:
  case Instruction::ZExt:
  case Instruction::SExt:
    break;
  default:
    return false;
  }

  for (const Use &U : I->operands())
    if (!isMaterializableTrapRecoveryOperand(F, U.get(), Depth + 1))
      return false;
  return true;
}

static bool isAllowedTrapRecoveryOperand(const Function &F,
                                         const BasicBlock &RecoveryBB,
                                         const Value *V);

static bool isAllowedBeforeTrapRecover(const Function &F,
                                       const BasicBlock &RecoveryBB,
                                       const Instruction &I) {
  if (I.mayReadOrWriteMemory() || I.mayThrow() || I.isTerminator() ||
      isa<PHINode>(I) || isa<LandingPadInst>(I))
    return false;

  for (const Use &U : I.operands())
    if (!isAllowedTrapRecoveryOperand(F, RecoveryBB, U.get()))
      return false;
  return true;
}

static const IntrinsicInst *
getOxCamlTrapRecoverAtBlockStart(const Function &F, const BasicBlock &BB) {
  if (BB.phis().begin() != BB.phis().end())
    return nullptr;

  for (const Instruction &I : BB) {
    if (I.isDebugOrPseudoInst())
      continue;
    const auto *II = dyn_cast<IntrinsicInst>(&I);
    if (II && isAArch64OxCamlTrapRecover(II))
      return II;
    if (!isAllowedBeforeTrapRecover(F, BB, I))
      return nullptr;
  }
  return nullptr;
}

static bool isAllowedTrapRecoveryOperand(const Function &F,
                                         const BasicBlock &RecoveryBB,
                                         const Value *V) {
  if (isa<Constant>(V) || isa<BasicBlock>(V) || isa<InlineAsm>(V) ||
      isa<MetadataAsValue>(V))
    return true;

  const auto *I = dyn_cast<Instruction>(V);
  if (!I)
    return false;
  if (I->getParent() == &RecoveryBB)
    return true;

  // Recovery blocks may store recovered ABI values into compiler-created
  // entry slots before branching to ordinary handler code. They must not read
  // protected-path SSA values. They may also rematerialize simple expressions
  // over entry allocas, such as aligned stack-frame slot addresses.
  return isMaterializableTrapRecoveryOperand(F, I);
}

bool llvm::isOxCamlGCFunction(const Function &F) {
  return F.hasGC() && (F.getGC() == "oxcaml" || F.getGC() == "ocaml");
}

bool llvm::isAArch64OxCamlTrapPublish(const Instruction *I) {
  const auto *II = dyn_cast_or_null<IntrinsicInst>(I);
  return II &&
         II->getIntrinsicID() == Intrinsic::aarch64_oxcaml_trap_publish;
}

bool llvm::isAArch64OxCamlTrapRecover(const Instruction *I) {
  const auto *II = dyn_cast_or_null<IntrinsicInst>(I);
  return II &&
         II->getIntrinsicID() == Intrinsic::aarch64_oxcaml_trap_recover;
}

const LandingPadInst *
llvm::getOxCamlTrapRecoveryLandingPad(const BasicBlock &BB) {
  const auto *LP = dyn_cast_or_null<LandingPadInst>(BB.getFirstNonPHI());
  if (!LP || !LP->getType()->isTokenTy() || !LP->isCleanup())
    return nullptr;
  return LP;
}

const IntrinsicInst *
llvm::getOxCamlTrapRecoverAfterLandingPad(const BasicBlock &BB) {
  const LandingPadInst *LP = getOxCamlTrapRecoveryLandingPad(BB);
  if (!LP)
    return nullptr;

  for (const Instruction &I : BB) {
    if (&I == LP || I.isDebugOrPseudoInst())
      continue;
    const auto *II = dyn_cast<IntrinsicInst>(&I);
    if (II && isAArch64OxCamlTrapRecover(II))
      return II;
    if (!isAllowedBeforeTrapRecover(*BB.getParent(), BB, I))
      return nullptr;
  }
  return nullptr;
}

bool llvm::isOxCamlTrapRecoveryPad(const Function &F, const BasicBlock &BB) {
  if (!isOxCamlGCFunction(F))
    return false;
  const IntrinsicInst *Recover = getOxCamlTrapRecoverAfterLandingPad(BB);
  if (!Recover)
    return false;
  if (BB.phis().begin() != BB.phis().end())
    return false;

  for (const Instruction &I : BB) {
    if (I.isDebugOrPseudoInst())
      continue;
    for (const Use &U : I.operands()) {
      if (!isAllowedTrapRecoveryOperand(F, BB, U.get()))
        return false;
    }
  }
  return true;
}

bool llvm::isOxCamlTrapRecoveryContinuation(const Function &F,
                                            const BasicBlock &BB) {
  if (!isOxCamlGCFunction(F))
    return false;
  // A callbr indirect target is a normal inline-asm branch target. It cannot
  // also be treated as an external runtime-entry ABI label.
  if (hasCallBrPredecessor(BB))
    return false;
  if (!getOxCamlTrapRecoverAtBlockStart(F, BB))
    return false;

  for (const Instruction &I : BB) {
    if (I.isDebugOrPseudoInst())
      continue;
    for (const Use &U : I.operands()) {
      if (!isAllowedTrapRecoveryOperand(F, BB, U.get()))
        return false;
    }
  }
  return true;
}

static const BasicBlock *
getBranchOnlyTrapRecoveryContinuationTarget(const Function &F,
                                            const BasicBlock &BB,
                                            SmallPtrSetImpl<const BasicBlock *>
                                                &Visited) {
  if (!Visited.insert(&BB).second)
    return nullptr;

  if (isOxCamlTrapRecoveryContinuation(F, BB))
    return &BB;

  if (BB.phis().begin() != BB.phis().end())
    return nullptr;

  for (const Instruction &I : BB) {
    if (I.isDebugOrPseudoInst())
      continue;

    const auto *Br = dyn_cast<BranchInst>(&I);
    if (!Br || !Br->isUnconditional())
      return nullptr;

    return getBranchOnlyTrapRecoveryContinuationTarget(F, *Br->getSuccessor(0),
                                                       Visited);
  }

  return nullptr;
}

static const BasicBlock *
getBranchOnlyTrapRecoveryContinuationTarget(const Function &F,
                                            const BasicBlock &BB) {
  SmallPtrSet<const BasicBlock *, 8> Visited;
  return getBranchOnlyTrapRecoveryContinuationTarget(F, BB, Visited);
}

bool llvm::isOxCamlTrapRecoveryLandingPadTrampoline(const Function &F,
                                                    const BasicBlock &BB,
                                                    const DominatorTree &DT) {
  if (!isOxCamlGCFunction(F))
    return false;
  const LandingPadInst *LP = getOxCamlTrapRecoveryLandingPad(BB);
  if (!LP || BB.phis().begin() != BB.phis().end())
    return false;

  bool SawLandingPad = false;
  for (const Instruction &I : BB) {
    if (I.isDebugOrPseudoInst())
      continue;
    if (&I == LP) {
      SawLandingPad = true;
      continue;
    }
    const auto *Br = dyn_cast<BranchInst>(&I);
    if (!SawLandingPad || !Br || !Br->isUnconditional())
      return false;
    const BasicBlock *Target =
        getBranchOnlyTrapRecoveryContinuationTarget(F, *Br->getSuccessor(0));
    return Target &&
           hasDominatingOxCamlTrapPublishForRecoveryPad(F, *Target, DT);
  }
  return false;
}

static const BasicBlock *
getOxCamlTrapRecoveryUnwindTargetShapeOnly(const Function &F,
                                           const BasicBlock &UnwindBB) {
  if (isOxCamlTrapRecoveryPad(F, UnwindBB))
    return &UnwindBB;

  const LandingPadInst *LP = getOxCamlTrapRecoveryLandingPad(UnwindBB);
  if (!LP || UnwindBB.phis().begin() != UnwindBB.phis().end())
    return nullptr;

  bool SawLandingPad = false;
  for (const Instruction &I : UnwindBB) {
    if (I.isDebugOrPseudoInst())
      continue;
    if (&I == LP) {
      SawLandingPad = true;
      continue;
    }
    const auto *Br = dyn_cast<BranchInst>(&I);
    if (!SawLandingPad || !Br || !Br->isUnconditional())
      return nullptr;
    return getBranchOnlyTrapRecoveryContinuationTarget(F,
                                                       *Br->getSuccessor(0));
  }
  return nullptr;
}

static bool
hasDominatingOxCamlTrapPublishForInvoke(const InvokeInst &I,
                                        const BasicBlock &RecoveryBB,
                                        const DominatorTree &DT) {
  const IntrinsicInst *Publish =
      getUniqueOxCamlTrapPublishForRecoveryPad(*I.getFunction(), RecoveryBB);
  return Publish && instructionDominates(*Publish, I, DT);
}

bool llvm::hasDominatingOxCamlTrapPublishForRecoveryPad(
    const Function &F, const BasicBlock &RecoveryBB, const DominatorTree &DT) {
  for (const BasicBlock &BB : F) {
    for (const Instruction &I : BB) {
      const auto *Invoke = dyn_cast<InvokeInst>(&I);
      if (!Invoke)
        continue;
      const BasicBlock *Target =
          getOxCamlTrapRecoveryUnwindTargetShapeOnly(F,
                                                     *Invoke->getUnwindDest());
      if (Target == &RecoveryBB &&
          hasDominatingOxCamlTrapPublishForInvoke(*Invoke, RecoveryBB, DT))
        return true;
    }
  }
  return false;
}

const BasicBlock *
llvm::getOxCamlTrapRecoveryInvokeTarget(const InvokeInst &I,
                                        const DominatorTree &DT) {
  const Function *F = I.getFunction();
  const BasicBlock *UnwindBB = I.getUnwindDest();
  const BasicBlock *Target =
      getOxCamlTrapRecoveryUnwindTargetShapeOnly(*F, *UnwindBB);
  if (!Target)
    return nullptr;
  return hasDominatingOxCamlTrapPublishForInvoke(I, *Target, DT) ? Target
                                                                : nullptr;
}

bool llvm::isOxCamlTrapRecoveryInvoke(const InvokeInst &I,
                                      const DominatorTree &DT) {
  return getOxCamlTrapRecoveryInvokeTarget(I, DT) != nullptr;
}
