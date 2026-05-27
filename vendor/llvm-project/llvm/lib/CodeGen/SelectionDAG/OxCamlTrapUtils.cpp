//===- OxCamlTrapUtils.cpp - OxCaml trap recovery helpers -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "OxCamlTrapUtils.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IntrinsicsAArch64.h"
#include "llvm/IR/Metadata.h"

using namespace llvm;

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
  // protected-path SSA values.
  return isa<AllocaInst>(I) && I->getParent() == &F.getEntryBlock();
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

bool llvm::hasOxCamlTrapPublishForRecoveryPad(const Function &F,
                                              const BasicBlock &RecoveryBB) {
  unsigned MatchingPublishes = 0;
  for (const BasicBlock &BB : F) {
    for (const Instruction &I : BB) {
      if (!isAArch64OxCamlTrapPublish(&I))
        continue;
      const auto *II = cast<IntrinsicInst>(&I);
      const Value *RecoveryTarget = II->getArgOperand(2)->stripPointerCasts();
      const auto *BA = dyn_cast<BlockAddress>(RecoveryTarget);
      if (BA && BA->getFunction() == &F && BA->getBasicBlock() == &RecoveryBB)
        ++MatchingPublishes;
    }
  }
  return MatchingPublishes == 1;
}

bool llvm::isOxCamlTrapRecoveryInvoke(const InvokeInst &I) {
  const Function *F = I.getFunction();
  const BasicBlock *RecoveryBB = I.getUnwindDest();
  return isOxCamlTrapRecoveryPad(*F, *RecoveryBB) &&
         hasOxCamlTrapPublishForRecoveryPad(*F, *RecoveryBB);
}
