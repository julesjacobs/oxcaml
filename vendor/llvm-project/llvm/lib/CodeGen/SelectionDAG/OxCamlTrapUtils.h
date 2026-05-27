//===- OxCamlTrapUtils.h - OxCaml trap recovery helpers --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_CODEGEN_SELECTIONDAG_OXCAMLTRAPUTILS_H
#define LLVM_LIB_CODEGEN_SELECTIONDAG_OXCAMLTRAPUTILS_H

namespace llvm {

class BasicBlock;
class Function;
class Instruction;
class IntrinsicInst;
class InvokeInst;
class LandingPadInst;

bool isOxCamlGCFunction(const Function &F);
bool isAArch64OxCamlTrapPublish(const Instruction *I);
bool isAArch64OxCamlTrapRecover(const Instruction *I);
const LandingPadInst *getOxCamlTrapRecoveryLandingPad(const BasicBlock &BB);
const IntrinsicInst *getOxCamlTrapRecoverAfterLandingPad(const BasicBlock &BB);
bool isOxCamlTrapRecoveryPad(const Function &F, const BasicBlock &BB);
bool hasOxCamlTrapPublishForRecoveryPad(const Function &F,
                                        const BasicBlock &RecoveryBB);
bool isOxCamlTrapRecoveryInvoke(const InvokeInst &I);

} // end namespace llvm

#endif
