//===-- AArch64OxCamlRuntimeEntry.cpp - OxCaml runtime entries -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// OxCaml trap recovery blocks are entered by the OCaml runtime, not by a
// normal branch from the throwing call.  The machine CFG keeps that edge so
// LLVM can still reason about reachability and layout, but ordinary registers
// are not live across it.  Only the recovery ABI registers are valid at entry.
//
//===----------------------------------------------------------------------===//

#include "AArch64.h"
#include "AArch64RegisterInfo.h"
#include "AArch64Subtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "aarch64-oxcaml-runtime-entry"

#define AARCH64_OXCAML_RUNTIME_ENTRY_NAME "AArch64 OxCaml runtime entry"

namespace {
class AArch64OxCamlRuntimeEntry : public MachineFunctionPass {
public:
  static char ID;

  AArch64OxCamlRuntimeEntry() : MachineFunctionPass(ID) {
    initializeAArch64OxCamlRuntimeEntryPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override {
    return AARCH64_OXCAML_RUNTIME_ENTRY_NAME;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
};
} // end anonymous namespace

char AArch64OxCamlRuntimeEntry::ID = 0;

INITIALIZE_PASS(AArch64OxCamlRuntimeEntry, "aarch64-oxcaml-runtime-entry",
                AARCH64_OXCAML_RUNTIME_ENTRY_NAME, false, false)

static bool isOxCamlRecoveryLiveIn(MCRegister Reg) {
  return Reg == AArch64::X0 || Reg == AArch64::X26 || Reg == AArch64::X27 ||
         Reg == AArch64::X28;
}

static MachineBasicBlock::iterator
getRuntimeEntryInsertPoint(MachineBasicBlock &MBB) {
  return skipDebugInstructionsForward(MBB.begin(), MBB.end());
}

static bool defineAllocatableRegisters(MachineBasicBlock &MBB,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo &TRI,
                                       const TargetInstrInfo &TII) {
  const MachineFunction &MF = *MBB.getParent();
  BitVector Allocatable = TRI.getAllocatableSet(MF, RC);
  MachineBasicBlock::iterator InsertPt = getRuntimeEntryInsertPoint(MBB);
  bool Changed = false;

  for (MCRegister Reg : *RC) {
    if (!Allocatable.test(Reg))
      continue;
    if (isOxCamlRecoveryLiveIn(Reg))
      continue;
    BuildMI(MBB, InsertPt, DebugLoc(), TII.get(TargetOpcode::IMPLICIT_DEF),
            Reg);
    Changed = true;
  }

  return Changed;
}

static bool addRuntimeEntryCallClobbers(MachineBasicBlock &RuntimeEntry,
                                        const TargetRegisterClass *RC,
                                        const TargetRegisterInfo &TRI) {
  MachineFunction &MF = *RuntimeEntry.getParent();
  BitVector Allocatable = TRI.getAllocatableSet(MF, RC);
  bool Changed = false;

  for (MachineBasicBlock *Pred : RuntimeEntry.predecessors()) {
    for (MachineInstr &MI : *Pred) {
      if (!MI.isCall())
        continue;

      // The machine CFG only says that the predecessor can enter the recovery
      // block; it does not identify a single call instruction when a block
      // contains more than one call. Treat every call in the predecessor as a
      // possible runtime entry so register allocation cannot keep ordinary
      // values live across an unmodelled runtime transfer.
      //
      // This is conservative for calls that only return normally. It is still
      // valid because the OCaml calling convention is caller-saved for the
      // allocatable registers listed here.
      for (MCRegister Reg : *RC) {
        if (!Allocatable.test(Reg))
          continue;
        if (isOxCamlRecoveryLiveIn(Reg))
          continue;
        if (MI.definesRegister(Reg, &TRI))
          continue;
        MI.addOperand(MF, MachineOperand::CreateReg(
                              Reg, /*isDef=*/true, /*isImp=*/true,
                              /*isKill=*/false, /*isDead=*/true));
        Changed = true;
      }
    }
  }

  return Changed;
}

bool AArch64OxCamlRuntimeEntry::runOnMachineFunction(MachineFunction &MF) {
  const auto &ST = MF.getSubtarget<AArch64Subtarget>();
  const TargetInstrInfo &TII = *ST.getInstrInfo();
  const TargetRegisterInfo &TRI = *ST.getRegisterInfo();
  bool Changed = false;

  for (MachineBasicBlock &MBB : MF) {
    if (!MBB.isRuntimeEntered())
      continue;

    Changed |= defineAllocatableRegisters(MBB, &AArch64::GPR64RegClass, TRI,
                                          TII);
    Changed |= defineAllocatableRegisters(MBB, &AArch64::FPR128RegClass, TRI,
                                          TII);
    Changed |= addRuntimeEntryCallClobbers(MBB, &AArch64::GPR64RegClass, TRI);
    Changed |= addRuntimeEntryCallClobbers(MBB, &AArch64::FPR128RegClass, TRI);
  }

  return Changed;
}

FunctionPass *llvm::createAArch64OxCamlRuntimeEntryPass() {
  return new AArch64OxCamlRuntimeEntry();
}
