//===-- X86OxCamlRuntimeEntry.cpp - OxCaml runtime entries -----*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// OxCaml trap recovery blocks are entered by the OCaml runtime, not by a
// normal branch from the throwing call. The machine CFG keeps that edge so LLVM
// can still reason about reachability and layout, but ordinary registers are
// not live across it. Only the recovery ABI registers are valid at entry.
//
//===----------------------------------------------------------------------===//

#include "X86.h"
#include "X86RegisterInfo.h"
#include "X86Subtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include <iterator>

using namespace llvm;

#define DEBUG_TYPE "x86-oxcaml-runtime-entry"

#define X86_OXCAML_RUNTIME_ENTRY_NAME "X86 OxCaml runtime entry"

namespace {
class X86OxCamlRuntimeEntry : public MachineFunctionPass {
  bool MoveRecoverOnly;

public:
  static char ID;

  X86OxCamlRuntimeEntry(bool MoveRecoverOnly = false)
      : MachineFunctionPass(ID), MoveRecoverOnly(MoveRecoverOnly) {
    initializeX86OxCamlRuntimeEntryPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override {
    return X86_OXCAML_RUNTIME_ENTRY_NAME;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
};
} // end anonymous namespace

char X86OxCamlRuntimeEntry::ID = 0;

INITIALIZE_PASS(X86OxCamlRuntimeEntry, "x86-oxcaml-runtime-entry",
                X86_OXCAML_RUNTIME_ENTRY_NAME, false, false)

static bool isOxCamlRecoveryLiveIn(MCRegister Reg) {
  return Reg == X86::RAX || Reg == X86::R14 || Reg == X86::R15;
}

static MachineBasicBlock::iterator
getRuntimeEntryInsertPoint(MachineBasicBlock &MBB) {
  MachineBasicBlock::iterator InsertPt =
      skipDebugInstructionsForward(MBB.begin(), MBB.end());
  while (InsertPt != MBB.end() &&
         (InsertPt->isPHI() || InsertPt->isPosition() ||
          InsertPt->getOpcode() == TargetOpcode::EH_LABEL)) {
    ++InsertPt;
    InsertPt = skipDebugInstructionsForward(InsertPt, MBB.end());
  }
  return InsertPt;
}

static bool moveTrapRecoverToRuntimeEntryStart(MachineBasicBlock &MBB) {
  MachineBasicBlock::iterator InsertPt = getRuntimeEntryInsertPoint(MBB);
  MachineBasicBlock::iterator Recover = llvm::find_if(MBB, [](MachineInstr &MI) {
    return MI.getOpcode() == X86::OXCAML_TRAP_RECOVER;
  });
  if (Recover == MBB.end() || Recover == InsertPt)
    return false;

  MBB.splice(InsertPt, &MBB, Recover);
  return true;
}

static MachineBasicBlock::iterator
getRuntimeEntryClobberInsertPoint(MachineBasicBlock &MBB) {
  MachineBasicBlock::iterator InsertPt = llvm::find_if(MBB, [](MachineInstr &MI) {
    return MI.getOpcode() == X86::OXCAML_TRAP_RECOVER;
  });
  if (InsertPt != MBB.end())
    return std::next(InsertPt);
  return getRuntimeEntryInsertPoint(MBB);
}

static bool defineAllocatableRegisters(MachineBasicBlock &MBB,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo &TRI,
                                       const TargetInstrInfo &TII) {
  const MachineFunction &MF = *MBB.getParent();
  BitVector Allocatable = TRI.getAllocatableSet(MF, RC);
  MachineBasicBlock::iterator InsertPt = getRuntimeEntryClobberInsertPoint(MBB);
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

bool X86OxCamlRuntimeEntry::runOnMachineFunction(MachineFunction &MF) {
  const auto &ST = MF.getSubtarget<X86Subtarget>();
  const TargetInstrInfo &TII = *ST.getInstrInfo();
  const TargetRegisterInfo &TRI = *ST.getRegisterInfo();
  bool Changed = false;

  for (MachineBasicBlock &MBB : MF) {
    if (!MBB.isRuntimeEntered())
      continue;

    Changed |= moveTrapRecoverToRuntimeEntryStart(MBB);
    if (MoveRecoverOnly)
      continue;

    Changed |= defineAllocatableRegisters(MBB, &X86::GR64RegClass, TRI, TII);
    Changed |= defineAllocatableRegisters(MBB, &X86::VR512RegClass, TRI, TII);
    Changed |= addRuntimeEntryCallClobbers(MBB, &X86::GR64RegClass, TRI);
    Changed |= addRuntimeEntryCallClobbers(MBB, &X86::VR512RegClass, TRI);
  }

  return Changed;
}

FunctionPass *llvm::createX86OxCamlRuntimeEntryPass(bool MoveRecoverOnly) {
  return new X86OxCamlRuntimeEntry(MoveRecoverOnly);
}
