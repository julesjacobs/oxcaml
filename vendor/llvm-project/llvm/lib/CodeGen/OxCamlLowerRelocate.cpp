//===- OxCamlLowerRelocate.cpp - Lower in-place relocate pseudos ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Under OxCaml in-place statepoint lowering, gc.relocate produces an
/// OXCAML_RELOCATE pseudo: a side-effecting identity of the original gc
/// value. Its opacity is what keeps every value-based optimization
/// (CSE, LICM, sinking, ISel folding) from identifying the pre- and
/// post-statepoint incarnations of a value — the GC rewrites root
/// LOCATIONS in place at statepoints, so a value's SSA name must not be
/// position-independent across one.
///
/// By this point in the pipeline all of those passes have run, and from
/// the register coalescer onward the backend reasons about locations,
/// where in-place semantics is naturally correct. So the pseudo is
/// rewritten to a plain COPY here, immediately before coalescing: the
/// source dies at the statepoint where the destination is born, the
/// ranges never interfere, and the coalescer merges them away, yielding
/// the same final code as identity lowering — without its hazards.
///
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/InitializePasses.h"

using namespace llvm;

#define DEBUG_TYPE "oxcaml-lower-relocate"

namespace {

class OxCamlLowerRelocate : public MachineFunctionPass {
public:
  static char ID;

  OxCamlLowerRelocate() : MachineFunctionPass(ID) {
    initializeOxCamlLowerRelocatePass(*PassRegistry::getPassRegistry());
  }

  StringRef getPassName() const override {
    return "OxCaml Lower In-Place Relocate Pseudos";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;
};

} // end anonymous namespace

char OxCamlLowerRelocate::ID = 0;
char &llvm::OxCamlLowerRelocateID = OxCamlLowerRelocate::ID;

INITIALIZE_PASS(OxCamlLowerRelocate, DEBUG_TYPE,
                "OxCaml Lower In-Place Relocate Pseudos", false, false)

bool OxCamlLowerRelocate::runOnMachineFunction(MachineFunction &MF) {
  const Function &F = MF.getFunction();
  if (!F.hasGC() || (F.getGC() != "oxcaml" && F.getGC() != "ocaml"))
    return false;

  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();
  bool Changed = false;
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : llvm::make_early_inc_range(MBB)) {
      if (StringRef(TII->getName(MI.getOpcode())) != "OXCAML_RELOCATE")
        continue;
      BuildMI(MBB, MI, MI.getDebugLoc(), TII->get(TargetOpcode::COPY))
          .add(MI.getOperand(0))
          .add(MI.getOperand(1));
      MI.eraseFromParent();
      Changed = true;
    }
  return Changed;
}
