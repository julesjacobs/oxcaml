//===- OxCamlStatepointSpillRoots.cpp - List sibling spill slots ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// With the OxCaml register-preserving allocation calling conventions, the
/// register allocator may keep a GC value live across a statepoint in a
/// register (which the statepoint lists, and the GC relocates) while a spill
/// slot created for the same value by live-range splitting also spans the
/// statepoint and is reloaded afterwards. The GC does not know about the
/// slot, so after a moving collection the reload resurrects a stale pointer.
/// Stock LLVM cannot hit this: calls clobber caller-saved registers and
/// AllowGCPtrInCSR is off, so at a statepoint the only live location of a GC
/// value is the listed one.
///
/// This pass runs after register allocation, before virtual registers are
/// rewritten, while VirtRegMap and LiveStacks are still available. For every
/// statepoint gc-pointer operand that is a virtual register, it finds the
/// spill slot of the register's original (pre-splitting) live range. If that
/// slot is live across the statepoint and not already listed, the slot is
/// appended to the statepoint's gc-pointer section so that it becomes an
/// ordinary frametable stack root and the GC updates the slot copy too.
/// This is purely metadata: no instructions are inserted.
///
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/LiveStacks.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/SlotIndexes.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/IR/Function.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "oxcaml-statepoint-spill-roots"

STATISTIC(NumSlotsAppended,
          "Number of sibling spill slots appended to statepoints");

static cl::opt<bool> EnableOxCamlStatepointSpillRoots(
    "oxcaml-statepoint-spill-roots", cl::Hidden, cl::init(true),
    cl::desc("Append statepoint-crossing sibling spill slots of gc values "
             "to the statepoint gc operand list"));

static cl::opt<bool> VerboseOxCamlStatepointSpillRoots(
    "oxcaml-statepoint-spill-roots-verbose", cl::Hidden, cl::init(false),
    cl::desc("Trace sibling spill slot root decisions"));

namespace {

class OxCamlStatepointSpillRoots : public MachineFunctionPass {
public:
  static char ID;

  OxCamlStatepointSpillRoots() : MachineFunctionPass(ID) {
    initializeOxCamlStatepointSpillRootsPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<SlotIndexes>();
    AU.addRequired<LiveStacks>();
    AU.addRequired<VirtRegMap>();
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  StringRef getPassName() const override {
    return "OxCaml Statepoint Spill Slot Roots";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;
};

} // end anonymous namespace

char OxCamlStatepointSpillRoots::ID = 0;
char &llvm::OxCamlStatepointSpillRootsID = OxCamlStatepointSpillRoots::ID;

INITIALIZE_PASS_BEGIN(OxCamlStatepointSpillRoots, DEBUG_TYPE,
                      "OxCaml Statepoint Spill Slot Roots", false, false)
INITIALIZE_PASS_DEPENDENCY(SlotIndexes)
INITIALIZE_PASS_DEPENDENCY(LiveStacks)
INITIALIZE_PASS_DEPENDENCY(VirtRegMap)
INITIALIZE_PASS_END(OxCamlStatepointSpillRoots, DEBUG_TYPE,
                    "OxCaml Statepoint Spill Slot Roots", false, false)

/// Append the spill slots in \p ToAdd to the gc-pointer section of \p MI,
/// updating the gc-pointer count and the base/derived gc map.
static void appendSlotsToStatepoint(MachineInstr &MI, MachineFunction &MF,
                                    ArrayRef<int> ToAdd) {
  StatepointOpers SO(&MI);
  unsigned NumGCPtrCountIdx = SO.getNumGCPtrIdx();
  unsigned OldNumGCPtrs = MI.getOperand(NumGCPtrCountIdx).getImm();
  unsigned AllocaCountIdx = SO.getNumAllocaIdx();
  unsigned MapCountIdx = SO.getNumGcMapEntriesIdx();
  unsigned OldMapCount = MI.getOperand(MapCountIdx).getImm();

  // The tail starts at the ConstantOp marker that precedes the gc-alloca
  // count and runs to the end of the instruction (incl. regmask/implicits).
  unsigned TailStart = AllocaCountIdx - 1;
  assert(MI.getOperand(TailStart).isImm() &&
         MI.getOperand(TailStart).getImm() == StackMaps::ConstantOp &&
         "expected ConstantOp before gc-alloca count");
  // Position (within the tail) just past the last base/derived map pair;
  // the new pairs are inserted there.
  unsigned PairsEndInTail = MapCountIdx + 1 + 2 * OldMapCount - TailStart;

  SmallVector<MachineOperand, 32> Tail;
  for (unsigned I = TailStart, E = MI.getNumOperands(); I < E; ++I)
    Tail.push_back(MI.getOperand(I));
  while (MI.getNumOperands() > TailStart)
    MI.removeOperand(MI.getNumOperands() - 1);

  // New gc pointer records (the folded-operand form), each with a fixed
  // stack memoperand as the verifier and AA expect for FI operands.
  for (int Slot : ToAdd) {
    MI.addOperand(MF, MachineOperand::CreateImm(StackMaps::IndirectMemRefOp));
    MI.addOperand(MF, MachineOperand::CreateImm(8));
    MI.addOperand(MF, MachineOperand::CreateFI(Slot));
    MI.addOperand(MF, MachineOperand::CreateImm(0));
    MI.addMemOperand(
        MF, MF.getMachineMemOperand(
                MachinePointerInfo::getFixedStack(MF, Slot),
                MachineMemOperand::MOLoad | MachineMemOperand::MOStore, 8,
                MF.getFrameInfo().getObjectAlign(Slot)));
  }

  auto AppendNewPairs = [&]() {
    for (unsigned J = 0; J < ToAdd.size(); ++J) {
      unsigned Logical = OldNumGCPtrs + J;
      MI.addOperand(MF, MachineOperand::CreateImm(Logical));
      MI.addOperand(MF, MachineOperand::CreateImm(Logical));
    }
  };

  for (unsigned I = 0, E = Tail.size(); I < E; ++I) {
    if (I == PairsEndInTail)
      AppendNewPairs();
    MI.addOperand(MF, Tail[I]);
  }
  if (PairsEndInTail == Tail.size())
    AppendNewPairs();

  // Fix the counts. Operands before the insertion point kept their indices;
  // the gc map count shifted by the inserted gc pointer records.
  MI.getOperand(NumGCPtrCountIdx).setImm(OldNumGCPtrs + ToAdd.size());
  MI.getOperand(MapCountIdx + 4 * ToAdd.size())
      .setImm(OldMapCount + ToAdd.size());
}

/// Walk the gc-pointer section of \p MI, recording stack slots that are
/// already listed (folded operands) into \p ListedSlots and virtual-register
/// gc operands into \p GCRegs (if non-null).
static void walkGCPtrSection(const MachineInstr &MI,
                             SmallSet<int, 16> &ListedSlots,
                             SmallVectorImpl<Register> *GCRegs) {
  StatepointOpers SO(&MI);
  int FirstGCPtrIdx = SO.getFirstGCPtrIdx();
  if (FirstGCPtrIdx == -1)
    return;
  unsigned NumGCPtrs = MI.getOperand(SO.getNumGCPtrIdx()).getImm();
  unsigned CurIdx = FirstGCPtrIdx;
  for (unsigned N = 0; N < NumGCPtrs; ++N) {
    const MachineOperand &MO = MI.getOperand(CurIdx);
    if (MO.isImm() && MO.getImm() == StackMaps::IndirectMemRefOp)
      ListedSlots.insert(MI.getOperand(CurIdx + 2).getIndex());
    else if (MO.isImm() && MO.getImm() == StackMaps::DirectMemRefOp)
      ListedSlots.insert(MI.getOperand(CurIdx + 1).getIndex());
    else if (MO.isFI())
      ListedSlots.insert(MO.getIndex());
    else if (MO.isReg() && MO.getReg().isVirtual() && GCRegs)
      GCRegs->push_back(MO.getReg());
    CurIdx = StackMaps::getNextMetaArgIdx(&MI, CurIdx);
  }
}

/// Identify every spill slot that can hold GC values: slots referenced as
/// folded gc operands at any statepoint, and spill slots of the original
/// (pre-splitting) live ranges of any statepoint gc register operand. Spill
/// slots are per-original until stack slot coloring (which runs later), so a
/// slot whose live interval crosses a statepoint always carries the value of
/// one gc live-range family if it belongs to this set.
static void collectGCSlots(ArrayRef<MachineInstr *> Statepoints,
                           VirtRegMap &VRM, SmallSet<int, 16> &GCSlots) {
  for (const MachineInstr *MI : Statepoints) {
    SmallVector<Register, 16> GCRegs;
    walkGCPtrSection(*MI, GCSlots, &GCRegs);
    for (Register R : GCRegs) {
      int Slot = VRM.getStackSlot(VRM.getOriginal(R));
      if (Slot != VirtRegMap::NO_STACK_SLOT)
        GCSlots.insert(Slot);
    }
  }
}

static bool processStatepoint(MachineInstr &MI, MachineFunction &MF,
                              const SmallSet<int, 16> &GCSlots, LiveStacks &LS,
                              SlotIndexes &Indexes) {
  SmallSet<int, 16> ListedSlots;
  walkGCPtrSection(MI, ListedSlots, nullptr);

  // The slot must hold a valid value on entry to the statepoint (live at the
  // early-clobber slot) and still be live during it (live at the register
  // slot): only then can the GC read and update it. A slot whose live range
  // merely STARTS at the statepoint (a folded relocate-def spill) contains
  // garbage on entry and must not be listed.
  SlotIndex Idx = Indexes.getInstructionIndex(MI);
  SlotIndex InSlot = Idx.getRegSlot(true);
  SlotIndex OutSlot = Idx.getRegSlot();
  SmallVector<int, 8> ToAdd;
  for (int Slot : GCSlots) {
    if (ListedSlots.count(Slot))
      continue;
    if (!LS.hasInterval(Slot))
      continue;
    const LiveInterval &LI = LS.getInterval(Slot);
    if (!LI.liveAt(InSlot) || !LI.liveAt(OutSlot))
      continue;
    ToAdd.push_back(Slot);
  }

  if (ToAdd.empty())
    return false;

  // Deterministic operand order.
  llvm::sort(ToAdd);

  if (VerboseOxCamlStatepointSpillRoots) {
    errs() << "[oxsr] " << MF.getName() << " statepoint ID "
           << StatepointOpers(&MI).getID() << " at " << Idx << ": appending";
    for (int S : ToAdd)
      errs() << " %stack." << S;
    errs() << "\n";
  }
  LLVM_DEBUG(dbgs() << "[OxCaml] appending " << ToAdd.size()
                    << " sibling spill slot root(s) to statepoint ID "
                    << StatepointOpers(&MI).getID() << " in " << MF.getName()
                    << "\n");
  appendSlotsToStatepoint(MI, MF, ToAdd);
  NumSlotsAppended += ToAdd.size();
  return true;
}

bool OxCamlStatepointSpillRoots::runOnMachineFunction(MachineFunction &MF) {
  if (!EnableOxCamlStatepointSpillRoots)
    return false;
  const Function &F = MF.getFunction();
  if (!F.hasGC() || (F.getGC() != "oxcaml" && F.getGC() != "ocaml"))
    return false;

  VirtRegMap &VRM = getAnalysis<VirtRegMap>();
  LiveStacks &LS = getAnalysis<LiveStacks>();
  SlotIndexes &Indexes = getAnalysis<SlotIndexes>();

  SmallVector<MachineInstr *, 32> Statepoints;
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB)
      if (MI.getOpcode() == TargetOpcode::STATEPOINT)
        Statepoints.push_back(&MI);
  if (Statepoints.empty())
    return false;

  SmallSet<int, 16> GCSlots;
  collectGCSlots(Statepoints, VRM, GCSlots);
  if (GCSlots.empty())
    return false;

  bool Changed = false;
  for (MachineInstr *MI : Statepoints)
    Changed |= processStatepoint(*MI, MF, GCSlots, LS, Indexes);
  return Changed;
}
