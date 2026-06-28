//===- OxCamlGCRootVerifier.cpp - Verify statepoint root completeness ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// The OxCaml GC updates roots in place: every location holding a GC pointer
/// across a statepoint must be listed in the statepoint's gc-pointer section,
/// or the location is stale after a moving collection. This pass verifies
/// that invariant after register allocation, while virtual registers,
/// VirtRegMap and LiveStacks are still available:
///
///  - every gc virtual register live across a statepoint is listed as a
///    gc operand of that statepoint (an unlisted live-across register is a
///    stale-copy hazard; an unlisted live-across register that the call's
///    regmask clobbers is a register allocation bug);
///  - every spill slot belonging to a gc live-range family that is live
///    across a statepoint is listed (this re-checks the work of
///    OxCamlStatepointSpillRoots, which must run first);
///  - no gc-DERIVED register (a value computed from a gc pointer by a
///    non-load, non-copy instruction, e.g. an interior address) is live
///    across any statepoint: the GC only updates listed base pointers.
///
/// Gc-ness of virtual registers comes from the gc bit seeded by the
/// instruction emitter (MachineRegisterInfo::isOxCamlGCPtr), augmented here
/// with the statepoint gc operands themselves and a closure over COPYs.
/// A statepoint gc operand whose vreg lacks the gc bit is additionally
/// reported as a bit-propagation gap (not a correctness violation; it
/// measures the health of the bit before code-motion gates rely on it).
///
/// The pass only reports (and optionally aborts); it never changes code.
///
//===----------------------------------------------------------------------===//

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/EquivalenceClasses.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/LiveStacks.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SlotIndexes.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/IR/Function.h"
#include "llvm/InitializePasses.h"

#include "OxCamlStatepointGCValueness.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "oxcaml-gc-root-verifier"

STATISTIC(NumUnlistedRegRoots,
          "Gc vregs live across a statepoint in an unlisted register while "
          "their value is listed at that statepoint (second location)");
STATISTIC(NumUnlistedFamilyRegs,
          "Gc-marked vregs live across a statepoint whose value family is "
          "not listed there at all (info: IR liveness says dead/immediate)");
STATISTIC(NumClobberedRegRoots,
          "Gc vregs live across a statepoint in a regmask-clobbered register");
STATISTIC(NumUnlistedSlotRoots,
          "Gc family spill slots live across a statepoint but not listed");
STATISTIC(NumDerivedAcross,
          "Gc-derived vregs live across a statepoint");
STATISTIC(NumUnmarkedGCOperands,
          "Statepoint gc vreg operands missing the gc bit");

static cl::opt<bool> EnableOxCamlGCRootVerifier(
    "oxcaml-gc-root-verifier", cl::Hidden, cl::init(false),
    cl::desc("Verify that every gc value location live across a statepoint "
             "is listed as a root, and report violations"));

static cl::opt<bool> OxCamlGCRootVerifierFatal(
    "oxcaml-gc-root-verifier-fatal", cl::Hidden, cl::init(false),
    cl::desc("Abort compilation when the oxcaml gc root verifier finds a "
             "violation"));

namespace {

class OxCamlGCRootVerifier : public MachineFunctionPass {
public:
  static char ID;

  OxCamlGCRootVerifier() : MachineFunctionPass(ID) {
    initializeOxCamlGCRootVerifierPass(*PassRegistry::getPassRegistry());
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<SlotIndexes>();
    AU.addRequired<LiveIntervals>();
    AU.addRequired<LiveStacks>();
    AU.addRequired<VirtRegMap>();
    AU.addRequired<MachineDominatorTree>();
    AU.setPreservesAll();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  StringRef getPassName() const override {
    return "OxCaml GC Root Verifier";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;
};

} // end anonymous namespace

char OxCamlGCRootVerifier::ID = 0;
char &llvm::OxCamlGCRootVerifierID = OxCamlGCRootVerifier::ID;

INITIALIZE_PASS_BEGIN(OxCamlGCRootVerifier, DEBUG_TYPE,
                      "OxCaml GC Root Verifier", false, false)
INITIALIZE_PASS_DEPENDENCY(SlotIndexes)
INITIALIZE_PASS_DEPENDENCY(LiveIntervals)
INITIALIZE_PASS_DEPENDENCY(LiveStacks)
INITIALIZE_PASS_DEPENDENCY(VirtRegMap)
INITIALIZE_PASS_END(OxCamlGCRootVerifier, DEBUG_TYPE,
                    "OxCaml GC Root Verifier", false, false)

/// Walk the gc-pointer section of \p MI, recording listed spill slots and
/// listed virtual registers.
static void walkGCPtrSection(const MachineInstr &MI,
                             SmallSet<int, 16> &ListedSlots,
                             SmallSet<Register, 16> &ListedRegs) {
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
    else if (MO.isReg() && MO.getReg().isVirtual())
      ListedRegs.insert(MO.getReg());
    CurIdx = StackMaps::getNextMetaArgIdx(&MI, CurIdx);
  }
}

/// True for statepoints whose callee runs SAVE_ALL_REGS (alloc family:
/// caml_call_gc / realloc entries) — the only sites where the runtime can
/// resolve frame-descriptor REGISTER roots (against the gc_regs bucket).
/// Identified by the regmask preserving x0, which no other OxCaml callee
/// convention does.
static bool isAllocFamilyMask(const uint32_t *RegMask,
                              const TargetRegisterInfo *TRI) {
  if (!RegMask)
    return false;
  for (unsigned R = 1, E = TRI->getNumRegs(); R != E; ++R)
    if (StringRef(TRI->getName(R)) == "X0")
      return !MachineOperand::clobbersPhysReg(RegMask, R);
  return false;
}

static const uint32_t *getStatepointRegMask(const MachineInstr &MI) {
  for (const MachineOperand &MO : MI.operands())
    if (MO.isRegMask())
      return MO.getRegMask();
  return nullptr;
}

/// Walk the gc-alloca section of \p MI (exnroot slots and friends),
/// recording the frame indices into \p AllocaFIs.
static void walkAllocaSection(const MachineInstr &MI,
                              SmallSet<int, 16> &AllocaFIs) {
  StatepointOpers SO(&MI);
  unsigned AllocaCountIdx = SO.getNumAllocaIdx();
  unsigned NumAllocas = MI.getOperand(AllocaCountIdx).getImm();
  unsigned CurIdx = AllocaCountIdx + 1;
  for (unsigned N = 0; N < NumAllocas; ++N) {
    const MachineOperand &MO = MI.getOperand(CurIdx);
    if (MO.isImm() && MO.getImm() == StackMaps::IndirectMemRefOp)
      AllocaFIs.insert(MI.getOperand(CurIdx + 2).getIndex());
    else if (MO.isImm() && MO.getImm() == StackMaps::DirectMemRefOp)
      AllocaFIs.insert(MI.getOperand(CurIdx + 1).getIndex());
    else if (MO.isFI())
      AllocaFIs.insert(MO.getIndex());
    CurIdx = StackMaps::getNextMetaArgIdx(&MI, CurIdx);
  }
}

/// Compute the set of gc vregs (gc bit, statepoint gc operands, closure over
/// COPYs) and the set of gc-derived vregs (defined from a gc or derived vreg
/// by a non-copy, non-load instruction other than a statepoint). Reports gc
/// operands missing the gc bit as bit-propagation gaps.
static void computeGCAndDerivedSets(MachineFunction &MF,
                                    ArrayRef<MachineInstr *> Statepoints,
                                    LiveStacks &LS,
                                    const TargetInstrInfo *TII,
                                    DenseSet<Register> &GCRegs,
                                    DenseSet<Register> &DerivedRegs) {
  MachineRegisterInfo &MRI = MF.getRegInfo();
  for (unsigned I = 0, E = MRI.getNumVirtRegs(); I != E; ++I) {
    Register R = Register::index2VirtReg(I);
    if (MRI.isOxCamlGCPtr(R))
      GCRegs.insert(R);
  }

  SmallSet<int, 16> ValueHomeFIs;
  for (const MachineInstr *MI : Statepoints) {
    SmallSet<int, 16> Slots;
    SmallSet<Register, 16> Regs;
    walkGCPtrSection(*MI, Slots, Regs);
    walkAllocaSection(*MI, ValueHomeFIs);
    for (int FI : Slots)
      if (!LS.hasInterval(FI))
        ValueHomeFIs.insert(FI);
    for (Register R : Regs) {
      if (!MRI.isOxCamlGCPtr(R)) {
        ++NumUnmarkedGCOperands;
        errs() << "[oxgc-verify] " << MF.getName()
               << ": gc bit missing on statepoint gc operand "
               << printReg(R) << " (bit-propagation gap)\n";
      }
      GCRegs.insert(R);
    }
  }
  // Registers loaded from value-home slots (ISel statepoint pool slots and
  // exnroot allocas) hold gc values even when they never appear as
  // statepoint operands: exnroot-homed values cross try-wrapped calls this
  // way, and allocator re-spills of such loads must be tracked.
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB) {
      int FI;
      if (Register R = TII->isLoadFromStackSlot(MI, FI))
        if (R.isVirtual() && ValueHomeFIs.count(FI))
          GCRegs.insert(R);
    }

  bool Changed = true;
  while (Changed) {
    Changed = false;
    for (MachineBasicBlock &MBB : MF) {
      for (MachineInstr &MI : MBB) {
        if (MI.getOpcode() == TargetOpcode::STATEPOINT)
          continue;
        bool UsesGC = false, UsesDerived = false;
        for (const MachineOperand &MO : MI.uses()) {
          if (!MO.isReg() || !MO.getReg().isVirtual())
            continue;
          UsesGC |= GCRegs.contains(MO.getReg());
          UsesDerived |= DerivedRegs.contains(MO.getReg());
        }
        if (!UsesGC && !UsesDerived)
          continue;
        if (MI.isCopy()) {
          Register D = MI.getOperand(0).getReg();
          if (D.isVirtual()) {
            if (UsesGC)
              Changed |= GCRegs.insert(D).second;
            if (UsesDerived)
              Changed |= DerivedRegs.insert(D).second;
          }
          continue;
        }
        // Loaded values are not derived: a reload of a spilled gc value is
        // covered by the inherited gc bit, and a value loaded FROM a gc
        // pointer is an independent base pointer.
        if (MI.mayLoad())
          continue;
        for (const MachineOperand &MO : MI.defs()) {
          if (!MO.isReg() || !MO.getReg().isVirtual())
            continue;
          Register D = MO.getReg();
          if (!GCRegs.contains(D))
            Changed |= DerivedRegs.insert(D).second;
        }
      }
    }
  }
}

bool OxCamlGCRootVerifier::runOnMachineFunction(MachineFunction &MF) {
  if (!EnableOxCamlGCRootVerifier)
    return false;
  const Function &F = MF.getFunction();
  if (!F.hasGC() || (F.getGC() != "oxcaml" && F.getGC() != "ocaml"))
    return false;

  SmallVector<MachineInstr *, 32> Statepoints;
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB)
      if (MI.getOpcode() == TargetOpcode::STATEPOINT)
        Statepoints.push_back(&MI);
  if (Statepoints.empty())
    return false;

  LiveIntervals &LIS = getAnalysis<LiveIntervals>();
  LiveStacks &LS = getAnalysis<LiveStacks>();
  VirtRegMap &VRM = getAnalysis<VirtRegMap>();
  SlotIndexes &Indexes = getAnalysis<SlotIndexes>();
  MachineDominatorTree &MDT = getAnalysis<MachineDominatorTree>();
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();

  DenseSet<Register> GCRegs, DerivedRegs;
  computeGCAndDerivedSets(MF, Statepoints, LS, TII, GCRegs, DerivedRegs);

  // The same flow-sensitive value identity the listing pass uses: a
  // family slot's content at a specific statepoint may be a raw range of
  // a colored slot, which the pass deliberately does not list. Only a
  // PROVABLE gc value left unlisted is a real violation.
  SmallSet<int, 16> VHListedAnywhere, ValueHomeFIs;
  oxcamlroots::collectValueHomeFIs(Statepoints, LS, VHListedAnywhere,
                                   ValueHomeFIs);
  oxcamlroots::GCValueness GCV(MF, Statepoints, ValueHomeFIs, LIS, LS,
                               Indexes, TII);

  // Value connectivity: virtual registers and spill slots that hold copies
  // of the same value, connected by COPYs, spill stores/reloads, and the
  // split-sibling relation (common VRM original). Distinguishes the real
  // bug shape — a SECOND location of a value that IS listed at a statepoint
  // (the GC updates the listed location only; the copy goes stale) — from
  // a gc-marked vreg whose value the IR deliberately does not list (dead
  // at the statepoint, or statically a tagged immediate that reached i64
  // code via ptrtoint). Vregs encode as their id, frame slots as -FI-1.
  EquivalenceClasses<int64_t> ValueComp;
  auto SlotNode = [](int FI) { return -(int64_t)FI - 1; };
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : MBB) {
      if (MI.isCopy()) {
        Register Dst = MI.getOperand(0).getReg();
        Register Src = MI.getOperand(1).getReg();
        if (Dst.isVirtual() && Src.isVirtual())
          ValueComp.unionSets((int64_t)Dst.id(), (int64_t)Src.id());
        continue;
      }
      int FI;
      if (Register R = TII->isLoadFromStackSlot(MI, FI))
        if (R.isVirtual())
          ValueComp.unionSets((int64_t)R.id(), SlotNode(FI));
      if (Register R = TII->isStoreToStackSlot(MI, FI))
        if (R.isVirtual())
          ValueComp.unionSets((int64_t)R.id(), SlotNode(FI));
    }
  }
  for (unsigned I = 0, E = MF.getRegInfo().getNumVirtRegs(); I != E; ++I) {
    Register R = Register::index2VirtReg(I);
    Register Orig = VRM.getOriginal(R);
    if (Orig != R)
      ValueComp.unionSets((int64_t)R.id(), (int64_t)Orig.id());
    int Slot = VRM.getStackSlot(Orig);
    if (Slot != VirtRegMap::NO_STACK_SLOT)
      ValueComp.unionSets((int64_t)R.id(), SlotNode(Slot));
  }

  // Family spill slots: slots listed at any statepoint plus the spill slots
  // of the originals of all gc vregs (slots are per-original before stack
  // slot coloring, so each carries values of exactly one family).
  SmallSet<int, 16> FamilySlots;
  for (const MachineInstr *MI : Statepoints) {
    SmallSet<Register, 16> Regs;
    walkGCPtrSection(*MI, FamilySlots, Regs);
  }
  for (Register R : GCRegs) {
    int Slot = VRM.getStackSlot(VRM.getOriginal(R));
    if (Slot != VirtRegMap::NO_STACK_SLOT)
      FamilySlots.insert(Slot);
  }

  unsigned Violations = 0;
  // Prints the report prefix and returns the stream for the message body.
  auto Report = [&](const MachineInstr &MI) -> raw_ostream & {
    ++Violations;
    errs() << "[oxgc-verify] " << MF.getName() << ": statepoint ID "
           << StatepointOpers(&MI).getID() << " at "
           << Indexes.getInstructionIndex(MI) << ": ";
    return errs();
  };

  for (MachineInstr *MI : Statepoints) {
    SmallSet<int, 16> ListedSlots;
    SmallSet<Register, 16> ListedRegs;
    walkGCPtrSection(*MI, ListedSlots, ListedRegs);
    const uint32_t *RegMask = getStatepointRegMask(*MI);

    SlotIndex Idx = Indexes.getInstructionIndex(*MI);
    SlotIndex InSlot = Idx.getRegSlot(true);
    SlotIndex OutSlot = Idx.getRegSlot();
    auto LiveAcross = [&](const LiveInterval &LI) {
      return LI.liveAt(InSlot) && LI.liveAt(OutSlot);
    };

    for (Register R : GCRegs) {
      if (!LIS.hasInterval(R) || !LiveAcross(LIS.getInterval(R)))
        continue;
      if (!VRM.hasPhys(R))
        continue;
      MCRegister Phys = VRM.getPhys(R);
      bool Clobbered =
          RegMask && MachineOperand::clobbersPhysReg(RegMask, Phys);
      if (ListedRegs.count(R)) {
        if (Clobbered) {
          ++NumClobberedRegRoots;
          Report(*MI) << "listed gc vreg " << printReg(R, TRI) << " in "
                      << printReg(Phys, TRI)
                      << " is clobbered by the call's regmask\n";
        }
      } else if (Clobbered) {
        ++NumClobberedRegRoots;
        Report(*MI) << "gc vreg " << printReg(R, TRI)
                    << " live across in regmask-clobbered register "
                    << printReg(Phys, TRI) << "\n";
      } else {
        bool ValueListedHere = false;
        for (Register L : ListedRegs)
          if (ValueComp.isEquivalent((int64_t)R.id(), (int64_t)L.id())) {
            ValueListedHere = true;
            break;
          }
        if (!ValueListedHere)
          for (int S : ListedSlots)
            if (ValueComp.isEquivalent((int64_t)R.id(), SlotNode(S))) {
              ValueListedHere = true;
              break;
            }
        if (ValueListedHere) {
          // Register entries are only resolvable at alloc-family
          // statepoints (gc_regs bucket); elsewhere an unlisted preserved
          // copy is the EXPECTED state — it is a hazard only if read
          // after the call, which slot-only relocation prevents for
          // RS4GC-tracked values. Report real findings at alloc sites,
          // info elsewhere.
          if (isAllocFamilyMask(RegMask, TRI)) {
            ++NumUnlistedRegRoots;
            Report(*MI) << "gc vreg " << printReg(R, TRI)
                        << " is a second, unlisted register location ("
                        << printReg(Phys, TRI)
                        << ") of a value listed at this statepoint "
                        << "(stale copy after GC)\n";
          } else {
            errs() << "[oxgc-verify-info] " << MF.getName()
                   << ": statepoint ID " << StatepointOpers(MI).getID()
                   << " at " << Idx << ": gc vreg " << printReg(R, TRI)
                   << " preserved in " << printReg(Phys, TRI)
                   << " across a non-alloc statepoint (unlistable; stale"
                   << " if read after the call)\n";
          }
        } else {
          ++NumUnlistedFamilyRegs;
          errs() << "[oxgc-verify-info] " << MF.getName()
                 << ": statepoint ID " << StatepointOpers(MI).getID()
                 << " at " << Idx << ": gc-marked vreg " << printReg(R, TRI)
                 << " live across in " << printReg(Phys, TRI)
                 << " but its value is not listed here at all (IR liveness"
                 << " says dead or immediate)\n";
        }
      }
    }

    // Stack-growth statepoints never collect: caml_try_realloc_stack is
    // CAMLnoalloc and the frame is copied verbatim to the new stack, so
    // heap values in unlisted slots survive unchanged. Their listings
    // exist for stack-internal state (trap pointers), not heap roots.
    bool IsStackRealloc = false;
    if (const MachineOperand &Callee =
            StatepointOpers(MI).getCallTarget();
        Callee.isGlobal() && Callee.getGlobal()->getName().contains(
                                 "call_realloc_stack"))
      IsStackRealloc = true;

    for (int Slot : FamilySlots) {
      if (IsStackRealloc)
        break;
      if (ListedSlots.count(Slot) || !LS.hasInterval(Slot))
        continue;
      if (!LiveAcross(LS.getInterval(Slot)))
        continue;
      bool Dominated = false;
      for (const auto &[Src, StIdx] : GCV.storesOf(Slot)) {
        MachineInstr *St = Indexes.getInstructionFromIndex(StIdx);
        if (St && MDT.dominates(St, MI)) {
          Dominated = true;
          break;
        }
      }
      // A slot whose content crossing THIS statepoint is never read is
      // not a hazard even when unlisted: the entry-init machinery makes
      // slots live from function entry, but the init copy is dead in
      // the gap before the first real store. Benign when every load of
      // the slot must-reads a store sequenced after this statepoint.
      bool ContentObserved = GCV.mayObserveContent(Slot, Idx);
      // The listing pass requires a dominating store (or an entry
      // init); slotValueAt is deliberately optimistic on storeless
      // paths, so an undominated slot is not provably-value-on-entry.
      if (!ContentObserved || !Dominated ||
          !GCV.slotValueAt(Slot, InSlot)) {
        // Content here is not provably a gc value (raw range of a
        // colored slot, or an unprovable producer): the listing pass
        // skips these deliberately. Visibility without the violation.
        errs() << "[oxgc-verify-info] " << MF.getName()
               << ": statepoint ID " << StatepointOpers(MI).getID()
               << " at " << Idx << ": family slot %stack." << Slot
               << " live across, unlisted, content not provably a"
               << " value\n";
        continue;
      }
      ++NumUnlistedSlotRoots;
      Report(*MI) << "gc family spill slot %stack." << Slot
                  << " live across holding a provable gc value but not"
                  << " listed\n";
    }

    for (Register R : DerivedRegs) {
      if (GCRegs.contains(R))
        continue;
      if (!LIS.hasInterval(R) || !LiveAcross(LIS.getInterval(R)))
        continue;
      ++NumDerivedAcross;
      Report(*MI) << "gc-derived vreg " << printReg(R, TRI)
                  << " live across (interior pointer not updated by GC)\n";
    }
  }

  if (Violations && OxCamlGCRootVerifierFatal)
    report_fatal_error("oxcaml gc root verifier found " + Twine(Violations) +
                       " violation(s) in " + MF.getName());
  return false;
}
