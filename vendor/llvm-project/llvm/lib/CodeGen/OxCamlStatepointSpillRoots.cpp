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
/// rewritten, while VirtRegMap, LiveIntervals and LiveStacks are still
/// available. It identifies every virtual register holding a gc value (the
/// gc bit seeded at instruction emission, statepoint gc operands, and their
/// closure over COPYs) and lists every unlisted location of such a value
/// that is live across a statepoint: the spill slots of the registers'
/// original (pre-splitting) live ranges, and the registers themselves when
/// they survive the call in a regmask-preserved physical register. Both
/// become ordinary frametable roots that the GC updates in place.
/// Listed slots whose stores do not dominate a listing statepoint (loop
/// carried spills) additionally get an entry-block initialization with a
/// known value, since the GC must never read uninitialized slot contents.
///
//===----------------------------------------------------------------------===//

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/LiveStacks.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SlotIndexes.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "oxcaml-statepoint-spill-roots"

STATISTIC(NumSlotsAppended,
          "Number of sibling spill slots appended to statepoints");
STATISTIC(NumRegsAppended,
          "Number of crossing gc registers appended to statepoints");
STATISTIC(NumSlotInits,
          "Number of listed spill slots initialized at function entry");

static cl::opt<bool> EnableOxCamlStatepointSpillRoots(
    "oxcaml-statepoint-spill-roots", cl::Hidden, cl::init(true),
    cl::desc("Append statepoint-crossing sibling spill slots of gc values "
             "to the statepoint gc operand list"));

static cl::opt<bool> EnableOxCamlStatepointRegisterRoots(
    "oxcaml-statepoint-register-roots", cl::Hidden, cl::init(true),
    cl::desc("Append regmask-preserved registers holding gc values across a "
             "statepoint to the statepoint gc operand list"));

static cl::opt<bool> EnableOxCamlStatepointSlotInit(
    "oxcaml-statepoint-slot-init", cl::Hidden, cl::init(true),
    cl::desc("Initialize listed-but-undominated slots at function entry; "
             "with this off such slots are skipped instead"));

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
    AU.addRequired<LiveIntervals>();
    AU.addRequired<VirtRegMap>();
    AU.addRequired<MachineDominatorTree>();
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
INITIALIZE_PASS_DEPENDENCY(LiveIntervals)
INITIALIZE_PASS_DEPENDENCY(VirtRegMap)
INITIALIZE_PASS_DEPENDENCY(MachineDominatorTree)
INITIALIZE_PASS_END(OxCamlStatepointSpillRoots, DEBUG_TYPE,
                    "OxCaml Statepoint Spill Slot Roots", false, false)

/// Append the spill slots in \p SlotsToAdd and the (virtual) registers in
/// \p RegsToAdd to the gc-pointer section of \p MI, updating the gc-pointer
/// count and the base/derived gc map. Register records are plain uses with
/// in-place update semantics: the GC rewrites the register, so later reads
/// of the same virtual register see the relocated value.
static void appendRootsToStatepoint(MachineInstr &MI, MachineFunction &MF,
                                    ArrayRef<int> SlotsToAdd,
                                    ArrayRef<Register> RegsToAdd) {
  StatepointOpers SO(&MI);
  unsigned NumGCPtrCountIdx = SO.getNumGCPtrIdx();
  unsigned OldNumGCPtrs = MI.getOperand(NumGCPtrCountIdx).getImm();
  unsigned AllocaCountIdx = SO.getNumAllocaIdx();
  unsigned MapCountIdx = SO.getNumGcMapEntriesIdx();
  unsigned OldMapCount = MI.getOperand(MapCountIdx).getImm();
  unsigned NumToAdd = SlotsToAdd.size() + RegsToAdd.size();

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
  for (int Slot : SlotsToAdd) {
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
  for (Register R : RegsToAdd) {
    MI.addOperand(MF, MachineOperand::CreateReg(R, /*isDef=*/false));
    // Copy-closure gc-ness is gc-ness; record it so the gc-root verifier
    // does not see the new operand as a bit-propagation gap.
    MF.getRegInfo().setOxCamlGCPtr(R);
  }

  auto AppendNewPairs = [&]() {
    for (unsigned J = 0; J < NumToAdd; ++J) {
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
  // the gc map count shifted by the inserted gc pointer record operands
  // (4 per folded slot, 1 per register).
  MI.getOperand(NumGCPtrCountIdx).setImm(OldNumGCPtrs + NumToAdd);
  MI.getOperand(MapCountIdx + 4 * SlotsToAdd.size() + RegsToAdd.size())
      .setImm(OldMapCount + NumToAdd);
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

/// Frame indices whose content is a gc VALUE whenever the program loads
/// from them, because the runtime keeps them up to date in place:
/// - ISel statepoint pool slots (gc-section folded operands without a
///   LiveStacks interval): only ever written with spilled gc values;
/// - gc-alloca-section slots (exnroot homes): initialized with values and
///   rewritten by the GC at every statepoint that lists them.
/// A load from such a slot yields a gc value even when the loaded value
/// never appears as a statepoint gc operand — the exnroot-only case is
/// how values cross try-wrapped calls, and allocator re-spills of those
/// loads were the unlisted-root miscompile found via typecore+young-flip.
static void collectValueHomeFIs(ArrayRef<MachineInstr *> Statepoints,
                                LiveStacks &LS,
                                SmallSet<int, 16> &ListedSlotsAnywhere,
                                SmallSet<int, 16> &ValueHomeFIs) {
  for (const MachineInstr *MI : Statepoints) {
    walkGCPtrSection(*MI, ListedSlotsAnywhere, nullptr);
    walkAllocaSection(*MI, ValueHomeFIs);
  }
  for (int FI : ListedSlotsAnywhere)
    if (!LS.hasInterval(FI))
      ValueHomeFIs.insert(FI);
}

/// Identify every virtual register known to hold a gc value: statepoint gc
/// register operands, registers carrying the gc bit (seeded at instruction
/// emission, inherited through splits, merged by the coalescer), registers
/// loaded from value-home slots (ISel pool slots and exnroot allocas), and
/// the closure of all of those over COPYs. This matters for values whose
/// only statepoint presence is a folded slot or an exnroot: a reload that
/// is re-spilled by the allocator creates a second location that no
/// statepoint operand ever names. Copies and phi joins preserve "holds an
/// OCaml value", so every live range of a marked register (and its spill
/// slot, while live) is a valid value for the GC to scan: pointers get
/// relocated, tagged immediates are skipped by the tag check.
static void computeGCRegs(MachineFunction &MF,
                          ArrayRef<MachineInstr *> Statepoints,
                          const SmallSet<int, 16> &ValueHomeFIs,
                          const TargetInstrInfo *TII,
                          DenseSet<Register> &GCRegs) {
  MachineRegisterInfo &MRI = MF.getRegInfo();
  for (unsigned I = 0, E = MRI.getNumVirtRegs(); I != E; ++I) {
    Register R = Register::index2VirtReg(I);
    if (MRI.isOxCamlGCPtr(R))
      GCRegs.insert(R);
  }
  for (const MachineInstr *MI : Statepoints) {
    SmallSet<int, 16> Ignored;
    SmallVector<Register, 16> OperandRegs;
    walkGCPtrSection(*MI, Ignored, &OperandRegs);
    GCRegs.insert(OperandRegs.begin(), OperandRegs.end());
  }
  for (const MachineBasicBlock &MBB : MF)
    for (const MachineInstr &MI : MBB) {
      int FI;
      if (Register R = TII->isLoadFromStackSlot(MI, FI))
        if (R.isVirtual() && ValueHomeFIs.count(FI))
          GCRegs.insert(R);
    }

  bool Changed = true;
  while (Changed) {
    Changed = false;
    for (const MachineBasicBlock &MBB : MF)
      for (const MachineInstr &MI : MBB) {
        if (!MI.isCopy())
          continue;
        Register Dst = MI.getOperand(0).getReg();
        Register Src = MI.getOperand(1).getReg();
        if (Dst.isVirtual() && Src.isVirtual() && GCRegs.contains(Src) &&
            !GCRegs.contains(Dst)) {
          GCRegs.insert(Dst);
          Changed = true;
        }
      }
  }
}

/// Spill slots of the originals (pre-splitting live ranges) of all gc
/// registers. Slots are per-original until stack slot coloring (which runs
/// later), so a slot whose live interval crosses a statepoint always
/// carries the value of one gc live-range family if it belongs to this set.
static void collectGCSlots(const DenseSet<Register> &GCRegs, VirtRegMap &VRM,
                           SmallSet<int, 16> &GCSlots) {
  for (Register R : GCRegs) {
    int Slot = VRM.getStackSlot(VRM.getOriginal(R));
    if (Slot != VirtRegMap::NO_STACK_SLOT)
      GCSlots.insert(Slot);
  }
}

/// True for statepoints whose callee enters the runtime through
/// SAVE_ALL_REGS (the OxCaml alloc family: caml_call_gc and the realloc
/// entries). Only these populate Caml_state->gc_regs, so frame-descriptor
/// REGISTER roots are only meaningful at these sites: scan_stack_frames
/// resolves odd live_ofs entries against the gc_regs bucket, which at any
/// other safepoint (e.g. a C call that allocates) dangles at the last
/// freed bucket — a register entry there hands the GC garbage. The family
/// is identified by its regmask: the alloc convention preserves x0, which
/// no other OxCaml callee does (both the ordinary and C-call conventions
/// return results in it).
static bool isAllocFamilyStatepoint(const MachineInstr &MI,
                                    const TargetRegisterInfo *TRI);

static const uint32_t *getStatepointRegMask(const MachineInstr &MI) {
  for (const MachineOperand &MO : MI.operands())
    if (MO.isRegMask())
      return MO.getRegMask();
  return nullptr;
}

static bool isAllocFamilyStatepoint(const MachineInstr &MI,
                                    const TargetRegisterInfo *TRI) {
  const uint32_t *RegMask = getStatepointRegMask(MI);
  if (!RegMask)
    return false;
  for (unsigned R = 1, E = TRI->getNumRegs(); R != E; ++R)
    if (StringRef(TRI->getName(R)) == "X0")
      return !MachineOperand::clobbersPhysReg(RegMask, R);
  return false;
}

namespace {

/// Flow-sensitive value identity. The gc bit and its COPY closure are
/// per-register and therefore per-FAMILY: after coalescing and phi
/// elimination one register can hold a gc value in one live range and an
/// unrelated raw integer in another. Listing a location whose content at
/// the statepoint is raw hands the GC a garbage root; if the bits alias a
/// young-heap address the GC forwards garbage and corrupts the heap. This
/// analysis decides VALUE-ness per live-range value (VNInfo): a location
/// may be listed at a statepoint only if the value it holds THERE provably
/// is an OCaml value.
///
/// Value sources (seeds): statepoint defs (relocated values, both register
/// tied-defs and folded spill-slot defs); listed statepoint gc operands at
/// their statepoint (RS4GC vouches for the content); loads from ISel
/// statepoint pool slots (those slots only ever hold spilled gc values);
/// entry-block copies of addrspace(1) formal arguments. Value-ness is
/// inherited through COPYs, spill stores and reloads, and through phi
/// joins when every incoming value is a value. Everything else (arithmetic
/// results, immediates, ordinary loads) is not a value. Cycles are
/// resolved optimistically, which is exact because every live cycle is
/// entered through a dominating definition.
class GCValueness {
  using Key = std::pair<int64_t, unsigned>; // (reg id, VNI id)

  MachineFunction &MF;
  LiveIntervals &LIS;
  LiveStacks &LS;
  SlotIndexes &Indexes;
  const TargetInstrInfo *TII;
  const SmallSet<int, 16> &ValueHomeFIs;
  DenseSet<Key> Seeds;
  // 1 = value, 2 = not a value, 3 = in progress (optimistic).
  DenseMap<Key, unsigned char> Memo;
  // All explicit stores to each LiveStacks slot. Slot contents are judged
  // per query point by REACHING stores, not per-FI: stack slot coloring
  // runs before this pass and merges disjoint live ranges of different
  // vregs into one frame index, so a single slot routinely holds gc values
  // on some ranges and raw data on others. The whole-slot summary both
  // under-lists (a live gc value skipped because an unrelated range stored
  // raw data — a missed root, stale after GC) and over-lists.
  DenseMap<int, SmallVector<std::pair<Register, SlotIndex>, 4>> SlotStores;
  // All reloads from each LiveStacks slot, for consumer evidence.
  DenseMap<int, SmallVector<std::pair<Register, SlotIndex>, 4>> SlotLoads;
  // (FI, MBB number) -> value-ness of the slot content at block exit.
  // 1 = value, 2 = not a value, 3 = in progress (optimistic).
  DenseMap<std::pair<int, int>, unsigned char> SlotExitMemo;

  bool regValue(Register R, const VNInfo *VNI);
  bool storeValue(int FI, Register Src, SlotIndex StIdx);
  bool physRegHoldsValueAt(Register Src, MachineInstr *At);
  const std::pair<Register, SlotIndex> *
  lastStoreBefore(int FI, const MachineBasicBlock *MBB, SlotIndex Before);
  bool slotExitValue(int FI, const MachineBasicBlock *MBB);

  /// All-predecessors-are-values for a phi-defined VNI of \p R.
  bool phiValue(Register R, const LiveInterval &LI, const VNInfo *VNI);

public:
  GCValueness(MachineFunction &MF, ArrayRef<MachineInstr *> Statepoints,
              const SmallSet<int, 16> &ValueHomeFIs, LiveIntervals &LIS,
              LiveStacks &LS, SlotIndexes &Indexes,
              const TargetInstrInfo *TII);

  bool regValueAt(Register R, SlotIndex Idx) {
    if (!LIS.hasInterval(R))
      return false;
    const VNInfo *VNI = LIS.getInterval(R).getVNInfoAt(Idx);
    return VNI && regValue(R, VNI);
  }

  bool slotValueAt(int FI, SlotIndex Idx);

  ArrayRef<std::pair<Register, SlotIndex>> storesOf(int FI) const {
    auto It = SlotStores.find(FI);
    return It == SlotStores.end()
               ? ArrayRef<std::pair<Register, SlotIndex>>()
               : ArrayRef<std::pair<Register, SlotIndex>>(It->second);
  }
};

} // end anonymous namespace

GCValueness::GCValueness(MachineFunction &MF,
                         ArrayRef<MachineInstr *> Statepoints,
                         const SmallSet<int, 16> &ValueHomeFIs,
                         LiveIntervals &LIS, LiveStacks &LS,
                         SlotIndexes &Indexes, const TargetInstrInfo *TII)
    : MF(MF), LIS(LIS), LS(LS), Indexes(Indexes), TII(TII),
      ValueHomeFIs(ValueHomeFIs) {
  for (const MachineInstr *MI : Statepoints) {
    SmallSet<int, 16> ListedSlots;
    SmallVector<Register, 16> ListedRegs;
    walkGCPtrSection(*MI, ListedSlots, &ListedRegs);
    SlotIndex Idx = Indexes.getInstructionIndex(*MI);
    for (Register R : ListedRegs) {
      // Seed the listed operand's value, and follow COPY defs backward:
      // the copy SOURCE holds the same runtime value at that point, and
      // its other locations (e.g. its spill slot, stored before the
      // copy) are only provable through it. This matters for values
      // whose producing instruction the structural rules cannot read,
      // e.g. an allocation's STRXpre write-back: with in-place lowering
      // the listed operand is often a spiller copy of it.
      Register Cur = R;
      SlotIndex At = Idx.getRegSlot(true);
      for (unsigned Hops = 0; Hops < 8 && Cur.isVirtual(); ++Hops) {
        if (!LIS.hasInterval(Cur))
          break;
        const VNInfo *VNI = LIS.getInterval(Cur).getVNInfoAt(At);
        if (!VNI)
          break;
        if (!Seeds.insert({(int64_t)Cur.id(), VNI->id}).second)
          break;
        MachineInstr *D = Indexes.getInstructionFromIndex(VNI->def);
        if (!D || !D->isCopy() || D->getOperand(0).getSubReg() ||
            D->getOperand(1).getSubReg())
          break;
        Cur = D->getOperand(1).getReg();
        At = VNI->def.getRegSlot(true);
      }
    }
  }
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB) {
      // A store the IR typed as writing a ptr addrspace(1) value (see
      // MOOxCamlGCValue, e.g. heap field initialization): its source vreg
      // holds a gc value at that point. The stored-value operand follows
      // the explicit defs (STRXui: operand 0; write-back STRXpre: the def
      // is the updated base, operand 1 is the value).
      if (MI.mayStore() && !MI.isCall() && MI.hasOneMemOperand() &&
          (MI.memoperands().front()->getFlags() & MOOxCamlGCValue)) {
        unsigned ValIdx = MI.getNumExplicitDefs();
        if (ValIdx < MI.getNumExplicitOperands()) {
          const MachineOperand &MO = MI.getOperand(ValIdx);
          if (MO.isReg() && MO.isUse() && MO.getReg().isVirtual() &&
              LIS.hasInterval(MO.getReg())) {
            SlotIndex Idx = Indexes.getInstructionIndex(MI);
            const LiveInterval &LI = LIS.getInterval(MO.getReg());
            const VNInfo *VNI = LI.getVNInfoAt(Idx.getRegSlot(true));
            if (!VNI)
              VNI = LI.getVNInfoBefore(Idx.getRegSlot());
            if (VNI)
              Seeds.insert({(int64_t)MO.getReg().id(), VNI->id});
          }
        }
      }
      int FI;
      if (Register Dst = TII->isLoadFromStackSlot(MI, FI))
        if (LS.hasInterval(FI))
          SlotLoads[FI].push_back(
              {Dst, Indexes.getInstructionIndex(MI)});
      if (Register Src = TII->isStoreToStackSlot(MI, FI)) {
        if (LS.hasInterval(FI))
          SlotStores[FI].push_back(
              {Src, Indexes.getInstructionIndex(MI)});
        // A store into a value-home slot (ISel statepoint pool slot or
        // exnroot alloca) marks its source as a gc value: ISel only spills
        // gc-section values there (llvmize deopt args are all constants,
        // so the pool never holds raw data). This is the only way to see
        // value-ness of vregs whose def ISel folded into an instruction
        // the structural rules below cannot interpret, e.g. the alloc
        // fast path's STRXpre write-back that forms the block pointer
        // while storing its first field.
        if (ValueHomeFIs.count(FI) && Src.isVirtual() &&
            LIS.hasInterval(Src)) {
          SlotIndex StIdx = Indexes.getInstructionIndex(MI);
          const LiveInterval &SrcLI = LIS.getInterval(Src);
          const VNInfo *VNI = SrcLI.getVNInfoAt(StIdx.getRegSlot(true));
          if (!VNI)
            VNI = SrcLI.getVNInfoBefore(StIdx.getRegSlot());
          if (VNI)
            Seeds.insert({(int64_t)Src.id(), VNI->id});
        }
      }
    }
}

bool GCValueness::phiValue(Register R, const LiveInterval &LI,
                           const VNInfo *VNI) {
  MachineBasicBlock *MBB = Indexes.getMBBFromIndex(VNI->def);
  if (!MBB || MBB->pred_empty())
    return false;
  for (MachineBasicBlock *Pred : MBB->predecessors()) {
    const VNInfo *Q = LI.getVNInfoBefore(Indexes.getMBBEndIdx(Pred));
    if (!Q || !regValue(R, Q))
      return false;
  }
  return true;
}

bool GCValueness::regValue(Register R, const VNInfo *VNI) {
  if (!VNI || VNI->isUnused())
    return false;
  Key K{(int64_t)R.id(), VNI->id};
  if (Seeds.contains(K))
    return true;
  auto It = Memo.find(K);
  if (It != Memo.end())
    return It->second != 2; // in progress counts as value (optimistic)
  Memo[K] = 3;
  bool V = false;
  const LiveInterval &LI = LIS.getInterval(R);
  if (VNI->isPHIDef()) {
    V = phiValue(R, LI, VNI);
  } else if (MachineInstr *D = Indexes.getInstructionFromIndex(VNI->def)) {
    if (D->getOpcode() == TargetOpcode::STATEPOINT) {
      // Tied relocate def: the relocated value.
      V = true;
    } else if (D->isCopy() && D->getOperand(0).getReg() == R &&
               !D->getOperand(0).getSubReg() &&
               !D->getOperand(1).getSubReg()) {
      Register Src = D->getOperand(1).getReg();
      if (Src.isVirtual()) {
        if (LIS.hasInterval(Src)) {
          const VNInfo *Q = LIS.getInterval(Src).getVNInfoAt(
              VNI->def.getRegSlot(true));
          if (!Q)
            Q = LIS.getInterval(Src).getVNInfoBefore(VNI->def);
          V = Q && regValue(Src, Q);
        }
      } else {
        // Copy from a physical register: an incoming gc argument or an
        // in-place statepoint call's p1-typed result. Indirect callees
        // (closure calls) have no symbol to type, so additionally trust
        // the gc bit on the destination when copying from an ordinary
        // result/argument register (x0-x15; never the runtime registers
        // x26-x28, whose raw copies can share a coalesced family with
        // gc vregs): the bit on such copies comes from the IR result
        // type via FunctionLoweringInfo.
        bool BitTrust = false;
        if (MF.getRegInfo().isOxCamlGCPtr(R)) {
          StringRef SrcName = MF.getSubtarget()
                                  .getRegisterInfo()
                                  ->getName(Src.asMCReg());
          unsigned K;
          BitTrust = SrcName.consume_front("X") &&
                     !SrcName.getAsInteger(10, K) && K <= 15;
        }
        V = (D->getParent() == &MF.front() &&
             MF.getRegInfo().isOxCamlGCArg(R)) ||
            BitTrust || physRegHoldsValueAt(Src, D);
      }
    } else {
      int FI;
      if (Register LR = TII->isLoadFromStackSlot(*D, FI)) {
        if (LR == R) {
          if (ValueHomeFIs.count(FI))
            V = true;
          else if (LS.hasInterval(FI))
            V = slotValueAt(FI, VNI->def);
        }
      } else if (D->mayStore() && D->hasOneMemOperand() &&
                 D->memoperands().front()->getAddrSpace() == 1 &&
                 D->getNumExplicitDefs() == 1 &&
                 D->getOperand(0).isReg() &&
                 D->getOperand(0).getReg() == R &&
                 D->getNumExplicitOperands() >= 4 &&
                 D->getOperand(2).isReg() &&
                 D->getOperand(2).getReg() == R &&
                 D->getOperand(3).isImm() &&
                 D->getOperand(3).getImm() == 8) {
        // Allocation write-back: llvmize forms the block value pointer
        // by storing the first field with a pre-indexed store at +8
        // from the header pointer (str xV, [xH, #8]!); the written-back
        // register IS the new block pointer. llvmize emits no other
        // tied write-back addrspace(1) stores at +8 (field stores use
        // absolute offsets), so the shape identifies the alloc result.
        V = true;
      } else if (D->getNumExplicitOperands() >= 2 &&
                 D->getNumExplicitDefs() == 1 &&
                 D->getOperand(0).isReg() &&
                 D->getOperand(0).getReg() == R &&
                 D->getOperand(1).isGlobal() &&
                 isa<GlobalVariable>(D->getOperand(1).getGlobal()) &&
                 D->getOperand(1).getGlobal()->getName().contains(
                     "caml")) {
        // Materialized address of an OCaml static data block (MOVaddr /
        // LOADgot of a caml data symbol). Statics are values: the GC
        // classifies them NOT_MARKABLE and leaves them alone, and they
        // never move, so listing such a root is sound. Rejecting them
        // would poison every phi mixing a static default with heap
        // values. Functions (code pointers) are excluded.
        V = true;
      } else if (D->isMoveImmediate() &&
                 D->getNumExplicitOperands() == 2 &&
                 D->getOperand(1).isImm() &&
                 (D->getOperand(1).getImm() & 1)) {
        // Odd immediates are tagged OCaml scalars: legitimate values the
        // GC ignores (not blocks). Common as the unit/nil seed of a phi
        // whose other arms carry real pointers; rejecting them would
        // poison the whole phi.
        V = true;
      } else if (D->mayLoad() && D->hasOneMemOperand() &&
                 (D->memoperands().front()->getFlags() &
                  MOOxCamlGCValue) &&
                 D->getNumExplicitDefs() == 1 &&
                 D->getOperand(0).isReg() &&
                 D->getOperand(0).getReg() == R) {
        // A load the IR typed as ptr addrspace(1) (see MOOxCamlGCValue):
        // the loaded data is a gc value wherever it lives. This covers
        // heap field loads, which have no statepoint connection to trace.
        V = true;
      }
    }
  }
  Memo[K] = V ? 1 : 2;
  if (!V)
    if (const char *FN = getenv("OXSR_DEBUG_VAL"))
      if (MF.getName().contains(FN)) {
        errs() << "[oxsr-val] " << printReg(R) << " vni" << VNI->id
               << " NOT value; def=";
        if (VNI->isPHIDef())
          errs() << "phi at " << VNI->def << "\n";
        else if (MachineInstr *D = Indexes.getInstructionFromIndex(VNI->def))
          errs() << *D;
        else
          errs() << "unknown\n";
      }
  return V;
}

// Whether physical register \p Src holds an OCaml gc value when read by
// \p At. Physreg reads with no defining instruction of their own come
// from two producers: an incoming argument (no definition between entry
// and the read) or a statepoint call's result (the most recent
// definition is a STATEPOINT that implicit-defs the register). For the
// latter, a direct callee's llvmize signature {{i64,i64},{results...}}
// says whether the element riding this x-register (integer/pointer
// results take x0,x1,... in element order; float results use d-regs) is
// ptr addrspace(1).
bool GCValueness::physRegHoldsValueAt(Register Src, MachineInstr *At) {
  if (!At)
    return false;
  const TargetRegisterInfo *TRI = MF.getSubtarget().getRegisterInfo();
  MachineBasicBlock *MBB = At->getParent();
  MachineBasicBlock::iterator I(At);
  while (I != MBB->begin()) {
    --I;
    if (!I->modifiesRegister(Src, TRI))
      continue;
    if (I->getOpcode() != TargetOpcode::STATEPOINT)
      return false;
    // Regmask-clobbered is not a result; require an implicit def.
    bool IsResultReg = false;
    for (const MachineOperand &MO : I->operands())
      if (MO.isReg() && MO.isDef() && MO.isImplicit() &&
          MO.getReg() == Src)
        IsResultReg = true;
    if (!IsResultReg)
      return false;
    const MachineOperand &Target = StatepointOpers(&*I).getCallTarget();
    if (!Target.isGlobal())
      return false;
    const auto *Callee = dyn_cast<Function>(Target.getGlobal());
    if (!Callee)
      return false;
    StringRef RName = TRI->getName(Src.asMCReg());
    unsigned K;
    if (!RName.consume_front("X") || RName.getAsInteger(10, K))
      return false;
    const auto *Outer = dyn_cast<StructType>(Callee->getReturnType());
    if (!Outer || Outer->getNumElements() != 2)
      return false;
    const auto *Res = dyn_cast<StructType>(Outer->getElementType(1));
    if (!Res)
      return false;
    unsigned XIdx = 0;
    for (Type *ET : Res->elements()) {
      if (ET->isFloatingPointTy() || ET->isVectorTy())
        continue;
      if (XIdx++ == K) {
        if (const auto *PT = dyn_cast<PointerType>(ET))
          if (PT->getAddressSpace() == 1)
            return true;
        return false;
      }
    }
    return false;
  }
  // No definition before the read: the register still carries its value
  // from function entry; a gc formal argument is a value.
  if (MBB != &MF.front())
    return false;
  Register ArgV = MF.getRegInfo().getLiveInVirtReg(Src.asMCReg());
  if (ArgV.isValid() && MF.getRegInfo().isOxCamlGCArg(ArgV))
    return true;
  // The livein vreg is ISel's raw copy target and usually not the
  // marked argument vreg; map the register to its formal directly. The
  // OxCaml conventions pass integer/pointer arguments in x28, x27, x0,
  // x1, ... in declaration order (floats ride d-regs separately).
  StringRef RName = TRI->getName(Src.asMCReg());
  unsigned Ordinal;
  if (RName == "X28")
    Ordinal = 0;
  else if (RName == "X27")
    Ordinal = 1;
  else {
    unsigned K;
    if (!RName.consume_front("X") || RName.getAsInteger(10, K) || K > 15)
      return false;
    Ordinal = K + 2;
  }
  unsigned Idx = 0;
  for (Type *PT : MF.getFunction().getFunctionType()->params()) {
    if (PT->isFloatingPointTy() || PT->isVectorTy())
      continue;
    if (Idx++ == Ordinal) {
      if (const auto *P = dyn_cast<PointerType>(PT))
        return P->getAddressSpace() == 1;
      return false;
    }
  }
  return false;
}

bool GCValueness::storeValue(int FI, Register Src, SlotIndex StIdx) {
  if (Src.isVirtual()) {
    const VNInfo *Q = nullptr;
    if (LIS.hasInterval(Src)) {
      Q = LIS.getInterval(Src).getVNInfoAt(StIdx.getRegSlot(true));
      if (!Q)
        Q = LIS.getInterval(Src).getVNInfoBefore(StIdx.getRegSlot());
    }
    if (Q && regValue(Src, Q))
      return true;
  } else {
    // Spills of physical registers are spiller foldings of values that
    // never got their own instruction (incoming arguments, statepoint
    // call results).
    if (physRegHoldsValueAt(Src, Indexes.getInstructionFromIndex(StIdx)))
      return true;
  }
  // Consumer evidence: the spiller can fold a statepoint call's returned
  // gc value into a direct physreg store (STRXui $x0, slot), leaving no
  // traceable producer. If a reload of this same stored content is
  // proven a gc value by how it is consumed (seeded via a statepoint
  // pool store or an IR-typed gc value store), the store wrote a value.
  // Only the seed set is consulted (no recursion through slotValueAt).
  auto LI = SlotLoads.find(FI);
  if (LI == SlotLoads.end())
    return false;
  bool SingleStore = storesOf(FI).size() == 1;
  for (const auto &[Dst, LIdx] : LI->second) {
    if (!(StIdx < LIdx))
      continue;
    bool Reaches = false;
    if (const auto *LB = lastStoreBefore(
            FI, Indexes.getMBBFromIndex(LIdx), LIdx))
      Reaches = LB->second == StIdx;
    else
      Reaches = SingleStore;
    if (!Reaches)
      continue;
    if (!Dst.isVirtual() || !LIS.hasInterval(Dst))
      continue;
    const VNInfo *V = LIS.getInterval(Dst).getVNInfoAt(LIdx.getRegSlot());
    if (V && Seeds.contains({(int64_t)Dst.id(), V->id}))
      return true;
  }
  return false;
}

const std::pair<Register, SlotIndex> *
GCValueness::lastStoreBefore(int FI, const MachineBasicBlock *MBB,
                             SlotIndex Before) {
  auto It = SlotStores.find(FI);
  if (It == SlotStores.end())
    return nullptr;
  const std::pair<Register, SlotIndex> *Best = nullptr;
  for (const auto &P : It->second) {
    if (!(P.second < Before))
      continue;
    if (Indexes.getMBBFromIndex(P.second) != MBB)
      continue;
    if (!Best || Best->second < P.second)
      Best = &P;
  }
  return Best;
}

// Value-ness of the slot content at \p MBB's exit. Statepoint relocation
// rewrites of listed slots preserve value-ness, so only explicit stores
// matter. A path that reaches the function entry with no store carries
// uninitialized content; that is treated optimistically here because
// listing such slots is separately guarded by the store-dominance check
// and the entry-init machinery (which installs a known gc value).
bool GCValueness::slotExitValue(int FI, const MachineBasicBlock *MBB) {
  auto K = std::make_pair(FI, MBB->getNumber());
  auto It = SlotExitMemo.find(K);
  if (It != SlotExitMemo.end())
    return It->second != 2; // in progress counts as value (optimistic)
  SlotExitMemo[K] = 3;
  bool V = true;
  if (const auto *Last =
          lastStoreBefore(FI, MBB, Indexes.getMBBEndIdx(MBB))) {
    V = storeValue(FI, Last->first, Last->second);
  } else {
    for (const MachineBasicBlock *Pred : MBB->predecessors())
      if (!slotExitValue(FI, Pred)) {
        V = false;
        break;
      }
  }
  SlotExitMemo[K] = V ? 1 : 2;
  return V;
}

bool GCValueness::slotValueAt(int FI, SlotIndex Idx) {
  const MachineBasicBlock *MBB = Indexes.getMBBFromIndex(Idx);
  if (!MBB)
    return false;
  if (const auto *Last = lastStoreBefore(FI, MBB, Idx))
    return storeValue(FI, Last->first, Last->second);
  for (const MachineBasicBlock *Pred : MBB->predecessors())
    if (!slotExitValue(FI, Pred))
      return false;
  return true;
}

namespace {

/// Liveness and defined-ness for value-home frame indices (ISel statepoint
/// pool slots and exnroot allocas), which have no LiveStacks intervals.
/// These slots hold copies of gc values that ISel lists only at the
/// statepoints it spilled them for; at ALLOC statepoints the same values
/// are forced into registers (tied defs) while the slot copies stay live
/// and are reloaded afterwards — the cont-pattern mass staleness. A slot
/// is live after a point if a load (or a statepoint listing it, which the
/// GC both reads and rewrites) is reachable with no intervening store; it
/// is defined at a point if a store or listing reaches it on every path.
/// Both directions must be exact over the access graph: over-listing makes
/// the GC chase a dead slot's stale contents, under-listing recreates the
/// missed-root bug. The access sets are complete because these slots are
/// only touched by plain spill stores/reloads and statepoint listings.
class ValueHomeLiveness {
  enum : char { Load = 0, Store = 1, Use = 2 /* listed: read+write */ };
  struct PerFI {
    // Indexed by MBB number.
    std::vector<SmallVector<std::pair<SlotIndex, char>, 2>> Acc;
    BitVector LiveOut, DefIn;
  };
  DenseMap<int, PerFI> FIs;
  SlotIndexes &Indexes;
  MachineFunction &MF;

public:
  ValueHomeLiveness(MachineFunction &MF,
                    ArrayRef<MachineInstr *> Statepoints,
                    const SmallSet<int, 16> &ValueHomeFIs,
                    const TargetInstrInfo *TII, SlotIndexes &Indexes)
      : Indexes(Indexes), MF(MF) {
    unsigned N = MF.getNumBlockIDs();
    auto Get = [&](int FI) -> PerFI & {
      PerFI &P = FIs[FI];
      if (P.Acc.empty()) {
        P.Acc.resize(N);
        P.LiveOut.resize(N);
        P.DefIn.resize(N);
      }
      return P;
    };
    for (MachineBasicBlock &MBB : MF)
      for (MachineInstr &MI : MBB) {
        int FI;
        if (TII->isLoadFromStackSlot(MI, FI) && ValueHomeFIs.count(FI))
          Get(FI).Acc[MBB.getNumber()].push_back(
              {Indexes.getInstructionIndex(MI), Load});
        else if (TII->isStoreToStackSlot(MI, FI) && ValueHomeFIs.count(FI))
          Get(FI).Acc[MBB.getNumber()].push_back(
              {Indexes.getInstructionIndex(MI), Store});
      }
    for (MachineInstr *MI : Statepoints) {
      SmallSet<int, 16> Listed;
      walkGCPtrSection(*MI, Listed, nullptr);
      walkAllocaSection(*MI, Listed);
      for (int FI : Listed)
        if (ValueHomeFIs.count(FI))
          Get(FI).Acc[MI->getParent()->getNumber()].push_back(
              {Indexes.getInstructionIndex(*MI), Use});
    }
    for (auto &[FI, P] : FIs) {
      for (auto &V : P.Acc)
        llvm::sort(V);
      // Backward liveness to fixpoint over all blocks.
      bool Changed = true;
      while (Changed) {
        Changed = false;
        for (MachineBasicBlock &MBB : MF) {
          unsigned B = MBB.getNumber();
          bool Out = false;
          for (MachineBasicBlock *S : MBB.successors()) {
            unsigned SB = S->getNumber();
            bool SIn = P.Acc[SB].empty() ? P.LiveOut[SB]
                                         : P.Acc[SB].front().second == Load ||
                                               P.Acc[SB].front().second == Use;
            if (SIn) {
              Out = true;
              break;
            }
          }
          if (Out != P.LiveOut[B]) {
            P.LiveOut[B] = Out;
            Changed = true;
          }
        }
      }
      // Forward defined-ness to fixpoint (pessimistic start, grow defined).
      // DefIn(B) = all preds DefOut; DefOut(B) = anyStore(B) || DefIn(B).
      // Initialize optimistically (all defined except entry) and demote.
      BitVector DefOut(MF.getNumBlockIDs(), true);
      P.DefIn.set();
      P.DefIn[MF.front().getNumber()] = false;
      bool DChanged = true;
      while (DChanged) {
        DChanged = false;
        for (MachineBasicBlock &MBB : MF) {
          unsigned B = MBB.getNumber();
          bool In = true;
          if (&MBB == &MF.front())
            In = false;
          else
            for (MachineBasicBlock *Pred : MBB.predecessors())
              if (!DefOut[Pred->getNumber()]) {
                In = false;
                break;
              }
          bool AnyStore = llvm::any_of(
              P.Acc[B], [](auto &A) { return A.second != Load; });
          bool Out = In || AnyStore;
          if (In != P.DefIn[B] || Out != DefOut[B]) {
            P.DefIn[B] = In;
            DefOut[B] = Out;
            DChanged = true;
          }
        }
      }
    }
  }

  bool liveAfter(int FI, MachineInstr &S) {
    auto It = FIs.find(FI);
    if (It == FIs.end())
      return false;
    PerFI &P = It->second;
    unsigned B = S.getParent()->getNumber();
    SlotIndex Idx = Indexes.getInstructionIndex(S);
    for (auto &[AIdx, K] : P.Acc[B])
      if (AIdx > Idx)
        return K != Store;
    return P.LiveOut[B];
  }

  bool definedAt(int FI, MachineInstr &S) {
    auto It = FIs.find(FI);
    if (It == FIs.end())
      return false;
    PerFI &P = It->second;
    unsigned B = S.getParent()->getNumber();
    SlotIndex Idx = Indexes.getInstructionIndex(S);
    bool In = P.DefIn[B];
    for (auto &[AIdx, K] : P.Acc[B]) {
      if (AIdx >= Idx)
        break;
      if (K != Load)
        In = true;
    }
    return In;
  }
};

} // end anonymous namespace

static bool processStatepoint(MachineInstr &MI, MachineFunction &MF,
                              const SmallSet<int, 16> &GCSlots,
                              const DenseSet<Register> &GCRegs,
                              const SmallSet<int, 16> &ValueHomeFIs,
                              GCValueness &GCV, ValueHomeLiveness &VHL,
                              LiveStacks &LS, LiveIntervals &LIS,
                              VirtRegMap &VRM, SlotIndexes &Indexes,
                              MachineDominatorTree &MDT, bool CanInitSlots,
                              SmallSet<int, 16> &SlotsNeedingInit) {
  SmallSet<int, 16> ListedSlots;
  SmallVector<Register, 16> ListedRegsVec;
  walkGCPtrSection(MI, ListedSlots, &ListedRegsVec);
  SmallSet<Register, 16> ListedRegs;
  ListedRegs.insert(ListedRegsVec.begin(), ListedRegsVec.end());

  // The location must hold a valid value on entry to the statepoint (live
  // at the early-clobber slot) and still be live during it (live at the
  // register slot): only then can the GC read and update it. A location
  // whose live range merely STARTS at the statepoint (a folded relocate-def
  // spill, or the statepoint's own defs) contains garbage on entry and must
  // not be listed.
  SlotIndex Idx = Indexes.getInstructionIndex(MI);
  SlotIndex InSlot = Idx.getRegSlot(true);
  SlotIndex OutSlot = Idx.getRegSlot();
  SmallVector<int, 8> SlotsToAdd;
  bool DebugSlots =
      VerboseOxCamlStatepointSpillRoots && getenv("OXSR_DEBUG_SLOTS");
  for (int Slot : GCSlots) {
    if (ListedSlots.count(Slot))
      continue;
    if (!LS.hasInterval(Slot)) {
      if (DebugSlots)
        errs() << "[oxsr] " << MF.getName() << " at " << Idx
               << ": no LiveStacks interval %stack." << Slot << "\n";
      continue;
    }
    const LiveInterval &LI = LS.getInterval(Slot);
    if (!LI.liveAt(InSlot) || !LI.liveAt(OutSlot)) {
      if (DebugSlots)
        errs() << "[oxsr] " << MF.getName() << " at " << Idx
               << ": not live across %stack." << Slot << "\n";
      continue;
    }
    // Only list the slot if the value it holds HERE provably is an OCaml
    // value; family membership alone is per-register and can cover raw
    // ranges of coalesced registers.
    if (!GCV.slotValueAt(Slot, InSlot)) {
      if (VerboseOxCamlStatepointSpillRoots)
        errs() << "[oxsr] " << MF.getName() << " at " << Idx
               << ": skip non-value %stack." << Slot << "\n";
      continue;
    }
    // LiveStacks liveness is path-insensitive: a loop-carried slot whose
    // store sits later in the loop body is "live across" a statepoint at
    // the loop head, yet holds uninitialized garbage on the first
    // iteration. Listing such a slot needs an entry-block initialization
    // with a known value (below); without one available, skip rather than
    // hand the GC garbage.
    bool Dominated = false;
    for (auto &[Src, StIdx] : GCV.storesOf(Slot)) {
      MachineInstr *St = Indexes.getInstructionFromIndex(StIdx);
      if (St && MDT.dominates(St, &MI)) {
        Dominated = true;
        break;
      }
    }
    if (!Dominated) {
      if (!CanInitSlots) {
        if (VerboseOxCamlStatepointSpillRoots)
          errs() << "[oxsr] " << MF.getName() << " at " << Idx
                 << ": skip undominated %stack." << Slot
                 << " (no init register)\n";
        continue;
      }
      SlotsNeedingInit.insert(Slot);
    }
    SlotsToAdd.push_back(Slot);
  }

  // Value-home slots (ISel statepoint pool slots, exnroot allocas) have no
  // LiveStacks intervals; use the dedicated dataflow. Their content is a
  // gc value by construction whenever defined, so no value filter is
  // needed. This catches the cont pattern: values forced into tied
  // registers at an alloc statepoint while their pool-slot copies stay
  // live and are reloaded afterwards.
  for (int Slot : ValueHomeFIs) {
    if (ListedSlots.count(Slot) || LS.hasInterval(Slot))
      continue;
    if (!VHL.liveAfter(Slot, MI)) {
      if (VerboseOxCamlStatepointSpillRoots && getenv("OXSR_DEBUG_VHL"))
        errs() << "[oxsr] " << MF.getName() << " at " << Idx
               << ": dead value-home %stack." << Slot << "\n";
      continue;
    }
    if (!VHL.definedAt(Slot, MI)) {
      if (!CanInitSlots) {
        if (VerboseOxCamlStatepointSpillRoots)
          errs() << "[oxsr] " << MF.getName() << " at " << Idx
                 << ": skip undefined value-home %stack." << Slot
                 << " (no init register)\n";
        continue;
      }
      SlotsNeedingInit.insert(Slot);
    }
    SlotsToAdd.push_back(Slot);
  }

  // Gc registers live across the statepoint in a regmask-preserved physical
  // register but not listed: the GC would relocate the listed locations
  // only and the register copy would go stale. List them as plain register
  // uses; the GC updates registers in place, so later reads of the same
  // virtual register see the relocated value. Registers whose value at the
  // statepoint comes from an IMPLICIT_DEF hold garbage and are skipped.
  // Register entries are resolved against the gc_regs bucket, which only
  // the alloc-family runtime entries populate (SAVE_ALL_REGS); at any
  // other statepoint a register entry reads the last FREED bucket and
  // hands the GC garbage. See isAllocFamilyStatepoint.
  SmallVector<Register, 8> RegsToAdd;
  if (EnableOxCamlStatepointRegisterRoots &&
      isAllocFamilyStatepoint(MI, MF.getSubtarget().getRegisterInfo())) {
    const uint32_t *RegMask = getStatepointRegMask(MI);
    for (Register R : GCRegs) {
      if (ListedRegs.count(R) || !LIS.hasInterval(R) || !VRM.hasPhys(R))
        continue;
      const LiveInterval &LI = LIS.getInterval(R);
      if (!LI.liveAt(InSlot) || !LI.liveAt(OutSlot))
        continue;
      if (RegMask &&
          MachineOperand::clobbersPhysReg(RegMask, VRM.getPhys(R)))
        continue;
      // Same per-value precision as for slots.
      if (!GCV.regValueAt(R, InSlot)) {
        if (VerboseOxCamlStatepointSpillRoots)
          errs() << "[oxsr] " << MF.getName() << " at " << Idx
                 << ": skip non-value " << printReg(R) << "\n";
        continue;
      }
      RegsToAdd.push_back(R);
    }
  }

  if (SlotsToAdd.empty() && RegsToAdd.empty())
    return false;

  // Deterministic operand order.
  llvm::sort(SlotsToAdd);
  llvm::sort(RegsToAdd);

  if (VerboseOxCamlStatepointSpillRoots) {
    errs() << "[oxsr] " << MF.getName() << " statepoint ID "
           << StatepointOpers(&MI).getID() << " at " << Idx << ": appending";
    for (int S : SlotsToAdd)
      errs() << " %stack." << S;
    for (Register R : RegsToAdd)
      errs() << " " << printReg(R);
    errs() << "\n";
  }
  LLVM_DEBUG(dbgs() << "[OxCaml] appending " << SlotsToAdd.size()
                    << " spill slot and " << RegsToAdd.size()
                    << " register root(s) to statepoint ID "
                    << StatepointOpers(&MI).getID() << " in " << MF.getName()
                    << "\n");
  appendRootsToStatepoint(MI, MF, SlotsToAdd, RegsToAdd);
  NumSlotsAppended += SlotsToAdd.size();
  NumRegsAppended += RegsToAdd.size();
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
  LiveIntervals &LIS = getAnalysis<LiveIntervals>();
  SlotIndexes &Indexes = getAnalysis<SlotIndexes>();

  SmallVector<MachineInstr *, 32> Statepoints;
  for (MachineBasicBlock &MBB : MF)
    for (MachineInstr &MI : MBB)
      if (MI.getOpcode() == TargetOpcode::STATEPOINT)
        Statepoints.push_back(&MI);
  if (Statepoints.empty())
    return false;

  const TargetInstrInfo *TII = MF.getSubtarget().getInstrInfo();
  SmallSet<int, 16> ListedSlotsAnywhere, ValueHomeFIs;
  collectValueHomeFIs(Statepoints, LS, ListedSlotsAnywhere, ValueHomeFIs);
  DenseSet<Register> GCRegs;
  computeGCRegs(MF, Statepoints, ValueHomeFIs, TII, GCRegs);
  SmallSet<int, 16> GCSlots = ListedSlotsAnywhere;
  collectGCSlots(GCRegs, VRM, GCSlots);
  if (GCSlots.empty() && GCRegs.empty())
    return false;

  GCValueness GCV(MF, Statepoints, ValueHomeFIs, LIS, LS, Indexes, TII);
  ValueHomeLiveness VHL(MF, Statepoints, ValueHomeFIs, TII, Indexes);
  MachineDominatorTree &MDT = getAnalysis<MachineDominatorTree>();

  // A known-valid OCaml value available at function entry, used to
  // initialize listed slots whose stores do not dominate a listing
  // statepoint: the ABI register of any addrspace(1) formal argument.
  MachineRegisterInfo &MRI = MF.getRegInfo();
  Register InitPhys;
  const TargetRegisterClass *InitRC = nullptr;
  for (MachineInstr &EMI : MF.front()) {
    if (!EMI.isCopy() || !EMI.getOperand(1).getReg().isPhysical())
      continue;
    Register Dst = EMI.getOperand(0).getReg();
    if (Dst.isVirtual() && MRI.isOxCamlGCArg(Dst)) {
      InitPhys = EMI.getOperand(1).getReg();
      InitRC = MRI.getRegClass(Dst);
      break;
    }
  }

  bool Changed = false;
  SmallSet<int, 16> SlotsNeedingInit;
  for (MachineInstr *MI : Statepoints)
    Changed |= processStatepoint(*MI, MF, GCSlots, GCRegs, ValueHomeFIs,
                                 GCV, VHL, LS, LIS, VRM, Indexes, MDT,
                                 InitPhys.isValid() && EnableOxCamlStatepointSlotInit, SlotsNeedingInit);

  // Emit the entry-block initializations: store the gc argument value
  // into each slot so its content is a valid value before the first real
  // store. The GC may relocate it there (harmlessly); the real store
  // overwrites it. Extend the LiveStacks interval to the init so stack
  // slot coloring cannot pack another slot into the now-meaningful gap.
  for (int Slot : SlotsNeedingInit) {
    MachineBasicBlock &Entry = MF.front();
    MachineBasicBlock::iterator OldBegin = Entry.begin();
    TII->storeRegToStackSlot(Entry, OldBegin, InitPhys, /*isKill=*/false,
                             Slot, InitRC, MF.getSubtarget().getRegisterInfo(),
                             Register());
    SlotIndex InitIdx;
    for (MachineBasicBlock::iterator It = Entry.begin(); It != OldBegin; ++It)
      InitIdx = Indexes.insertMachineInstrInMaps(*It);
    if (LS.hasInterval(Slot)) {
      LiveInterval &LI = LS.getInterval(Slot);
      if (InitIdx.isValid() && LI.beginIndex() > InitIdx.getRegSlot()) {
        VNInfo *VNI = LI.getVNInfoAt(LI.beginIndex());
        if (VNI)
          LI.addSegment(LiveRange::Segment(InitIdx.getRegSlot(),
                                           LI.beginIndex(), VNI));
      }
    }
    NumSlotInits++;
    if (VerboseOxCamlStatepointSpillRoots)
      errs() << "[oxsr] " << MF.getName() << ": entry init of %stack."
             << Slot << " from " << printReg(InitPhys) << "\n";
    Changed = true;
  }
  return Changed;
}
