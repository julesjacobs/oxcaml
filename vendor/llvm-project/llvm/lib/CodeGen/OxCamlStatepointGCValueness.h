//===- OxCamlStatepointGCValueness.h - Statepoint value identity -*- C++ -*-=//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Shared between OxCamlStatepointSpillRoots (the authoritative root-listing
// pass) and OxCamlGCRootVerifier (which must judge value-ness with exactly
// the same analysis to separate real unlisted roots from deliberately
// skipped raw-content ranges). Definitions live in
// OxCamlStatepointSpillRoots.cpp.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_CODEGEN_OXCAMLSTATEPOINTGCVALUENESS_H
#define LLVM_LIB_CODEGEN_OXCAMLSTATEPOINTGCVALUENESS_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/LiveStacks.h"
#include "llvm/CodeGen/SlotIndexes.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

namespace llvm {
namespace oxcamlroots {

/// Walk the gc-pointer section of \p MI, recording stack slots that are
/// already listed (folded operands) into \p ListedSlots and
/// virtual-register gc operands into \p GCRegs (if non-null).
void walkGCPtrSection(const MachineInstr &MI, SmallSet<int, 16> &ListedSlots,
                      SmallVectorImpl<Register> *GCRegs);

/// Walk the gc-alloca section of \p MI (exnroot slots and friends),
/// recording the frame indices into \p AllocaFIs.
void walkAllocaSection(const MachineInstr &MI, SmallSet<int, 16> &AllocaFIs);

/// Frame indices whose content is a gc VALUE whenever the program loads
/// from them (ISel statepoint pool slots, gc-alloca exnroot homes).
void collectValueHomeFIs(ArrayRef<MachineInstr *> Statepoints, LiveStacks &LS,
                         SmallSet<int, 16> &ListedSlotsAnywhere,
                         SmallSet<int, 16> &ValueHomeFIs);


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

  // Must-reaching-store lattice for mustReachStoreAt: Any agrees with
  // everything (cycle-optimistic), None means an uninitialized path
  // reaches (kills must), One is a unique store, Bottom is a conflict.
  struct ReachVal {
    enum Kind { Any, None, One, Bottom, InProgress } K = Any;
    const std::pair<Register, SlotIndex> *St = nullptr;
  };
  DenseMap<std::pair<int, int>, ReachVal> ReachMemo;

  static ReachVal meetReach(ReachVal A, ReachVal B) {
    if (A.K == ReachVal::Any)
      return B;
    if (B.K == ReachVal::Any)
      return A;
    if (A.K == ReachVal::Bottom || B.K == ReachVal::Bottom)
      return {ReachVal::Bottom, nullptr};
    if (A.K == ReachVal::None && B.K == ReachVal::None)
      return A;
    if (A.K == ReachVal::One && B.K == ReachVal::One && A.St == B.St)
      return A;
    return {ReachVal::Bottom, nullptr};
  }

  ReachVal exitStore(int FI, const MachineBasicBlock *MBB);

  void collectAccesses();
  bool isAllocCursor(Register Src, SlotIndex At);
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

  const DenseMap<int, SmallVector<std::pair<Register, SlotIndex>, 4>> &
  allStores() const {
    return SlotStores;
  }

  ArrayRef<std::pair<Register, SlotIndex>> loadsOf(int FI) const {
    auto It = SlotLoads.find(FI);
    return It == SlotLoads.end()
               ? ArrayRef<std::pair<Register, SlotIndex>>()
               : ArrayRef<std::pair<Register, SlotIndex>>(It->second);
  }

  /// The unique store to \p FI that reaches \p Idx on EVERY path (or
  /// nullptr if no single store must-reaches: conflicting stores, or an
  /// uninitialized path). Sound across loops: the per-block exit values
  /// are computed with optimistic cycle handling; a conflicting store
  /// anywhere on a cycle reaches the meet through its block's exit value.
  const std::pair<Register, SlotIndex> *mustReachStoreAt(int FI,
                                                         SlotIndex Idx);

  /// True if some load of \p FI can observe the slot's content as it
  /// stands at \p At: a path At -> load with no intervening store
  /// exists. False means every later read is re-fed by a closer store
  /// on every path (e.g. entry-init or argument-spill segments whose
  /// content dies before the first real read). Bounded; bails true.
  bool mayObserveContent(int FI, SlotIndex At);

  /// True if no store to \p FI can execute strictly between \p From and
  /// \p To on ANY path, where From's instruction dominates To's (a
  /// reload feeding a statepoint operand). Checks From's block suffix,
  /// To's block prefix, and every block on any in-between path,
  /// including cycles re-entering To's block. Bounded; bails false.
  bool noStoreBetween(int FI, SlotIndex From, SlotIndex To);
};

} // end namespace oxcamlroots
} // end namespace llvm

#endif // LLVM_LIB_CODEGEN_OXCAMLSTATEPOINTGCVALUENESS_H
