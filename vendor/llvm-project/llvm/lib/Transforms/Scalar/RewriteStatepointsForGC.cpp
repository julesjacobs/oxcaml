//===- RewriteStatepointsForGC.cpp - Make GC relocations explicit ---------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Rewrite call/invoke instructions so as to make potential relocations
// performed by the garbage collector explicit in the IR.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar/RewriteStatepointsForGC.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Analysis/DomTreeUpdater.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicsAArch64.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <limits>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

#define DEBUG_TYPE "rewrite-statepoints-for-gc"

using namespace llvm;

// Print the liveset found at the insert location
static cl::opt<bool> PrintLiveSet("spp-print-liveset", cl::Hidden,
                                  cl::init(false));
static cl::opt<bool> PrintLiveSetSize("spp-print-liveset-size", cl::Hidden,
                                      cl::init(false));

// Print out the base pointers for debugging
static cl::opt<bool> PrintBasePointers("spp-print-base-pointers", cl::Hidden,
                                       cl::init(false));

// Cost threshold measuring when it is profitable to rematerialize value instead
// of relocating it
static cl::opt<unsigned>
RematerializationThreshold("spp-rematerialization-threshold", cl::Hidden,
                           cl::init(6));

#ifdef EXPENSIVE_CHECKS
static bool ClobberNonLive = true;
#else
static bool ClobberNonLive = false;
#endif

static cl::opt<bool, true> ClobberNonLiveOverride("rs4gc-clobber-non-live",
                                                  cl::location(ClobberNonLive),
                                                  cl::Hidden);

static cl::opt<bool>
    AllowStatepointWithNoDeoptInfo("rs4gc-allow-statepoint-with-no-deopt-info",
                                   cl::Hidden, cl::init(true));

static cl::opt<bool> RematAddrSpace1DerivedFromBaseAtAlloc(
    "rs4gc-remat-addrspace1-derived-from-base-at-alloc", cl::Hidden,
    cl::init(true),
    cl::desc("At OxCaml statepoints, rematerialize GEP-derived "
             "addrspace(1) address expressions from relocated bases instead "
             "of relocating them"));

static cl::opt<bool> DebugOxCamlDerivedRemat(
    "rs4gc-debug-oxcaml-derived-remat", cl::Hidden, cl::init(false),
    cl::desc("Print OxCaml statepoint derived/base candidates"));

static cl::opt<bool> DebugOxCamlGCPointerAllocas(
    "rs4gc-debug-oxcaml-gc-pointer-allocas", cl::Hidden, cl::init(false),
    cl::desc("Print OxCaml GC pointer allocas that RS4GC promotes or rejects"));

static cl::opt<bool> OxCamlRootSlotAliasing(
    "rs4gc-oxcaml-root-slot-aliasing", cl::Hidden, cl::init(true),
    cl::desc("Let reloads/selectors of a stable root slot share that slot"));

static cl::opt<bool> OxCamlVerifyRootSlots(
    "rs4gc-oxcaml-verify-root-slots", cl::Hidden, cl::init(false),
    cl::desc("Verify that every root slot reload is dominated by the slot's "
             "defining store"));

static cl::opt<unsigned> OxCamlCallHomeBudget(
    "rs4gc-oxcaml-call-home-budget", cl::Hidden,
    cl::init(std::numeric_limits<unsigned>::max()),
    cl::desc("Debug: only home values at this many call statepoints"));
static cl::opt<unsigned> OxCamlCallHomeSkip(
    "rs4gc-oxcaml-call-home-skip", cl::Hidden, cl::init(0),
    cl::desc("Debug: do not home values at the first N call statepoints"));
static cl::opt<unsigned> OxCamlCallHomeDump(
    "rs4gc-oxcaml-call-home-dump", cl::Hidden,
    cl::init(std::numeric_limits<unsigned>::max()),
    cl::desc("Debug: dump the call statepoint with this home ordinal"));
static unsigned OxCamlCallHomesUsed = 0;

static cl::opt<unsigned> OxCamlValueSlotHomes(
    "rs4gc-oxcaml-value-slot-homes", cl::Hidden, cl::init(3),
    cl::desc("Home live handler-rooted values through their slot instead of "
             "relocating them (0=off, 1=invokes only, 2=calls only, 3=all)"));

static cl::opt<bool> OxCamlPruneRootSlotInitStores(
    "rs4gc-oxcaml-prune-root-slot-init-stores", cl::Hidden, cl::init(true),
    cl::desc("Drop entry-block init stores of OxCaml root slots whose single "
             "defining store dominates every load and registered statepoint "
             "of the slot"));

static cl::opt<bool> OxCamlVolatileExnRootSlots(
    "rs4gc-oxcaml-volatile-exnroot-slots", cl::Hidden, cl::init(false),
    cl::desc("Use volatile accesses for OxCaml exception root slots instead "
             "of relying on the slots escaping into statepoint gc-live "
             "bundles"));

static cl::opt<bool> OxCamlLazyBoundaryLoads(
    "rs4gc-oxcaml-lazy-boundary-loads", cl::Hidden, cl::init(true),
    cl::desc("Reload boundary-only recovery values on their own edge"));

static cl::opt<bool> UseOxCamlVolatileRootAllocas(
    "rs4gc-oxcaml-volatile-root-allocas", cl::Hidden, cl::init(false),
    cl::desc("For OxCaml statepoints, materialize live GC pointers through "
             "volatile root allocas instead of gc.relocate"));

static cl::opt<bool> HeuristicReportOxCamlStatepointCrossingIntToPtr(
    "rs4gc-heuristic-report-oxcaml-statepoint-crossing-inttoptr", cl::Hidden,
    cl::init(false),
    cl::desc("Heuristically print OxCaml integer values that are live across "
             "a statepoint and later contribute to an inttoptr to ptr "
             "addrspace(1). This is a debugging aid, not a correctness "
             "invariant."));

static cl::opt<unsigned> HeuristicReportOxCamlStatepointCrossingIntToPtrLimit(
    "rs4gc-heuristic-report-oxcaml-statepoint-crossing-inttoptr-limit",
    cl::Hidden, cl::init(100),
    cl::desc("Maximum number of OxCaml heuristic statepoint-crossing inttoptr "
             "reports per function. 0 means unlimited."));

static cl::opt<unsigned>
    HeuristicReportOxCamlStatepointCrossingIntToPtrTotalLimit(
        "rs4gc-heuristic-report-oxcaml-statepoint-crossing-inttoptr-total-limit",
        cl::Hidden, cl::init(100),
        cl::desc("Maximum number of OxCaml heuristic statepoint-crossing "
                 "inttoptr reports per compiler process. 0 means unlimited."));

static unsigned OxCamlHeuristicStatepointCrossingIntToPtrReports = 0;
static bool OxCamlHeuristicStatepointCrossingIntToPtrLimitReported = false;

static cl::opt<bool> FailOnOxCamlDerivedRelocates(
    "rs4gc-fail-on-oxcaml-derived-relocates", cl::Hidden, cl::init(true),
    cl::desc("Fail if an OxCaml statepoint would relocate a derived "
             "addrspace(1) pointer as an independent post-statepoint value"));

static cl::opt<bool> FailOnUnhandledGCPointerAggregate(
    "rs4gc-fail-on-unhandled-gc-aggregate", cl::Hidden, cl::init(false),
    cl::desc("Fail if RS4GC liveness sees a first-class aggregate containing "
             "GC pointers"));

static cl::opt<bool> ExplodeGCPointerAggregates(
    "rs4gc-explode-gc-aggregates", cl::Hidden, cl::init(true),
    cl::desc("Expose GC pointers hidden inside first-class aggregate SSA "
             "values before RS4GC liveness"));

static cl::opt<unsigned> RematAddrSpace1DerivedFromBaseAtAllocSkip(
    "rs4gc-remat-addrspace1-derived-from-base-at-alloc-skip", cl::Hidden,
    cl::init(0), cl::desc("Skip this many accepted OxCaml statepoint derived "
                          "rematerialization candidates"));

static cl::opt<unsigned> RematAddrSpace1DerivedFromBaseAtAllocLimit(
    "rs4gc-remat-addrspace1-derived-from-base-at-alloc-limit", cl::Hidden,
    cl::init(std::numeric_limits<unsigned>::max()),
    cl::desc("Allow at most this many accepted OxCaml statepoint derived "
             "rematerialization candidates after the skip count"));

static unsigned OxCamlDerivedRematAcceptedOrdinal = 0;

static bool isOxCamlCallingConv(CallingConv::ID CC) {
  switch (CC) {
  case CallingConv::OxCaml_WithFP:
  case CallingConv::OxCaml_WithoutFP:
  case CallingConv::OxCaml_C_Call:
  case CallingConv::OxCaml_C_Call_StackArgs:
  case CallingConv::OxCaml_Alloc:
  case CallingConv::OxCaml_C_Direct_Call:
    return true;
  default:
    return false;
  }
}

static bool isOxCamlCWrapperCallingConv(CallingConv::ID CC) {
  return CC == CallingConv::OxCaml_C_Call ||
         CC == CallingConv::OxCaml_C_Call_StackArgs;
}

static cl::opt<bool> RematDerivedAtUses("rs4gc-remat-derived-at-uses",
                                        cl::Hidden, cl::init(true));

static cl::opt<bool> CanonicalizeOxCamlRawHeapMemoryAddresses(
    "rs4gc-canonicalize-oxcaml-raw-heap-addresses", cl::Hidden,
    cl::init(false),
    cl::desc("Rewrite OxCaml raw memory operands that are recoverable as "
             "addrspace(1) heap base-plus-offset addresses"));

/// The IR fed into RewriteStatepointsForGC may have had attributes and
/// metadata implying dereferenceability that are no longer valid/correct after
/// RewriteStatepointsForGC has run. This is because semantically, after
/// RewriteStatepointsForGC runs, all calls to gc.statepoint "free" the entire
/// heap. stripNonValidData (conservatively) restores
/// correctness by erasing all attributes in the module that externally imply
/// dereferenceability. Similar reasoning also applies to the noalias
/// attributes and metadata. gc.statepoint can touch the entire heap including
/// noalias objects.
/// Apart from attributes and metadata, we also remove instructions that imply
/// constant physical memory: llvm.invariant.start.
static void stripNonValidData(Module &M);

static bool shouldRewriteStatepointsIn(Function &F);

PreservedAnalyses RewriteStatepointsForGC::run(Module &M,
                                               ModuleAnalysisManager &AM) {
  bool Changed = false;
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  for (Function &F : M) {
    // Nothing to do for declarations.
    if (F.isDeclaration() || F.empty())
      continue;

    // Policy choice says not to rewrite - the most common reason is that we're
    // compiling code without a GCStrategy.
    if (!shouldRewriteStatepointsIn(F))
      continue;

    auto &DT = FAM.getResult<DominatorTreeAnalysis>(F);
    auto &TTI = FAM.getResult<TargetIRAnalysis>(F);
    auto &TLI = FAM.getResult<TargetLibraryAnalysis>(F);
    Changed |= runOnFunction(F, DT, TTI, TLI);
  }
  if (!Changed)
    return PreservedAnalyses::all();

  // stripNonValidData asserts that shouldRewriteStatepointsIn
  // returns true for at least one function in the module.  Since at least
  // one function changed, we know that the precondition is satisfied.
  stripNonValidData(M);

  PreservedAnalyses PA;
  PA.preserve<TargetIRAnalysis>();
  PA.preserve<TargetLibraryAnalysis>();
  return PA;
}

namespace {

class RewriteStatepointsForGCLegacyPass : public ModulePass {
  RewriteStatepointsForGC Impl;

public:
  static char ID; // Pass identification, replacement for typeid

  RewriteStatepointsForGCLegacyPass() : ModulePass(ID), Impl() {
    initializeRewriteStatepointsForGCLegacyPassPass(
        *PassRegistry::getPassRegistry());
  }

  bool runOnModule(Module &M) override {
    bool Changed = false;
    for (Function &F : M) {
      // Nothing to do for declarations.
      if (F.isDeclaration() || F.empty())
        continue;

      // Policy choice says not to rewrite - the most common reason is that
      // we're compiling code without a GCStrategy.
      if (!shouldRewriteStatepointsIn(F))
        continue;

      TargetTransformInfo &TTI =
          getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
      const TargetLibraryInfo &TLI =
          getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);
      auto &DT = getAnalysis<DominatorTreeWrapperPass>(F).getDomTree();

      Changed |= Impl.runOnFunction(F, DT, TTI, TLI);
    }

    if (!Changed)
      return false;

    // stripNonValidData asserts that shouldRewriteStatepointsIn
    // returns true for at least one function in the module.  Since at least
    // one function changed, we know that the precondition is satisfied.
    stripNonValidData(M);
    return true;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    // We add and rewrite a bunch of instructions, but don't really do much
    // else.  We could in theory preserve a lot more analyses here.
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<TargetTransformInfoWrapperPass>();
    AU.addRequired<TargetLibraryInfoWrapperPass>();
  }
};

} // end anonymous namespace

char RewriteStatepointsForGCLegacyPass::ID = 0;

ModulePass *llvm::createRewriteStatepointsForGCLegacyPass() {
  return new RewriteStatepointsForGCLegacyPass();
}

INITIALIZE_PASS_BEGIN(RewriteStatepointsForGCLegacyPass,
                      "rewrite-statepoints-for-gc",
                      "Make relocations explicit at statepoints", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_END(RewriteStatepointsForGCLegacyPass,
                    "rewrite-statepoints-for-gc",
                    "Make relocations explicit at statepoints", false, false)

namespace {

struct GCPtrLivenessData {
  /// Values defined in this block.
  MapVector<BasicBlock *, SetVector<Value *>> KillSet;

  /// Values used in this block (and thus live); does not included values
  /// killed within this block.
  MapVector<BasicBlock *, SetVector<Value *>> LiveSet;

  /// Values live into this basic block (i.e. used by any
  /// instruction in this basic block or ones reachable from here)
  MapVector<BasicBlock *, SetVector<Value *>> LiveIn;

  /// Values live out of this basic block (i.e. live into
  /// any successor block)
  MapVector<BasicBlock *, SetVector<Value *>> LiveOut;
};

// The type of the internal cache used inside the findBasePointers family
// of functions.  From the callers perspective, this is an opaque type and
// should not be inspected.
//
// In the actual implementation this caches two relations:
// - The base relation itself (i.e. this pointer is based on that one)
// - The base defining value relation (i.e. before base_phi insertion)
// Generally, after the execution of a full findBasePointer call, only the
// base relation will remain.  Internally, we add a mixture of the two
// types, then update all the second type to the first type
using DefiningValueMapTy = MapVector<Value *, Value *>;
using IsKnownBaseMapTy = MapVector<Value *, bool>;
using PointerToBaseTy = MapVector<Value *, Value *>;
using StatepointLiveSetTy = SetVector<Value *>;
using RematerializedValueMapTy =
    MapVector<AssertingVH<Instruction>, AssertingVH<Value>>;

struct PartiallyConstructedSafepointRecord {
  /// The set of values known to be live across this safepoint
  StatepointLiveSetTy LiveSet;

  /// GC pointer call arguments passed to C-wrapper calls. These arguments must
  /// be visible to the collector while C runs, but do not need
  /// post-statepoint gc.relocates in the caller unless they are also live after
  /// the call.
  StatepointLiveSetTy CallArgRoots;

  /// The *new* gc.statepoint instruction itself.  This produces the token
  /// that normal path gc.relocates and the gc.result are tied to.
  GCStatepointInst *StatepointToken;

  /// Instruction to which exceptional gc relocates are attached
  /// Makes it easier to iterate through them during relocationViaAlloca.
  Instruction *UnwindToken;

  /// Record live values we are rematerialized instead of relocating.
  /// They are not included into 'LiveSet' field.
  /// Maps rematerialized copy to it's original value.
  RematerializedValueMapTy RematerializedValues;

  /// Late materialized explicit stack roots that should be listed in the
  /// statepoint's gc-live bundle but should not get gc.relocate calls.
  SmallVector<Value *, 4> ExplicitRootSlots;

  /// This invoke unwinds to an OxCaml trap recovery landingpad whose
  /// handler-live GC values have been materialized into ExplicitRootSlots.
  /// Do not insert exceptional gc.relocate calls on that unwind path.
  bool UsesExplicitExceptionRoots = false;

  /// Values whose live-through state at this statepoint is represented by an
  /// explicit root slot.  If the value is still needed on the normal
  /// continuation, the slot is loaded after the statepoint and fed through the
  /// normal relocationViaAlloca SSA repair path.  Statepoint rewriting RAUWs
  /// earlier call results to fresh gc.results, so the tracked handle must
  /// follow that replacement or the new value would never be repaired.
  SmallVector<std::pair<WeakTrackingVH, AllocaInst *>, 4> ExplicitRootHomes;
};

struct RematerializationAlternative {
  SmallVector<Instruction *, 3> ChainToBase;
  Value *RootOfChain = nullptr;
  unsigned Tag = 0;
};

struct RematerizlizationCandidateRecord {
  // Chain from derived pointer to base.
  SmallVector<Instruction *, 3> ChainToBase;
  // Original base.
  Value *RootOfChain;
  // If set, this value is an i1 computed before a specific statepoint. When
  // it has RematerializedValueConditionSelectsRematValue's truth value, the
  // rematerialized chain is used after relocation; otherwise the relocated
  // base is already the right value. This handles PHIs/selects whose incoming
  // values are either a derived chain from the base or the base itself.
  Value *UseRematerializedValue = nullptr;
  bool RematerializedValueConditionSelectsRematValue = true;
  // If set, rematerialize one of several possible chains from the relocated
  // base selected by this non-pointer case tag.
  Value *RematerializationTag = nullptr;
  SmallVector<RematerializationAlternative, 4> Alternatives;
  // Cost of chain.
  InstructionCost Cost;
};
using RematCandTy = MapVector<Value *, RematerizlizationCandidateRecord>;

using ExplicitRootSlotMapTy = DenseMap<CallBase *, SmallVector<Value *, 4>>;
using ExplicitExceptionRootCallSetTy = DenseSet<CallBase *>;
using ExplicitRootHomeMapTy =
    DenseMap<CallBase *, SmallVector<std::pair<Value *, AllocaInst *>, 4>>;

/// One explicit root slot per handler-live SSA value.  The slot is stored
/// exactly once, at the value's definition, and reloaded after each
/// safepoint the value crosses and at each trap recovery entry that needs
/// it.  A ValueMap keeps entries coherent across the RAUWs and deletions of
/// statepoint rewriting, so the mapping stays usable in the late pass.
using OxCamlValueRootSlotMapTy = ValueMap<Value *, AllocaInst *>;

} // end anonymous namespace

static bool isNormalOnlyAfterInvoke(InvokeInst *Invoke, Value *V,
                                    DominatorTree &DT) {
  BasicBlock *NormalDest = Invoke->getNormalDest();
  BasicBlock *UnwindDest = Invoke->getUnwindDest();

  auto PrintReject = [&](StringRef Reason, User *U = nullptr,
                         BasicBlock *Incoming = nullptr) {
    if (!DebugOxCamlDerivedRemat)
      return;
    errs() << "rs4gc-oxcaml-normal-only-reject " << Reason << "\n";
    errs() << "  invoke normal dest: " << NormalDest->getName() << "\n";
    errs() << "  value: ";
    V->print(errs());
    errs() << "\n";
    if (U) {
      errs() << "  user: ";
      U->print(errs());
      errs() << "\n";
      if (auto *UserI = dyn_cast<Instruction>(U))
        errs() << "  user block: " << UserI->getParent()->getName() << "\n";
    }
    if (Incoming)
      errs() << "  incoming block: " << Incoming->getName() << "\n";
  };

  for (User *U : V->users()) {
    auto *UserI = dyn_cast<Instruction>(U);
    if (!UserI) {
      PrintReject("non-instruction-user", U);
      return false;
    }

    if (UserI == Invoke)
      continue;

    if (UserI->getParent() == Invoke->getParent()) {
      assert(UserI->comesBefore(Invoke) &&
             "nothing can be placed after an invoke in the same block");
      continue;
    }

    // PHIs consume values on incoming edges. A value rematerialized in the
    // normal destination can feed later PHI edges reached from that normal
    // destination, but it cannot feed the direct edge from the invoke block to
    // the normal destination, nor an edge that is only reached from the unwind
    // path. Edges through loop headers that dominate the invoke are pre-invoke
    // uses for this dynamic statepoint and do not constrain rematerialization.
    if (auto *Phi = dyn_cast<PHINode>(UserI)) {
      for (unsigned I = 0, E = Phi->getNumIncomingValues(); I != E; ++I) {
        if (Phi->getIncomingValue(I) != V)
          continue;

        BasicBlock *Incoming = Phi->getIncomingBlock(I);
        if (Phi->getParent() == NormalDest &&
            Incoming == Invoke->getParent()) {
          PrintReject("phi-direct-normal-edge", U, Incoming);
          return false;
        }

        if (DT.dominates(NormalDest, Incoming))
          continue;

        if (DT.dominates(Phi->getParent(), Invoke->getParent()))
          continue;

        bool IncomingReachableFromNormal =
            isPotentiallyReachable(NormalDest, Incoming, nullptr, &DT);
        bool IncomingReachableFromUnwind =
            isPotentiallyReachable(UnwindDest, Incoming, nullptr, &DT);
        if (!IncomingReachableFromNormal && !IncomingReachableFromUnwind)
          continue;
        if (IncomingReachableFromNormal)
          continue;

        PrintReject("phi-incoming-not-normal-or-preinvoke", U, Incoming);
        return false;
      }
      continue;
    }

    // A value can have users in blocks that are not reachable after this
    // invoke. Those users belong to earlier or disjoint dynamic control-flow
    // paths and do not constrain where this statepoint can rematerialize the
    // value.
    bool ReachableFromNormal =
        isPotentiallyReachable(NormalDest, UserI->getParent(), nullptr, &DT);
    bool ReachableFromUnwind =
        isPotentiallyReachable(UnwindDest, UserI->getParent(), nullptr, &DT);
    if (!ReachableFromNormal && !ReachableFromUnwind)
      continue;

    // Relocation via allocas can merge a rematerialized normal-edge value with
    // the original pre-invoke value, so the normal destination does not need to
    // dominate every later use. It is enough that every later instruction use
    // is reachable from the normal edge.
    if (!ReachableFromNormal) {
      PrintReject("user-not-reachable-from-normal", U);
      return false;
    }
  }

  return true;
}

enum class OxCamlBaseEquivalenceState {
  Visiting,
  Equivalent,
  NotEquivalent,
};

static bool isOxCamlBaseEquivalentGCPointer(
    Value *V, Value *Base,
    DenseMap<std::pair<Value *, Value *>, OxCamlBaseEquivalenceState> &Cache) {
  if (V == Base)
    return true;

  auto *VTy = dyn_cast<PointerType>(V->getType());
  auto *BaseTy = dyn_cast<PointerType>(Base->getType());
  if (!VTy || !BaseTy || VTy->getAddressSpace() != 1 ||
      BaseTy->getAddressSpace() != 1)
    return false;

  auto Key = std::make_pair(V, Base);
  auto Cached = Cache.find(Key);
  if (Cached != Cache.end()) {
    switch (Cached->second) {
    case OxCamlBaseEquivalenceState::Visiting:
      return true;
    case OxCamlBaseEquivalenceState::Equivalent:
      return true;
    case OxCamlBaseEquivalenceState::NotEquivalent:
      return false;
    }
  }
  Cache[Key] = OxCamlBaseEquivalenceState::Visiting;

  bool Equivalent = false;
  if (auto *Freeze = dyn_cast<FreezeInst>(V)) {
    Equivalent =
        isOxCamlBaseEquivalentGCPointer(Freeze->getOperand(0), Base, Cache);
  } else if (auto *VPhi = dyn_cast<PHINode>(V)) {
    if (auto *BasePhi = dyn_cast<PHINode>(Base)) {
      Equivalent = VPhi->getNumIncomingValues() ==
                   BasePhi->getNumIncomingValues();
      for (unsigned I = 0; Equivalent && I < VPhi->getNumIncomingValues();
           ++I) {
        int BaseIdx = BasePhi->getBasicBlockIndex(VPhi->getIncomingBlock(I));
        Equivalent =
            BaseIdx >= 0 &&
            isOxCamlBaseEquivalentGCPointer(
                VPhi->getIncomingValue(I),
                BasePhi->getIncomingValue(static_cast<unsigned>(BaseIdx)),
                Cache);
      }
    }
    if (!Equivalent) {
      Equivalent = true;
      for (unsigned I = 0; Equivalent && I < VPhi->getNumIncomingValues();
           ++I)
        Equivalent = isOxCamlBaseEquivalentGCPointer(
            VPhi->getIncomingValue(I), Base, Cache);
    }
  } else if (auto *VSelect = dyn_cast<SelectInst>(V)) {
    if (auto *BaseSelect = dyn_cast<SelectInst>(Base)) {
      Equivalent =
          VSelect->getCondition() == BaseSelect->getCondition() &&
          isOxCamlBaseEquivalentGCPointer(VSelect->getTrueValue(),
                                          BaseSelect->getTrueValue(),
                                          Cache) &&
          isOxCamlBaseEquivalentGCPointer(VSelect->getFalseValue(),
                                          BaseSelect->getFalseValue(),
                                          Cache);
    } else {
      Equivalent =
          isOxCamlBaseEquivalentGCPointer(VSelect->getTrueValue(), Base,
                                          Cache) &&
          isOxCamlBaseEquivalentGCPointer(VSelect->getFalseValue(), Base,
                                          Cache);
    }
  }

  Cache[Key] = Equivalent ? OxCamlBaseEquivalenceState::Equivalent
                          : OxCamlBaseEquivalenceState::NotEquivalent;
  return Equivalent;
}

static bool isOxCamlBaseEquivalentGCPointer(Value *V, Value *Base) {
  DenseMap<std::pair<Value *, Value *>, OxCamlBaseEquivalenceState> Cache;
  return isOxCamlBaseEquivalentGCPointer(V, Base, Cache);
}

static bool isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(Value *V,
                                                              Value *Base) {
  if (V == Base)
    return false;
  auto *VTy = dyn_cast<PointerType>(V->getType());
  auto *BaseTy = dyn_cast<PointerType>(Base->getType());
  return VTy && BaseTy && VTy->getAddressSpace() == 1 &&
         BaseTy->getAddressSpace() == 1 &&
         isOxCamlBaseEquivalentGCPointer(V, Base);
}

static ArrayRef<Use> GetDeoptBundleOperands(const CallBase *Call) {
  std::optional<OperandBundleUse> DeoptBundle =
      Call->getOperandBundle(LLVMContext::OB_deopt);

  if (!DeoptBundle) {
    assert(AllowStatepointWithNoDeoptInfo &&
           "Found non-leaf call without deopt info!");
    return std::nullopt;
  }

  return DeoptBundle->Inputs;
}

/// Compute the live-in set for every basic block in the function
static void computeLiveInValues(DominatorTree &DT, Function &F,
                                GCPtrLivenessData &Data);

/// Given results from the dataflow liveness computation, find the set of live
/// Values at a particular instruction.
static void findLiveSetAtInst(Instruction *inst, GCPtrLivenessData &Data,
                              StatepointLiveSetTy &out);

// TODO: Once we can get to the GCStrategy, this becomes
// std::optional<bool> isGCManagedPointer(const Type *Ty) const override {

static bool isGCPointerType(Type *T) {
  if (auto *PT = dyn_cast<PointerType>(T))
    // For the sake of this example GC, we arbitrarily pick addrspace(1) as our
    // GC managed heap.  We know that a pointer into this heap needs to be
    // updated and that no other pointer does.
    return PT->getAddressSpace() == 1;
  return false;
}

// Return true if this type is one which a) is a gc pointer or contains a GC
// pointer and b) is of a type this code expects to encounter as a live value.
// (The insertion code will assert that a type which matches (a) and not (b)
// is not encountered.)
static bool isHandledGCPointerType(Type *T) {
  // We fully support gc pointers
  if (isGCPointerType(T))
    return true;
  // We partially support vectors of gc pointers. The code will assert if it
  // can't handle something.
  if (auto VT = dyn_cast<VectorType>(T))
    if (isGCPointerType(VT->getElementType()))
      return true;
  return false;
}

static bool isOxCamlFunction(const Function &F);

static Constant *getOxCamlNonMovingImmediate(Type *Ty) {
  assert(isHandledGCPointerType(Ty) &&
         "expected a GC pointer or vector of GC pointers");

  auto *One = ConstantInt::get(Type::getInt64Ty(Ty->getContext()), 1);
  if (isGCPointerType(Ty))
    return ConstantExpr::getIntToPtr(One, Ty);

  auto *VT = cast<VectorType>(Ty);
  Type *EltTy = VT->getElementType();
  assert(isGCPointerType(EltTy) && "expected a vector of GC pointers");
  Constant *Elt = ConstantExpr::getIntToPtr(One, EltTy);
  return ConstantVector::getSplat(VT->getElementCount(), Elt);
}

static Value *nonPoisonOxCamlGCPointerValue(Value *V) {
  if (!isa<UndefValue>(V) && !isa<PoisonValue>(V))
    return V;
  if (!isHandledGCPointerType(V->getType()))
    return V;
  return getOxCamlNonMovingImmediate(V->getType());
}

static bool sanitizeOxCamlUndefinedGCPointers(Function &F) {
  if (!isOxCamlFunction(F))
    return false;

  bool Changed = false;
  for (Instruction &I : instructions(F)) {
    for (Use &U : I.operands()) {
      Value *V = U.get();
      Value *Replacement = nonPoisonOxCamlGCPointerValue(V);
      if (Replacement == V)
        continue;

      U.set(Replacement);
      Changed = true;
    }
  }
  return Changed;
}

/// Returns true if this type contains a gc pointer whether we know how to
/// handle that type or not.
static bool containsGCPtrType(Type *Ty) {
  if (isGCPointerType(Ty))
    return true;
  if (VectorType *VT = dyn_cast<VectorType>(Ty))
    return isGCPointerType(VT->getScalarType());
  if (ArrayType *AT = dyn_cast<ArrayType>(Ty))
    return containsGCPtrType(AT->getElementType());

  if (StructType *ST = dyn_cast<StructType>(Ty))
    return llvm::any_of(ST->elements(), containsGCPtrType);
  
  return false;
}

// Returns true if this is a type which a) is a gc pointer or contains a GC
// pointer and b) is of a type which the code doesn't expect (i.e. first class
// aggregates).  Used to trip assertions.
static bool isUnhandledGCPointerType(Type *Ty) {
  return containsGCPtrType(Ty) && !isHandledGCPointerType(Ty);
}

static void checkNoUnhandledGCPointerType(Value *V, StringRef Context) {
  if (!isUnhandledGCPointerType(V->getType()))
    return;

  if (!FailOnUnhandledGCPointerAggregate)
    return;

  assert(false && "support for FCA unimplemented");
  errs() << "Unhandled GC pointer-containing aggregate in "
         << Context << ":\n  ";
  V->print(errs());
  errs() << "\n";
  report_fatal_error("support for first-class aggregate GC values is "
                     "unimplemented");
}

static bool isGCPointerAggregateType(Type *Ty) {
  return Ty->isAggregateType() && containsGCPtrType(Ty);
}

static bool isMustTailCallValue(Value *V) {
  auto *CB = dyn_cast<CallBase>(V);
  return CB && CB->isMustTailCall();
}

static std::string suffixed_name_or(Value *V, StringRef Suffix,
                                    StringRef DefaultName);

namespace {

struct AggregateLeafInfo {
  SmallVector<unsigned, 4> Indices;
  Type *LeafTy;
};

class GCPointerAggregateExploder {
  Function &F;
  DominatorTree &DT;
  DenseMap<Value *, SmallVector<Value *, 4>> LeafValues;
  DenseSet<PHINode *> PHIsWithPlaceholders;
  DenseSet<PHINode *> FilledPHIs;
  SmallVector<Instruction *, 32> MaybeDead;

  static void collectLeaves(Type *Ty, SmallVectorImpl<unsigned> &Prefix,
                            SmallVectorImpl<AggregateLeafInfo> &Leaves) {
    if (auto *ST = dyn_cast<StructType>(Ty)) {
      for (unsigned I = 0, E = ST->getNumElements(); I != E; ++I) {
        Prefix.push_back(I);
        collectLeaves(ST->getElementType(I), Prefix, Leaves);
        Prefix.pop_back();
      }
      return;
    }

    if (auto *AT = dyn_cast<ArrayType>(Ty)) {
      for (unsigned I = 0, E = AT->getNumElements(); I != E; ++I) {
        Prefix.push_back(I);
        collectLeaves(AT->getElementType(), Prefix, Leaves);
        Prefix.pop_back();
      }
      return;
    }

    Leaves.push_back({SmallVector<unsigned, 4>(Prefix.begin(), Prefix.end()),
                      Ty});
  }

  static SmallVector<AggregateLeafInfo, 4> collectLeaves(Type *Ty) {
    SmallVector<AggregateLeafInfo, 4> Leaves;
    SmallVector<unsigned, 4> Prefix;
    collectLeaves(Ty, Prefix, Leaves);
    return Leaves;
  }

  static bool startsWith(ArrayRef<unsigned> Indices,
                         ArrayRef<unsigned> Prefix) {
    return Indices.size() >= Prefix.size() &&
           std::equal(Prefix.begin(), Prefix.end(), Indices.begin());
  }

  static std::optional<unsigned>
  findLeafIndex(ArrayRef<AggregateLeafInfo> Leaves,
                ArrayRef<unsigned> Indices) {
    for (unsigned I = 0, E = Leaves.size(); I != E; ++I)
      if (ArrayRef<unsigned>(Leaves[I].Indices) == Indices)
        return I;
    return std::nullopt;
  }

  Instruction *firstInsertionIn(BasicBlock *BB) {
    return &*BB->getFirstInsertionPt();
  }

  BasicBlock *splitInvokeNormalEdgeForExtracts(InvokeInst *II) {
    BasicBlock *NormalDest = II->getNormalDest();
    if (NormalDest->getUniquePredecessor())
      return NormalDest;

    BasicBlock *InvokeBB = II->getParent();
    BasicBlock *SplitBB =
        BasicBlock::Create(F.getContext(), NormalDest->getName() + ".gcagg",
                           &F, NormalDest);
    BranchInst *BI = BranchInst::Create(NormalDest, SplitBB);
    BI->setDebugLoc(NormalDest->getFirstNonPHIOrDbg()->getDebugLoc());
    II->setNormalDest(SplitBB);

    for (PHINode &PN : NormalDest->phis())
      for (unsigned I = 0, E = PN.getNumIncomingValues(); I != E; ++I)
        if (PN.getIncomingBlock(I) == InvokeBB)
          PN.setIncomingBlock(I, SplitBB);

    DT.recalculate(F);
    return SplitBB;
  }

  Instruction *insertionAfterDef(Instruction *I) {
    if (auto *II = dyn_cast<InvokeInst>(I)) {
      return firstInsertionIn(splitInvokeNormalEdgeForExtracts(II));
    }

    assert(!I->isTerminator() && "non-invoke aggregate terminator?");
    return I->getNextNode();
  }

  SmallVector<Value *, 4> createExtractLeaves(Value *V, Instruction *IP) {
    SmallVector<Value *, 4> Result;
    IRBuilder<> Builder(IP);
    for (const AggregateLeafInfo &Leaf : collectLeaves(V->getType())) {
      Value *Extract =
          Builder.CreateExtractValue(V, Leaf.Indices,
                                     suffixed_name_or(V, ".gcagg", "gcagg"));
      Result.push_back(Extract);
    }
    return Result;
  }

  static void constantLeaves(Constant *C, SmallVectorImpl<Value *> &Result) {
    Type *Ty = C->getType();
    if (auto *ST = dyn_cast<StructType>(Ty)) {
      for (unsigned I = 0, E = ST->getNumElements(); I != E; ++I) {
        Constant *Element = C->getAggregateElement(I);
        constantLeaves(Element ? Element : UndefValue::get(ST->getElementType(I)),
                       Result);
      }
      return;
    }

    if (auto *AT = dyn_cast<ArrayType>(Ty)) {
      for (unsigned I = 0, E = AT->getNumElements(); I != E; ++I) {
        Constant *Element = C->getAggregateElement(I);
        constantLeaves(Element ? Element : UndefValue::get(AT->getElementType()),
                       Result);
      }
      return;
    }

    Result.push_back(C);
  }

  SmallVector<Value *, 4> constantLeaves(Constant *C) {
    SmallVector<Value *, 4> Result;
    constantLeaves(C, Result);
    return Result;
  }

  SmallVector<Value *, 4> createPHIPlaceholders(PHINode *PN) {
    SmallVector<Value *, 4> Result;
    for (const AggregateLeafInfo &Leaf : collectLeaves(PN->getType())) {
      PHINode *LeafPN = PHINode::Create(
          Leaf.LeafTy, PN->getNumIncomingValues(),
          suffixed_name_or(PN, ".gcagg", "gcagg"), PN);
      Result.push_back(LeafPN);
    }
    PHIsWithPlaceholders.insert(PN);
    MaybeDead.push_back(PN);
    return Result;
  }

  SmallVector<Value *, 4> createSelectLeaves(SelectInst *SI) {
    SmallVector<Value *, 4> TrueLeaves = getLeaves(SI->getTrueValue());
    SmallVector<Value *, 4> FalseLeaves = getLeaves(SI->getFalseValue());
    SmallVector<Value *, 4> Result;
    IRBuilder<> Builder(SI);
    auto Leaves = collectLeaves(SI->getType());
    assert(TrueLeaves.size() == Leaves.size());
    assert(FalseLeaves.size() == Leaves.size());
    for (unsigned I = 0, E = Leaves.size(); I != E; ++I) {
      Value *Leaf = Builder.CreateSelect(
          SI->getCondition(), TrueLeaves[I], FalseLeaves[I],
          suffixed_name_or(SI, ".gcagg", "gcagg"));
      Result.push_back(Leaf);
    }
    MaybeDead.push_back(SI);
    return Result;
  }

  SmallVector<Value *, 4> createInsertValueLeaves(InsertValueInst *IVI) {
    SmallVector<Value *, 4> Result = getLeaves(IVI->getAggregateOperand());
    Value *Inserted = IVI->getInsertedValueOperand();
    ArrayRef<unsigned> InsertedPath = IVI->getIndices();
    auto Leaves = collectLeaves(IVI->getType());

    if (isGCPointerAggregateType(Inserted->getType())) {
      SmallVector<Value *, 4> InsertedLeaves = getLeaves(Inserted);
      auto InsertedLeafInfos = collectLeaves(Inserted->getType());
      assert(InsertedLeaves.size() == InsertedLeafInfos.size());
      for (unsigned I = 0, E = InsertedLeafInfos.size(); I != E; ++I) {
        SmallVector<unsigned, 4> FullPath(InsertedPath.begin(),
                                          InsertedPath.end());
        llvm::append_range(FullPath, InsertedLeafInfos[I].Indices);
        std::optional<unsigned> LeafIndex = findLeafIndex(Leaves, FullPath);
        assert(LeafIndex && "inserted aggregate leaf not found");
        Result[*LeafIndex] = InsertedLeaves[I];
      }
      return Result;
    }

    std::optional<unsigned> LeafIndex = findLeafIndex(Leaves, InsertedPath);
    assert(LeafIndex && "inserted scalar leaf not found");
    Result[*LeafIndex] = Inserted;
    return Result;
  }

  SmallVector<Value *, 4> createExtractValueAggregateLeaves(ExtractValueInst *EVI) {
    SmallVector<Value *, 4> AggregateLeaves = getLeaves(EVI->getAggregateOperand());
    auto SourceLeaves = collectLeaves(EVI->getAggregateOperand()->getType());
    SmallVector<Value *, 4> Result;
    for (unsigned I = 0, E = SourceLeaves.size(); I != E; ++I)
      if (startsWith(SourceLeaves[I].Indices, EVI->getIndices()))
        Result.push_back(AggregateLeaves[I]);
    return Result;
  }

public:
  GCPointerAggregateExploder(Function &F, DominatorTree &DT) : F(F), DT(DT) {}

  SmallVector<Value *, 4> getLeaves(Value *V) {
    assert(isGCPointerAggregateType(V->getType()));

    if (isMustTailCallValue(V))
      return {};

    auto It = LeafValues.find(V);
    if (It != LeafValues.end())
      return It->second;

    SmallVector<Value *, 4> Result;
    if (auto *C = dyn_cast<Constant>(V)) {
      Result = constantLeaves(C);
    } else if (auto *PN = dyn_cast<PHINode>(V)) {
      Result = createPHIPlaceholders(PN);
    } else if (auto *SI = dyn_cast<SelectInst>(V)) {
      Result = createSelectLeaves(SI);
    } else if (auto *IVI = dyn_cast<InsertValueInst>(V)) {
      Result = createInsertValueLeaves(IVI);
      MaybeDead.push_back(IVI);
    } else if (auto *EVI = dyn_cast<ExtractValueInst>(V)) {
      Result = createExtractValueAggregateLeaves(EVI);
      MaybeDead.push_back(EVI);
    } else if (auto *Arg = dyn_cast<Argument>(V)) {
      Result = createExtractLeaves(Arg, firstInsertionIn(&F.getEntryBlock()));
    } else if (auto *I = dyn_cast<Instruction>(V)) {
      Result = createExtractLeaves(I, insertionAfterDef(I));
    } else {
      return {};
    }

    assert(Result.size() == collectLeaves(V->getType()).size());
    LeafValues[V] = Result;
    return Result;
  }

  bool fillPHIs() {
    bool Changed = false;
    for (PHINode *PN : PHIsWithPlaceholders) {
      if (!FilledPHIs.insert(PN).second)
        continue;

      SmallVector<Value *, 4> PNLeaves = getLeaves(PN);
      auto Leaves = collectLeaves(PN->getType());
      assert(PNLeaves.size() == Leaves.size());

      for (unsigned LeafNo = 0, E = Leaves.size(); LeafNo != E; ++LeafNo) {
        auto *LeafPN = cast<PHINode>(PNLeaves[LeafNo]);
        for (unsigned I = 0, NE = PN->getNumIncomingValues(); I != NE; ++I) {
          Value *Incoming = PN->getIncomingValue(I);
          SmallVector<Value *, 4> IncomingLeaves = getLeaves(Incoming);
          assert(IncomingLeaves.size() == Leaves.size());
          LeafPN->addIncoming(IncomingLeaves[LeafNo],
                              PN->getIncomingBlock(I));
        }
      }
      Changed = true;
    }
    return Changed;
  }

  bool rewriteScalarExtracts() {
    bool Changed = false;
    SmallVector<ExtractValueInst *, 32> Extracts;
    for (Instruction &I : instructions(F))
      if (auto *EVI = dyn_cast<ExtractValueInst>(&I))
        if (!EVI->getType()->isAggregateType() &&
            isGCPointerAggregateType(EVI->getAggregateOperand()->getType()) &&
            !isMustTailCallValue(EVI->getAggregateOperand()))
          Extracts.push_back(EVI);

    for (ExtractValueInst *EVI : Extracts) {
      SmallVector<Value *, 4> Leaves = getLeaves(EVI->getAggregateOperand());
      if (Leaves.empty())
        continue;

      auto LeafInfos = collectLeaves(EVI->getAggregateOperand()->getType());
      std::optional<unsigned> LeafIndex = findLeafIndex(LeafInfos,
                                                        EVI->getIndices());
      if (!LeafIndex)
        continue;

      if (Leaves[*LeafIndex] == EVI)
        continue;

      EVI->replaceAllUsesWith(Leaves[*LeafIndex]);
      MaybeDead.push_back(EVI);
      Changed = true;
    }
    return Changed;
  }

  bool eraseDeadInstructions() {
    bool Changed = false;
    for (Instruction *I : llvm::reverse(MaybeDead)) {
      if (!I->use_empty())
        continue;
      RecursivelyDeleteTriviallyDeadInstructions(I);
      Changed = true;
    }
    return Changed;
  }
};

} // end anonymous namespace

static bool exposeGCPointersInAggregates(Function &F, DominatorTree &DT) {
  if (!ExplodeGCPointerAggregates || !isOxCamlFunction(F))
    return false;

  bool HasCandidate = false;
  for (Instruction &I : instructions(F))
    if (isGCPointerAggregateType(I.getType())) {
      HasCandidate = true;
      break;
    }
  if (!HasCandidate)
    for (Argument &Arg : F.args())
      if (isGCPointerAggregateType(Arg.getType())) {
        HasCandidate = true;
        break;
      }
  if (!HasCandidate)
    return false;

  GCPointerAggregateExploder Exploder(F, DT);
  bool Changed = false;
  SmallVector<Value *, 32> Candidates;
  for (Argument &Arg : F.args())
    if (isGCPointerAggregateType(Arg.getType()))
      Candidates.push_back(&Arg);
  for (Instruction &I : instructions(F))
    if (isGCPointerAggregateType(I.getType()) && !isMustTailCallValue(&I))
      Candidates.push_back(&I);

  for (Value *V : Candidates)
    Changed |= !Exploder.getLeaves(V).empty();
  Changed |= Exploder.fillPHIs();
  Changed |= Exploder.rewriteScalarExtracts();
  Changed |= Exploder.eraseDeadInstructions();
  return Changed;
}

static void checkNoUnsupportedGCPointerAggregateUses(Function &F) {
  if (!FailOnUnhandledGCPointerAggregate)
    return;

  auto IsAllowedInternalUse = [](User *U) {
    return isa<ExtractValueInst>(U) || isa<InsertValueInst>(U);
  };

  auto CheckValue = [&](Value *V) {
    if (!isGCPointerAggregateType(V->getType()))
      return;

    for (User *U : V->users()) {
      if (IsAllowedInternalUse(U))
        continue;
      if (isMustTailCallValue(V) && isa<ReturnInst>(U))
        continue;

      errs() << "Unhandled GC pointer-containing aggregate use after "
                "aggregate explosion:\n  value: ";
      V->print(errs());
      errs() << "\n  user: ";
      U->print(errs());
      errs() << "\n";
      report_fatal_error("support for this first-class aggregate GC value "
                         "boundary is unimplemented");
    }
  };

  for (Argument &Arg : F.args())
    CheckValue(&Arg);
  for (Instruction &I : instructions(F))
    CheckValue(&I);
}

// Return the name of the value suffixed with the provided value, or if the
// value didn't have a name, the default value specified.
static std::string suffixed_name_or(Value *V, StringRef Suffix,
                                    StringRef DefaultName) {
  return V->hasName() ? (V->getName() + Suffix).str() : DefaultName.str();
}

// Conservatively identifies any definitions which might be live at the
// given instruction. The  analysis is performed immediately before the
// given instruction. Values defined by that instruction are not considered
// live.  Values used by that instruction are considered live.
static void analyzeParsePointLiveness(
    DominatorTree &DT, GCPtrLivenessData &OriginalLivenessData, CallBase *Call,
    PartiallyConstructedSafepointRecord &Result) {
  StatepointLiveSetTy LiveSet;
  findLiveSetAtInst(Call, OriginalLivenessData, LiveSet);

  SmallVector<Value *, 8> OxCamlCallArgs;
  if (isOxCamlFunction(*Call->getFunction()) &&
      isOxCamlCallingConv(Call->getCallingConv())) {
    for (Value *Arg : Call->args()) {
      if (!isHandledGCPointerType(Arg->getType()) || isa<Constant>(Arg))
        continue;

      OxCamlCallArgs.push_back(Arg);
      if (isOxCamlCWrapperCallingConv(Call->getCallingConv()))
        Result.CallArgRoots.insert(Arg);
    }
  }

  auto CallOperandUseCanRecurAfterCall = [&]() -> bool {
    if (auto *CI = dyn_cast<CallInst>(Call)) {
      Instruction *Next = CI->getNextNode();
      return Next && isPotentiallyReachable(Next, CI, nullptr, &DT);
    }

    if (auto *II = dyn_cast<InvokeInst>(Call)) {
      BasicBlock *CallBB = II->getParent();
      return isPotentiallyReachable(II->getNormalDest(), CallBB, nullptr,
                                    &DT) ||
             isPotentiallyReachable(II->getUnwindDest(), CallBB, nullptr,
                                    &DT);
    }

    return true;
  };

  auto HasUseAfterCall = [&](Value *V) {
    for (Use &U : V->uses()) {
      auto *UserI = dyn_cast<Instruction>(U.getUser());
      if (!UserI)
        continue;
      if (UserI == Call) {
        if (CallOperandUseCanRecurAfterCall())
          return true;
        continue;
      }

      if (auto *PN = dyn_cast<PHINode>(UserI)) {
        for (unsigned I = 0, E = PN->getNumIncomingValues(); I != E; ++I) {
          if (PN->getIncomingValue(I) != V)
            continue;
          BasicBlock *Incoming = PN->getIncomingBlock(I);
          Instruction *IncomingTerminator = Incoming->getTerminator();
          if (Incoming == Call->getParent() ||
              DT.dominates(Call, IncomingTerminator))
            return true;
        }
        continue;
      }

      if (UserI->getParent() == Call->getParent()) {
        if (Call->comesBefore(UserI))
          return true;
        continue;
      }

      if (DT.dominates(Call, UserI))
        return true;
    }

    return false;
  };

  SmallPtrSet<Value *, 8> VisitedCallArgRoots;
  auto RemoveCallArgRootOnlyLiveness = [&](auto &&Self, Value *V) -> void {
    if (!VisitedCallArgRoots.insert(V).second)
      return;
    if (HasUseAfterCall(V))
      return;

    LiveSet.remove(V);
    auto *I = dyn_cast<Instruction>(V);
    if (!I)
      return;

    for (Value *Operand : I->operands())
      if (isHandledGCPointerType(Operand->getType()) && !isa<Constant>(Operand))
        Self(Self, Operand);
  };
  for (Value *Arg : OxCamlCallArgs)
    RemoveCallArgRootOnlyLiveness(RemoveCallArgRootOnlyLiveness, Arg);

  if (PrintLiveSet) {
    dbgs() << "Live Variables:\n";
    for (Value *V : LiveSet)
      dbgs() << " " << V->getName() << " " << *V << "\n";
  }
  if (PrintLiveSetSize) {
    dbgs() << "Safepoint For: " << Call->getCalledOperand()->getName() << "\n";
    dbgs() << "Number live values: " << LiveSet.size() << "\n";
  }
  Result.LiveSet = LiveSet;
}

/// Returns true if V is a known base.
static bool isKnownBase(Value *V, const IsKnownBaseMapTy &KnownBases);

/// Caches the IsKnownBase flag for a value and asserts that it wasn't present
/// in the cache before.
static void setKnownBase(Value *V, bool IsKnownBase,
                         IsKnownBaseMapTy &KnownBases);

static Value *findBaseDefiningValue(Value *I, DefiningValueMapTy &Cache,
                                    IsKnownBaseMapTy &KnownBases);

static Value *findAddrSpace1IntBaseDefiningValue(Value *I,
                                                 DefiningValueMapTy &Cache,
                                                 IsKnownBaseMapTy &KnownBases);

static bool containsOxCamlPointerFormDerivedAddrSpace1Pointer(Value *I);

/// Return a base defining value for the 'Index' element of the given vector
/// instruction 'I'.  If Index is null, returns a BDV for the entire vector
/// 'I'.  As an optimization, this method will try to determine when the
/// element is known to already be a base pointer.  If this can be established,
/// the second value in the returned pair will be true.  Note that either a
/// vector or a pointer typed value can be returned.  For the former, the
/// vector returned is a BDV (and possibly a base) of the entire vector 'I'.
/// If the later, the return pointer is a BDV (or possibly a base) for the
/// particular element in 'I'.
static Value *findBaseDefiningValueOfVector(Value *I, DefiningValueMapTy &Cache,
                                            IsKnownBaseMapTy &KnownBases) {
  // Each case parallels findBaseDefiningValue below, see that code for
  // detailed motivation.

  auto Cached = Cache.find(I);
  if (Cached != Cache.end())
    return Cached->second;

  if (isa<Argument>(I)) {
    // An incoming argument to the function is a base pointer
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  if (isa<Constant>(I)) {
    // Base of constant vector consists only of constant null pointers.
    // For reasoning see similar case inside 'findBaseDefiningValue' function.
    auto *CAZ = ConstantAggregateZero::get(I->getType());
    Cache[I] = CAZ;
    setKnownBase(CAZ, /* IsKnownBase */true, KnownBases);
    return CAZ;
  }

  if (isa<LoadInst>(I)) {
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  if (isa<InsertElementInst>(I)) {
    // We don't know whether this vector contains entirely base pointers or
    // not.  To be conservatively correct, we treat it as a BDV and will
    // duplicate code as needed to construct a parallel vector of bases.
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */false, KnownBases);
    return I;
  }

  if (isa<ShuffleVectorInst>(I)) {
    // We don't know whether this vector contains entirely base pointers or
    // not.  To be conservatively correct, we treat it as a BDV and will
    // duplicate code as needed to construct a parallel vector of bases.
    // TODO: There a number of local optimizations which could be applied here
    // for particular sufflevector patterns.
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */false, KnownBases);
    return I;
  }

  // The behavior of getelementptr instructions is the same for vector and
  // non-vector data types.
  if (auto *GEP = dyn_cast<GetElementPtrInst>(I)) {
    if (GEP->getMetadata("is_base_value")) {
      Cache[GEP] = GEP;
      setKnownBase(GEP, /* IsKnownBase */true, KnownBases);
      return GEP;
    }
    auto *BDV =
        findBaseDefiningValue(GEP->getPointerOperand(), Cache, KnownBases);
    Cache[GEP] = BDV;
    return BDV;
  }

  // The behavior of freeze instructions is the same for vector and
  // non-vector data types.
  if (auto *Freeze = dyn_cast<FreezeInst>(I)) {
    auto *BDV = findBaseDefiningValue(Freeze->getOperand(0), Cache, KnownBases);
    Cache[Freeze] = BDV;
    return BDV;
  }

  // If the pointer comes through a bitcast of a vector of pointers to
  // a vector of another type of pointer, then look through the bitcast
  if (auto *BC = dyn_cast<BitCastInst>(I)) {
    auto *BDV = findBaseDefiningValue(BC->getOperand(0), Cache, KnownBases);
    Cache[BC] = BDV;
    return BDV;
  }

  // We assume that functions in the source language only return base
  // pointers.  This should probably be generalized via attributes to support
  // both source language and internal functions.
  if (isa<CallInst>(I) || isa<InvokeInst>(I)) {
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  // A PHI or Select is a base defining value.  The outer findBasePointer
  // algorithm is responsible for constructing a base value for this BDV.
  assert((isa<SelectInst>(I) || isa<PHINode>(I)) &&
         "unknown vector instruction - no base found for vector element");
  Cache[I] = I;
  setKnownBase(I, /* IsKnownBase */false, KnownBases);
  return I;
}

/// Helper function for findBasePointer - Will return a value which either a)
/// defines the base pointer for the input, b) blocks the simple search
/// (i.e. a PHI or Select of two derived pointers), or c) involves a change
/// from pointer to vector type or back.
static Value *findBaseDefiningValue(Value *I, DefiningValueMapTy &Cache,
                                    IsKnownBaseMapTy &KnownBases) {
  assert(I->getType()->isPtrOrPtrVectorTy() &&
         "Illegal to ask for the base pointer of a non-pointer type");
  auto Cached = Cache.find(I);
  if (Cached != Cache.end())
    return Cached->second;

  if (I->getType()->isVectorTy())
    return findBaseDefiningValueOfVector(I, Cache, KnownBases);

  if (isa<Argument>(I)) {
    // An incoming argument to the function is a base pointer
    // We should have never reached here if this argument isn't an gc value
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  if (isa<Constant>(I)) {
    if (cast<PointerType>(I->getType())->getAddressSpace() == 1) {
      Cache[I] = I;
      setKnownBase(I, /* IsKnownBase */true, KnownBases);
      return I;
    }

    // We assume that objects with a constant base (e.g. a global) can't move
    // and don't need to be reported to the collector because they are always
    // live. Besides global references, all kinds of constants (e.g. undef,
    // constant expressions, null pointers) can be introduced by the inliner or
    // the optimizer, especially on dynamically dead paths.
    // Here we treat all of them as having single null base. By doing this we
    // trying to avoid problems reporting various conflicts in a form of
    // "phi (const1, const2)" or "phi (const, regular gc ptr)".
    // See constant.ll file for relevant test cases.

    auto *CPN = ConstantPointerNull::get(cast<PointerType>(I->getType()));
    Cache[I] = CPN;
    setKnownBase(CPN, /* IsKnownBase */true, KnownBases);
    return CPN;
  }

  if (auto *I2P = dyn_cast<IntToPtrInst>(I)) {
    if (cast<PointerType>(I2P->getType())->getAddressSpace() == 1)
      if (auto *BDV = findAddrSpace1IntBaseDefiningValue(I2P->getOperand(0),
                                                         Cache, KnownBases)) {
        Cache[I2P] = BDV;
        return BDV;
      }

    // inttoptrs in an integral address space are currently ill-defined.  We
    // treat them as defining base pointers here for consistency with the
    // constant rule above and because we don't really have a better semantic
    // to give them.  Note that the optimizer is always free to insert undefined
    // behavior on dynamically dead paths as well.
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  if (CastInst *CI = dyn_cast<CastInst>(I)) {
    Value *Def = CI->stripPointerCasts();
    // If stripping pointer casts changes the address space there is an
    // addrspacecast in between.
    assert(cast<PointerType>(Def->getType())->getAddressSpace() ==
               cast<PointerType>(CI->getType())->getAddressSpace() &&
           "unsupported addrspacecast");
    // If we find a cast instruction here, it means we've found a cast which is
    // not simply a pointer cast (i.e. an inttoptr).  We don't know how to
    // handle int->ptr conversion.
    assert(!isa<CastInst>(Def) && "shouldn't find another cast here");
    auto *BDV = findBaseDefiningValue(Def, Cache, KnownBases);
    Cache[CI] = BDV;
    return BDV;
  }

  if (isa<LoadInst>(I)) {
    // The value loaded is an gc base itself
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I)) {
    if (GEP->getMetadata("is_base_value")) {
      Cache[GEP] = GEP;
      setKnownBase(GEP, /* IsKnownBase */true, KnownBases);
      return GEP;
    }
    // The base of this GEP is the base
    auto *BDV =
        findBaseDefiningValue(GEP->getPointerOperand(), Cache, KnownBases);
    Cache[GEP] = BDV;
    return BDV;
  }

  if (auto *Freeze = dyn_cast<FreezeInst>(I)) {
    auto *BDV = findBaseDefiningValue(Freeze->getOperand(0), Cache, KnownBases);
    Cache[Freeze] = BDV;
    return BDV;
  }

  if (IntrinsicInst *II = dyn_cast<IntrinsicInst>(I)) {
    switch (II->getIntrinsicID()) {
    default:
      // fall through to general call handling
      break;
    case Intrinsic::experimental_gc_statepoint:
      llvm_unreachable("statepoints don't produce pointers");
    case Intrinsic::experimental_gc_relocate:
      if (auto *Relocate = dyn_cast<GCRelocateInst>(II)) {
        Value *Base = Relocate->getBasePtr();
        Value *Derived = Relocate->getDerivedPtr();
        auto *BaseTy = dyn_cast<PointerType>(Base->getType());
        auto *DerivedTy = dyn_cast<PointerType>(Derived->getType());
        if (Base != Derived && BaseTy && DerivedTy &&
            BaseTy->getAddressSpace() == 1 &&
            DerivedTy->getAddressSpace() == 1)
          report_fatal_error(
              "OxCaml late root discovery found a derived gc.relocate");
        Cache[II] = II;
        setKnownBase(II, /* IsKnownBase */ true, KnownBases);
        return II;
      }
      llvm_unreachable("malformed gc.relocate intrinsic");
    case Intrinsic::gcroot:
      // Currently, this mechanism hasn't been extended to work with gcroot.
      // There's no reason it couldn't be, but I haven't thought about the
      // implications much.
      llvm_unreachable(
          "interaction with the gcroot mechanism is not supported");
    case Intrinsic::experimental_gc_get_pointer_base:
      auto *BDV = findBaseDefiningValue(II->getOperand(0), Cache, KnownBases);
      Cache[II] = BDV;
      return BDV;
    }
  }
  // We assume that functions in the source language only return base
  // pointers.  This should probably be generalized via attributes to support
  // both source language and internal functions.
  if (isa<CallInst>(I) || isa<InvokeInst>(I)) {
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  // TODO: I have absolutely no idea how to implement this part yet.  It's not
  // necessarily hard, I just haven't really looked at it yet.
  assert(!isa<LandingPadInst>(I) && "Landing Pad is unimplemented");

  if (isa<AtomicCmpXchgInst>(I)) {
    // A CAS is effectively a atomic store and load combined under a
    // predicate.  From the perspective of base pointers, we just treat it
    // like a load.
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  assert(!isa<AtomicRMWInst>(I) && "Xchg handled above, all others are "
                                   "binary ops which don't apply to pointers");

  // The aggregate ops.  Aggregates can either be in the heap or on the
  // stack, but in either case, this is simply a field load.  As a result,
  // this is a defining definition of the base just like a load is.
  if (isa<ExtractValueInst>(I)) {
    Cache[I] = I;
    setKnownBase(I, /* IsKnownBase */true, KnownBases);
    return I;
  }

  // We should never see an insert vector since that would require we be
  // tracing back a struct value not a pointer value.
  assert(!isa<InsertValueInst>(I) &&
         "Base pointer for a struct is meaningless");

  // This value might have been generated by findBasePointer() called when
  // substituting gc.get.pointer.base() intrinsic.
  bool IsKnownBase =
      isa<Instruction>(I) && cast<Instruction>(I)->getMetadata("is_base_value");
  setKnownBase(I, /* IsKnownBase */IsKnownBase, KnownBases);
  Cache[I] = I;

  // An extractelement produces a base result exactly when it's input does.
  // We may need to insert a parallel instruction to extract the appropriate
  // element out of the base vector corresponding to the input. Given this,
  // it's analogous to the phi and select case even though it's not a merge.
  if (isa<ExtractElementInst>(I))
    // Note: There a lot of obvious peephole cases here.  This are deliberately
    // handled after the main base pointer inference algorithm to make writing
    // test cases to exercise that code easier.
    return I;

  // The last two cases here don't return a base pointer.  Instead, they
  // return a value which dynamically selects from among several base
  // derived pointers (each with it's own base potentially).  It's the job of
  // the caller to resolve these.
  assert((isa<SelectInst>(I) || isa<PHINode>(I)) &&
         "missing instruction case in findBaseDefiningValue");
  return I;
}

static Value *findAddrSpace1IntBaseDefiningValue(Value *I,
                                                 DefiningValueMapTy &Cache,
                                                 IsKnownBaseMapTy &KnownBases) {
  if (auto *P2I = dyn_cast<PtrToIntInst>(I)) {
    Value *Ptr = P2I->getOperand(0);
    auto *PtrTy = dyn_cast<PointerType>(Ptr->getType());
    if (!PtrTy)
      return nullptr;
    if (PtrTy->getAddressSpace() != 1) {
      if (isOxCamlFunction(*P2I->getFunction()) &&
          containsOxCamlPointerFormDerivedAddrSpace1Pointer(Ptr)) {
        errs() << "OxCaml statepoint cannot treat an integer round-trip from "
                  "a raw pointer derived from an addrspace(1) interior "
                  "address as an ordinary root\n  value: ";
        P2I->print(errs());
        errs() << "\n";
        report_fatal_error("invalid hidden interior address through raw ptr");
      }
      return nullptr;
    }
    return findBaseDefiningValue(Ptr, Cache, KnownBases);
  }

  auto *BO = dyn_cast<BinaryOperator>(I);
  if (!BO || (BO->getOpcode() != Instruction::Add &&
              BO->getOpcode() != Instruction::Sub))
    return nullptr;

  if (isa<ConstantInt>(BO->getOperand(1)))
    return findAddrSpace1IntBaseDefiningValue(BO->getOperand(0), Cache,
                                              KnownBases);

  if (BO->getOpcode() == Instruction::Add &&
      isa<ConstantInt>(BO->getOperand(0)))
    return findAddrSpace1IntBaseDefiningValue(BO->getOperand(1), Cache,
                                              KnownBases);

  return nullptr;
}

static bool containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
    Value *V, SmallPtrSetImpl<Value *> &ActiveValues) {
  if (!ActiveValues.insert(V).second)
    return false;
  auto RemoveActive = make_scope_exit([&]() { ActiveValues.erase(V); });

  if (auto *GEP = dyn_cast<GetElementPtrInst>(V)) {
    auto *Ty = dyn_cast<PointerType>(GEP->getType());
    if (Ty && Ty->getAddressSpace() == 1 && !GEP->getMetadata("is_base_value"))
      return true;
  }

  if (auto *ASC = dyn_cast<AddrSpaceCastInst>(V))
    return containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
        ASC->getPointerOperand(), ActiveValues);

  if (auto *CI = dyn_cast<CastInst>(V)) {
    if (!isa<IntToPtrInst>(CI) && !isa<PtrToIntInst>(CI))
      return containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
          CI->getOperand(0), ActiveValues);
    return false;
  }

  if (auto *Freeze = dyn_cast<FreezeInst>(V))
    return containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
        Freeze->getOperand(0), ActiveValues);

  if (auto *Phi = dyn_cast<PHINode>(V)) {
    for (Value *Incoming : Phi->incoming_values())
      if (containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
              Incoming, ActiveValues))
        return true;
    return false;
  }

  if (auto *Select = dyn_cast<SelectInst>(V))
    return containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
               Select->getTrueValue(), ActiveValues) ||
           containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(
               Select->getFalseValue(), ActiveValues);

  return false;
}

static bool containsOxCamlPointerFormDerivedAddrSpace1Pointer(Value *I) {
  SmallPtrSet<Value *, 16> ActiveValues;
  return containsOxCamlPointerFormDerivedAddrSpace1PointerImpl(I,
                                                              ActiveValues);
}

static bool isDistinctOxCamlAddrSpace1AddressExpression(Value *V,
                                                        Value *Base) {
  if (V == Base)
    return false;

  auto *VTy = dyn_cast<PointerType>(V->getType());
  auto *BaseTy = dyn_cast<PointerType>(Base->getType());
  if (!VTy || !BaseTy || VTy->getAddressSpace() != 1 ||
      BaseTy->getAddressSpace() != 1 ||
      isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(V, Base))
    return false;

  if (containsOxCamlPointerFormDerivedAddrSpace1Pointer(V))
    return true;

  if (auto *I2P = dyn_cast<IntToPtrInst>(V)) {
    DefiningValueMapTy Cache;
    IsKnownBaseMapTy KnownBases;
    Value *IntBase =
        findAddrSpace1IntBaseDefiningValue(I2P->getOperand(0), Cache,
                                           KnownBases);
    return IntBase && isOxCamlBaseEquivalentGCPointer(IntBase, Base);
  }

  return false;
}

/// Returns the base defining value for this value.
static Value *findBaseDefiningValueCached(Value *I, DefiningValueMapTy &Cache,
                                          IsKnownBaseMapTy &KnownBases) {
  if (Cache.find(I) == Cache.end()) {
    auto *BDV = findBaseDefiningValue(I, Cache, KnownBases);
    Cache[I] = BDV;
    LLVM_DEBUG(dbgs() << "fBDV-cached: " << I->getName() << " -> "
                      << Cache[I]->getName() << ", is known base = "
                      << KnownBases[I] << "\n");
  }
  assert(Cache[I] != nullptr);
  assert(KnownBases.find(Cache[I]) != KnownBases.end() &&
         "Cached value must be present in known bases map");
  return Cache[I];
}

/// Return a base pointer for this value if known.  Otherwise, return it's
/// base defining value.
static Value *findBaseOrBDV(Value *I, DefiningValueMapTy &Cache,
                            IsKnownBaseMapTy &KnownBases) {
  Value *Def = findBaseDefiningValueCached(I, Cache, KnownBases);
  auto Found = Cache.find(Def);
  if (Found != Cache.end()) {
    // Either a base-of relation, or a self reference.  Caller must check.
    return Found->second;
  }
  // Only a BDV available
  return Def;
}

#ifndef NDEBUG
/// This value is a base pointer that is not generated by RS4GC, i.e. it already
/// exists in the code.
static bool isOriginalBaseResult(Value *V) {
  // no recursion possible
  return !isa<PHINode>(V) && !isa<SelectInst>(V) &&
         !isa<ExtractElementInst>(V) && !isa<InsertElementInst>(V) &&
         !isa<ShuffleVectorInst>(V);
}
#endif

static bool isKnownBase(Value *V, const IsKnownBaseMapTy &KnownBases) {
  auto It = KnownBases.find(V);
  assert(It != KnownBases.end() && "Value not present in the map");
  return It->second;
}

static void setKnownBase(Value *V, bool IsKnownBase,
                         IsKnownBaseMapTy &KnownBases) {
#ifndef NDEBUG
  auto It = KnownBases.find(V);
  if (It != KnownBases.end())
    assert(It->second == IsKnownBase && "Changing already present value");
#endif
  KnownBases[V] = IsKnownBase;
}

struct OxCamlHeapAddress {
  Value *Base;
  Value *Offset;
};

static std::optional<OxCamlHeapAddress>
findOxCamlHeapAddress(Value *V, IRBuilder<> &Builder) {
  const DataLayout &DL = Builder.GetInsertBlock()->getModule()->getDataLayout();
  Type *IntPtrTy = Builder.getIntNTy(DL.getPointerSizeInBits(1));

  auto NormalizeOffset = [&](Value *Offset) -> Value * {
    if (Offset->getType() == IntPtrTy)
      return Offset;
    return Builder.CreateIntCast(Offset, IntPtrTy, false,
                                 "oxcaml.heap.addr.offset.cast");
  };

  auto WithOffset = [&](OxCamlHeapAddress Addr, Value *Offset,
                        Instruction::BinaryOps Op) {
    Offset = NormalizeOffset(Offset);
    Value *NewOffset = nullptr;
    if (Op == Instruction::Add)
      NewOffset = Builder.CreateAdd(Addr.Offset, Offset,
                                    "oxcaml.heap.addr.offset");
    else {
      assert(Op == Instruction::Sub);
      NewOffset = Builder.CreateSub(Addr.Offset, Offset,
                                    "oxcaml.heap.addr.offset");
    }
    return std::optional<OxCamlHeapAddress>(
        OxCamlHeapAddress{Addr.Base, NewOffset});
  };

  auto RebuildFromPtr =
      [&](auto &&SelfPtr, auto &&SelfInt,
          Value *Ptr) -> std::optional<OxCamlHeapAddress> {
    auto *PtrTy = dyn_cast<PointerType>(Ptr->getType());
    if (!PtrTy)
      return std::nullopt;

    if (auto *ASC = dyn_cast<AddrSpaceCastInst>(Ptr))
      return SelfPtr(SelfPtr, SelfInt, ASC->getPointerOperand());

    if (auto *I2P = dyn_cast<IntToPtrInst>(Ptr))
      return SelfInt(SelfPtr, SelfInt, I2P->getOperand(0));

    if (auto *GEP = dyn_cast<GetElementPtrInst>(Ptr)) {
      auto Addr = SelfPtr(SelfPtr, SelfInt, GEP->getPointerOperand());
      if (!Addr)
        return std::nullopt;
      APInt Offset(DL.getIndexSizeInBits(PtrTy->getAddressSpace()), 0);
      if (!GEP->accumulateConstantOffset(DL, Offset))
        return std::nullopt;
      return WithOffset(*Addr, ConstantInt::get(IntPtrTy, Offset),
                        Instruction::Add);
    }

    if (PtrTy->getAddressSpace() == 1)
      return OxCamlHeapAddress{Ptr, ConstantInt::get(IntPtrTy, 0)};

    return std::nullopt;
  };

  auto RebuildFromInt =
      [&](auto &&SelfPtr, auto &&SelfInt,
          Value *IntV) -> std::optional<OxCamlHeapAddress> {
    if (auto *P2I = dyn_cast<PtrToIntInst>(IntV)) {
      auto Addr = SelfPtr(SelfPtr, SelfInt, P2I->getPointerOperand());
      if (!Addr)
        return std::nullopt;
      Addr->Offset = NormalizeOffset(Addr->Offset);
      return Addr;
    }

    auto *BO = dyn_cast<BinaryOperator>(IntV);
    if (!BO)
      return std::nullopt;

    if (BO->getOpcode() == Instruction::Add) {
      if (auto Left = SelfInt(SelfPtr, SelfInt, BO->getOperand(0)))
        return WithOffset(*Left, BO->getOperand(1), Instruction::Add);
      if (auto Right = SelfInt(SelfPtr, SelfInt, BO->getOperand(1)))
        return WithOffset(*Right, BO->getOperand(0), Instruction::Add);
      return std::nullopt;
    }

    if (BO->getOpcode() == Instruction::Sub)
      if (auto Left = SelfInt(SelfPtr, SelfInt, BO->getOperand(0)))
        return WithOffset(*Left, BO->getOperand(1), Instruction::Sub);

    return std::nullopt;
  };

  return RebuildFromPtr(RebuildFromPtr, RebuildFromInt, V);
}

static bool canonicalizeOxCamlRawHeapMemoryAddresses(Function &F) {
  if (!F.hasGC() || F.getGC() != "oxcaml")
    return false;

  bool Changed = false;
  SmallVector<Instruction *, 32> MemoryInsts;
  for (Instruction &I : instructions(F))
    if (isa<LoadInst>(I) || isa<StoreInst>(I) || isa<AtomicRMWInst>(I) ||
        isa<AtomicCmpXchgInst>(I))
      MemoryInsts.push_back(&I);

  for (Instruction *I : MemoryInsts) {
    auto GetPointerOperand = [&]() -> std::pair<Value *, unsigned> {
      if (auto *LI = dyn_cast<LoadInst>(I))
        return {LI->getPointerOperand(), LoadInst::getPointerOperandIndex()};
      if (auto *SI = dyn_cast<StoreInst>(I))
        return {SI->getPointerOperand(), StoreInst::getPointerOperandIndex()};
      if (auto *RMW = dyn_cast<AtomicRMWInst>(I))
        return {RMW->getPointerOperand(),
                AtomicRMWInst::getPointerOperandIndex()};
      auto *CXI = cast<AtomicCmpXchgInst>(I);
      return {CXI->getPointerOperand(),
              AtomicCmpXchgInst::getPointerOperandIndex()};
    };

    auto [Ptr, OperandNo] = GetPointerOperand();
    auto *PtrTy = dyn_cast<PointerType>(Ptr->getType());
    if (!PtrTy || PtrTy->getAddressSpace() != 0)
      continue;
    IRBuilder<> Builder(I);
    std::optional<OxCamlHeapAddress> Addr =
        findOxCamlHeapAddress(Ptr, Builder);
    if (!Addr)
      continue;

    Value *NewPtr = Addr->Base;
    auto *OffsetCI = dyn_cast<ConstantInt>(Addr->Offset);
    if (!OffsetCI || !OffsetCI->isZero())
      NewPtr = Builder.CreateGEP(Builder.getInt8Ty(), Addr->Base,
                                 Addr->Offset, "oxcaml.heap.addr");

    I->setOperand(OperandNo, NewPtr);
    Changed = true;
  }

  return Changed;
}

// Returns true if First and Second values are both scalar or both vector.
static bool areBothVectorOrScalar(Value *First, Value *Second) {
  return isa<VectorType>(First->getType()) ==
         isa<VectorType>(Second->getType());
}

namespace {

/// Models the state of a single base defining value in the findBasePointer
/// algorithm for determining where a new instruction is needed to propagate
/// the base of this BDV.
class BDVState {
public:
  enum StatusTy {
     // Starting state of lattice
     Unknown,
     // Some specific base value -- does *not* mean that instruction
     // propagates the base of the object
     // ex: gep %arg, 16 -> %arg is the base value
     Base,
     // Need to insert a node to represent a merge.
     Conflict
  };

  BDVState() {
    llvm_unreachable("missing state in map");
  }

  explicit BDVState(Value *OriginalValue)
    : OriginalValue(OriginalValue) {}
  explicit BDVState(Value *OriginalValue, StatusTy Status, Value *BaseValue = nullptr)
    : OriginalValue(OriginalValue), Status(Status), BaseValue(BaseValue) {
    assert(Status != Base || BaseValue);
  }

  StatusTy getStatus() const { return Status; }
  Value *getOriginalValue() const { return OriginalValue; }
  Value *getBaseValue() const { return BaseValue; }

  bool isBase() const { return getStatus() == Base; }
  bool isUnknown() const { return getStatus() == Unknown; }
  bool isConflict() const { return getStatus() == Conflict; }

  // Values of type BDVState form a lattice, and this function implements the
  // meet
  // operation.
  void meet(const BDVState &Other) {
    auto markConflict = [&]() {
      Status = BDVState::Conflict;
      BaseValue = nullptr;
    };
    // Conflict is a final state.
    if (isConflict())
      return;
    // if we are not known - just take other state.
    if (isUnknown()) {
      Status = Other.getStatus();
      BaseValue = Other.getBaseValue();
      return;
    }
    // We are base.
    assert(isBase() && "Unknown state");
    // If other is unknown - just keep our state.
    if (Other.isUnknown())
      return;
    // If other is conflict - it is a final state.
    if (Other.isConflict())
      return markConflict();
    // Other is base as well.
    assert(Other.isBase() && "Unknown state");
    // If bases are different - Conflict.
    if (getBaseValue() != Other.getBaseValue())
      return markConflict();
    // We are identical, do nothing.
  }

  bool operator==(const BDVState &Other) const {
    return OriginalValue == Other.OriginalValue && BaseValue == Other.BaseValue &&
      Status == Other.Status;
  }

  bool operator!=(const BDVState &other) const { return !(*this == other); }

  LLVM_DUMP_METHOD
  void dump() const {
    print(dbgs());
    dbgs() << '\n';
  }

  void print(raw_ostream &OS) const {
    switch (getStatus()) {
    case Unknown:
      OS << "U";
      break;
    case Base:
      OS << "B";
      break;
    case Conflict:
      OS << "C";
      break;
    }
    OS << " (base " << getBaseValue() << " - "
       << (getBaseValue() ? getBaseValue()->getName() : "nullptr") << ")"
       << " for  "  << OriginalValue->getName() << ":";
  }

private:
  AssertingVH<Value> OriginalValue; // instruction this state corresponds to
  StatusTy Status = Unknown;
  AssertingVH<Value> BaseValue = nullptr; // Non-null only if Status == Base.
};

} // end anonymous namespace

#ifndef NDEBUG
static raw_ostream &operator<<(raw_ostream &OS, const BDVState &State) {
  State.print(OS);
  return OS;
}
#endif

/// For a given value or instruction, figure out what base ptr its derived from.
/// For gc objects, this is simply itself.  On success, returns a value which is
/// the base pointer.  (This is reliable and can be used for relocation.)  On
/// failure, returns nullptr.
static Value *findBasePointer(Value *I, DefiningValueMapTy &Cache,
                              IsKnownBaseMapTy &KnownBases) {
  Value *Def = findBaseOrBDV(I, Cache, KnownBases);

  if (isKnownBase(Def, KnownBases) && areBothVectorOrScalar(Def, I))
    return Def;

  // Here's the rough algorithm:
  // - For every SSA value, construct a mapping to either an actual base
  //   pointer or a PHI which obscures the base pointer.
  // - Construct a mapping from PHI to unknown TOP state.  Use an
  //   optimistic algorithm to propagate base pointer information.  Lattice
  //   looks like:
  //   UNKNOWN
  //   b1 b2 b3 b4
  //   CONFLICT
  //   When algorithm terminates, all PHIs will either have a single concrete
  //   base or be in a conflict state.
  // - For every conflict, insert a dummy PHI node without arguments.  Add
  //   these to the base[Instruction] = BasePtr mapping.  For every
  //   non-conflict, add the actual base.
  //  - For every conflict, add arguments for the base[a] of each input
  //   arguments.
  //
  // Note: A simpler form of this would be to add the conflict form of all
  // PHIs without running the optimistic algorithm.  This would be
  // analogous to pessimistic data flow and would likely lead to an
  // overall worse solution.

#ifndef NDEBUG
  auto isExpectedBDVType = [](Value *BDV) {
    return isa<PHINode>(BDV) || isa<SelectInst>(BDV) ||
           isa<ExtractElementInst>(BDV) || isa<InsertElementInst>(BDV) ||
           isa<ShuffleVectorInst>(BDV);
  };
#endif

  // Once populated, will contain a mapping from each potentially non-base BDV
  // to a lattice value (described above) which corresponds to that BDV.
  // We use the order of insertion (DFS over the def/use graph) to provide a
  // stable deterministic ordering for visiting DenseMaps (which are unordered)
  // below.  This is important for deterministic compilation.
  MapVector<Value *, BDVState> States;

#ifndef NDEBUG
  auto VerifyStates = [&]() {
    for (auto &Entry : States) {
      assert(Entry.first == Entry.second.getOriginalValue());
    }
  };
#endif

  auto visitBDVOperands = [](Value *BDV, std::function<void (Value*)> F) {
    if (PHINode *PN = dyn_cast<PHINode>(BDV)) {
      for (Value *InVal : PN->incoming_values())
        F(InVal);
    } else if (SelectInst *SI = dyn_cast<SelectInst>(BDV)) {
      F(SI->getTrueValue());
      F(SI->getFalseValue());
    } else if (auto *EE = dyn_cast<ExtractElementInst>(BDV)) {
      F(EE->getVectorOperand());
    } else if (auto *IE = dyn_cast<InsertElementInst>(BDV)) {
      F(IE->getOperand(0));
      F(IE->getOperand(1));
    } else if (auto *SV = dyn_cast<ShuffleVectorInst>(BDV)) {
      // For a canonical broadcast, ignore the undef argument
      // (without this, we insert a parallel base shuffle for every broadcast)
      F(SV->getOperand(0));
      if (!SV->isZeroEltSplat())
        F(SV->getOperand(1));
    } else {
      llvm_unreachable("unexpected BDV type");
    }
  };


  // Recursively fill in all base defining values reachable from the initial
  // one for which we don't already know a definite base value for
  /* scope */ {
    SmallVector<Value*, 16> Worklist;
    Worklist.push_back(Def);
    States.insert({Def, BDVState(Def)});
    while (!Worklist.empty()) {
      Value *Current = Worklist.pop_back_val();
      assert(!isOriginalBaseResult(Current) && "why did it get added?");

      auto visitIncomingValue = [&](Value *InVal) {
        Value *Base = findBaseOrBDV(InVal, Cache, KnownBases);
        if (isKnownBase(Base, KnownBases) && areBothVectorOrScalar(Base, InVal))
          // Known bases won't need new instructions introduced and can be
          // ignored safely. However, this can only be done when InVal and Base
          // are both scalar or both vector. Otherwise, we need to find a
          // correct BDV for InVal, by creating an entry in the lattice
          // (States).
          return;
        assert(isExpectedBDVType(Base) && "the only non-base values "
               "we see should be base defining values");
        if (States.insert(std::make_pair(Base, BDVState(Base))).second)
          Worklist.push_back(Base);
      };

      visitBDVOperands(Current, visitIncomingValue);
    }
  }

#ifndef NDEBUG
  VerifyStates();
  LLVM_DEBUG(dbgs() << "States after initialization:\n");
  for (const auto &Pair : States) {
    LLVM_DEBUG(dbgs() << " " << Pair.second << " for " << *Pair.first << "\n");
  }
#endif

  // Iterate forward through the value graph pruning any node from the state
  // list where all of the inputs are base pointers.  The purpose of this is to
  // reuse existing values when the derived pointer we were asked to materialize
  // a base pointer for happens to be a base pointer itself.  (Or a sub-graph
  // feeding it does.)
  //
  // For OxCaml addrspace(1) pointers, also prune whole PHI/select SCCs whose
  // inputs are already base pointers.  The generic pruning below proves this
  // for acyclic cases, but a loop-carried base pointer may be represented as a
  // cycle of PHIs and gc.relocates.  In that case the PHI itself is still the
  // base pointer; inserting a parallel .base PHI only creates redundant roots.
  SmallPtrSet<Value *, 16> OxCamlSelfBaseBDVs;
  auto IsScalarOxCamlGCPointer = [](Value *V) {
    auto *PT = dyn_cast<PointerType>(V->getType());
    return PT && PT->getAddressSpace() == 1;
  };
  auto IsOxCamlSelfBaseCandidate = [&](Value *V) {
    return IsScalarOxCamlGCPointer(V) &&
           (isa<PHINode>(V) || isa<SelectInst>(V));
  };
  for (auto Pair : States)
    if (IsOxCamlSelfBaseCandidate(Pair.first))
      OxCamlSelfBaseBDVs.insert(Pair.first);

  bool ChangedOxCamlSelfBaseBDVs = true;
  while (ChangedOxCamlSelfBaseBDVs) {
    ChangedOxCamlSelfBaseBDVs = false;
    SmallVector<Value *, 8> ToErase;
    for (Value *BDV : OxCamlSelfBaseBDVs) {
      bool CanPrune = true;
      visitBDVOperands(BDV, [&](Value *Op) {
        if (!CanPrune)
          return;
        if (Op->stripPointerCasts() == BDV)
          return;

        Value *OpBDV = findBaseOrBDV(Op, Cache, KnownBases);
        if (isKnownBase(OpBDV, KnownBases) &&
            areBothVectorOrScalar(OpBDV, Op) &&
            isOxCamlBaseEquivalentGCPointer(Op, OpBDV))
          return;
        if (OxCamlSelfBaseBDVs.contains(OpBDV) &&
            isOxCamlBaseEquivalentGCPointer(Op, OpBDV))
          return;

        CanPrune = false;
      });
      if (!CanPrune)
        ToErase.push_back(BDV);
    }
    for (Value *BDV : ToErase) {
      OxCamlSelfBaseBDVs.erase(BDV);
      ChangedOxCamlSelfBaseBDVs = true;
    }
  }

  for (Value *BDV : OxCamlSelfBaseBDVs) {
    States.erase(BDV);
    Cache[BDV] = BDV;
  }

  SmallVector<Value *> ToRemove;
  do {
    ToRemove.clear();
    for (auto Pair : States) {
      Value *BDV = Pair.first;
      auto canPruneInput = [&](Value *V) {
        // If the input of the BDV is the BDV itself we can prune it. This is
        // only possible if the BDV is a PHI node.
        if (V->stripPointerCasts() == BDV)
          return true;
        Value *VBDV = findBaseOrBDV(V, Cache, KnownBases);
        if (V->stripPointerCasts() != VBDV)
          return false;
        // The assumption is that anything not in the state list is
        // propagates a base pointer.
        return States.count(VBDV) == 0;
      };

      bool CanPrune = true;
      visitBDVOperands(BDV, [&](Value *Op) {
        CanPrune = CanPrune && canPruneInput(Op);
      });
      if (CanPrune)
        ToRemove.push_back(BDV);
    }
    for (Value *V : ToRemove) {
      States.erase(V);
      // Cache the fact V is it's own base for later usage.
      Cache[V] = V;
    }
  } while (!ToRemove.empty());

  // Did we manage to prove that Def itself must be a base pointer?
  if (!States.count(Def))
    return Def;

  // Return a phi state for a base defining value.  We'll generate a new
  // base state for known bases and expect to find a cached state otherwise.
  auto GetStateForBDV = [&](Value *BaseValue, Value *Input) {
    auto I = States.find(BaseValue);
    if (I != States.end())
      return I->second;
    assert(areBothVectorOrScalar(BaseValue, Input));
    return BDVState(BaseValue, BDVState::Base, BaseValue);
  };

  bool Progress = true;
  while (Progress) {
#ifndef NDEBUG
    const size_t OldSize = States.size();
#endif
    Progress = false;
    // We're only changing values in this loop, thus safe to keep iterators.
    // Since this is computing a fixed point, the order of visit does not
    // effect the result.  TODO: We could use a worklist here and make this run
    // much faster.
    for (auto Pair : States) {
      Value *BDV = Pair.first;
      // Only values that do not have known bases or those that have differing
      // type (scalar versus vector) from a possible known base should be in the
      // lattice.
      assert((!isKnownBase(BDV, KnownBases) ||
             !areBothVectorOrScalar(BDV, Pair.second.getBaseValue())) &&
                 "why did it get added?");

      BDVState NewState(BDV);
      visitBDVOperands(BDV, [&](Value *Op) {
        Value *BDV = findBaseOrBDV(Op, Cache, KnownBases);
        auto OpState = GetStateForBDV(BDV, Op);
        NewState.meet(OpState);
      });

      BDVState OldState = States[BDV];
      if (OldState != NewState) {
        Progress = true;
        States[BDV] = NewState;
      }
    }

    assert(OldSize == States.size() &&
           "fixed point shouldn't be adding any new nodes to state");
  }

#ifndef NDEBUG
  VerifyStates();
  LLVM_DEBUG(dbgs() << "States after meet iteration:\n");
  for (const auto &Pair : States) {
    LLVM_DEBUG(dbgs() << " " << Pair.second << " for " << *Pair.first << "\n");
  }
#endif

  // Handle all instructions that have a vector BDV, but the instruction itself
  // is of scalar type.
  for (auto Pair : States) {
    Instruction *I = cast<Instruction>(Pair.first);
    BDVState State = Pair.second;
    auto *BaseValue = State.getBaseValue();
    // Only values that do not have known bases or those that have differing
    // type (scalar versus vector) from a possible known base should be in the
    // lattice.
    assert(
        (!isKnownBase(I, KnownBases) || !areBothVectorOrScalar(I, BaseValue)) &&
        "why did it get added?");
    assert(!State.isUnknown() && "Optimistic algorithm didn't complete!");

    if (!State.isBase() || !isa<VectorType>(BaseValue->getType()))
      continue;
    // extractelement instructions are a bit special in that we may need to
    // insert an extract even when we know an exact base for the instruction.
    // The problem is that we need to convert from a vector base to a scalar
    // base for the particular indice we're interested in.
    if (isa<ExtractElementInst>(I)) {
      auto *EE = cast<ExtractElementInst>(I);
      // TODO: In many cases, the new instruction is just EE itself.  We should
      // exploit this, but can't do it here since it would break the invariant
      // about the BDV not being known to be a base.
      auto *BaseInst = ExtractElementInst::Create(
          State.getBaseValue(), EE->getIndexOperand(), "base_ee", EE);
      BaseInst->setMetadata("is_base_value", MDNode::get(I->getContext(), {}));
      States[I] = BDVState(I, BDVState::Base, BaseInst);
      setKnownBase(BaseInst, /* IsKnownBase */true, KnownBases);
    } else if (!isa<VectorType>(I->getType())) {
      // We need to handle cases that have a vector base but the instruction is
      // a scalar type (these could be phis or selects or any instruction that
      // are of scalar type, but the base can be a vector type).  We
      // conservatively set this as conflict.  Setting the base value for these
      // conflicts is handled in the next loop which traverses States.
      States[I] = BDVState(I, BDVState::Conflict);
    }
  }

#ifndef NDEBUG
  VerifyStates();
#endif

  // Insert Phis for all conflicts
  // TODO: adjust naming patterns to avoid this order of iteration dependency
  for (auto Pair : States) {
    Instruction *I = cast<Instruction>(Pair.first);
    BDVState State = Pair.second;
    // Only values that do not have known bases or those that have differing
    // type (scalar versus vector) from a possible known base should be in the
    // lattice.
    assert((!isKnownBase(I, KnownBases) ||
            !areBothVectorOrScalar(I, State.getBaseValue())) &&
           "why did it get added?");
    assert(!State.isUnknown() && "Optimistic algorithm didn't complete!");

    // Since we're joining a vector and scalar base, they can never be the
    // same.  As a result, we should always see insert element having reached
    // the conflict state.
    assert(!isa<InsertElementInst>(I) || State.isConflict());

    if (!State.isConflict())
      continue;

    auto getMangledName = [](Instruction *I) -> std::string {
      if (isa<PHINode>(I)) {
        return suffixed_name_or(I, ".base", "base_phi");
      } else if (isa<SelectInst>(I)) {
        return suffixed_name_or(I, ".base", "base_select");
      } else if (isa<ExtractElementInst>(I)) {
        return suffixed_name_or(I, ".base", "base_ee");
      } else if (isa<InsertElementInst>(I)) {
        return suffixed_name_or(I, ".base", "base_ie");
      } else {
        return suffixed_name_or(I, ".base", "base_sv");
      }
    };

    Instruction *BaseInst = I->clone();
    BaseInst->insertBefore(I);
    BaseInst->setName(getMangledName(I));
    // Add metadata marking this as a base value
    BaseInst->setMetadata("is_base_value", MDNode::get(I->getContext(), {}));
    States[I] = BDVState(I, BDVState::Conflict, BaseInst);
    setKnownBase(BaseInst, /* IsKnownBase */true, KnownBases);
  }

#ifndef NDEBUG
  VerifyStates();
#endif

  // Returns a instruction which produces the base pointer for a given
  // instruction.  The instruction is assumed to be an input to one of the BDVs
  // seen in the inference algorithm above.  As such, we must either already
  // know it's base defining value is a base, or have inserted a new
  // instruction to propagate the base of it's BDV and have entered that newly
  // introduced instruction into the state table.  In either case, we are
  // assured to be able to determine an instruction which produces it's base
  // pointer.
  auto getBaseForInput = [&](Value *Input, Instruction *InsertPt) {
    Value *BDV = findBaseOrBDV(Input, Cache, KnownBases);
    Value *Base = nullptr;
    if (!States.count(BDV)) {
      assert(areBothVectorOrScalar(BDV, Input));
      Base = BDV;
    } else {
      // Either conflict or base.
      assert(States.count(BDV));
      Base = States[BDV].getBaseValue();
    }
    assert(Base && "Can't be null");
    // The cast is needed since base traversal may strip away bitcasts
    if (Base->getType() != Input->getType() && InsertPt)
      Base = new BitCastInst(Base, Input->getType(), "cast", InsertPt);
    return Base;
  };

  // Fixup all the inputs of the new PHIs.  Visit order needs to be
  // deterministic and predictable because we're naming newly created
  // instructions.
  for (auto Pair : States) {
    Instruction *BDV = cast<Instruction>(Pair.first);
    BDVState State = Pair.second;

    // Only values that do not have known bases or those that have differing
    // type (scalar versus vector) from a possible known base should be in the
    // lattice.
    assert((!isKnownBase(BDV, KnownBases) ||
            !areBothVectorOrScalar(BDV, State.getBaseValue())) &&
           "why did it get added?");
    assert(!State.isUnknown() && "Optimistic algorithm didn't complete!");
    if (!State.isConflict())
      continue;

    if (PHINode *BasePHI = dyn_cast<PHINode>(State.getBaseValue())) {
      PHINode *PN = cast<PHINode>(BDV);
      const unsigned NumPHIValues = PN->getNumIncomingValues();

      // The IR verifier requires phi nodes with multiple entries from the
      // same basic block to have the same incoming value for each of those
      // entries.  Since we're inserting bitcasts in the loop, make sure we
      // do so at least once per incoming block.
      DenseMap<BasicBlock *, Value*> BlockToValue;
      for (unsigned i = 0; i < NumPHIValues; i++) {
        Value *InVal = PN->getIncomingValue(i);
        BasicBlock *InBB = PN->getIncomingBlock(i);
        if (!BlockToValue.count(InBB))
          BlockToValue[InBB] = getBaseForInput(InVal, InBB->getTerminator());
        else {
#ifndef NDEBUG
          Value *OldBase = BlockToValue[InBB];
          Value *Base = getBaseForInput(InVal, nullptr);

          // We can't use `stripPointerCasts` instead of this function because
          // `stripPointerCasts` doesn't handle vectors of pointers.
          auto StripBitCasts = [](Value *V) -> Value * {
            while (auto *BC = dyn_cast<BitCastInst>(V))
              V = BC->getOperand(0);
            return V;
          };
          // In essence this assert states: the only way two values
          // incoming from the same basic block may be different is by
          // being different bitcasts of the same value.  A cleanup
          // that remains TODO is changing findBaseOrBDV to return an
          // llvm::Value of the correct type (and still remain pure).
          // This will remove the need to add bitcasts.
          assert(StripBitCasts(Base) == StripBitCasts(OldBase) &&
                 "findBaseOrBDV should be pure!");
#endif
        }
        Value *Base = BlockToValue[InBB];
        BasePHI->setIncomingValue(i, Base);
      }
    } else if (SelectInst *BaseSI =
                   dyn_cast<SelectInst>(State.getBaseValue())) {
      SelectInst *SI = cast<SelectInst>(BDV);

      // Find the instruction which produces the base for each input.
      // We may need to insert a bitcast.
      BaseSI->setTrueValue(getBaseForInput(SI->getTrueValue(), BaseSI));
      BaseSI->setFalseValue(getBaseForInput(SI->getFalseValue(), BaseSI));
    } else if (auto *BaseEE =
                   dyn_cast<ExtractElementInst>(State.getBaseValue())) {
      Value *InVal = cast<ExtractElementInst>(BDV)->getVectorOperand();
      // Find the instruction which produces the base for each input.  We may
      // need to insert a bitcast.
      BaseEE->setOperand(0, getBaseForInput(InVal, BaseEE));
    } else if (auto *BaseIE = dyn_cast<InsertElementInst>(State.getBaseValue())){
      auto *BdvIE = cast<InsertElementInst>(BDV);
      auto UpdateOperand = [&](int OperandIdx) {
        Value *InVal = BdvIE->getOperand(OperandIdx);
        Value *Base = getBaseForInput(InVal, BaseIE);
        BaseIE->setOperand(OperandIdx, Base);
      };
      UpdateOperand(0); // vector operand
      UpdateOperand(1); // scalar operand
    } else {
      auto *BaseSV = cast<ShuffleVectorInst>(State.getBaseValue());
      auto *BdvSV = cast<ShuffleVectorInst>(BDV);
      auto UpdateOperand = [&](int OperandIdx) {
        Value *InVal = BdvSV->getOperand(OperandIdx);
        Value *Base = getBaseForInput(InVal, BaseSV);
        BaseSV->setOperand(OperandIdx, Base);
      };
      UpdateOperand(0); // vector operand
      if (!BdvSV->isZeroEltSplat())
        UpdateOperand(1); // vector operand
      else {
        // Never read, so just use undef
        Value *InVal = BdvSV->getOperand(1);
        BaseSV->setOperand(1, UndefValue::get(InVal->getType()));
      }
    }
  }

#ifndef NDEBUG
  VerifyStates();
#endif

  // Cache all of our results so we can cheaply reuse them
  // NOTE: This is actually two caches: one of the base defining value
  // relation and one of the base pointer relation!  FIXME
  for (auto Pair : States) {
    auto *BDV = Pair.first;
    Value *Base = Pair.second.getBaseValue();
    assert(BDV && Base);
    // Only values that do not have known bases or those that have differing
    // type (scalar versus vector) from a possible known base should be in the
    // lattice.
    assert(
        (!isKnownBase(BDV, KnownBases) || !areBothVectorOrScalar(BDV, Base)) &&
        "why did it get added?");

    LLVM_DEBUG(
        dbgs() << "Updating base value cache"
               << " for: " << BDV->getName() << " from: "
               << (Cache.count(BDV) ? Cache[BDV]->getName().str() : "none")
               << " to: " << Base->getName() << "\n");

    Cache[BDV] = Base;
  }
  assert(Cache.count(Def));
  return Cache[Def];
}

// For a set of live pointers (base and/or derived), identify the base
// pointer of the object which they are derived from.  This routine will
// mutate the IR graph as needed to make the 'base' pointer live at the
// definition site of 'derived'.  This ensures that any use of 'derived' can
// also use 'base'.  This may involve the insertion of a number of
// additional PHI nodes.
//
// preconditions: live is a set of pointer type Values
//
// side effects: may insert PHI nodes into the existing CFG, will preserve
// CFG, will not remove or mutate any existing nodes
//
// post condition: PointerToBase contains one (derived, base) pair for every
// pointer in live.  Note that derived can be equal to base if the original
// pointer was a base pointer.
static void findBasePointers(const StatepointLiveSetTy &live,
                             PointerToBaseTy &PointerToBase, DominatorTree *DT,
                             DefiningValueMapTy &DVCache,
                             IsKnownBaseMapTy &KnownBases) {
  for (Value *ptr : live) {
    Value *base = findBasePointer(ptr, DVCache, KnownBases);
    assert(base && "failed to find base pointer");
    PointerToBase[ptr] = base;
    assert((!isa<Instruction>(base) || !isa<Instruction>(ptr) ||
            DT->dominates(cast<Instruction>(base)->getParent(),
                          cast<Instruction>(ptr)->getParent())) &&
           "The base we found better dominate the derived pointer");
  }
}

/// Find the required based pointers (and adjust the live set) for the given
/// parse point.
static void findBasePointers(DominatorTree &DT, DefiningValueMapTy &DVCache,
                             CallBase *Call,
                             PartiallyConstructedSafepointRecord &result,
                             PointerToBaseTy &PointerToBase,
                             IsKnownBaseMapTy &KnownBases) {
  StatepointLiveSetTy PotentiallyDerivedPointers = result.LiveSet;
  // We assume that all pointers passed to deopt are base pointers; as an
  // optimization, we can use this to avoid seperately materializing the base
  // pointer graph.  This is only relevant since we're very conservative about
  // generating new conflict nodes during base pointer insertion.  If we were
  // smarter there, this would be irrelevant.
  if (auto Opt = Call->getOperandBundle(LLVMContext::OB_deopt))
    for (Value *V : Opt->Inputs) {
      if (!PotentiallyDerivedPointers.count(V))
        continue;
      PotentiallyDerivedPointers.remove(V);
      PointerToBase[V] = V;
    }
  PotentiallyDerivedPointers.set_union(result.CallArgRoots);
  findBasePointers(PotentiallyDerivedPointers, PointerToBase, &DT, DVCache,
                   KnownBases);
}

/// Given an updated version of the dataflow liveness results, update the
/// liveset and base pointer maps for the call site CS.
static void recomputeLiveInValues(GCPtrLivenessData &RevisedLivenessData,
                                  CallBase *Call,
                                  PartiallyConstructedSafepointRecord &result,
                                  PointerToBaseTy &PointerToBase);

static void recomputeLiveInValues(
    Function &F, DominatorTree &DT, ArrayRef<CallBase *> toUpdate,
    MutableArrayRef<struct PartiallyConstructedSafepointRecord> records,
    PointerToBaseTy &PointerToBase) {
  // TODO-PERF: reuse the original liveness, then simply run the dataflow
  // again.  The old values are still live and will help it stabilize quickly.
  GCPtrLivenessData RevisedLivenessData;
  computeLiveInValues(DT, F, RevisedLivenessData);
  for (size_t i = 0; i < records.size(); i++) {
    struct PartiallyConstructedSafepointRecord &info = records[i];
    recomputeLiveInValues(RevisedLivenessData, toUpdate[i], info,
                          PointerToBase);
  }
}

static Value *findRematerializableChainToBasePointer(
    SmallVectorImpl<Instruction *> &ChainToBase, Value *CurrentValue,
    bool AllowIntegerAddressArithmetic = false);
static bool findRematerializableChainToSpecificBase(
    SmallVectorImpl<Instruction *> &ChainToBase, Value *CurrentValue,
    Value *Base, bool AllowIntegerAddressArithmetic = false);
static Value *findRematerializablePhiChainToBasePhi(
    SmallVectorImpl<Instruction *> &ChainToBase, PHINode *DerivedPhi,
    PHINode *BasePhi, const PointerToBaseTy &PointerToBase,
    bool AllowIntegerAddressArithmetic, bool &NeedsBaseSelect);
static Value *findRematerializablePhiChainToSingleBase(
    SmallVectorImpl<Instruction *> &ChainToBase, PHINode *DerivedPhi,
    Value *Base, const PointerToBaseTy &PointerToBase,
    bool AllowIntegerAddressArithmetic, bool &NeedsBaseSelect);
static bool areEquivalentRematerializableInstructions(Instruction *A,
                                                      Instruction *B);
static Value *findRematerializablePhiChainToBasePhiImpl(
    SmallVectorImpl<Instruction *> &ChainToBase, PHINode *DerivedPhi,
    PHINode *BasePhi, const PointerToBaseTy &PointerToBase,
    bool AllowIntegerAddressArithmetic, bool &NeedsBaseSelect,
    SmallPtrSetImpl<PHINode *> &ActiveDerivedPhis);

// Utility function which clones all instructions from "ChainToBase"
// and inserts them before "InsertBefore". Returns rematerialized value
// which should be used after statepoint.
static Instruction *rematerializeChain(ArrayRef<Instruction *> ChainToBase,
                                       Instruction *InsertBefore,
                                       Value *RootOfChain,
                                       Value *AlternateLiveBase) {
  Instruction *LastClonedValue = nullptr;
  Instruction *LastValue = nullptr;
  // Walk backwards to visit top-most instructions first.
  for (Instruction *Instr :
       make_range(ChainToBase.rbegin(), ChainToBase.rend())) {
    // Only GEP's, casts, freezes, and simple pointer/integer address
    // arithmetic are supported as we need to be careful to not introduce any
    // new uses of pointers not in the liveset.
    // Note that it's fine to introduce new uses of pointers which were
    // otherwise not used after this statepoint.
    assert(isa<GetElementPtrInst>(Instr) || isa<CastInst>(Instr) ||
           isa<FreezeInst>(Instr) || isa<BinaryOperator>(Instr));

    Instruction *ClonedValue = Instr->clone();
    ClonedValue->insertBefore(InsertBefore);
    ClonedValue->setName(Instr->getName() + ".remat");

    // If it is not first instruction in the chain then it uses previously
    // cloned value. We should update it to use cloned value.
    if (LastClonedValue) {
      assert(LastValue);
      ClonedValue->replaceUsesOfWith(LastValue, LastClonedValue);
#ifndef NDEBUG
      for (auto *OpValue : ClonedValue->operand_values()) {
        // Assert that cloned instruction does not use any instructions from
        // this chain other than LastClonedValue
        assert(!is_contained(ChainToBase, OpValue) &&
               "incorrect use in rematerialization chain");
        // Assert that the cloned instruction does not use the RootOfChain
        // or the AlternateLiveBase.
        assert(OpValue != RootOfChain && OpValue != AlternateLiveBase);
      }
#endif
    } else {
      // For the first instruction, replace the use of unrelocated base i.e.
      // RootOfChain/OrigRootPhi, with the corresponding PHI present in the
      // live set. They have been proved to be the same PHI nodes.  Note
      // that the *only* use of the RootOfChain in the ChainToBase list is
      // the first Value in the list.
      if (RootOfChain != AlternateLiveBase)
        ClonedValue->replaceUsesOfWith(RootOfChain, AlternateLiveBase);
    }

    LastClonedValue = ClonedValue;
    LastValue = Instr;
  }
  assert(LastClonedValue);
  return LastClonedValue;
}

// When inserting gc.relocate and gc.result calls, we need to ensure there are
// no uses of the original value / return value between the gc.statepoint and
// the gc.relocate / gc.result call.  One case which can arise is a phi node
// starting one of the successor blocks.  We also need to be able to insert the
// gc.relocates only on the path which goes through the statepoint.  We might
// need to split an edge to make this possible.
static BasicBlock *
normalizeForInvokeSafepoint(BasicBlock *BB, BasicBlock *InvokeParent,
                            DominatorTree &DT) {
  BasicBlock *Ret = BB;
  if (!BB->getUniquePredecessor())
    Ret = SplitBlockPredecessors(BB, InvokeParent, "", &DT);

  // Now that 'Ret' has unique predecessor we can safely remove all phi nodes
  // from it
  FoldSingleEntryPHINodes(Ret);
  assert(!isa<PHINode>(Ret->begin()) &&
         "All PHI nodes should have been removed!");

  // At this point, we can safely insert a gc.relocate or gc.result as the first
  // instruction in Ret if needed.
  return Ret;
}

static bool isOxCamlFunction(const Function &F) {
  return F.hasGC() && F.getGC() == "oxcaml";
}

static IntrinsicInst *getOxCamlTrapRecover(BasicBlock &BB) {
  for (Instruction &I : BB) {
    auto *II = dyn_cast<IntrinsicInst>(&I);
    if (II && II->getIntrinsicID() ==
                  Intrinsic::aarch64_oxcaml_trap_recover)
      return II;
  }
  return nullptr;
}

static bool isOxCamlRecoveryBlockStart(BasicBlock &BB,
                                       IntrinsicInst *Recover) {
  if (!Recover)
    return false;

  bool SawLandingPad = false;
  for (Instruction &I : BB) {
    if (isa<PHINode>(I) || I.isDebugOrPseudoInst())
      continue;
    if (auto *LPad = dyn_cast<LandingPadInst>(&I)) {
      if (!LPad->getType()->isTokenTy() || !LPad->isCleanup() ||
          SawLandingPad)
        return false;
      SawLandingPad = true;
      continue;
    }
    if (SawLandingPad && isa<AddrSpaceCastInst>(I))
      continue;
    return &I == Recover;
  }
  return false;
}

static bool isBranchOnlyOxCamlRecoveryLandingPad(BasicBlock *BB,
                                                 BasicBlock *Target,
                                                 bool AllowPHIs = false) {
  if (!BB || (!AllowPHIs && BB->phis().begin() != BB->phis().end()))
    return false;

  LandingPadInst *LPad = BB->getLandingPadInst();
  if (!LPad || !LPad->getType()->isTokenTy() || !LPad->isCleanup())
    return false;

  bool SawLandingPad = false;
  for (Instruction &I : *BB) {
    if ((AllowPHIs && isa<PHINode>(I)) || I.isDebugOrPseudoInst())
      continue;
    if (&I == LPad) {
      SawLandingPad = true;
      continue;
    }
    auto *II = dyn_cast<IntrinsicInst>(&I);
    if (SawLandingPad && II &&
        II->getIntrinsicID() == Intrinsic::experimental_gc_relocate)
      continue;
    auto *Br = dyn_cast<BranchInst>(&I);
    return SawLandingPad && Br && Br->isUnconditional() &&
           Br->getSuccessor(0) == Target;
  }
  return false;
}

static bool isBranchOnlyOxCamlRecoveryForwarder(BasicBlock *BB,
                                                BasicBlock *Target,
                                                bool AllowPHIs = false) {
  if (!BB || (!AllowPHIs && BB->phis().begin() != BB->phis().end()))
    return false;
  if (BB->getLandingPadInst())
    return false;

  for (Instruction &I : *BB) {
    if ((AllowPHIs && isa<PHINode>(I)) || I.isDebugOrPseudoInst())
      continue;
    auto *Br = dyn_cast<BranchInst>(&I);
    return Br && Br->isUnconditional() && Br->getSuccessor(0) == Target;
  }
  return false;
}

static bool isBranchOnlyOxCamlRecoveryTrampoline(BasicBlock *BB,
                                                 BasicBlock *Target,
                                                 bool AllowPHIs = false) {
  return isBranchOnlyOxCamlRecoveryLandingPad(BB, Target, AllowPHIs) ||
         isBranchOnlyOxCamlRecoveryForwarder(BB, Target, AllowPHIs);
}

struct OxCamlRecoveryStoreSite {
  InvokeInst *Invoke;
  BasicBlock *PhiIncomingBlock;
};

struct OxCamlRecoveryBoundaryPhi {
  PHINode *Phi;
  unsigned IncomingIndex;
  BasicBlock *IncomingBlock;
  Value *IncomingValue;
};

struct OxCamlRecoveryBoundaryUse {
  BasicBlock *IncomingBlock;
  BasicBlock *BoundaryBlock;
  Value *Value;
};

struct OxCamlRecoveryBoundaryEdge {
  BasicBlock *IncomingBlock;
  BasicBlock *BoundaryBlock;
};

static bool
collectOxCamlRecoveryStoreSites(BasicBlock &RecoveryBB, BasicBlock *IncomingBB,
                                SmallVectorImpl<OxCamlRecoveryStoreSite> &Out) {
  SmallPtrSet<BasicBlock *, 8> Visited;
  std::function<bool(BasicBlock *, BasicBlock *, BasicBlock *)> Collect =
      [&](BasicBlock *BB, BasicBlock *Target,
          BasicBlock *PhiIncomingBlock) -> bool {
    if (!BB || !Visited.insert(BB).second)
      return false;

    if (auto *II = dyn_cast<InvokeInst>(BB->getTerminator())) {
      if (II->getUnwindDest() != Target)
        return false;
      Out.push_back({II, PhiIncomingBlock});
      return true;
    }

    if (!isBranchOnlyOxCamlRecoveryTrampoline(BB, Target,
                                              /*AllowPHIs=*/true))
      return false;

    bool SawStoreSite = false;
    for (BasicBlock *Pred : predecessors(BB)) {
      if (!Collect(Pred, BB, PhiIncomingBlock))
        return false;
      SawStoreSite = true;
    }
    return SawStoreSite;
  };

  return Collect(IncomingBB, &RecoveryBB, IncomingBB);
}

static bool collectDirectOxCamlRecoveryStoreSites(
    BasicBlock &RecoveryBB, SmallVectorImpl<OxCamlRecoveryStoreSite> &Out) {
  bool SawStoreSite = false;
  for (BasicBlock *Pred : predecessors(&RecoveryBB)) {
    if (!collectOxCamlRecoveryStoreSites(RecoveryBB, Pred, Out))
      return false;
    SawStoreSite = true;
  }
  return SawStoreSite;
}

static BasicBlock *
getOxCamlTrapRecoveryTargetShapeOnly(BasicBlock *BB,
                                     SmallPtrSetImpl<BasicBlock *> &Visited) {
  if (!BB || !Visited.insert(BB).second)
    return nullptr;

  if (isOxCamlRecoveryBlockStart(*BB, getOxCamlTrapRecover(*BB)))
    return BB;

  if (BB->phis().begin() != BB->phis().end())
    return nullptr;

  LandingPadInst *LPad = BB->getLandingPadInst();
  if (!LPad || !LPad->getType()->isTokenTy() || !LPad->isCleanup())
    return nullptr;

  bool SawLandingPad = false;
  for (Instruction &I : *BB) {
    if (I.isDebugOrPseudoInst())
      continue;
    if (&I == LPad) {
      SawLandingPad = true;
      continue;
    }
    auto *II = dyn_cast<IntrinsicInst>(&I);
    if (SawLandingPad && II &&
        II->getIntrinsicID() == Intrinsic::experimental_gc_relocate)
      continue;
    auto *Br = dyn_cast<BranchInst>(&I);
    if (!SawLandingPad || !Br || !Br->isUnconditional())
      return nullptr;
    return getOxCamlTrapRecoveryTargetShapeOnly(Br->getSuccessor(0), Visited);
  }

  return nullptr;
}

static BasicBlock *getOxCamlTrapRecoveryTargetShapeOnly(BasicBlock *BB) {
  SmallPtrSet<BasicBlock *, 8> Visited;
  return getOxCamlTrapRecoveryTargetShapeOnly(BB, Visited);
}

static bool isUnsupportedNestedOxCamlRecoveryInstruction(Instruction &I) {
  auto *II = dyn_cast<IntrinsicInst>(&I);
  if (!II)
    return false;

  switch (II->getIntrinsicID()) {
  case Intrinsic::aarch64_oxcaml_push_trap:
    return true;
  default:
    return false;
  }
}

static bool isOxCamlRecoveryRegionBoundaryBlock(BasicBlock &BB) {
  if (BB.getLandingPadInst())
    return true;

  for (Instruction &I : BB) {
    if (isa<PHINode>(I) || I.isDebugOrPseudoInst())
      continue;
    if (isUnsupportedNestedOxCamlRecoveryInstruction(I))
      return true;
  }

  return false;
}

static bool isOxCamlExceptionRootLoad(Value *V) {
  auto *LI = dyn_cast<LoadInst>(V);
  return LI && LI->getMetadata("oxcaml.exnroot.load");
}

static bool isOxCamlBaseRelationThroughExceptionRoots(
    Value *V, Value *B, SmallVectorImpl<std::pair<Value *, Value *>> &Active) {
  if (V == B)
    return true;
  if (isOxCamlBaseEquivalentGCPointer(V, B))
    return true;

  if (is_contained(Active, std::make_pair(V, B)))
    return true;
  Active.push_back({V, B});
  auto PopActive = make_scope_exit([&]() { Active.pop_back(); });

  auto CollectNonConstantStores =
      [](Value *Slot, SmallVectorImpl<Value *> &StoredValues) {
        for (User *U : Slot->users()) {
          auto *Store = dyn_cast<StoreInst>(U);
          if (!Store || Store->getPointerOperand() != Slot)
            continue;
          Value *Stored =
              nonPoisonOxCamlGCPointerValue(Store->getValueOperand());
          if (!isa<Constant>(Stored))
            StoredValues.push_back(Stored);
        }
      };

  if (auto *Load = dyn_cast<LoadInst>(V)) {
    if (!isOxCamlExceptionRootLoad(Load))
      return false;

    SmallVector<Value *, 4> StoredValues;
    CollectNonConstantStores(Load->getPointerOperand(), StoredValues);

    if (auto *BaseLoad = dyn_cast<LoadInst>(B)) {
      if (!isOxCamlExceptionRootLoad(BaseLoad))
        return false;
      SmallVector<Value *, 4> BaseStoredValues;
      CollectNonConstantStores(BaseLoad->getPointerOperand(),
                               BaseStoredValues);
      if (StoredValues.empty() || BaseStoredValues.empty())
        return false;
      for (Value *Stored : StoredValues) {
        bool FoundMatchingBaseStore = false;
        for (Value *BaseStored : BaseStoredValues) {
          if (Stored->getType() != BaseStored->getType())
            continue;
          if (isOxCamlBaseRelationThroughExceptionRoots(Stored, BaseStored,
                                                        Active)) {
            FoundMatchingBaseStore = true;
            break;
          }
        }
        if (!FoundMatchingBaseStore)
          return false;
      }
      return true;
    }

    bool SawNonConstantStore = false;
    for (Value *Stored : StoredValues) {
      if (Stored->getType() != B->getType())
        return false;
      SawNonConstantStore = true;
      if (!isOxCamlBaseRelationThroughExceptionRoots(Stored, B, Active))
        return false;
    }
    return SawNonConstantStore;
  }

  auto *VPhi = dyn_cast<PHINode>(V);
  auto *BPhi = dyn_cast<PHINode>(B);
  if (!VPhi || !BPhi || VPhi->getParent() != BPhi->getParent() ||
      VPhi->getNumIncomingValues() != BPhi->getNumIncomingValues())
    return false;

  for (unsigned I = 0, E = VPhi->getNumIncomingValues(); I != E; ++I) {
    if (VPhi->getIncomingBlock(I) != BPhi->getIncomingBlock(I))
      return false;
    if (!isOxCamlBaseRelationThroughExceptionRoots(
            VPhi->getIncomingValue(I), BPhi->getIncomingValue(I), Active))
      return false;
  }
  return true;
}

static bool isOxCamlBaseRelationThroughExceptionRoots(Value *V, Value *B) {
  SmallVector<std::pair<Value *, Value *>, 8> Active;
  return isOxCamlBaseRelationThroughExceptionRoots(V, B, Active);
}

static Value *getOxCamlRecoveryGCPointer(Value *V) {
  if (isHandledGCPointerType(V->getType()))
    return V;

  auto *ASC = dyn_cast<AddrSpaceCastInst>(V);
  if (!ASC)
    return nullptr;

  Value *Base = ASC->getPointerOperand();
  if (!isHandledGCPointerType(Base->getType()))
    return nullptr;

  return Base;
}

static bool collectStrictOxCamlRecoveryOnlyRegion(
    BasicBlock &RecoveryBB, IntrinsicInst &Recover,
    SmallPtrSetImpl<BasicBlock *> &Region,
    SmallVectorImpl<OxCamlRecoveryBoundaryPhi> &BoundaryPhis,
    SmallVectorImpl<OxCamlRecoveryBoundaryUse> &BoundaryUses,
    SmallVectorImpl<OxCamlRecoveryBoundaryEdge> &BoundaryEdges) {
  SmallVector<BasicBlock *, 8> Worklist;
  Region.insert(&RecoveryBB);
  Worklist.push_back(&RecoveryBB);

  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.pop_back_val();

    if (BB != &RecoveryBB) {
      for (Instruction &I : *BB) {
        if (isa<PHINode>(I) || I.isDebugOrPseudoInst())
          continue;
        if (isa<LandingPadInst>(I) ||
            isUnsupportedNestedOxCamlRecoveryInstruction(I))
          return false;
      }
    }

    for (BasicBlock *Succ : successors(BB)) {
      if (Region.contains(Succ))
        continue;

      bool AllPredsInRegion =
          !isOxCamlRecoveryRegionBoundaryBlock(*Succ);
      for (BasicBlock *Pred : predecessors(Succ)) {
        if (!Region.contains(Pred)) {
          AllPredsInRegion = false;
          break;
        }
      }

      if (AllPredsInRegion) {
        Region.insert(Succ);
        Worklist.push_back(Succ);
        continue;
      }

      bool SeenEdge = false;
      for (const OxCamlRecoveryBoundaryEdge &BoundaryEdge : BoundaryEdges) {
        if (BoundaryEdge.IncomingBlock == BB &&
            BoundaryEdge.BoundaryBlock == Succ) {
          SeenEdge = true;
          break;
        }
      }
      if (!SeenEdge)
        BoundaryEdges.push_back({BB, Succ});

      for (PHINode &PN : Succ->phis()) {
        for (unsigned I = 0, E = PN.getNumIncomingValues(); I != E; ++I) {
          if (PN.getIncomingBlock(I) != BB)
            continue;
          Value *Incoming = PN.getIncomingValue(I);
          if (!isHandledGCPointerType(Incoming->getType()) ||
              isa<Constant>(Incoming) || isOxCamlExceptionRootLoad(Incoming))
            continue;
          BoundaryPhis.push_back({&PN, I, BB, Incoming});
        }
      }

      for (Instruction &I : *Succ) {
        if (isa<PHINode>(I))
          continue;
        if (I.isDebugOrPseudoInst())
          continue;
        for (Use &U : I.operands()) {
          Value *V = getOxCamlRecoveryGCPointer(U.get());
          if (!V)
            continue;
          if (!isHandledGCPointerType(V->getType()) || isa<Constant>(V) ||
              isOxCamlExceptionRootLoad(V))
            continue;
          if (auto *VI = dyn_cast<Instruction>(V))
            if (Region.contains(VI->getParent()) || VI->getParent() == Succ)
              continue;
          bool Seen = false;
          for (const OxCamlRecoveryBoundaryUse &BoundaryUse : BoundaryUses) {
            if (BoundaryUse.IncomingBlock == BB &&
                BoundaryUse.BoundaryBlock == Succ &&
                BoundaryUse.Value == V) {
              Seen = true;
              break;
            }
          }
          if (!Seen)
            BoundaryUses.push_back({BB, Succ, V});
        }
      }
    }
  }

  (void)Recover;
  return true;
}

static void collectOxCamlRecoveryRegionLiveGCValues(
    BasicBlock &RecoveryBB, IntrinsicInst &Recover,
    const SmallPtrSetImpl<BasicBlock *> &Region,
    SmallVectorImpl<Value *> &LiveGCValues, DenseSet<Value *> &Seen) {
  auto AddLiveValue = [&](Value *V) {
    V = getOxCamlRecoveryGCPointer(V);
    if (!V)
      return;
    if (!isHandledGCPointerType(V->getType()) || isa<Constant>(V) ||
        isOxCamlExceptionRootLoad(V))
      return;
    if (auto *VI = dyn_cast<Instruction>(V))
      if (Region.contains(VI->getParent()))
        return;
    if (Seen.insert(V).second)
      LiveGCValues.push_back(V);
  };

  for (BasicBlock *BB : Region) {
    if (BB != &RecoveryBB) {
      for (PHINode &PN : BB->phis()) {
        for (Use &U : PN.incoming_values())
          AddLiveValue(U.get());
      }
    }

    for (Instruction &I : *BB) {
      if (isa<PHINode>(I) || I.isDebugOrPseudoInst())
        continue;
      if (BB == &RecoveryBB && !Recover.comesBefore(&I))
        continue;

      for (Use &U : I.operands())
        AddLiveValue(U.get());
    }
  }
}

static Value *nonPoisonTrapRecoverySlotValue(Value *V) {
  Type *Ty = V->getType();
  if (!isa<UndefValue>(V) && !isa<PoisonValue>(V))
    return V;

  if (isHandledGCPointerType(Ty))
    return getOxCamlNonMovingImmediate(Ty);
  return Constant::getNullValue(Ty);
}

static bool rematerializeOxCamlDerivedGEPsAtUses(Function &F) {
  if (!RematAddrSpace1DerivedFromBaseAtAlloc || !isOxCamlFunction(F))
    return false;

  bool Changed = false;
  bool ChangedThisRound = true;
  while (ChangedThisRound) {
    ChangedThisRound = false;

    SmallVector<GetElementPtrInst *, 16> Worklist;
    for (Instruction &I : instructions(F)) {
      auto *GEP = dyn_cast<GetElementPtrInst>(&I);
      if (!GEP)
        continue;
      if (GEP->getMetadata("is_base_value"))
        continue;
      if (GEP->getMetadata("oxcaml.local.remat"))
        continue;
      if (!isHandledGCPointerType(GEP->getType()) ||
          !isHandledGCPointerType(GEP->getPointerOperandType()))
        continue;
      Worklist.push_back(GEP);
    }

    for (GetElementPtrInst *GEP : Worklist) {
      if (GEP->use_empty() || GEP->getParent() == nullptr)
        continue;

      SmallVector<Use *, 8> Uses;
      for (Use &U : GEP->uses())
        Uses.push_back(&U);

      for (Use *U : Uses) {
        auto *UserI = dyn_cast<Instruction>(U->getUser());
        if (!UserI)
          continue;

        Instruction *InsertBefore;
        if (auto *PN = dyn_cast<PHINode>(UserI)) {
          unsigned IncomingIndex = U->getOperandNo();
          InsertBefore = PN->getIncomingBlock(IncomingIndex)->getTerminator();
        } else {
          InsertBefore = UserI;
        }

        auto *Clone = cast<GetElementPtrInst>(GEP->clone());
        Clone->setName(suffixed_name_or(GEP, ".remat", "remat"));
        Clone->setMetadata("oxcaml.local.remat",
                           MDNode::get(F.getContext(), {}));
        Clone->insertBefore(InsertBefore);
        U->set(Clone);
      }

      if (GEP->use_empty()) {
        GEP->eraseFromParent();
        ChangedThisRound = true;
        Changed = true;
      }
    }
  }

  if (Changed) {
    for (Instruction &I : instructions(F))
      if (auto *GEP = dyn_cast<GetElementPtrInst>(&I))
        GEP->setMetadata("oxcaml.local.remat", nullptr);
  }

  return Changed;
}

static bool isRootableOxCamlExceptionValue(Value *V,
                                           DenseMap<Value *, bool> &Cache,
                                           SmallPtrSetImpl<Value *> &Visiting) {
  if (!isHandledGCPointerType(V->getType()))
    return false;

  auto Cached = Cache.find(V);
  if (Cached != Cache.end())
    return Cached->second;

  if (isa<Constant>(V) || isa<Argument>(V) || isa<LoadInst>(V) ||
      isa<ExtractValueInst>(V) || isa<CallInst>(V) || isa<InvokeInst>(V) ||
      isa<AtomicCmpXchgInst>(V)) {
    Cache[V] = true;
    return true;
  }

  if (auto *GEP = dyn_cast<GetElementPtrInst>(V))
    if (GEP->getMetadata("is_base_value")) {
      Cache[V] = true;
      return true;
    }

  if (!Visiting.insert(V).second)
    return true;

  auto Finish = [&](bool Result) {
    Visiting.erase(V);
    Cache[V] = Result;
    return Result;
  };

  if (auto *PN = dyn_cast<PHINode>(V)) {
    for (Value *Incoming : PN->incoming_values()) {
      if (isa<PoisonValue>(Incoming) || isa<UndefValue>(Incoming))
        continue;
      if (!isRootableOxCamlExceptionValue(Incoming, Cache, Visiting))
        return Finish(false);
    }
    return Finish(true);
  }

  if (auto *SI = dyn_cast<SelectInst>(V)) {
    return Finish(isRootableOxCamlExceptionValue(SI->getTrueValue(), Cache,
                                                 Visiting) &&
                  isRootableOxCamlExceptionValue(SI->getFalseValue(), Cache,
                                                 Visiting));
  }

  if (auto *Freeze = dyn_cast<FreezeInst>(V))
    return Finish(isRootableOxCamlExceptionValue(Freeze->getOperand(0), Cache,
                                                 Visiting));

  if (isa<IntToPtrInst>(V)) {
    Cache[V] = true;
    Visiting.erase(V);
    return true;
  }

  if (auto *CI = dyn_cast<CastInst>(V)) {
    Value *Def = CI->stripPointerCasts();
    if (Def == V)
      return Finish(false);
    if (Def->getType() != V->getType())
      return Finish(false);
    return Finish(isRootableOxCamlExceptionValue(Def, Cache, Visiting));
  }

  return Finish(false);
}

/// The constant initialization in the entry block is not a defining store;
/// everything else is.  Defining stores must be identified by their value,
/// not their block: a consumer-placed defining store can legitimately sit in
/// the entry block below interior call statepoints.
static bool isOxCamlRootSlotInitStore(const StoreInst *Store) {
  return isa<Constant>(Store->getValueOperand()) &&
         Store->getParent() == &Store->getFunction()->getEntryBlock();
}

/// A slot with at most one defining store always holds the same object
/// (modulo GC moves) from that store onward, so other SSA names for that
/// object may share it.  Per-invoke phi-edge slots have several defining
/// stores and must not be shared.
static bool oxcamlRootSlotHasSingleDefiningStore(const AllocaInst *Slot) {
  unsigned DefStores = 0;
  for (const User *U : Slot->users()) {
    auto *Store = dyn_cast<StoreInst>(U);
    if (!Store || Store->getPointerOperand() != Slot)
      continue;
    if (isOxCamlRootSlotInitStore(Store))
      continue;
    if (++DefStores > 1)
      return false;
  }
  return true;
}

static bool materializeOxCamlExceptionRootSlots(
    Function &F, DominatorTree &DT, ExplicitRootSlotMapTy &ExplicitRootSlots,
    ExplicitExceptionRootCallSetTy &ExplicitExceptionRootCalls,
    ExplicitRootHomeMapTy &ExplicitRootHomes,
    OxCamlValueRootSlotMapTy &SlotForValue, DefiningValueMapTy &DVCache,
    IsKnownBaseMapTy &KnownBases) {
  if (!isOxCamlFunction(F))
    return false;

  const DataLayout &DL = F.getParent()->getDataLayout();
  bool Changed = false;
  SmallVector<PHINode *, 8> HandlerLivePhis;
  SmallVector<Value *, 8> HandlerLiveGCValues;
  DenseSet<Value *> SeenHandlerLiveGCValues;
  SmallVector<OxCamlRecoveryBoundaryPhi, 8> BoundaryPhis;
  SmallVector<OxCamlRecoveryBoundaryUse, 8> BoundaryUses;
  SmallVector<OxCamlRecoveryBoundaryEdge, 8> BoundaryEdges;
  // The single defining store of each value root slot; it migrates towards
  // the entry as more regions come to share the slot.
  DenseMap<AllocaInst *, StoreInst *> SlotDefStore;
  for (BasicBlock &BB : F) {
    IntrinsicInst *Recover = getOxCamlTrapRecover(BB);
    if (!isOxCamlRecoveryBlockStart(BB, Recover))
      continue;

    DVCache.clear();
    KnownBases.clear();

    HandlerLivePhis.clear();
    for (PHINode &PN : BB.phis()) {
      if (isHandledGCPointerType(PN.getType()))
        HandlerLivePhis.push_back(&PN);
    }
    for (BasicBlock *Pred : predecessors(&BB)) {
      if (!isBranchOnlyOxCamlRecoveryTrampoline(Pred, &BB,
                                                /*AllowPHIs=*/true))
        continue;
      for (PHINode &PN : Pred->phis())
        if (isHandledGCPointerType(PN.getType()))
          HandlerLivePhis.push_back(&PN);
    }

    HandlerLiveGCValues.clear();
    SeenHandlerLiveGCValues.clear();
    for (Instruction &I : BB) {
      if (!Recover->comesBefore(&I))
        continue;

      for (Use &U : I.operands()) {
        Value *V = getOxCamlRecoveryGCPointer(U.get());
        if (!V)
          continue;
        if (!isHandledGCPointerType(V->getType()) || isa<Constant>(V))
          continue;
        if (auto *VI = dyn_cast<Instruction>(V))
          if (VI->getParent() == &BB)
            continue;
        if (SeenHandlerLiveGCValues.insert(V).second)
          HandlerLiveGCValues.push_back(V);
      }
    }

    DenseMap<Value *, bool> RootableExceptionValueCache;
    SmallPtrSet<Value *, 8> VisitingRootableExceptionValue;
    auto IsRootableExceptionValue = [&](Value *V) {
      return isRootableOxCamlExceptionValue(V, RootableExceptionValueCache,
                                            VisitingRootableExceptionValue);
    };

    BoundaryPhis.clear();
    BoundaryUses.clear();
    BoundaryEdges.clear();
    SmallPtrSet<BasicBlock *, 8> RecoveryOnlyRegion;
    bool HasStrictRecoveryOnlyRegion =
        collectStrictOxCamlRecoveryOnlyRegion(BB, *Recover,
                                              RecoveryOnlyRegion,
                                              BoundaryPhis, BoundaryUses,
                                              BoundaryEdges);
    if (HasStrictRecoveryOnlyRegion)
      collectOxCamlRecoveryRegionLiveGCValues(
          BB, *Recover, RecoveryOnlyRegion, HandlerLiveGCValues,
          SeenHandlerLiveGCValues);
    else {
      RecoveryOnlyRegion.clear();
      RecoveryOnlyRegion.insert(&BB);
      BoundaryPhis.clear();
      BoundaryUses.clear();
      BoundaryEdges.clear();
    }

    bool HasPotentialBoundaryLiveUses =
        HasStrictRecoveryOnlyRegion && !BoundaryEdges.empty();
    if (HandlerLivePhis.empty() && HandlerLiveGCValues.empty() &&
        BoundaryPhis.empty() && BoundaryUses.empty() &&
        !HasPotentialBoundaryLiveUses)
      continue;

    SmallPtrSet<PHINode *, 8> HandlerLivePhiSet(HandlerLivePhis.begin(),
                                                HandlerLivePhis.end());
    HandlerLiveGCValues.erase(
        llvm::remove_if(HandlerLiveGCValues,
                        [&](Value *V) {
                          auto *PN = dyn_cast<PHINode>(V);
                          return PN && HandlerLivePhiSet.contains(PN);
                        }),
        HandlerLiveGCValues.end());

    HandlerLiveGCValues.erase(
        llvm::remove_if(
            HandlerLiveGCValues,
            [&](Value *V) { return !IsRootableExceptionValue(V); }),
        HandlerLiveGCValues.end());

    BasicBlock &EntryBlock = F.getEntryBlock();
    IRBuilder<> EntryBuilder(&EntryBlock, EntryBlock.getFirstInsertionPt());
    IRBuilder<> RecoverBuilder(Recover->getNextNode());

    SmallVector<OxCamlRecoveryStoreSite, 8> RegionStoreSites;
    if (!collectDirectOxCamlRecoveryStoreSites(BB, RegionStoreSites)) {
      std::string Message;
      raw_string_ostream OS(Message);
      OS << "unsupported OxCaml recovery predecessor for explicit "
            "exception roots: recovery block "
         << BB.getName();
      report_fatal_error(Twine(OS.str()));
    }

    auto RegisterSlotAtStoreSites =
        [&](AllocaInst *Slot, ArrayRef<OxCamlRecoveryStoreSite> StoreSites) {
          for (const OxCamlRecoveryStoreSite &StoreSite : StoreSites) {
            SmallVectorImpl<Value *> &Slots =
                ExplicitRootSlots[StoreSite.Invoke];
            if (!llvm::is_contained(Slots, Slot))
              Slots.push_back(Slot);
            ExplicitExceptionRootCalls.insert(StoreSite.Invoke);
          }
        };

    auto CreateRootSlotAlloca = [&](Value *HandlerValue) -> AllocaInst * {
      if (!HandlerValue->getType()->isSized()) {
        HandlerValue->print(errs());
        errs() << "\n";
        report_fatal_error(
            "cannot materialize unsized OxCaml exception value");
      }
      AllocaInst *Slot = EntryBuilder.CreateAlloca(
          HandlerValue->getType(), DL.getAllocaAddrSpace(), nullptr,
          suffixed_name_or(HandlerValue, ".exnroot", "exnroot"));
      Slot->setAlignment(DL.getABITypeAlign(HandlerValue->getType()));
      return Slot;
    };

    auto CreateRecoverLoadAt = [&](Value *HandlerValue, AllocaInst *Slot,
                                   IRBuilder<> &Builder) -> LoadInst * {
      LoadInst *Load = Builder.CreateAlignedLoad(
          HandlerValue->getType(), Slot,
          DL.getABITypeAlign(HandlerValue->getType()),
          suffixed_name_or(HandlerValue, ".exnroot.load", "exnroot.load"));
      if (OxCamlVolatileExnRootSlots &&
          isHandledGCPointerType(HandlerValue->getType()))
        Load->setVolatile(true);
      Load->setMetadata("oxcaml.exnroot.load",
                        MDNode::get(F.getContext(), {}));
      return Load;
    };
    auto CreateRecoverLoad = [&](Value *HandlerValue,
                                 AllocaInst *Slot) -> LoadInst * {
      return CreateRecoverLoadAt(HandlerValue, Slot, RecoverBuilder);
    };

    // A PHI at a recovery entry or trampoline takes its value across unwind
    // edges, so its slot cannot be stored at the definition; the edge value
    // is stored before each protected invoke instead.  The trampoline shape
    // alone is not enough: a plain phis-plus-branch block matches it, so the
    // branch target must actually lead to a trap recovery block.
    auto IsUnwindEdgeDefinedPhi = [&](PHINode *PN) -> bool {
      BasicBlock *Parent = PN->getParent();
      if (IntrinsicInst *ParentRecover = getOxCamlTrapRecover(*Parent))
        if (isOxCamlRecoveryBlockStart(*Parent, ParentRecover))
          return true;
      // Landing pads are direct unwind targets.
      if (Parent->isLandingPad())
        return true;
      if (auto *Br = dyn_cast<BranchInst>(Parent->getTerminator()))
        if (Br->isUnconditional() &&
            isBranchOnlyOxCamlRecoveryTrampoline(Parent, Br->getSuccessor(0),
                                                 /*AllowPHIs=*/true) &&
            getOxCamlTrapRecoveryTargetShapeOnly(Br->getSuccessor(0)))
          return true;
      return false;
    };

    auto GetOrCreatePhiEdgeSlot = [&](PHINode *SlotPhi) -> AllocaInst * {
      if (AllocaInst *Slot = SlotForValue.lookup(SlotPhi))
        return Slot;

      bool IsGCRoot = isHandledGCPointerType(SlotPhi->getType());
      SmallVector<OxCamlRecoveryStoreSite, 8> StoreSites;
      if (SlotPhi->getParent() == &BB)
        llvm::append_range(StoreSites, RegionStoreSites);
      else if (!collectDirectOxCamlRecoveryStoreSites(*SlotPhi->getParent(),
                                                      StoreSites)) {
        errs() << "phi: ";
        SlotPhi->print(errs());
        errs() << "\nparent: " << SlotPhi->getParent()->getName()
               << "\nfunction: " << F.getName() << "\npreds:";
        for (BasicBlock *Pred : predecessors(SlotPhi->getParent()))
          errs() << " " << Pred->getName();
        errs() << "\n";
        report_fatal_error("unsupported OxCaml recovery trampoline "
                           "predecessor for explicit exception roots");
      }

      AllocaInst *Slot = CreateRootSlotAlloca(SlotPhi);
      StoreInst *InitialStore = EntryBuilder.CreateAlignedStore(
          IsGCRoot
              ? cast<Value>(getOxCamlNonMovingImmediate(SlotPhi->getType()))
              : Constant::getNullValue(SlotPhi->getType()),
          Slot, DL.getABITypeAlign(SlotPhi->getType()));
      if (IsGCRoot && OxCamlVolatileExnRootSlots)
        InitialStore->setVolatile(true);

      for (const OxCamlRecoveryStoreSite &StoreSite : StoreSites) {
        InvokeInst *II = StoreSite.Invoke;
        Value *EdgeValue = nullptr;
        for (unsigned I = 0, E = SlotPhi->getNumIncomingValues(); I != E;
             ++I) {
          if (SlotPhi->getIncomingBlock(I) == StoreSite.PhiIncomingBlock) {
            EdgeValue = SlotPhi->getIncomingValue(I);
            break;
          }
        }
        if (!EdgeValue)
          report_fatal_error("missing OxCaml recovery PHI incoming value");
        if (auto *EdgePhi = dyn_cast<PHINode>(EdgeValue)) {
          if (EdgePhi->getParent() == II->getUnwindDest()) {
            int IncomingIndex = EdgePhi->getBasicBlockIndex(II->getParent());
            if (IncomingIndex < 0)
              report_fatal_error(
                  "missing OxCaml trampoline PHI incoming value");
            EdgeValue = EdgePhi->getIncomingValue(IncomingIndex);
          }
        }
        EdgeValue = nonPoisonTrapRecoverySlotValue(EdgeValue);
        if (auto *EdgeInst = dyn_cast<Instruction>(EdgeValue))
          if (!DT.dominates(EdgeInst, II)) {
            errs() << "explicit exception root value does not dominate "
                      "invoke\n  value: ";
            EdgeValue->print(errs());
            errs() << "\n  invoke: ";
            II->print(errs());
            errs() << "\n";
            report_fatal_error(
                "explicit exception root value does not dominate invoke");
          }

        IRBuilder<> StoreBuilder(II);
        StoreInst *RootStore = StoreBuilder.CreateAlignedStore(
            EdgeValue, Slot, DL.getABITypeAlign(SlotPhi->getType()));
        if (IsGCRoot) {
          if (OxCamlVolatileExnRootSlots)
            RootStore->setVolatile(true);
          Value *RootHomeValue = nonPoisonOxCamlGCPointerValue(EdgeValue);
          if (!isa<Constant>(RootHomeValue))
            ExplicitRootHomes[II].push_back({RootHomeValue, Slot});
        }
      }

      if (IsGCRoot)
        RegisterSlotAtStoreSites(Slot, StoreSites);
      SlotForValue[SlotPhi] = Slot;
      return Slot;
    };

    // Sharing a slot is only sound while the slot still holds the value: the
    // slot's defining store must dominate the aliasing value's definition and
    // must not be able to execute again during its lifetime.  A loop-carried
    // PHI of reloads, for example, holds the previous iteration's value while
    // the re-executed store rebinds the slot to the current one.
    auto CanAliasToSlot = [&](Instruction *AliasDef,
                              AllocaInst *Slot) -> bool {
      StoreInst *DefStore = nullptr;
      for (User *U : Slot->users()) {
        auto *Store = dyn_cast<StoreInst>(U);
        if (!Store || Store->getPointerOperand() != Slot)
          continue;
        if (isOxCamlRootSlotInitStore(Store))
          continue;
        if (DefStore)
          return false;
        DefStore = Store;
      }
      if (!DefStore)
        return true;
      if (!DT.dominates(DefStore, AliasDef))
        return false;
      return !isPotentiallyReachable(AliasDef->getParent(),
                                     DefStore->getParent(), nullptr, &DT);
    };

    // A value that is provably a reload of a single-store root slot, or a
    // PHI merging such reloads, names the same object the slot tracks and
    // can simply share that slot.
    auto ResolveCommonRootSlot = [&](Value *Root) -> AllocaInst * {
      SmallPtrSet<Value *, 8> Visited;
      SmallVector<Value *, 8> Worklist;
      Worklist.push_back(Root);
      AllocaInst *Common = nullptr;
      while (!Worklist.empty()) {
        Value *V = Worklist.pop_back_val();
        if (!Visited.insert(V).second)
          continue;
        if (isa<UndefValue>(V) || isa<PoisonValue>(V))
          continue;

        AllocaInst *Slot = SlotForValue.lookup(V);
        if (!Slot)
          if (auto *LI = dyn_cast<LoadInst>(V))
            if (isOxCamlExceptionRootLoad(LI))
              Slot = dyn_cast<AllocaInst>(LI->getPointerOperand());
        if (Slot) {
          if (!oxcamlRootSlotHasSingleDefiningStore(Slot))
            return nullptr;
          if (Common && Slot != Common)
            return nullptr;
          Common = Slot;
          continue;
        }

        auto *PN = dyn_cast<PHINode>(V);
        if (!PN)
          return nullptr;
        for (Value *Incoming : PN->incoming_values())
          Worklist.push_back(Incoming);
      }
      return Common;
    };

    // Insertion point for the value's defining store right after its
    // definition; used when no later dominating position exists.
    auto DefAdjacentInsertPt = [&](Instruction *RootI) -> Instruction * {
      if (auto *II = dyn_cast<InvokeInst>(RootI)) {
        BasicBlock *NormalDest = II->getNormalDest();
        if (!NormalDest->getUniquePredecessor())
          NormalDest =
              normalizeForInvokeSafepoint(NormalDest, II->getParent(), DT);
        return &*NormalDest->getFirstInsertionPt();
      }
      if (auto *PN = dyn_cast<PHINode>(RootI))
        return &*PN->getParent()->getFirstInsertionPt();
      if (RootI->isTerminator()) {
        RootI->print(errs());
        errs() << "\n";
        report_fatal_error(
            "unsupported terminator definition for OxCaml exception root");
      }
      return RootI->getNextNode();
    };

    // The defining store goes to the latest point that dominates every
    // protected invoke of the regions sharing the slot, so paths that never
    // reach a protected region pay nothing.  It is hoisted out of cycles
    // while the value stays available so loop-invariant values are stored
    // once.  Falls back to the definition itself when the value does not
    // dominate that position.
    auto ComputeDefStorePos = [&](Value *StoredVal,
                                  BasicBlock *Seed) -> Instruction * {
      BasicBlock *Pos = Seed;
      for (const OxCamlRecoveryStoreSite &Site : RegionStoreSites) {
        BasicBlock *SB = Site.Invoke->getParent();
        Pos = Pos ? DT.findNearestCommonDominator(Pos, SB) : SB;
      }
      if (!Pos)
        return nullptr;
      auto InCycle = [&](BasicBlock *Block) {
        for (BasicBlock *Succ : successors(Block))
          if (Succ == Block ||
              isPotentiallyReachable(Succ, Block, nullptr, &DT))
            return true;
        return false;
      };
      while (InCycle(Pos)) {
        auto *Node = DT.getNode(Pos);
        auto *IDom = Node ? Node->getIDom() : nullptr;
        if (!IDom)
          break;
        BasicBlock *Up = IDom->getBlock();
        if (auto *I = dyn_cast<Instruction>(StoredVal))
          if (!DT.dominates(I, Up->getTerminator()))
            break;
        Pos = Up;
      }
      Instruction *Pt = Pos->getTerminator();
      if (auto *I = dyn_cast<Instruction>(StoredVal))
        if (!DT.dominates(I, Pt))
          return nullptr;
      return Pt;
    };

    // A shared slot must keep its store dominating the new region's invokes.
    auto EnsureDefStoreCovers = [&](AllocaInst *Slot) {
      auto It = SlotDefStore.find(Slot);
      if (It == SlotDefStore.end())
        return;
      StoreInst *Store = It->second;
      Instruction *NewPt =
          ComputeDefStorePos(Store->getValueOperand(), Store->getParent());
      if (NewPt && NewPt->getParent() != Store->getParent())
        Store->moveBefore(NewPt);
    };

    // One root slot per handler-live SSA value, with a single defining
    // store.  The collector keeps the slot current at every statepoint the
    // slot is registered on, so no per-invoke re-stores are needed.
    auto GetOrCreateValueRootSlot = [&](Value *RootValue) -> AllocaInst * {
      if (AllocaInst *Slot = SlotForValue.lookup(RootValue)) {
        EnsureDefStoreCovers(Slot);
        return Slot;
      }

      if (auto *RootI = dyn_cast<Instruction>(RootValue);
          RootI && OxCamlRootSlotAliasing)
        if (AllocaInst *Common = ResolveCommonRootSlot(RootValue))
          if (CanAliasToSlot(RootI, Common)) {
            EnsureDefStoreCovers(Common);
            SlotForValue[RootValue] = Common;
            return Common;
          }

      if (auto *PN = dyn_cast<PHINode>(RootValue))
        if (IsUnwindEdgeDefinedPhi(PN))
          return GetOrCreatePhiEdgeSlot(PN);

      bool IsGCRoot = isHandledGCPointerType(RootValue->getType());
      AllocaInst *Slot = CreateRootSlotAlloca(RootValue);

      Instruction *StorePt = ComputeDefStorePos(RootValue, nullptr);
      bool FuseEntryInit =
          StorePt && isa<Argument>(RootValue) &&
          StorePt->getParent() == &F.getEntryBlock();
      if (!StorePt && !isa<Instruction>(RootValue))
        FuseEntryInit = true;

      // The slot must never contain garbage when the collector scans it.  A
      // store fully hoisted to the entry doubles as the initialization.
      Value *InitValue = RootValue;
      if (!FuseEntryInit)
        InitValue =
            IsGCRoot
                ? cast<Value>(
                      getOxCamlNonMovingImmediate(RootValue->getType()))
                : Constant::getNullValue(RootValue->getType());
      StoreInst *InitialStore = EntryBuilder.CreateAlignedStore(
          InitValue, Slot, DL.getABITypeAlign(RootValue->getType()));
      if (IsGCRoot && OxCamlVolatileExnRootSlots)
        InitialStore->setVolatile(true);

      if (!FuseEntryInit) {
        if (!StorePt)
          StorePt = DefAdjacentInsertPt(cast<Instruction>(RootValue));
        StoreInst *DefStore = new StoreInst(
            RootValue, Slot,
            /*isVolatile=*/IsGCRoot && OxCamlVolatileExnRootSlots,
            DL.getABITypeAlign(RootValue->getType()), StorePt);
        SlotDefStore[Slot] = DefStore;
      }

      SlotForValue[RootValue] = Slot;
      return Slot;
    };

    DenseMap<Value *, LoadInst *> RecoveryLoadForValue;
    auto GetOrCreateRecoveryLoad = [&](Value *RootValue) -> LoadInst * {
      if (LoadInst *Load = RecoveryLoadForValue.lookup(RootValue))
        return Load;
      AllocaInst *Slot = GetOrCreateValueRootSlot(RootValue);
      if (isHandledGCPointerType(RootValue->getType()))
        RegisterSlotAtStoreSites(Slot, RegionStoreSites);
      LoadInst *Load = CreateRecoverLoad(RootValue, Slot);
      RecoveryLoadForValue[RootValue] = Load;
      // The load yields the slot's current content, so the slot is also the
      // load's home across later safepoints when the content cannot change
      // under it; no defining store is needed then.
      if (OxCamlRootSlotAliasing && oxcamlRootSlotHasSingleDefiningStore(Slot) &&
          CanAliasToSlot(Load, Slot))
        SlotForValue[Load] = Slot;
      return Load;
    };

    // Values consumed only when control leaves the recovery region are
    // reloaded lazily on the specific boundary edge, so handlers that do not
    // exit that way never pay for the load.
    DenseMap<std::pair<Value *, BasicBlock *>, LoadInst *>
        BoundaryEdgeLoadForValue;
    auto GetOrCreateBoundaryEdgeLoad =
        [&](Value *RootValue, BasicBlock *IncomingBlock) -> LoadInst * {
      if (!OxCamlLazyBoundaryLoads)
        return GetOrCreateRecoveryLoad(RootValue);
      if (LoadInst *Load = RecoveryLoadForValue.lookup(RootValue))
        return Load;
      auto Key = std::make_pair(RootValue, IncomingBlock);
      if (LoadInst *Load = BoundaryEdgeLoadForValue.lookup(Key))
        return Load;
      AllocaInst *Slot = GetOrCreateValueRootSlot(RootValue);
      if (isHandledGCPointerType(RootValue->getType()))
        RegisterSlotAtStoreSites(Slot, RegionStoreSites);
      IRBuilder<> EdgeBuilder(IncomingBlock->getTerminator());
      LoadInst *Load = CreateRecoverLoadAt(RootValue, Slot, EdgeBuilder);
      BoundaryEdgeLoadForValue[Key] = Load;
      if (oxcamlRootSlotHasSingleDefiningStore(Slot))
        SlotForValue[Load] = Slot;
      return Load;
    };

    auto FindExceptionRootValue = [&](Value *V) {
      Value *Base = findBasePointer(V, DVCache, KnownBases);
      if (!Base || Base == V || !isHandledGCPointerType(Base->getType()))
        return V;

      auto *VTy = dyn_cast<PointerType>(V->getType());
      auto *BaseTy = dyn_cast<PointerType>(Base->getType());
      if (!VTy || !BaseTy || VTy->getAddressSpace() != 1 ||
          BaseTy->getAddressSpace() != 1)
        return V;

      if (!isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(V, Base)) {
        if (!isDistinctOxCamlAddrSpace1AddressExpression(V, Base))
          return V;

        V->print(errs());
        errs() << "\n";
        Base->print(errs());
        errs() << "\n";
        report_fatal_error(
            "OxCaml exception root would store a derived GC pointer");
      }

      return Base;
    };

    SmallVector<PHINode *, 8> HandlerLivePhisToErase;
    for (PHINode *PN : HandlerLivePhis) {
      Value *RootValue = FindExceptionRootValue(PN);
      LoadInst *Load = GetOrCreateRecoveryLoad(RootValue);

      PN->replaceAllUsesWith(Load);
      HandlerLivePhisToErase.push_back(PN);
      Changed = true;
      DVCache.clear();
      KnownBases.clear();
    }

    for (Value *V : HandlerLiveGCValues) {
      Value *RootValue = FindExceptionRootValue(V);
      LoadInst *Load = GetOrCreateRecoveryLoad(RootValue);
      for (BasicBlock *RegionBB : RecoveryOnlyRegion) {
        for (Instruction &I : *RegionBB) {
          if (RegionBB == &BB && !Recover->comesBefore(&I))
            continue;
          if (&I == Load)
            continue;
          I.replaceUsesOfWith(V, Load);
        }
      }

      SmallVector<AddrSpaceCastInst *, 4> CastsToRebuild;
      SmallPtrSet<AddrSpaceCastInst *, 4> SeenCastsToRebuild;
      for (BasicBlock *RegionBB : RecoveryOnlyRegion) {
        for (Instruction &I : *RegionBB) {
          if (RegionBB == &BB && !Recover->comesBefore(&I))
            continue;
          for (Use &U : I.operands()) {
            auto *ASC = dyn_cast<AddrSpaceCastInst>(U.get());
            if (!ASC || ASC->getPointerOperand() != V)
              continue;
            if (auto *CastParent = ASC->getParent())
              if (RecoveryOnlyRegion.contains(CastParent) &&
                  (CastParent != &BB || Recover->comesBefore(ASC)))
                continue;
            if (SeenCastsToRebuild.insert(ASC).second)
              CastsToRebuild.push_back(ASC);
          }
        }
      }
      for (AddrSpaceCastInst *ASC : CastsToRebuild) {
        IRBuilder<> CastBuilder(Load->getNextNode());
        Value *RecoveredCast =
            CastBuilder.CreateAddrSpaceCast(Load, ASC->getType(),
                                            suffixed_name_or(
                                                ASC, ".exnroot.cast",
                                                "exnroot.cast"));
        for (Use &U : make_early_inc_range(ASC->uses())) {
          auto *UserI = dyn_cast<Instruction>(U.getUser());
          if (!UserI || !RecoveryOnlyRegion.contains(UserI->getParent()))
            continue;
          if (UserI->getParent() == &BB && !Recover->comesBefore(UserI))
            continue;
          U.set(RecoveredCast);
        }
      }
      Changed = true;
      DVCache.clear();
      KnownBases.clear();
    }

    if (HasStrictRecoveryOnlyRegion) {
      GCPtrLivenessData BoundaryLiveness;
      computeLiveInValues(DT, F, BoundaryLiveness);

      DenseMap<Value *, DenseMap<BasicBlock *, PHINode *>>
          BoundaryRootSelectors;

      auto IsRecoveryBoundaryEdge = [&](BasicBlock *IncomingBlock,
                                        BasicBlock *BoundaryBlock) {
        for (const OxCamlRecoveryBoundaryEdge &BoundaryEdge : BoundaryEdges)
          if (BoundaryEdge.IncomingBlock == IncomingBlock &&
              BoundaryEdge.BoundaryBlock == BoundaryBlock)
            return true;
        return false;
      };

      auto LookupBoundaryRootSelector =
          [&](Value *RootValue, BasicBlock *BoundaryBlock) -> PHINode * {
        auto RootIt = BoundaryRootSelectors.find(RootValue);
        if (RootIt == BoundaryRootSelectors.end())
          return nullptr;
        auto BlockIt = RootIt->second.find(BoundaryBlock);
        if (BlockIt == RootIt->second.end())
          return nullptr;
        return BlockIt->second;
      };

      auto IsAvailableOnEdgeFrom = [&](Value *V, BasicBlock *Pred) {
        auto *I = dyn_cast<Instruction>(V);
        return !I || I == Pred->getTerminator() ||
               DT.dominates(I, Pred->getTerminator());
      };

      auto CountPredecessorEdges = [](BasicBlock *Pred,
                                      BasicBlock *Succ) -> unsigned {
        unsigned Count = 0;
        for (BasicBlock *Candidate : successors(Pred))
          if (Candidate == Succ)
            ++Count;
        return Count;
      };

      auto ForEachPredecessorEdge =
          [&](BasicBlock *Block, function_ref<void(BasicBlock *)> Fn) {
        SmallPtrSet<BasicBlock *, 8> SeenPreds;
        for (BasicBlock *Pred : predecessors(Block)) {
          if (!SeenPreds.insert(Pred).second)
            continue;
          unsigned EdgeCount = CountPredecessorEdges(Pred, Block);
          for (unsigned I = 0; I != EdgeCount; ++I)
            Fn(Pred);
        }
      };

      auto SetBoundarySelectorIncoming =
          [&](PHINode *Selector, BasicBlock *IncomingBlock, Value *Incoming) {
        unsigned ExistingIncoming = 0;
        for (unsigned I = 0, E = Selector->getNumIncomingValues(); I != E;
             ++I) {
          if (Selector->getIncomingBlock(I) != IncomingBlock)
            continue;
          Selector->setIncomingValue(I, Incoming);
          ++ExistingIncoming;
        }
        unsigned EdgeCount =
            CountPredecessorEdges(IncomingBlock, Selector->getParent());
        while (ExistingIncoming < EdgeCount) {
          Selector->addIncoming(Incoming, IncomingBlock);
          ++ExistingIncoming;
        }
      };

      auto BoundarySelectorNonRecoveryIncoming =
          [&](Value *RootValue, BasicBlock *BoundaryBlock,
              BasicBlock *Pred) -> Value * {
        if (auto *RootPhi = dyn_cast<PHINode>(RootValue)) {
          if (RootPhi->getParent() == BoundaryBlock) {
            int IncomingIndex = RootPhi->getBasicBlockIndex(Pred);
            if (IncomingIndex < 0)
              return nullptr;
            Value *Incoming = RootPhi->getIncomingValue(IncomingIndex);
            return IsAvailableOnEdgeFrom(Incoming, Pred) ? Incoming : nullptr;
          }
        }

        return IsAvailableOnEdgeFrom(RootValue, Pred) ? RootValue : nullptr;
      };

      auto ReportInvalidBoundarySelectorIncoming =
          [](Value *RootValue, BasicBlock *Pred, BasicBlock *BoundaryBlock) {
        std::string Message;
        raw_string_ostream OS(Message);
        OS << "cannot construct OxCaml recovery boundary selector for ";
        RootValue->printAsOperand(OS, false);
        OS << " on non-recovery edge " << Pred->getName() << " -> "
           << BoundaryBlock->getName();
        report_fatal_error(Twine(OS.str()));
      };

      auto BoundarySelectorIncoming =
          [&](Value *RootValue, BasicBlock *BoundaryBlock,
              BasicBlock *Pred) -> Value * {
        if (IsRecoveryBoundaryEdge(Pred, BoundaryBlock))
          return nullptr;

        Value *Incoming =
            BoundarySelectorNonRecoveryIncoming(RootValue, BoundaryBlock, Pred);
        if (!Incoming)
          ReportInvalidBoundarySelectorIncoming(RootValue, Pred, BoundaryBlock);
        return Incoming;
      };

      auto GetOrCreateBoundaryRootSelector =
          [&](Value *RootValue, BasicBlock *BoundaryBlock) -> PHINode * {
        if (PHINode *Selector =
                LookupBoundaryRootSelector(RootValue, BoundaryBlock))
          return Selector;

        PHINode *Selector = PHINode::Create(
            RootValue->getType(), pred_size(BoundaryBlock),
            suffixed_name_or(RootValue, ".exnroot.select", "exnroot.select"),
            BoundaryBlock->getFirstNonPHI());
        BoundaryRootSelectors[RootValue][BoundaryBlock] = Selector;

        ForEachPredecessorEdge(BoundaryBlock, [&](BasicBlock *Pred) {
          if (!IsRecoveryBoundaryEdge(Pred, BoundaryBlock)) {
            Selector->addIncoming(
                BoundarySelectorIncoming(RootValue, BoundaryBlock, Pred),
                Pred);
            return;
          }

          LoadInst *Load = GetOrCreateBoundaryEdgeLoad(RootValue, Pred);
          SetBoundarySelectorIncoming(Selector, Pred, Load);
        });

        // Every selector incoming is the root value or its slot reload, so
        // the selector shares the root's slot across later safepoints when
        // the slot's content cannot change under it.
        if (AllocaInst *Slot = SlotForValue.lookup(RootValue))
          if (OxCamlRootSlotAliasing &&
              oxcamlRootSlotHasSingleDefiningStore(Slot) &&
              CanAliasToSlot(Selector, Slot))
            SlotForValue[Selector] = Slot;

        return Selector;
      };

      auto FindPointerChainToBase =
          [&](auto &&Self, SmallVectorImpl<Instruction *> &ChainToBase,
              Value *CurrentValue, Value *Base) -> Value * {
        if (CurrentValue == Base)
          return CurrentValue;

        if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(CurrentValue)) {
          ChainToBase.push_back(GEP);
          return Self(Self, ChainToBase, GEP->getPointerOperand(), Base);
        }

        if (CastInst *CI = dyn_cast<CastInst>(CurrentValue)) {
          auto *IntToPtrDstTy = dyn_cast<PointerType>(CI->getType());
          auto *PtrToIntSrcTy =
              dyn_cast<PointerType>(CI->getOperand(0)->getType());
          bool IsAddrSpace1IntToPtr =
              isa<IntToPtrInst>(CI) && IntToPtrDstTy &&
              IntToPtrDstTy->getAddressSpace() == 1;
          bool IsAddrSpace1PtrToInt =
              isa<PtrToIntInst>(CI) && PtrToIntSrcTy &&
              PtrToIntSrcTy->getAddressSpace() == 1;
          if (!IsAddrSpace1IntToPtr && !IsAddrSpace1PtrToInt &&
              (!CI->getOperand(0)->getType()->isPtrOrPtrVectorTy() ||
               !CI->isNoopCast(CI->getModule()->getDataLayout())))
            return CI;

          ChainToBase.push_back(CI);
          return Self(Self, ChainToBase, CI->getOperand(0), Base);
        }

        if (auto *BO = dyn_cast<BinaryOperator>(CurrentValue)) {
          if (BO->getOpcode() != Instruction::Add &&
              BO->getOpcode() != Instruction::Sub)
            return BO;

          if (isa<ConstantInt>(BO->getOperand(1))) {
            ChainToBase.push_back(BO);
            return Self(Self, ChainToBase, BO->getOperand(0), Base);
          }

          if (BO->getOpcode() == Instruction::Add &&
              isa<ConstantInt>(BO->getOperand(0))) {
            ChainToBase.push_back(BO);
            return Self(Self, ChainToBase, BO->getOperand(1), Base);
          }

          return BO;
        }

        if (FreezeInst *Freeze = dyn_cast<FreezeInst>(CurrentValue)) {
          ChainToBase.push_back(Freeze);
          return Self(Self, ChainToBase, Freeze->getOperand(0), Base);
        }

        return CurrentValue;
      };

      SmallVector<OxCamlRecoveryBoundaryUse, 16> BoundaryLiveUses;
      for (OxCamlRecoveryBoundaryEdge &BoundaryEdge : BoundaryEdges) {
        auto LiveInIt = BoundaryLiveness.LiveIn.find(BoundaryEdge.BoundaryBlock);
        if (LiveInIt == BoundaryLiveness.LiveIn.end())
          continue;
        for (Value *V : LiveInIt->second) {
          if (!isHandledGCPointerType(V->getType()) || isa<Constant>(V) ||
              isOxCamlExceptionRootLoad(V))
            continue;
          if (auto *VI = dyn_cast<Instruction>(V))
            if (RecoveryOnlyRegion.contains(VI->getParent()))
              continue;
          bool Seen = false;
          for (const OxCamlRecoveryBoundaryUse &BoundaryUse : BoundaryUses) {
            if (BoundaryUse.IncomingBlock == BoundaryEdge.IncomingBlock &&
                BoundaryUse.BoundaryBlock == BoundaryEdge.BoundaryBlock &&
                BoundaryUse.Value == V) {
              Seen = true;
              break;
            }
          }
          for (const OxCamlRecoveryBoundaryUse &BoundaryUse :
               BoundaryLiveUses) {
            if (BoundaryUse.IncomingBlock == BoundaryEdge.IncomingBlock &&
                BoundaryUse.BoundaryBlock == BoundaryEdge.BoundaryBlock &&
                BoundaryUse.Value == V) {
              Seen = true;
              break;
            }
          }
          if (!Seen)
            BoundaryLiveUses.push_back({BoundaryEdge.IncomingBlock,
                                        BoundaryEdge.BoundaryBlock, V});
        }
      }

      for (OxCamlRecoveryBoundaryUse &BoundaryUse : BoundaryLiveUses) {
        Value *V = BoundaryUse.Value;
        DVCache.clear();
        KnownBases.clear();
        if (DebugOxCamlDerivedRemat) {
          errs() << "rs4gc-oxcaml-boundary-live "
                 << BoundaryUse.IncomingBlock->getName() << " -> "
                 << BoundaryUse.BoundaryBlock->getName() << "\n  value: ";
          V->print(errs());
          errs() << "\n";
        }
        Value *Base = findBasePointer(V, DVCache, KnownBases);
        if (!Base || !isHandledGCPointerType(Base->getType()))
          report_fatal_error("missing base for OxCaml recovery live value");
        Value *RootValue = Base;
        bool IsBaseEquivalent =
            isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(V, Base);
        if (!IsRootableExceptionValue(RootValue))
          report_fatal_error("unrootable GC base escapes OxCaml recovery path");

        PHINode *Selector = GetOrCreateBoundaryRootSelector(
            RootValue, BoundaryUse.BoundaryBlock);

        Value *Replacement = Selector;
        if (V != RootValue && !IsBaseEquivalent) {
          SmallVector<Instruction *, 3> ChainToBase;
          Value *RootOfChain =
              FindPointerChainToBase(FindPointerChainToBase, ChainToBase, V,
                                     RootValue);
          if (RootOfChain != RootValue || ChainToBase.empty())
            report_fatal_error(
                "unrematerializable derived GC value escapes OxCaml recovery "
                "path");
          Instruction *InsertBefore = Selector->getNextNode();
          Replacement =
              rematerializeChain(ChainToBase, InsertBefore, RootValue, Selector);
        }

        SmallVector<Use *, 8> UsesToReplace;
        for (Use &U : V->uses()) {
          auto *UserI = dyn_cast<Instruction>(U.getUser());
          if (!UserI || UserI == Selector || UserI == Replacement)
            continue;
          if (isa<PHINode>(UserI))
            continue;
          if (DT.dominates(cast<Instruction>(Replacement), UserI))
            UsesToReplace.push_back(&U);
        }
        for (Use *U : UsesToReplace)
          U->set(Replacement);

        Changed = true;
        DVCache.clear();
        KnownBases.clear();
      }

      for (OxCamlRecoveryBoundaryUse &BoundaryUse : BoundaryUses) {
        Value *V = BoundaryUse.Value;
        DVCache.clear();
        KnownBases.clear();

        Value *RootValue = V;
        bool IsBaseEquivalent = false;
        Value *Base = findBasePointer(V, DVCache, KnownBases);
        if (Base && Base != V && isHandledGCPointerType(Base->getType())) {
          auto *VTy = dyn_cast<PointerType>(V->getType());
          auto *BaseTy = dyn_cast<PointerType>(Base->getType());
          if (VTy && BaseTy && VTy->getAddressSpace() == 1 &&
              BaseTy->getAddressSpace() == 1) {
            IsBaseEquivalent =
                isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(V, Base);
            if (IsBaseEquivalent ||
                isDistinctOxCamlAddrSpace1AddressExpression(V, Base))
              RootValue = Base;
          }
        }

        if (!IsRootableExceptionValue(RootValue))
          report_fatal_error("derived GC value escapes OxCaml recovery path");

        PHINode *Selector = GetOrCreateBoundaryRootSelector(
            RootValue, BoundaryUse.BoundaryBlock);

        Value *Replacement = Selector;
        if (V != RootValue && !IsBaseEquivalent) {
          SmallVector<Instruction *, 3> ChainToBase;
          Value *RootOfChain =
              FindPointerChainToBase(FindPointerChainToBase, ChainToBase, V,
                                     RootValue);
          if (RootOfChain != RootValue || ChainToBase.empty())
            report_fatal_error(
                "unrematerializable derived GC value escapes OxCaml recovery "
                "path");
          Instruction *InsertBefore = Selector->getNextNode();
          Replacement =
              rematerializeChain(ChainToBase, InsertBefore, RootValue, Selector);
        }

        SmallVector<Use *, 8> UsesToReplace;
        for (Use &U : V->uses()) {
          auto *UserI = dyn_cast<Instruction>(U.getUser());
          if (!UserI || UserI == Selector || UserI == Replacement)
            continue;
          if (isa<PHINode>(UserI))
            continue;
          if (DT.dominates(cast<Instruction>(Replacement), UserI))
            UsesToReplace.push_back(&U);
        }
        for (Use *U : UsesToReplace)
          U->set(Replacement);

        Changed = true;
        DVCache.clear();
        KnownBases.clear();
      }

      DenseMap<std::pair<PHINode *, BasicBlock *>, Value *>
          BoundaryPhiIncomings;
      for (OxCamlRecoveryBoundaryPhi &BoundaryPhi : BoundaryPhis) {
        Value *Incoming = BoundaryPhi.IncomingValue;
        if (BoundaryPhi.Phi->getIncomingValue(BoundaryPhi.IncomingIndex) !=
            Incoming)
          continue;
        if (auto *IncomingInst = dyn_cast<Instruction>(Incoming))
          if (RecoveryOnlyRegion.contains(IncomingInst->getParent()))
            continue;
        if (!IsRootableExceptionValue(Incoming))
          report_fatal_error("derived GC value escapes OxCaml recovery path");

        auto Key = std::make_pair(BoundaryPhi.Phi, BoundaryPhi.IncomingBlock);
        auto It = BoundaryPhiIncomings.find(Key);
        if (It != BoundaryPhiIncomings.end() && It->second != Incoming)
          report_fatal_error("inconsistent OxCaml recovery PHI incoming "
                             "value for duplicate predecessor");
        BoundaryPhiIncomings[Key] = Incoming;

        LoadInst *Load =
            GetOrCreateBoundaryEdgeLoad(Incoming, BoundaryPhi.IncomingBlock);
        BoundaryPhi.Phi->setIncomingValue(BoundaryPhi.IncomingIndex, Load);
        Changed = true;
        DVCache.clear();
        KnownBases.clear();
      }

      SmallVector<PHINode *, 8> PotentiallyDeadBoundaryPhis;
      for (auto &RootSelectors : BoundaryRootSelectors)
        for (auto &BlockSelector : RootSelectors.second)
          PotentiallyDeadBoundaryPhis.push_back(BlockSelector.second);
      for (const OxCamlRecoveryBoundaryPhi &BoundaryPhi : BoundaryPhis)
        PotentiallyDeadBoundaryPhis.push_back(BoundaryPhi.Phi);

      SmallPtrSet<PHINode *, 8> ErasedBoundaryPhis;
      bool ErasedDeadBoundaryValue = true;
      while (ErasedDeadBoundaryValue) {
        ErasedDeadBoundaryValue = false;
        for (PHINode *PN : PotentiallyDeadBoundaryPhis) {
          if (ErasedBoundaryPhis.contains(PN) || !PN->use_empty())
            continue;
          ErasedBoundaryPhis.insert(PN);
          PN->eraseFromParent();
          Changed = true;
          ErasedDeadBoundaryValue = true;
        }

        SmallVector<Value *, 8> DeadLoadKeys;
        for (auto &Entry : RecoveryLoadForValue) {
          LoadInst *Load = Entry.second;
          if (Load->use_empty() && isOxCamlExceptionRootLoad(Load))
            DeadLoadKeys.push_back(Entry.first);
        }
        for (Value *Key : DeadLoadKeys) {
          RecoveryLoadForValue.lookup(Key)->eraseFromParent();
          RecoveryLoadForValue.erase(Key);
          Changed = true;
          ErasedDeadBoundaryValue = true;
        }

        SmallVector<std::pair<Value *, BasicBlock *>, 8> DeadEdgeLoadKeys;
        for (auto &Entry : BoundaryEdgeLoadForValue) {
          LoadInst *Load = Entry.second;
          if (Load->use_empty() && isOxCamlExceptionRootLoad(Load))
            DeadEdgeLoadKeys.push_back(Entry.first);
        }
        for (auto &Key : DeadEdgeLoadKeys) {
          BoundaryEdgeLoadForValue.lookup(Key)->eraseFromParent();
          BoundaryEdgeLoadForValue.erase(Key);
          Changed = true;
          ErasedDeadBoundaryValue = true;
        }
      }
    }

    for (PHINode *PN : HandlerLivePhisToErase)
      if (PN->use_empty())
        PN->eraseFromParent();
  }

  if (Changed) {
    DVCache.clear();
    KnownBases.clear();
    DT.recalculate(F);
  }
  return Changed;
}

static bool appendExplicitRootSlotsToStatepoints(
    const ExplicitRootSlotMapTy &ExplicitRootSlots) {
  bool Changed = false;
  for (const auto &Pair : ExplicitRootSlots) {
    CallBase *Call = Pair.first;
    ArrayRef<Value *> Slots = Pair.second;
    if (Slots.empty())
      continue;

    SmallVector<OperandBundleDef, 4> Bundles;
    Call->getOperandBundlesAsDefs(Bundles);

    bool FoundGCLive = false;
    bool ChangedThisCall = false;
    for (OperandBundleDef &Bundle : Bundles) {
      if (Bundle.getTag() != "gc-live")
        continue;

      FoundGCLive = true;
      SmallVector<Value *, 64> Inputs(Bundle.input_begin(),
                                      Bundle.input_end());
      SmallPtrSet<Value *, 16> Seen(Inputs.begin(), Inputs.end());
      for (Value *Slot : Slots) {
        if (!Seen.insert(Slot).second)
          continue;
        Inputs.push_back(Slot);
        ChangedThisCall = true;
      }
      if (ChangedThisCall)
        Bundle = OperandBundleDef("gc-live", Inputs);
      break;
    }

    if (!FoundGCLive) {
      Bundles.emplace_back("gc-live", Slots);
      ChangedThisCall = true;
    }

    if (!ChangedThisCall)
      continue;

    CallBase *NewCall = CallBase::Create(Call, Bundles, Call);
    Call->replaceAllUsesWith(NewCall);
    NewCall->takeName(Call);
    Call->eraseFromParent();
    Changed = true;
  }
  return Changed;
}

static void collectExplicitRootSlots(const ExplicitRootSlotMapTy &RootSlots,
                                     SmallVectorImpl<Value *> &Slots) {
  SmallPtrSet<Value *, 16> Seen;
  for (const auto &Pair : RootSlots)
    for (Value *Slot : Pair.second)
      if (Seen.insert(Slot).second)
        Slots.push_back(Slot);
}

static bool hasOxCamlNormalContinuationUse(Value *RootValue,
                                           Instruction *Statepoint,
                                           DominatorTree &DT) {
  auto *Invoke = dyn_cast<InvokeInst>(Statepoint);
  if (!Invoke)
    return true;

  BasicBlock *NormalDest = Invoke->getNormalDest();
  for (User *U : RootValue->users()) {
    auto *UseInst = dyn_cast<Instruction>(U);
    if (!UseInst || UseInst == Statepoint)
      continue;

    if (auto *Phi = dyn_cast<PHINode>(UseInst)) {
      for (unsigned I = 0, E = Phi->getNumIncomingValues(); I != E; ++I) {
        if (Phi->getIncomingValue(I) != RootValue)
          continue;
        if (isPotentiallyReachable(NormalDest, Phi->getIncomingBlock(I),
                                   nullptr, &DT))
          return true;
      }
      continue;
    }

    if (isPotentiallyReachable(NormalDest, UseInst->getParent(), nullptr, &DT))
      return true;
  }

  return false;
}

/// Route every live handler-rooted value through its explicit root slot
/// instead of gc.relocate, at every statepoint it crosses.  The value is
/// removed from the live set (no relocates anywhere), the slot is listed in
/// the statepoint's gc-live bundle, and a post-statepoint volatile reload
/// feeds the normal relocationViaAlloca SSA repair when the value is still
/// needed on the normal continuation.
static bool assignExplicitRootHomesAndFilterLiveSets(
    MutableArrayRef<PartiallyConstructedSafepointRecord> Records,
    ArrayRef<CallBase *> ToUpdate,
    const OxCamlValueRootSlotMapTy &SlotForValue,
    const PointerToBaseTy &PointerToBase, DominatorTree &DT) {
  bool Changed = false;
  for (size_t I = 0; I < Records.size(); ++I) {
    PartiallyConstructedSafepointRecord &Info = Records[I];
    CallBase *Call = ToUpdate[I];

    // A base that some other live derived pointer relocates against must stay
    // in the live set; relocation needs the base relocated alongside.
    SmallPtrSet<Value *, 8> RequiredBases;
    for (Value *LiveV : Info.LiveSet) {
      auto BaseIt = PointerToBase.find(LiveV);
      if (BaseIt != PointerToBase.end() && BaseIt->second != LiveV)
        RequiredBases.insert(BaseIt->second);
    }

    SmallVector<std::pair<WeakTrackingVH, AllocaInst *>, 4> Homes;
    SmallPtrSet<Value *, 8> SeenRootValues;
    SmallPtrSet<Value *, 8> SeenSlots(Info.ExplicitRootSlots.begin(),
                                      Info.ExplicitRootSlots.end());

    auto TryHome = [&](Value *RootValue, AllocaInst *Slot) {
      if (!RootValue || !Slot || isa<Constant>(RootValue))
        return;
      if (!isHandledGCPointerType(RootValue->getType()))
        return;
      if (RequiredBases.contains(RootValue))
        return;
      if (!Info.LiveSet.remove(RootValue))
        return;
      Changed = true;

      if (SeenSlots.insert(Slot).second)
        Info.ExplicitRootSlots.push_back(Slot);
      if (hasOxCamlNormalContinuationUse(RootValue, Call, DT) &&
          SeenRootValues.insert(RootValue).second)
        Homes.push_back({RootValue, Slot});
    };

    // Per-edge PHI slot homes recorded at materialization time are only
    // meaningful at their own invoke.
    for (auto [RootValue, Slot] : Info.ExplicitRootHomes)
      TryHome(RootValue, Slot);

    // Function-wide value slots cover every statepoint the value crosses
    // once their defining store has executed; a statepoint not dominated by
    // the store keeps plain relocation.  Multi-store phi-edge slots only
    // match their value at their own invoke, which the per-record homes
    // above already cover.
    auto SlotDefStoreDominates = [&](AllocaInst *Slot) {
      for (User *U : Slot->users()) {
        auto *Store = dyn_cast<StoreInst>(U);
        if (!Store || Store->getPointerOperand() != Slot)
          continue;
        if (isOxCamlRootSlotInitStore(Store))
          continue;
        return DT.dominates(Store, Call);
      }
      return true;
    };
    unsigned HomeMask = isa<InvokeInst>(Call) ? 1u : 2u;
    bool BudgetOk = isa<InvokeInst>(Call) ||
                    (OxCamlCallHomesUsed >= OxCamlCallHomeSkip &&
                     OxCamlCallHomesUsed < OxCamlCallHomeBudget);
    if (!isa<InvokeInst>(Call) && OxCamlCallHomesUsed < OxCamlCallHomeSkip)
      ++OxCamlCallHomesUsed;
    bool DumpThis = !isa<InvokeInst>(Call) &&
                    OxCamlCallHomesUsed == OxCamlCallHomeDump;
    if ((OxCamlValueSlotHomes & HomeMask) && BudgetOk) {
      if (!isa<InvokeInst>(Call))
        ++OxCamlCallHomesUsed;
      if (DumpThis) {
        errs() << "CALL-HOME[" << (OxCamlCallHomesUsed - 1) << "] in "
               << Call->getFunction()->getName() << "\n  call: " << *Call
               << "\n  live:";
        for (Value *LV : Info.LiveSet)
          errs() << " " << LV->getName();
        errs() << "\n";
      }
      SmallVector<Value *, 8> LiveValues(Info.LiveSet.begin(),
                                         Info.LiveSet.end());
      for (Value *LiveV : LiveValues)
        if (AllocaInst *Slot = SlotForValue.lookup(LiveV))
          if (oxcamlRootSlotHasSingleDefiningStore(Slot) &&
              SlotDefStoreDominates(Slot))
            TryHome(LiveV, Slot);
    }

    Info.ExplicitRootHomes = std::move(Homes);
  }

  return Changed;
}

static bool rewriteOxCamlExplicitRootStoresToBases(
    ArrayRef<Value *> Slots, const PointerToBaseTy &PointerToBase,
    DominatorTree &DT) {
  bool Changed = false;
  for (Value *Slot : Slots) {
    for (User *U : make_early_inc_range(Slot->users())) {
      auto *Store = dyn_cast<StoreInst>(U);
      if (!Store || Store->getPointerOperand() != Slot)
        continue;

      Value *Stored =
          nonPoisonOxCamlGCPointerValue(Store->getValueOperand());
      if (!isHandledGCPointerType(Stored->getType()) || isa<Constant>(Stored))
        continue;

      auto BaseIt = PointerToBase.find(Stored);
      if (BaseIt == PointerToBase.end())
        continue;

      Value *Base = BaseIt->second;
      if (Base == Stored)
        continue;

      auto *StoredTy = dyn_cast<PointerType>(Stored->getType());
      auto *BaseTy = dyn_cast<PointerType>(Base->getType());
      if (!StoredTy || !BaseTy || StoredTy->getAddressSpace() != 1 ||
          BaseTy->getAddressSpace() != 1)
        continue;

      if (!isOxCamlBaseRelationThroughExceptionRoots(Stored, Base)) {
        if (!isDistinctOxCamlAddrSpace1AddressExpression(Stored, Base))
          continue;

        Stored->print(errs());
        errs() << "\n";
        Base->print(errs());
        errs() << "\n";
        report_fatal_error(
            "OxCaml explicit root store would store a derived GC pointer");
      }

      if (auto *BaseI = dyn_cast<Instruction>(Base))
        if (!DT.dominates(BaseI, Store))
          report_fatal_error(
              "OxCaml explicit root base does not dominate root store");

      Store->setOperand(0, Base);
      Changed = true;
    }
  }
  return Changed;
}

static bool appendExplicitRootSlotsToAllStatepoints(
    Function &F, ArrayRef<Value *> Slots) {
  if (Slots.empty())
    return false;

  ExplicitRootSlotMapTy RootSlotsForStatepoints;
  for (Instruction &I : instructions(F)) {
    auto *Call = dyn_cast<CallBase>(&I);
    if (!Call || !isa<GCStatepointInst>(Call))
      continue;
    llvm::append_range(RootSlotsForStatepoints[Call], Slots);
  }

  return appendExplicitRootSlotsToStatepoints(RootSlotsForStatepoints);
}

static bool materializeRemainingOxCamlRecoveryPhis(
    Function &F, DominatorTree &DT, SmallVectorImpl<Value *> &RootSlots) {
  if (!isOxCamlFunction(F))
    return false;

  const DataLayout &DL = F.getParent()->getDataLayout();
  BasicBlock &EntryBlock = F.getEntryBlock();
  IRBuilder<> EntryBuilder(&EntryBlock, EntryBlock.getFirstInsertionPt());
  bool Changed = false;

  auto MaterializeRecoveryPhi =
      [&](PHINode *PN, IntrinsicInst *Recover) -> bool {
    BasicBlock *PhiBlock = PN->getParent();
    if (!PN->getType()->isSized()) {
      PN->print(errs());
      errs() << "\n";
      report_fatal_error("cannot materialize unsized OxCaml recovery PHI");
    }

    AllocaInst *Slot = EntryBuilder.CreateAlloca(
        PN->getType(), DL.getAllocaAddrSpace(), nullptr,
        suffixed_name_or(PN, ".recoverphi", "recoverphi"));
    Slot->setAlignment(DL.getABITypeAlign(PN->getType()));
    bool IsGCRoot = isHandledGCPointerType(PN->getType());
    EntryBuilder.CreateAlignedStore(
        IsGCRoot ? getOxCamlNonMovingImmediate(PN->getType())
                 : Constant::getNullValue(PN->getType()),
        Slot, DL.getABITypeAlign(PN->getType()));
    if (IsGCRoot)
      RootSlots.push_back(Slot);

    auto StoreForInvokeEdge = [&](InvokeInst *II, Value *IncomingValue) {
      if (auto *IncomingPhi = dyn_cast<PHINode>(IncomingValue)) {
        if (IncomingPhi->getParent() == II->getUnwindDest()) {
          int IncomingIndex =
              IncomingPhi->getBasicBlockIndex(II->getParent());
          if (IncomingIndex < 0)
            report_fatal_error(
                "missing OxCaml recovery PHI incoming value");
          IncomingValue = IncomingPhi->getIncomingValue(IncomingIndex);
        }
      }

      IncomingValue = nonPoisonTrapRecoverySlotValue(IncomingValue);
      if (auto *IncomingInst = dyn_cast<Instruction>(IncomingValue))
        if (!DT.dominates(IncomingInst, II))
          report_fatal_error(
              "recovery PHI incoming value does not dominate edge store");
      IRBuilder<> StoreBuilder(II);
      StoreBuilder.CreateAlignedStore(
          IncomingValue, Slot, DL.getABITypeAlign(PN->getType()));
    };

    for (unsigned I = 0, E = PN->getNumIncomingValues(); I != E; ++I) {
      BasicBlock *IncomingBlock = PN->getIncomingBlock(I);
      Value *IncomingValue = PN->getIncomingValue(I);
      Instruction *InsertBefore = IncomingBlock->getTerminator();
      if (auto *II = dyn_cast<InvokeInst>(InsertBefore)) {
        if (II->getUnwindDest() != PhiBlock)
          report_fatal_error(
              "unsupported invoke predecessor for recovery PHI");
        StoreForInvokeEdge(II, IncomingValue);
        continue;
      }

      SmallVector<OxCamlRecoveryStoreSite, 8> StoreSites;
      if (collectOxCamlRecoveryStoreSites(*PhiBlock, IncomingBlock,
                                          StoreSites)) {
        for (OxCamlRecoveryStoreSite StoreSite : StoreSites)
          StoreForInvokeEdge(StoreSite.Invoke, IncomingValue);
        continue;
      }

      auto *BI = dyn_cast<BranchInst>(InsertBefore);
      if (!BI || !BI->isUnconditional() || BI->getSuccessor(0) != PhiBlock)
        report_fatal_error("unsupported branch predecessor for recovery PHI");

      IncomingValue = nonPoisonTrapRecoverySlotValue(IncomingValue);
      if (auto *IncomingInst = dyn_cast<Instruction>(IncomingValue))
        if (!DT.dominates(IncomingInst, InsertBefore))
          report_fatal_error(
              "recovery PHI incoming value does not dominate edge store");
      IRBuilder<> StoreBuilder(InsertBefore);
      StoreBuilder.CreateAlignedStore(
          IncomingValue, Slot, DL.getABITypeAlign(PN->getType()));
    }

    IRBuilder<> LoadBuilder(Recover->getNextNode());
    LoadInst *Load = LoadBuilder.CreateAlignedLoad(
        PN->getType(), Slot, DL.getABITypeAlign(PN->getType()),
        suffixed_name_or(PN, ".recoverphi.load", "recoverphi.load"));
    PN->replaceAllUsesWith(Load);
    PN->eraseFromParent();
    return true;
  };

  for (BasicBlock &BB : F) {
    IntrinsicInst *Recover = getOxCamlTrapRecover(BB);
    if (!isOxCamlRecoveryBlockStart(BB, Recover))
      continue;

    SmallVector<PHINode *, 8> DirectRecoveryPhis;
    for (PHINode &PN : BB.phis())
      DirectRecoveryPhis.push_back(&PN);
    for (PHINode *PN : DirectRecoveryPhis)
      Changed |= MaterializeRecoveryPhi(PN, Recover);

    SmallVector<PHINode *, 8> TrampolinePhis;
    for (BasicBlock *Pred : predecessors(&BB)) {
      if (!isBranchOnlyOxCamlRecoveryTrampoline(Pred, &BB,
                                                /*AllowPHIs=*/true))
        continue;
      for (PHINode &PN : Pred->phis())
        TrampolinePhis.push_back(&PN);
    }
    for (PHINode *PN : TrampolinePhis)
      Changed |= MaterializeRecoveryPhi(PN, Recover);
  }

  if (Changed)
    DT.recalculate(F);
  return Changed;
}

// List of all function attributes which must be stripped when lowering from
// abstract machine model to physical machine model.  Essentially, these are
// all the effects a safepoint might have which we ignored in the abstract
// machine model for purposes of optimization.  We have to strip these on
// both function declarations and call sites.
static constexpr Attribute::AttrKind FnAttrsToStrip[] =
  {Attribute::Memory, Attribute::NoSync, Attribute::NoFree};

// Create new attribute set containing only attributes which can be transferred
// from original call to the safepoint.
static AttributeList legalizeCallAttributes(LLVMContext &Ctx,
                                            AttributeList OrigAL,
                                            AttributeList StatepointAL) {
  if (OrigAL.isEmpty())
    return StatepointAL;

  // Remove the readonly, readnone, and statepoint function attributes.
  AttrBuilder FnAttrs(Ctx, OrigAL.getFnAttrs());
  for (auto Attr : FnAttrsToStrip)
    FnAttrs.removeAttribute(Attr);

  for (Attribute A : OrigAL.getFnAttrs()) {
    if (isStatepointDirectiveAttr(A))
      FnAttrs.removeAttribute(A);
  }

  // Just skip parameter and return attributes for now
  return StatepointAL.addFnAttributes(Ctx, FnAttrs);
}

/// Helper function to place all gc relocates necessary for the given
/// statepoint.
/// Inputs:
///   liveVariables - list of variables to be relocated.
///   basePtrs - base pointers.
///   statepointToken - statepoint instruction to which relocates should be
///   bound.
///   Builder - Llvm IR builder to be used to construct new calls.
static void CreateGCRelocates(ArrayRef<Value *> LiveVariables,
                              ArrayRef<Value *> BasePtrs,
                              Instruction *StatepointToken,
                              IRBuilder<> &Builder) {
  if (LiveVariables.empty())
    return;

  auto FindIndex = [](ArrayRef<Value *> LiveVec, Value *Val) {
    auto ValIt = llvm::find(LiveVec, Val);
    assert(ValIt != LiveVec.end() && "Val not found in LiveVec!");
    size_t Index = std::distance(LiveVec.begin(), ValIt);
    assert(Index < LiveVec.size() && "Bug in std::find?");
    return Index;
  };
  Module *M = StatepointToken->getModule();

  // All gc_relocate are generated as i8 addrspace(1)* (or a vector type whose
  // element type is i8 addrspace(1)*). We originally generated unique
  // declarations for each pointer type, but this proved problematic because
  // the intrinsic mangling code is incomplete and fragile.  Since we're moving
  // towards a single unified pointer type anyways, we can just cast everything
  // to an i8* of the right address space.  A bitcast is added later to convert
  // gc_relocate to the actual value's type.
  auto getGCRelocateDecl = [&] (Type *Ty) {
    assert(isHandledGCPointerType(Ty));
    auto AS = Ty->getScalarType()->getPointerAddressSpace();
    Type *NewTy = Type::getInt8PtrTy(M->getContext(), AS);
    if (auto *VT = dyn_cast<VectorType>(Ty))
      NewTy = FixedVectorType::get(NewTy,
                                   cast<FixedVectorType>(VT)->getNumElements());
    return Intrinsic::getDeclaration(M, Intrinsic::experimental_gc_relocate,
                                     {NewTy});
  };

  // Lazily populated map from input types to the canonicalized form mentioned
  // in the comment above.  This should probably be cached somewhere more
  // broadly.
  DenseMap<Type *, Function *> TypeToDeclMap;

  for (unsigned i = 0; i < LiveVariables.size(); i++) {
    // Generate the gc.relocate call and save the result
    Value *BaseIdx = Builder.getInt32(FindIndex(LiveVariables, BasePtrs[i]));
    Value *LiveIdx = Builder.getInt32(i);

    Type *Ty = LiveVariables[i]->getType();
    if (!TypeToDeclMap.count(Ty))
      TypeToDeclMap[Ty] = getGCRelocateDecl(Ty);
    Function *GCRelocateDecl = TypeToDeclMap[Ty];

    // only specify a debug name if we can give a useful one
    CallInst *Reloc = Builder.CreateCall(
        GCRelocateDecl, {StatepointToken, BaseIdx, LiveIdx},
        suffixed_name_or(LiveVariables[i], ".relocated", ""));
    // Trick CodeGen into thinking there are lots of free registers at this
    // fake call.
    Reloc->setCallingConv(CallingConv::Cold);
  }
}

static bool isLiveValueSupportedByOxCamlVolatileRootAllocas(
    Value *LiveValue, const PointerToBaseTy &PointerToBase) {
  auto It = PointerToBase.find(LiveValue);
  if (It == PointerToBase.end())
    return false;

  Value *Base = It->second;
  if (Base == LiveValue)
    return true;

  auto *LiveTy = dyn_cast<PointerType>(LiveValue->getType());
  auto *BaseTy = dyn_cast<PointerType>(Base->getType());
  if (!LiveTy || !BaseTy || LiveTy->getAddressSpace() != 1 ||
      BaseTy->getAddressSpace() != 1)
    return true;

  return isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(LiveValue, Base);
}

namespace {

/// This struct is used to defer RAUWs and `eraseFromParent` s.  Using this
/// avoids having to worry about keeping around dangling pointers to Values.
class DeferredReplacement {
  AssertingVH<Instruction> Old;
  AssertingVH<Instruction> New;
  bool IsDeoptimize = false;

  DeferredReplacement() = default;

public:
  static DeferredReplacement createRAUW(Instruction *Old, Instruction *New) {
    assert(Old != New && Old && New &&
           "Cannot RAUW equal values or to / from null!");

    DeferredReplacement D;
    D.Old = Old;
    D.New = New;
    return D;
  }

  static DeferredReplacement createDelete(Instruction *ToErase) {
    DeferredReplacement D;
    D.Old = ToErase;
    return D;
  }

  static DeferredReplacement createDeoptimizeReplacement(Instruction *Old) {
#ifndef NDEBUG
    auto *F = cast<CallInst>(Old)->getCalledFunction();
    assert(F && F->getIntrinsicID() == Intrinsic::experimental_deoptimize &&
           "Only way to construct a deoptimize deferred replacement");
#endif
    DeferredReplacement D;
    D.Old = Old;
    D.IsDeoptimize = true;
    return D;
  }

  /// Does the task represented by this instance.
  void doReplacement() {
    Instruction *OldI = Old;
    Instruction *NewI = New;

    assert(OldI != NewI && "Disallowed at construction?!");
    assert((!IsDeoptimize || !New) &&
           "Deoptimize intrinsics are not replaced!");

    Old = nullptr;
    New = nullptr;

    if (NewI)
      OldI->replaceAllUsesWith(NewI);

    if (IsDeoptimize) {
      // Note: we've inserted instructions, so the call to llvm.deoptimize may
      // not necessarily be followed by the matching return.
      auto *RI = cast<ReturnInst>(OldI->getParent()->getTerminator());
      new UnreachableInst(RI->getContext(), RI);
      RI->eraseFromParent();
    }

    OldI->eraseFromParent();
  }
};

} // end anonymous namespace

static StringRef getDeoptLowering(CallBase *Call) {
  const char *DeoptLowering = "deopt-lowering";
  if (Call->hasFnAttr(DeoptLowering)) {
    // FIXME: Calls have a *really* confusing interface around attributes
    // with values.
    const AttributeList &CSAS = Call->getAttributes();
    if (CSAS.hasFnAttr(DeoptLowering))
      return CSAS.getFnAttr(DeoptLowering).getValueAsString();
    Function *F = Call->getCalledFunction();
    assert(F && F->hasFnAttribute(DeoptLowering));
    return F->getFnAttribute(DeoptLowering).getValueAsString();
  }
  return "live-through";
}

static void
makeStatepointExplicitImpl(CallBase *Call, /* to replace */
                           const SmallVectorImpl<Value *> &BasePtrs,
                           const SmallVectorImpl<Value *> &LiveVariables,
                           PartiallyConstructedSafepointRecord &Result,
                           std::vector<DeferredReplacement> &Replacements,
                           const PointerToBaseTy &PointerToBase,
                           DominatorTree &DT) {
  assert(BasePtrs.size() == LiveVariables.size());

  // Then go ahead and use the builder do actually do the inserts.  We insert
  // immediately before the previous instruction under the assumption that all
  // arguments will be available here.  We can't insert afterwards since we may
  // be replacing a terminator.
  IRBuilder<> Builder(Call);

  DenseSet<Value *> LiveVariableSet;
  for (Value *V : LiveVariables)
    LiveVariableSet.insert(V);

  DenseSet<Value *> RematerializedDerivedValues;
  RematCandTy RematerializedDerivedChains;
  const bool IsOxCamlStatepoint = isOxCamlCallingConv(Call->getCallingConv());
  auto PrintOxCamlDerivedCandidate = [&](Value *Derived, Value *Base) {
    if (!DebugOxCamlDerivedRemat || !IsOxCamlStatepoint)
      return;
    errs() << "rs4gc-oxcaml-live in "
           << Call->getFunction()->getName() << "\n  call: ";
    Call->print(errs());
    errs() << "\n  derived: ";
    Derived->print(errs());
    errs() << "\n  base: ";
    Base->print(errs());
    errs() << "\n  derived-kind: ";
    if (auto *I = dyn_cast<Instruction>(Derived))
      errs() << I->getOpcodeName();
    else
      errs() << "non-instruction";
    errs() << "\n  base-kind: ";
    if (auto *I = dyn_cast<Instruction>(Base))
      errs() << I->getOpcodeName();
    else
      errs() << "non-instruction";
    errs() << "\n";
  };
  auto ShouldRematerializeDerivedFromBase = [&](Value *Derived) {
    auto Reject = [&](const char *Reason) {
      if (DebugOxCamlDerivedRemat && IsOxCamlStatepoint)
        errs() << "rs4gc-oxcaml-remat-reject " << Reason << "\n";
      return false;
    };
    if (!RematAddrSpace1DerivedFromBaseAtAlloc)
      return Reject("flag");
    if (!IsOxCamlStatepoint)
      return Reject("cc");
    auto *Invoke = dyn_cast<InvokeInst>(Call);
    if (Invoke && !Result.UsesExplicitExceptionRoots)
      return Reject("invoke-without-explicit-roots");
    auto BaseIt = PointerToBase.find(Derived);
    if (BaseIt == PointerToBase.end())
      return Reject("no-base");
    Value *Base = BaseIt->second;
    if (Base == Derived || !LiveVariableSet.contains(Base))
      return Reject("self-or-base-not-live");
    auto BaseBaseIt = PointerToBase.find(Base);
    if (BaseBaseIt == PointerToBase.end() || BaseBaseIt->second != Base)
      return Reject("base-not-self");
    auto *DerivedTy = dyn_cast<PointerType>(Derived->getType());
    auto *BaseTy = dyn_cast<PointerType>(Base->getType());
    if (!DerivedTy || !BaseTy)
      return Reject("not-pointers");
    if (DerivedTy->getAddressSpace() != 1 || BaseTy->getAddressSpace() != 1)
      return Reject("not-as1");
    if (isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(Derived, Base))
      return Reject("base-equivalent");
    if (!isDistinctOxCamlAddrSpace1AddressExpression(Derived, Base))
      return Reject("not-address-expression");

    auto *DerivedInst = dyn_cast<Instruction>(Derived);
    if (!DerivedInst)
      return Reject("not-instruction");

    RematerizlizationCandidateRecord Record;
    auto FindPointerChainToBase =
        [&](auto &&Self, SmallVectorImpl<Instruction *> &ChainToBase,
            Value *CurrentValue) -> Value * {
      if (CurrentValue == Base)
        return CurrentValue;

      if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(CurrentValue)) {
        ChainToBase.push_back(GEP);
        return Self(Self, ChainToBase, GEP->getPointerOperand());
      }

      if (CastInst *CI = dyn_cast<CastInst>(CurrentValue)) {
        auto *IntToPtrDstTy = dyn_cast<PointerType>(CI->getType());
        auto *PtrToIntSrcTy =
            dyn_cast<PointerType>(CI->getOperand(0)->getType());
        bool IsAddrSpace1IntToPtr =
            isa<IntToPtrInst>(CI) && IntToPtrDstTy &&
            IntToPtrDstTy->getAddressSpace() == 1;
        bool IsAddrSpace1PtrToInt =
            isa<PtrToIntInst>(CI) && PtrToIntSrcTy &&
            PtrToIntSrcTy->getAddressSpace() == 1;
        if (!IsAddrSpace1IntToPtr && !IsAddrSpace1PtrToInt &&
            (!CI->getOperand(0)->getType()->isPtrOrPtrVectorTy() ||
             !CI->isNoopCast(CI->getModule()->getDataLayout())))
          return CI;

        ChainToBase.push_back(CI);
        return Self(Self, ChainToBase, CI->getOperand(0));
      }

      if (auto *BO = dyn_cast<BinaryOperator>(CurrentValue)) {
        if (BO->getOpcode() != Instruction::Add &&
            BO->getOpcode() != Instruction::Sub)
          return BO;

        if (isa<ConstantInt>(BO->getOperand(1))) {
          ChainToBase.push_back(BO);
          return Self(Self, ChainToBase, BO->getOperand(0));
        }

        if (BO->getOpcode() == Instruction::Add &&
            isa<ConstantInt>(BO->getOperand(0))) {
          ChainToBase.push_back(BO);
          return Self(Self, ChainToBase, BO->getOperand(1));
        }

        return BO;
      }

      if (FreezeInst *Freeze = dyn_cast<FreezeInst>(CurrentValue)) {
        ChainToBase.push_back(Freeze);
        return Self(Self, ChainToBase, Freeze->getOperand(0));
      }

      return CurrentValue;
    };
    Value *RootOfChain =
        FindPointerChainToBase(FindPointerChainToBase, Record.ChainToBase,
                               Derived);
    bool IsPhiChainToBasePhi = false;
    bool IsPhiChainToSingleBase = false;
    bool IsBaseEquivalent =
        Record.ChainToBase.empty() &&
        isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(Derived, Base);
    if ((RootOfChain != Base || Record.ChainToBase.empty()) &&
        !IsBaseEquivalent && isa<PHINode>(Derived) && isa<PHINode>(Base)) {
      Record.ChainToBase.clear();
      bool NeedsBaseSelect = false;
      if (Value *PhiRoot = findRematerializablePhiChainToBasePhi(
              Record.ChainToBase, cast<PHINode>(Derived),
              cast<PHINode>(Base), PointerToBase,
              /*AllowIntegerAddressArithmetic=*/true, NeedsBaseSelect)) {
        RootOfChain = PhiRoot;
        IsPhiChainToBasePhi = true;
        if (NeedsBaseSelect) {
          IRBuilder<> ConditionBuilder(Call);
          Record.UseRematerializedValue =
              ConditionBuilder.CreateICmpNE(Derived, Base,
                                            suffixed_name_or(Derived,
                                                            ".needs_remat",
                                                            "needs_remat"));
        }
      }
    }
    if (!IsPhiChainToBasePhi &&
        (RootOfChain != Base || Record.ChainToBase.empty()) &&
        !IsBaseEquivalent && isa<PHINode>(Derived)) {
      Record.ChainToBase.clear();
      bool NeedsBaseSelect = false;
      if (Value *PhiRoot = findRematerializablePhiChainToSingleBase(
              Record.ChainToBase, cast<PHINode>(Derived), Base, PointerToBase,
              /*AllowIntegerAddressArithmetic=*/true, NeedsBaseSelect)) {
        RootOfChain = PhiRoot;
        IsPhiChainToSingleBase = true;
        if (NeedsBaseSelect) {
          IRBuilder<> ConditionBuilder(Call);
          Record.UseRematerializedValue =
              ConditionBuilder.CreateICmpNE(Derived, Base,
                                            suffixed_name_or(Derived,
                                                            ".needs_remat",
                                                            "needs_remat"));
        }
      }
    }
    bool IsRootBaseEquivalent =
        !Record.ChainToBase.empty() && RootOfChain != Base &&
        isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(RootOfChain, Base);
    if (IsBaseEquivalent)
      RootOfChain = Base;

    if (Invoke && !IsBaseEquivalent && !isNormalOnlyAfterInvoke(Invoke, Derived, DT))
      return Reject("invoke-not-normal-only");

    auto TryBuildBaseOrDerivedSelectRemat = [&]() {
      auto *Select = dyn_cast<SelectInst>(Derived);
      if (!Select)
        return false;

      auto IsBaseValue = [&](Value *V) {
        return V == Base || isOxCamlBaseEquivalentGCPointer(V, Base);
      };
      auto RootMatchesBase = [&](Value *Root) {
        if (IsBaseValue(Root))
          return true;
        auto It = PointerToBase.find(Root);
        return It != PointerToBase.end() && It->second == Base;
      };
      auto BuildChainFromArm = [&](Value *Arm,
                                   SmallVectorImpl<Instruction *> &Chain,
                                   Value *&ArmRoot) {
        if (findRematerializableChainToSpecificBase(
                Chain, Arm, Base, /*AllowIntegerAddressArithmetic=*/true)) {
          ArmRoot = Base;
          return !Chain.empty();
        }

        ArmRoot = findRematerializableChainToBasePointer(
            Chain, Arm, /*AllowIntegerAddressArithmetic=*/true);
        if (Chain.empty())
          return false;
        if (RootMatchesBase(ArmRoot))
          return true;
        auto It = PointerToBase.find(Arm);
        return It != PointerToBase.end() && It->second == Base;
      };

      Value *TrueValue = Select->getTrueValue();
      Value *FalseValue = Select->getFalseValue();
      bool TrueIsBase = IsBaseValue(TrueValue);
      bool FalseIsBase = IsBaseValue(FalseValue);
      if (TrueIsBase != FalseIsBase) {
        Value *DerivedArm = TrueIsBase ? FalseValue : TrueValue;
        SmallVector<Instruction *, 3> SelectChain;
        Value *SelectRoot = nullptr;
        if (!BuildChainFromArm(DerivedArm, SelectChain, SelectRoot))
          return false;

        Record.ChainToBase = SelectChain;
        RootOfChain = SelectRoot;
        Record.UseRematerializedValue = Select->getCondition();
        Record.RematerializedValueConditionSelectsRematValue = !TrueIsBase;
        return true;
      }

      if (TrueIsBase || FalseIsBase)
        return false;

      SmallVector<Instruction *, 3> TrueChain;
      SmallVector<Instruction *, 3> FalseChain;
      Value *TrueRoot = nullptr;
      Value *FalseRoot = nullptr;
      if (!BuildChainFromArm(TrueValue, TrueChain, TrueRoot) ||
          !BuildChainFromArm(FalseValue, FalseChain, FalseRoot))
        return false;

      bool SameChain = TrueChain.size() == FalseChain.size();
      for (unsigned I = 0, E = TrueChain.size(); SameChain && I != E; ++I)
        SameChain = areEquivalentRematerializableInstructions(TrueChain[I],
                                                              FalseChain[I]);
      if (SameChain) {
        Record.ChainToBase = TrueChain;
        RootOfChain = TrueRoot;
        return true;
      }

      Record.ChainToBase.clear();
      RootOfChain = Base;
      Record.UseRematerializedValue = nullptr;
      Record.RematerializationTag = Select->getCondition();

      RematerializationAlternative &FalseAlt =
          Record.Alternatives.emplace_back();
      FalseAlt.ChainToBase = FalseChain;
      FalseAlt.RootOfChain = FalseRoot;
      FalseAlt.Tag = 0;

      RematerializationAlternative &TrueAlt =
          Record.Alternatives.emplace_back();
      TrueAlt.ChainToBase = TrueChain;
      TrueAlt.RootOfChain = TrueRoot;
      TrueAlt.Tag = 1;
      return true;
    };

    auto TryBuildAlternativePhiRemat = [&]() {
      auto *DerivedPhi = dyn_cast<PHINode>(Derived);
      auto *BasePhi = dyn_cast<PHINode>(Base);
      if (!DerivedPhi || !BasePhi ||
          DerivedPhi->getParent() != BasePhi->getParent() ||
          DerivedPhi->getNumIncomingValues() != BasePhi->getNumIncomingValues())
        return false;

      struct PendingAlternative {
        SmallVector<Instruction *, 3> ChainToBase;
        Value *RootOfChain = nullptr;
      };
      SmallVector<PendingAlternative, 4> PendingAlternatives;
      SmallVector<unsigned, 8> IncomingTags;

      SmallVector<std::pair<Value *, Value *>, 8> ActiveBaseRelations;
      auto IsBaseRelation = [&](auto &&Self, Value *V, Value *B) -> bool {
        if (V == B)
          return true;
        if (isOxCamlBaseEquivalentGCPointer(V, B))
          return true;

        if (is_contained(ActiveBaseRelations, std::make_pair(V, B)))
          return true;
        ActiveBaseRelations.push_back({V, B});
        auto PopActive =
            make_scope_exit([&]() { ActiveBaseRelations.pop_back(); });

        if (auto *Load = dyn_cast<LoadInst>(V)) {
          if (!isOxCamlExceptionRootLoad(Load))
            return false;
          auto CollectNonConstantStores =
              [&](Value *Slot, SmallVectorImpl<Value *> &StoredValues) {
                for (User *U : Slot->users()) {
                  auto *Store = dyn_cast<StoreInst>(U);
                  if (!Store || Store->getPointerOperand() != Slot)
                    continue;
                  Value *Stored = nonPoisonOxCamlGCPointerValue(
                      Store->getValueOperand());
                  if (!isa<Constant>(Stored))
                    StoredValues.push_back(Stored);
                }
              };

          bool SawNonConstantStore = false;
          Value *Slot = Load->getPointerOperand();
          SmallVector<Value *, 4> StoredValues;
          CollectNonConstantStores(Slot, StoredValues);

          if (auto *BaseLoad = dyn_cast<LoadInst>(B)) {
            if (!isOxCamlExceptionRootLoad(BaseLoad))
              return false;
            SmallVector<Value *, 4> BaseStoredValues;
            CollectNonConstantStores(BaseLoad->getPointerOperand(),
                                     BaseStoredValues);
            if (StoredValues.empty() || BaseStoredValues.empty())
              return false;
            for (Value *Stored : StoredValues) {
              bool FoundMatchingBaseStore = false;
              for (Value *BaseStored : BaseStoredValues) {
                if (Stored->getType() != BaseStored->getType())
                  continue;
                if (Self(Self, Stored, BaseStored)) {
                  FoundMatchingBaseStore = true;
                  break;
                }
              }
              if (!FoundMatchingBaseStore)
                return false;
            }
            return true;
          }

          for (Value *Stored : StoredValues) {
            if (Stored->getType() != B->getType())
              return false;
            SawNonConstantStore = true;
            if (!Self(Self, Stored, B))
              return false;
          }
          return SawNonConstantStore;
        }

        auto *VPhi = dyn_cast<PHINode>(V);
        auto *BPhi = dyn_cast<PHINode>(B);
        if (!VPhi || !BPhi || VPhi->getParent() != BPhi->getParent() ||
            VPhi->getNumIncomingValues() != BPhi->getNumIncomingValues())
          return false;

        for (unsigned I = 0, E = VPhi->getNumIncomingValues(); I != E; ++I) {
          if (VPhi->getIncomingBlock(I) != BPhi->getIncomingBlock(I))
            return false;
          if (!Self(Self, VPhi->getIncomingValue(I),
                    BPhi->getIncomingValue(I)))
            return false;
        }
        return true;
      };
      auto RootMatchesIncomingBase = [&](Value *Root, Value *IncomingBase) {
        return IsBaseRelation(IsBaseRelation, Root, IncomingBase);
      };
      auto IncomingChainMatchesBase =
          [&](Value *Incoming, Value *Root, Value *IncomingBase,
              ArrayRef<Instruction *> IncomingChain) {
        if (Incoming == IncomingBase)
          return true;
        if (RootMatchesIncomingBase(Root, IncomingBase))
          return true;
        auto It = PointerToBase.find(Incoming);
        return !IncomingChain.empty() && It != PointerToBase.end() &&
               It->second == IncomingBase;
      };
      auto FindMatchingAlternative =
          [&](ArrayRef<Instruction *> ChainToBase) -> std::optional<unsigned> {
        for (unsigned Index = 0, End = PendingAlternatives.size();
             Index != End; ++Index) {
          ArrayRef<Instruction *> Existing =
              PendingAlternatives[Index].ChainToBase;
          if (Existing.size() != ChainToBase.size())
            continue;
          bool Match = true;
          for (unsigned ChainIndex = 0, ChainEnd = Existing.size();
               ChainIndex != ChainEnd; ++ChainIndex)
            if (!areEquivalentRematerializableInstructions(
                    Existing[ChainIndex], ChainToBase[ChainIndex])) {
              Match = false;
              break;
            }
          if (Match)
            return Index;
        }
        return std::nullopt;
      };

      for (unsigned I = 0, E = DerivedPhi->getNumIncomingValues(); I != E;
           ++I) {
        Value *Incoming = DerivedPhi->getIncomingValue(I);
        Value *IncomingBase = BasePhi->getIncomingValue(I);
        if (DerivedPhi->getIncomingBlock(I) != BasePhi->getIncomingBlock(I))
          return false;

        SmallVector<Instruction *, 3> IncomingChain;
        Value *IncomingRoot = IncomingBase;
        if (Incoming != IncomingBase) {
          if (!findRematerializableChainToSpecificBase(
                  IncomingChain, Incoming, IncomingBase,
                  /*AllowIntegerAddressArithmetic=*/true))
            IncomingRoot = findRematerializableChainToBasePointer(
                IncomingChain, Incoming,
                /*AllowIntegerAddressArithmetic=*/true);
          if (!IncomingChainMatchesBase(Incoming, IncomingRoot, IncomingBase,
                                        IncomingChain))
            return false;
        }

        std::optional<unsigned> Existing =
            FindMatchingAlternative(IncomingChain);
        unsigned Tag;
        if (Existing) {
          Tag = *Existing;
        } else {
          Tag = PendingAlternatives.size();
          PendingAlternative &Alt = PendingAlternatives.emplace_back();
          Alt.ChainToBase = IncomingChain;
          Alt.RootOfChain = IncomingRoot;
        }
        IncomingTags.push_back(Tag);
      }

      if (PendingAlternatives.size() == 1 &&
          PendingAlternatives.front().ChainToBase.empty()) {
        Record.ChainToBase.clear();
        Record.RootOfChain = Base;
        Record.UseRematerializedValue = nullptr;
        Record.RematerializationTag = nullptr;
        return true;
      }

      if (PendingAlternatives.size() <= 1)
        return false;

      auto *TagTy = Type::getInt32Ty(Call->getContext());
      PHINode *TagPhi =
          PHINode::Create(TagTy, DerivedPhi->getNumIncomingValues(),
                          suffixed_name_or(DerivedPhi, ".remat.tag",
                                           "remat.tag"),
                          &*DerivedPhi->getParent()->getFirstInsertionPt());
      for (unsigned I = 0, E = DerivedPhi->getNumIncomingValues(); I != E;
           ++I)
        TagPhi->addIncoming(ConstantInt::get(TagTy, IncomingTags[I]),
                            DerivedPhi->getIncomingBlock(I));

      Record.ChainToBase.clear();
      Record.RootOfChain = Base;
      Record.UseRematerializedValue = nullptr;
      Record.RematerializationTag = TagPhi;
      for (unsigned I = 0, E = PendingAlternatives.size(); I != E; ++I) {
        RematerializationAlternative &Alt = Record.Alternatives.emplace_back();
        Alt.ChainToBase = PendingAlternatives[I].ChainToBase;
        Alt.RootOfChain = PendingAlternatives[I].RootOfChain;
        Alt.Tag = I;
      }
      return true;
    };

    if ((!IsPhiChainToBasePhi && !IsPhiChainToSingleBase &&
         !IsBaseEquivalent &&
         !IsRootBaseEquivalent && RootOfChain != Base) ||
        (!IsBaseEquivalent && Record.ChainToBase.empty())) {
      if (DebugOxCamlDerivedRemat && IsOxCamlStatepoint) {
        errs() << "  root-of-chain: ";
        RootOfChain->print(errs());
        errs() << "\n  chain-size: " << Record.ChainToBase.size() << "\n";
      }
      if (!TryBuildBaseOrDerivedSelectRemat() &&
          !TryBuildAlternativePhiRemat())
        return Reject("not-chain");
    }

    const unsigned Ordinal = ++OxCamlDerivedRematAcceptedOrdinal;
    const unsigned Skip = RematAddrSpace1DerivedFromBaseAtAllocSkip;
    const unsigned Limit = RematAddrSpace1DerivedFromBaseAtAllocLimit;
    if (Ordinal <= Skip || Ordinal - Skip > Limit) {
      if (DebugOxCamlDerivedRemat) {
        errs() << "rs4gc-oxcaml-remat-reject range ordinal " << Ordinal
               << "\n";
      }
      return false;
    }

    Record.RootOfChain = RootOfChain;
    Record.Cost = 0;
    RematerializedDerivedChains.insert({Derived, Record});
    if (DebugOxCamlDerivedRemat) {
      errs() << "rs4gc-oxcaml-remat-accepted ordinal " << Ordinal << " in "
             << Call->getFunction()->getName() << "\n  derived: ";
      Derived->print(errs());
      errs() << "\n  base: ";
      Base->print(errs());
      errs() << "\n";
    }
    return true;
  };

  SmallVector<Value *, 64> FilteredLiveVariables;
  SmallVector<Value *, 64> FilteredBasePtrs;
  for (size_t I = 0; I < LiveVariables.size(); ++I) {
    Value *LiveVariable = LiveVariables[I];
    auto BaseIt = PointerToBase.find(LiveVariable);
    if (DebugOxCamlDerivedRemat && IsOxCamlStatepoint) {
      if (BaseIt != PointerToBase.end())
        PrintOxCamlDerivedCandidate(LiveVariable, BaseIt->second);
    }
    if (UseOxCamlVolatileRootAllocas && IsOxCamlStatepoint &&
        !isLiveValueSupportedByOxCamlVolatileRootAllocas(LiveVariable,
                                                         PointerToBase)) {
      errs() << "OxCaml volatile root allocas cannot materialize a derived "
                "addrspace(1) pointer\n  call: ";
      Call->print(errs());
      errs() << "\n  live: ";
      LiveVariable->print(errs());
      if (BaseIt != PointerToBase.end()) {
        errs() << "\n  base: ";
        BaseIt->second->print(errs());
      }
      errs() << "\n";
      report_fatal_error("unsupported OxCaml volatile root alloca live value");
    }
    if (ShouldRematerializeDerivedFromBase(LiveVariable)) {
      RematerializedDerivedValues.insert(LiveVariable);
      continue;
    }
    bool IsOxCamlSelfBaseEquivalent =
        IsOxCamlStatepoint && BaseIt != PointerToBase.end() &&
        isDistinctOxCamlAddrSpace1BaseEquivalentGCPointer(LiveVariable,
                                                          BaseIt->second);
    bool IsOxCamlAddressExpression =
        IsOxCamlStatepoint && BaseIt != PointerToBase.end() &&
        isDistinctOxCamlAddrSpace1AddressExpression(LiveVariable,
                                                    BaseIt->second);
    if (FailOnOxCamlDerivedRelocates && IsOxCamlStatepoint &&
        BaseIt != PointerToBase.end()) {
      Value *Base = BaseIt->second;
      auto *LiveTy = dyn_cast<PointerType>(LiveVariable->getType());
      auto *BaseTy = dyn_cast<PointerType>(Base->getType());
      if (Base != LiveVariable && LiveTy && BaseTy &&
          LiveTy->getAddressSpace() == 1 && BaseTy->getAddressSpace() == 1 &&
          IsOxCamlAddressExpression) {
        errs() << "OxCaml statepoint would relocate a derived addrspace(1) "
                  "pointer independently\n  call: ";
        Call->print(errs());
        errs() << "\n  derived: ";
        LiveVariable->print(errs());
        errs() << "\n  base: ";
        Base->print(errs());
        errs() << "\n";
        report_fatal_error("unrematerialized OxCaml derived pointer across "
                           "statepoint");
      }
    }
    Value *BasePtr = BasePtrs[I];
    if (IsOxCamlSelfBaseEquivalent)
      BasePtr = LiveVariable;
    else if (IsOxCamlStatepoint && !IsOxCamlAddressExpression &&
             BaseIt != PointerToBase.end()) {
      Value *Base = BaseIt->second;
      auto *LiveTy = dyn_cast<PointerType>(LiveVariable->getType());
      auto *BaseTy = dyn_cast<PointerType>(Base->getType());
      if (Base != LiveVariable && LiveTy && BaseTy &&
          LiveTy->getAddressSpace() == 1 && BaseTy->getAddressSpace() == 1)
        BasePtr = LiveVariable;
    }
    FilteredLiveVariables.push_back(
        nonPoisonOxCamlGCPointerValue(LiveVariable));
    FilteredBasePtrs.push_back(nonPoisonOxCamlGCPointerValue(BasePtr));
  }

  SmallVector<Value *, 16> GCArgsStorage(FilteredLiveVariables.begin(),
                                         FilteredLiveVariables.end());
  if (auto Bundle = Call->getOperandBundle(LLVMContext::OB_gc_live))
    for (const Use &U : Bundle->Inputs) {
      if (!RematerializedDerivedValues.contains(U.get()))
        GCArgsStorage.push_back(nonPoisonOxCamlGCPointerValue(U.get()));
    }

  SmallPtrSet<Value *, 32> SeenGCArgs(GCArgsStorage.begin(),
                                      GCArgsStorage.end());
  for (Value *Arg : Result.CallArgRoots) {
    auto BaseIt = PointerToBase.find(Arg);
    assert(BaseIt != PointerToBase.end() &&
           "Missed base for statepoint call argument");
    Value *Root = BaseIt->second;
    if (isa<Constant>(Root))
      continue;
    Root = nonPoisonOxCamlGCPointerValue(Root);
    if (SeenGCArgs.insert(Root).second)
      GCArgsStorage.push_back(Root);
  }

  llvm::append_range(GCArgsStorage, Result.ExplicitRootSlots);
  uint64_t StatepointID = StatepointDirectives::DefaultStatepointID;
  uint32_t NumPatchBytes = 0;
  uint32_t Flags = uint32_t(StatepointFlags::None);

  SmallVector<Value *, 8> CallArgs(Call->args());
  std::optional<ArrayRef<Use>> DeoptArgs;
  if (auto Bundle = Call->getOperandBundle(LLVMContext::OB_deopt))
    DeoptArgs = Bundle->Inputs;
  std::optional<ArrayRef<Use>> TransitionArgs;
  if (auto Bundle = Call->getOperandBundle(LLVMContext::OB_gc_transition)) {
    TransitionArgs = Bundle->Inputs;
    // TODO: This flag no longer serves a purpose and can be removed later
    Flags |= uint32_t(StatepointFlags::GCTransition);
  }

  // Instead of lowering calls to @llvm.experimental.deoptimize as normal calls
  // with a return value, we lower then as never returning calls to
  // __llvm_deoptimize that are followed by unreachable to get better codegen.
  bool IsDeoptimize = false;

  StatepointDirectives SD =
      parseStatepointDirectivesFromAttrs(Call->getAttributes());
  if (SD.NumPatchBytes)
    NumPatchBytes = *SD.NumPatchBytes;
  if (SD.StatepointID)
    StatepointID = *SD.StatepointID;

  // Pass through the requested lowering if any.  The default is live-through.
  StringRef DeoptLowering = getDeoptLowering(Call);
  if (DeoptLowering.equals("live-in"))
    Flags |= uint32_t(StatepointFlags::DeoptLiveIn);
  else {
    assert(DeoptLowering.equals("live-through") && "Unsupported value!");
  }

  FunctionCallee CallTarget(Call->getFunctionType(), Call->getCalledOperand());
  if (Function *F = dyn_cast<Function>(CallTarget.getCallee())) {
    auto IID = F->getIntrinsicID();
    if (IID == Intrinsic::experimental_deoptimize) {
      // Calls to llvm.experimental.deoptimize are lowered to calls to the
      // __llvm_deoptimize symbol.  We want to resolve this now, since the
      // verifier does not allow taking the address of an intrinsic function.

      SmallVector<Type *, 8> DomainTy;
      for (Value *Arg : CallArgs)
        DomainTy.push_back(Arg->getType());
      auto *FTy = FunctionType::get(Type::getVoidTy(F->getContext()), DomainTy,
                                    /* isVarArg = */ false);

      // Note: CallTarget can be a bitcast instruction of a symbol if there are
      // calls to @llvm.experimental.deoptimize with different argument types in
      // the same module.  This is fine -- we assume the frontend knew what it
      // was doing when generating this kind of IR.
      CallTarget = F->getParent()
                       ->getOrInsertFunction("__llvm_deoptimize", FTy);

      IsDeoptimize = true;
    } else if (IID == Intrinsic::memcpy_element_unordered_atomic ||
               IID == Intrinsic::memmove_element_unordered_atomic) {
      // Unordered atomic memcpy and memmove intrinsics which are not explicitly
      // marked as "gc-leaf-function" should be lowered in a GC parseable way.
      // Specifically, these calls should be lowered to the
      // __llvm_{memcpy|memmove}_element_unordered_atomic_safepoint symbols.
      // Similarly to __llvm_deoptimize we want to resolve this now, since the
      // verifier does not allow taking the address of an intrinsic function.
      //
      // Moreover we need to shuffle the arguments for the call in order to
      // accommodate GC. The underlying source and destination objects might be
      // relocated during copy operation should the GC occur. To relocate the
      // derived source and destination pointers the implementation of the
      // intrinsic should know the corresponding base pointers.
      //
      // To make the base pointers available pass them explicitly as arguments:
      //   memcpy(dest_derived, source_derived, ...) =>
      //   memcpy(dest_base, dest_offset, source_base, source_offset, ...)
      auto &Context = Call->getContext();
      auto &DL = Call->getModule()->getDataLayout();
      auto GetBaseAndOffset = [&](Value *Derived) {
        Value *Base = nullptr;
        // Optimizations in unreachable code might substitute the real pointer
        // with undef, poison or null-derived constant. Return null base for
        // them to be consistent with the handling in the main algorithm in
        // findBaseDefiningValue.
        if (isa<Constant>(Derived))
          Base =
              ConstantPointerNull::get(cast<PointerType>(Derived->getType()));
        else {
          assert(PointerToBase.count(Derived));
          Base = PointerToBase.find(Derived)->second;
        }
        unsigned AddressSpace = Derived->getType()->getPointerAddressSpace();
        unsigned IntPtrSize = DL.getPointerSizeInBits(AddressSpace);
        Value *Base_int = Builder.CreatePtrToInt(
            Base, Type::getIntNTy(Context, IntPtrSize));
        Value *Derived_int = Builder.CreatePtrToInt(
            Derived, Type::getIntNTy(Context, IntPtrSize));
        return std::make_pair(Base, Builder.CreateSub(Derived_int, Base_int));
      };

      auto *Dest = CallArgs[0];
      Value *DestBase, *DestOffset;
      std::tie(DestBase, DestOffset) = GetBaseAndOffset(Dest);

      auto *Source = CallArgs[1];
      Value *SourceBase, *SourceOffset;
      std::tie(SourceBase, SourceOffset) = GetBaseAndOffset(Source);

      auto *LengthInBytes = CallArgs[2];
      auto *ElementSizeCI = cast<ConstantInt>(CallArgs[3]);

      CallArgs.clear();
      CallArgs.push_back(DestBase);
      CallArgs.push_back(DestOffset);
      CallArgs.push_back(SourceBase);
      CallArgs.push_back(SourceOffset);
      CallArgs.push_back(LengthInBytes);

      SmallVector<Type *, 8> DomainTy;
      for (Value *Arg : CallArgs)
        DomainTy.push_back(Arg->getType());
      auto *FTy = FunctionType::get(Type::getVoidTy(F->getContext()), DomainTy,
                                    /* isVarArg = */ false);

      auto GetFunctionName = [](Intrinsic::ID IID, ConstantInt *ElementSizeCI) {
        uint64_t ElementSize = ElementSizeCI->getZExtValue();
        if (IID == Intrinsic::memcpy_element_unordered_atomic) {
          switch (ElementSize) {
          case 1:
            return "__llvm_memcpy_element_unordered_atomic_safepoint_1";
          case 2:
            return "__llvm_memcpy_element_unordered_atomic_safepoint_2";
          case 4:
            return "__llvm_memcpy_element_unordered_atomic_safepoint_4";
          case 8:
            return "__llvm_memcpy_element_unordered_atomic_safepoint_8";
          case 16:
            return "__llvm_memcpy_element_unordered_atomic_safepoint_16";
          default:
            llvm_unreachable("unexpected element size!");
          }
        }
        assert(IID == Intrinsic::memmove_element_unordered_atomic);
        switch (ElementSize) {
        case 1:
          return "__llvm_memmove_element_unordered_atomic_safepoint_1";
        case 2:
          return "__llvm_memmove_element_unordered_atomic_safepoint_2";
        case 4:
          return "__llvm_memmove_element_unordered_atomic_safepoint_4";
        case 8:
          return "__llvm_memmove_element_unordered_atomic_safepoint_8";
        case 16:
          return "__llvm_memmove_element_unordered_atomic_safepoint_16";
        default:
          llvm_unreachable("unexpected element size!");
        }
      };

      CallTarget =
          F->getParent()
              ->getOrInsertFunction(GetFunctionName(IID, ElementSizeCI), FTy);
    }
  }

  ArrayRef<Value *> GCArgs(GCArgsStorage);

  // Create the statepoint given all the arguments
  GCStatepointInst *Token = nullptr;
  if (auto *CI = dyn_cast<CallInst>(Call)) {
    CallInst *SPCall = Builder.CreateGCStatepointCall(
        StatepointID, NumPatchBytes, CallTarget, Flags, CallArgs,
        TransitionArgs, DeoptArgs, GCArgs, "safepoint_token");

    SPCall->setTailCallKind(CI->getTailCallKind());
    SPCall->setCallingConv(CI->getCallingConv());

    // Currently we will fail on parameter attributes and on certain
    // function attributes.  In case if we can handle this set of attributes -
    // set up function attrs directly on statepoint and return attrs later for
    // gc_result intrinsic.
    SPCall->setAttributes(legalizeCallAttributes(
        CI->getContext(), CI->getAttributes(), SPCall->getAttributes()));

    Token = cast<GCStatepointInst>(SPCall);

    // Put the following gc_result and gc_relocate calls immediately after the
    // the old call (which we're about to delete)
    assert(CI->getNextNode() && "Not a terminator, must have next!");
    Builder.SetInsertPoint(CI->getNextNode());
    Builder.SetCurrentDebugLocation(CI->getNextNode()->getDebugLoc());
  } else {
    auto *II = cast<InvokeInst>(Call);

    // Insert the new invoke into the old block.  We'll remove the old one in a
    // moment at which point this will become the new terminator for the
    // original block.
    InvokeInst *SPInvoke = Builder.CreateGCStatepointInvoke(
        StatepointID, NumPatchBytes, CallTarget, II->getNormalDest(),
        II->getUnwindDest(), Flags, CallArgs, TransitionArgs, DeoptArgs, GCArgs,
        "statepoint_token");

    SPInvoke->setCallingConv(II->getCallingConv());

    // Currently we will fail on parameter attributes and on certain
    // function attributes.  In case if we can handle this set of attributes -
    // set up function attrs directly on statepoint and return attrs later for
    // gc_result intrinsic.
    SPInvoke->setAttributes(legalizeCallAttributes(
        II->getContext(), II->getAttributes(), SPInvoke->getAttributes()));

    Token = cast<GCStatepointInst>(SPInvoke);

    // Generate gc relocates in exceptional path
    BasicBlock *UnwindBlock = II->getUnwindDest();
    if (Result.UsesExplicitExceptionRoots) {
      Result.UnwindToken = UnwindBlock->getLandingPadInst();
      assert(Result.UnwindToken &&
             "explicit exception roots require a landingpad unwind block");
    } else {
      assert(!isa<PHINode>(UnwindBlock->begin()) &&
             UnwindBlock->getUniquePredecessor() &&
             "can't safely insert in this block!");

      Builder.SetInsertPoint(&*UnwindBlock->getFirstInsertionPt());
      Builder.SetCurrentDebugLocation(II->getDebugLoc());

      // Attach exceptional gc relocates to the landingpad.
      Instruction *ExceptionalToken = UnwindBlock->getLandingPadInst();
      Result.UnwindToken = ExceptionalToken;

      if (!UseOxCamlVolatileRootAllocas || !IsOxCamlStatepoint)
        CreateGCRelocates(FilteredLiveVariables, FilteredBasePtrs,
                          ExceptionalToken, Builder);
    }

    // Generate gc relocates and returns for normal block
    BasicBlock *NormalDest = II->getNormalDest();
    assert(!isa<PHINode>(NormalDest->begin()) &&
           NormalDest->getUniquePredecessor() &&
           "can't safely insert in this block!");

    Builder.SetInsertPoint(&*NormalDest->getFirstInsertionPt());

    // gc relocates will be generated later as if it were regular call
    // statepoint
  }
  assert(Token && "Should be set in one of the above branches!");

  auto AddDerivedRematerializations = [&](Instruction *StatepointToken) {
    if (RematerializedDerivedValues.empty())
      return;

    DenseMap<Value *, GCRelocateInst *> RelocByDerived;
    for (User *U : StatepointToken->users())
      if (auto *Relocate = dyn_cast<GCRelocateInst>(U))
        RelocByDerived[Relocate->getDerivedPtr()] = Relocate;

    for (Value *Derived : RematerializedDerivedValues) {
      Value *Base = PointerToBase.find(Derived)->second;
      auto It = RelocByDerived.find(Base);
      assert(It != RelocByDerived.end() &&
             "base must be relocated to rematerialize derived pointer");
      auto *BaseRelocate = It->second;
      assert(BaseRelocate->getNextNode() &&
             "gc.relocate should not be a terminator");

      RematerizlizationCandidateRecord &Record =
          RematerializedDerivedChains.find(Derived)->second;
      Instruction *RematerializedValue = nullptr;
      if (Record.RematerializationTag) {
        IRBuilder<> SelectBuilder(BaseRelocate->getNextNode());
        Value *Selected = nullptr;
        for (const RematerializationAlternative &Alt : Record.Alternatives) {
          Value *AlternativeValue = BaseRelocate;
          if (!Alt.ChainToBase.empty())
            AlternativeValue =
                rematerializeChain(Alt.ChainToBase,
                                   &*SelectBuilder.GetInsertPoint(),
                                   Alt.RootOfChain, BaseRelocate);
          if (!Selected) {
            Selected = AlternativeValue;
            continue;
          }
          Value *TagMatches = SelectBuilder.CreateICmpEQ(
              Record.RematerializationTag,
              ConstantInt::get(Record.RematerializationTag->getType(),
                               Alt.Tag),
              suffixed_name_or(Derived, ".remat.tag.match",
                               "remat.tag.match"));
          Selected = SelectBuilder.CreateSelect(
              TagMatches, AlternativeValue, Selected,
              suffixed_name_or(Derived, ".remat.select", "remat.select"));
        }
        RematerializedValue = cast<Instruction>(Selected);
        if (!RematerializedValue)
          report_fatal_error("missing OxCaml PHI rematerialization alternative");
      } else if (Record.ChainToBase.empty()) {
        RematerializedValue = BaseRelocate;
      } else {
        RematerializedValue =
            rematerializeChain(Record.ChainToBase, BaseRelocate->getNextNode(),
                               Record.RootOfChain, BaseRelocate);
      }
      if (Record.UseRematerializedValue) {
        IRBuilder<> SelectBuilder(RematerializedValue->getNextNode());
        Value *TrueValue = RematerializedValue;
        Value *FalseValue = BaseRelocate;
        if (!Record.RematerializedValueConditionSelectsRematValue)
          std::swap(TrueValue, FalseValue);
        auto *SelectedValue = cast<Instruction>(SelectBuilder.CreateSelect(
            Record.UseRematerializedValue, TrueValue, FalseValue,
            suffixed_name_or(Derived, ".remat.select", "remat.select")));
        Result.RematerializedValues[SelectedValue] = Derived;
      } else {
        Result.RematerializedValues[RematerializedValue] = Derived;
      }
    }
  };

  if (IsDeoptimize) {
    // If we're wrapping an @llvm.experimental.deoptimize in a statepoint, we
    // transform the tail-call like structure to a call to a void function
    // followed by unreachable to get better codegen.
    Replacements.push_back(
        DeferredReplacement::createDeoptimizeReplacement(Call));
  } else {
    Token->setName("statepoint_token");
    if (!Call->getType()->isVoidTy() && !Call->use_empty()) {
      StringRef Name = Call->hasName() ? Call->getName() : "";
      CallInst *GCResult = Builder.CreateGCResult(Token, Call->getType(), Name);
      GCResult->setAttributes(
          AttributeList::get(GCResult->getContext(), AttributeList::ReturnIndex,
                             Call->getAttributes().getRetAttrs()));

      // We cannot RAUW or delete CS.getInstruction() because it could be in the
      // live set of some other safepoint, in which case that safepoint's
      // PartiallyConstructedSafepointRecord will hold a raw pointer to this
      // llvm::Instruction.  Instead, we defer the replacement and deletion to
      // after the live sets have been made explicit in the IR, and we no longer
      // have raw pointers to worry about.
      Replacements.emplace_back(
          DeferredReplacement::createRAUW(Call, GCResult));
    } else {
      Replacements.emplace_back(DeferredReplacement::createDelete(Call));
    }
  }

  Result.StatepointToken = Token;

  // Second, create a gc.relocate for every live variable
  if (!UseOxCamlVolatileRootAllocas || !IsOxCamlStatepoint) {
    CreateGCRelocates(FilteredLiveVariables, FilteredBasePtrs, Token, Builder);
    AddDerivedRematerializations(Token);
  }
}

// Replace an existing gc.statepoint with a new one and a set of gc.relocates
// which make the relocations happening at this safepoint explicit.
//
// WARNING: Does not do any fixup to adjust users of the original live
// values.  That's the callers responsibility.
static void
makeStatepointExplicit(DominatorTree &DT, CallBase *Call,
                       PartiallyConstructedSafepointRecord &Result,
                       std::vector<DeferredReplacement> &Replacements,
                       const PointerToBaseTy &PointerToBase) {
  const auto &LiveSet = Result.LiveSet;

  // Convert to vector for efficient cross referencing.
  SmallVector<Value *, 64> BaseVec, LiveVec;
  LiveVec.reserve(LiveSet.size());
  BaseVec.reserve(LiveSet.size());
  for (Value *L : LiveSet) {
    LiveVec.push_back(L);
    assert(PointerToBase.count(L));
    Value *Base = PointerToBase.find(L)->second;
    BaseVec.push_back(Base);
  }
  assert(LiveVec.size() == BaseVec.size());

  // Do the actual rewriting and delete the old statepoint
  makeStatepointExplicitImpl(Call, BaseVec, LiveVec, Result, Replacements,
                             PointerToBase, DT);
}

// Helper function for the relocationViaAlloca.
//
// It receives iterator to the statepoint gc relocates and emits a store to the
// assigned location (via allocaMap) for the each one of them.  It adds the
// visited values into the visitedLiveValues set, which we will later use them
// for validation checking.
static void
insertRelocationStores(iterator_range<Value::user_iterator> GCRelocs,
                       DenseMap<Value *, AllocaInst *> &AllocaMap,
                       DenseSet<Value *> &VisitedLiveValues) {
  for (User *U : GCRelocs) {
    GCRelocateInst *Relocate = dyn_cast<GCRelocateInst>(U);
    if (!Relocate)
      continue;

    Value *OriginalValue = Relocate->getDerivedPtr();
    assert(AllocaMap.count(OriginalValue));
    Value *Alloca = AllocaMap[OriginalValue];

    // Emit store into the related alloca
    // All gc_relocates are i8 addrspace(1)* typed, and it must be bitcasted to
    // the correct type according to alloca.
    assert(Relocate->getNextNode() &&
           "Should always have one since it's not a terminator");
    IRBuilder<> Builder(Relocate->getNextNode());
    Value *CastedRelocatedValue =
      Builder.CreateBitCast(Relocate,
                            cast<AllocaInst>(Alloca)->getAllocatedType(),
                            suffixed_name_or(Relocate, ".casted", ""));

    new StoreInst(CastedRelocatedValue, Alloca,
                  cast<Instruction>(CastedRelocatedValue)->getNextNode());

#ifndef NDEBUG
    VisitedLiveValues.insert(OriginalValue);
#endif
  }
}

// Helper function for the "relocationViaAlloca". Similar to the
// "insertRelocationStores" but works for rematerialized values.
static void insertRematerializationStores(
    const RematerializedValueMapTy &RematerializedValues,
    DenseMap<Value *, AllocaInst *> &AllocaMap,
    DenseSet<Value *> &VisitedLiveValues) {
  for (auto RematerializedValuePair: RematerializedValues) {
    Instruction *RematerializedValue = RematerializedValuePair.first;
    Value *OriginalValue = RematerializedValuePair.second;

    assert(AllocaMap.count(OriginalValue) &&
           "Can not find alloca for rematerialized value");
    Value *Alloca = AllocaMap[OriginalValue];

    new StoreInst(RematerializedValue, Alloca,
                  RematerializedValue->getNextNode());

#ifndef NDEBUG
    VisitedLiveValues.insert(OriginalValue);
#endif
  }
}

static bool isStatepointResultOrRelocateFor(Instruction *I,
                                            Instruction *Statepoint) {
  auto *Intrinsic = dyn_cast<IntrinsicInst>(I);
  if (!Intrinsic)
    return false;

  switch (Intrinsic->getIntrinsicID()) {
  case Intrinsic::experimental_gc_result:
  case Intrinsic::experimental_gc_relocate:
    return Intrinsic->getArgOperand(0) == Statepoint;
  default:
    return false;
  }
}

static Instruction *
getPostStatepointNormalInsertBefore(Instruction *Statepoint) {
  Instruction *InsertBefore = nullptr;
  if (auto *Invoke = dyn_cast<InvokeInst>(Statepoint))
    InsertBefore = &*Invoke->getNormalDest()->getFirstInsertionPt();
  else
    InsertBefore = Statepoint->getNextNode();

  while (InsertBefore &&
         isStatepointResultOrRelocateFor(InsertBefore, Statepoint))
    InsertBefore = InsertBefore->getNextNode();

  assert(InsertBefore && "statepoint must have a normal insertion point");
  return InsertBefore;
}

static void insertExplicitRootHomeStores(
    ArrayRef<std::pair<WeakTrackingVH, AllocaInst *>> ExplicitRootHomes,
    Instruction *Statepoint, DenseMap<Value *, AllocaInst *> &AllocaMap,
    DenseSet<Value *> &VisitedLiveValues) {
  if (ExplicitRootHomes.empty())
    return;

  const DataLayout &DL = Statepoint->getModule()->getDataLayout();
  Instruction *InsertBefore = getPostStatepointNormalInsertBefore(Statepoint);
  IRBuilder<> Builder(InsertBefore);

  for (auto [RootHandle, Slot] : ExplicitRootHomes) {
    Value *RootValue = RootHandle;
    if (!RootValue) {
      errs() << "[rs4gc-root-verify] "
             << Statepoint->getFunction()->getName()
             << ": explicit root home handle died before SSA repair; the "
                "replacement value is unrepaired\n";
      continue;
    }
    auto AllocaIt = AllocaMap.find(RootValue);
    assert(AllocaIt != AllocaMap.end() &&
           "missing SSA repair alloca for explicit root home");
    AllocaInst *Alloca = AllocaIt->second;

    LoadInst *Load = Builder.CreateAlignedLoad(
        Slot->getAllocatedType(), Slot,
        DL.getABITypeAlign(Slot->getAllocatedType()),
        suffixed_name_or(RootValue, ".exnroot.normal.load",
                         "exnroot.normal.load"));
    if (OxCamlVolatileExnRootSlots)
      Load->setVolatile(true);
    Load->setMetadata("oxcaml.exnroot.load",
                      MDNode::get(Statepoint->getContext(), {}));

    Value *Reloaded = Load;
    if (Reloaded->getType() != Alloca->getAllocatedType())
      Reloaded = Builder.CreateBitCast(
          Reloaded, Alloca->getAllocatedType(),
          suffixed_name_or(RootValue, ".exnroot.normal.cast",
                           "exnroot.normal.cast"));

    Builder.CreateAlignedStore(
        Reloaded, Alloca, DL.getABITypeAlign(Alloca->getAllocatedType()));

#ifndef NDEBUG
    VisitedLiveValues.insert(RootValue);
#endif
  }
}

/// Do all the relocation update via allocas and mem2reg
static void relocationViaAlloca(
    Function &F, DominatorTree &DT, ArrayRef<Value *> Live,
    ArrayRef<PartiallyConstructedSafepointRecord> Records) {
#ifndef NDEBUG
  // record initial number of (static) allocas; we'll check we have the same
  // number when we get done.
  int InitialAllocaNum = 0;
  for (Instruction &I : F.getEntryBlock())
    if (isa<AllocaInst>(I))
      InitialAllocaNum++;
#endif

  // TODO-PERF: change data structures, reserve
  DenseMap<Value *, AllocaInst *> AllocaMap;
  SmallVector<AllocaInst *, 200> PromotableAllocas;
  // Used later to chack that we have enough allocas to store all values
  std::size_t NumRematerializedValues = 0;
  PromotableAllocas.reserve(Live.size());

  // Emit alloca for "LiveValue" and record it in "allocaMap" and
  // "PromotableAllocas"
  const DataLayout &DL = F.getParent()->getDataLayout();
  auto emitAllocaFor = [&](Value *LiveValue) {
    AllocaInst *Alloca = new AllocaInst(LiveValue->getType(),
                                        DL.getAllocaAddrSpace(), "",
                                        F.getEntryBlock().getFirstNonPHI());
    AllocaMap[LiveValue] = Alloca;
    PromotableAllocas.push_back(Alloca);

    // OCaml frame tables may describe any promoted value as a runtime-scanned
    // root after mem2reg.  If no defining store reaches a path, mem2reg would
    // otherwise synthesize undef.  Choose a valid non-moving OCaml immediate
    // for those abstract undefined paths so no physical root slot can contain
    // arbitrary bits.
    if (isOxCamlFunction(F) && !isa<Argument>(LiveValue) &&
        isHandledGCPointerType(LiveValue->getType())) {
      new StoreInst(getOxCamlNonMovingImmediate(LiveValue->getType()), Alloca,
                    Alloca->getNextNode());
    }
  };

  // Emit alloca for each live gc pointer
  for (Value *V : Live)
    emitAllocaFor(V);

  // Emit allocas for rematerialized values
  for (const auto &Info : Records)
    for (auto RematerializedValuePair : Info.RematerializedValues) {
      Value *OriginalValue = RematerializedValuePair.second;
      if (AllocaMap.count(OriginalValue) != 0)
        continue;

      emitAllocaFor(OriginalValue);
      ++NumRematerializedValues;
    }

  // The next two loops are part of the same conceptual operation.  We need to
  // insert a store to the alloca after the original def and at each
  // redefinition.  We need to insert a load before each use.  These are split
  // into distinct loops for performance reasons.

  // Update gc pointer after each statepoint: either store a relocated value or
  // null (if no relocated value was found for this gc pointer and it is not a
  // gc_result).  This must happen before we update the statepoint with load of
  // alloca otherwise we lose the link between statepoint and old def.
  for (const auto &Info : Records) {
    Value *Statepoint = Info.StatepointToken;

    // This will be used for consistency check
    DenseSet<Value *> VisitedLiveValues;

    // Insert stores for normal statepoint gc relocates
    insertRelocationStores(Statepoint->users(), AllocaMap, VisitedLiveValues);

    // In case if it was invoke statepoint
    // we will insert stores for exceptional path gc relocates.
    if (isa<InvokeInst>(Statepoint) && !Info.UsesExplicitExceptionRoots) {
      insertRelocationStores(Info.UnwindToken->users(), AllocaMap,
                             VisitedLiveValues);
    }

    // Do similar thing with rematerialized values
    insertRematerializationStores(Info.RematerializedValues, AllocaMap,
                                  VisitedLiveValues);

    insertExplicitRootHomeStores(Info.ExplicitRootHomes,
                                 cast<Instruction>(Statepoint), AllocaMap,
                                 VisitedLiveValues);

    if (ClobberNonLive) {
      // As a debugging aid, pretend that an unrelocated pointer becomes null at
      // the gc.statepoint.  This will turn some subtle GC problems into
      // slightly easier to debug SEGVs.  Note that on large IR files with
      // lots of gc.statepoints this is extremely costly both memory and time
      // wise.
      SmallVector<AllocaInst *, 64> ToClobber;
      for (auto Pair : AllocaMap) {
        Value *Def = Pair.first;
        AllocaInst *Alloca = Pair.second;

        // This value was relocated
        if (VisitedLiveValues.count(Def)) {
          continue;
        }
        ToClobber.push_back(Alloca);
      }

      auto InsertClobbersAt = [&](Instruction *IP) {
        for (auto *AI : ToClobber) {
          auto AT = AI->getAllocatedType();
          Constant *CPN;
          if (AT->isVectorTy())
            CPN = ConstantAggregateZero::get(AT);
          else
            CPN = ConstantPointerNull::get(cast<PointerType>(AT));
          new StoreInst(CPN, AI, IP);
        }
      };

      // Insert the clobbering stores.  These may get intermixed with the
      // gc.results and gc.relocates, but that's fine.
      if (auto II = dyn_cast<InvokeInst>(Statepoint)) {
        InsertClobbersAt(&*II->getNormalDest()->getFirstInsertionPt());
        if (!Info.UsesExplicitExceptionRoots)
          InsertClobbersAt(&*II->getUnwindDest()->getFirstInsertionPt());
      } else {
        InsertClobbersAt(cast<Instruction>(Statepoint)->getNextNode());
      }
    }
  }

  // Update use with load allocas and add store for gc_relocated.
  for (auto Pair : AllocaMap) {
    Value *Def = Pair.first;
    AllocaInst *Alloca = Pair.second;

    // We pre-record the uses of allocas so that we dont have to worry about
    // later update that changes the user information..

    SmallVector<Instruction *, 20> Uses;
    // PERF: trade a linear scan for repeated reallocation
    Uses.reserve(Def->getNumUses());
    for (User *U : Def->users()) {
      if (auto *I = dyn_cast<Instruction>(U)) {
        Uses.push_back(I);
      } else {
        // If the def has a constant use, then the def is either a constant use
        // itself or null.  In either case (recursively in the first, directly
        // in the second), the oop it is ultimately dependent on is null and
        // this particular use does not need to be fixed up.
        assert(isa<Constant>(U) &&
               "expected non-instruction user to be a constant");
      }
    }

    llvm::sort(Uses);
    auto Last = std::unique(Uses.begin(), Uses.end());
    Uses.erase(Last, Uses.end());

    for (Instruction *Use : Uses) {
      if (isa<PHINode>(Use)) {
        PHINode *Phi = cast<PHINode>(Use);
        for (unsigned i = 0; i < Phi->getNumIncomingValues(); i++) {
          if (Def == Phi->getIncomingValue(i)) {
            LoadInst *Load =
                new LoadInst(Alloca->getAllocatedType(), Alloca, "",
                             Phi->getIncomingBlock(i)->getTerminator());
            Phi->setIncomingValue(i, Load);
          }
        }
      } else {
        LoadInst *Load =
            new LoadInst(Alloca->getAllocatedType(), Alloca, "", Use);
        Use->replaceUsesOfWith(Def, Load);
      }
    }

    // Emit store for the initial gc value.  Store must be inserted after load,
    // otherwise store will be in alloca's use list and an extra load will be
    // inserted before it.
    StoreInst *Store = new StoreInst(Def, Alloca, /*volatile*/ false,
                                     DL.getABITypeAlign(Def->getType()));
    if (Instruction *Inst = dyn_cast<Instruction>(Def)) {
      if (auto *PN = dyn_cast<PHINode>(Inst)) {
        Store->insertBefore(PN->getParent()->getFirstNonPHI());
      } else if (InvokeInst *Invoke = dyn_cast<InvokeInst>(Inst)) {
        // InvokeInst is a terminator so the store need to be inserted into its
        // normal destination block.
        BasicBlock *NormalDest = Invoke->getNormalDest();
        Store->insertBefore(NormalDest->getFirstNonPHI());
      } else {
        assert(!Inst->isTerminator() &&
               "The only terminator that can produce a value is "
               "InvokeInst which is handled above.");
        Store->insertAfter(Inst);
      }
    } else {
      assert((isa<Argument>(Def) || isa<Constant>(Def)) &&
             "expected non-instruction live value to be an argument or "
             "constant");
      Store->insertAfter(cast<Instruction>(Alloca));
    }
  }

  assert(PromotableAllocas.size() == Live.size() + NumRematerializedValues &&
         "we must have the same allocas with lives");
  (void) NumRematerializedValues;
  if (!PromotableAllocas.empty()) {
    // Apply mem2reg to promote alloca to SSA
    PromoteMemToReg(PromotableAllocas, DT);
  }

#ifndef NDEBUG
  for (auto &I : F.getEntryBlock())
    if (isa<AllocaInst>(I))
      InitialAllocaNum--;
  assert(InitialAllocaNum == 0 && "We must not introduce any extra allocas");
#endif
}

static void volatileRootAllocaViaStatepointLiveSet(
    Function &F, DominatorTree &DT,
    MutableArrayRef<PartiallyConstructedSafepointRecord> Records,
    const PointerToBaseTy &PointerToBase) {
  const DataLayout &DL = F.getParent()->getDataLayout();
  BasicBlock &EntryBlock = F.getEntryBlock();
  IRBuilder<> EntryBuilder(&EntryBlock, EntryBlock.getFirstInsertionPt());

  DenseMap<Value *, AllocaInst *> AllocaMap;
  DenseMap<Value *, SmallVector<Instruction *, 4>> StatepointsByRootValue;
  SmallVector<Value *, 64> RootValues;
  SmallPtrSet<Value *, 32> SeenRootValues;

  auto RootValueFor = [&](Value *V) -> Value * {
    auto It = PointerToBase.find(V);
    if (It != PointerToBase.end())
      return It->second;
    return V;
  };

  auto RememberLiveValueAt = [&](Value *V, Instruction *Statepoint) {
    if (!isHandledGCPointerType(V->getType()))
      return;
    if (isa<Constant>(V))
      return;
    Value *Root = RootValueFor(V);
    if (isa<Constant>(Root))
      return;
    if (SeenRootValues.insert(Root).second)
      RootValues.push_back(Root);
    StatepointsByRootValue[Root].push_back(Statepoint);
  };

  for (PartiallyConstructedSafepointRecord &Info : Records)
    for (Value *V : Info.StatepointToken->gc_args())
      RememberLiveValueAt(V, Info.StatepointToken);

  auto InsertVolatileStore = [&](Value *Stored, AllocaInst *Slot,
                                 Instruction *InsertBefore) {
    IRBuilder<> Builder(InsertBefore);
    StoreInst *Store = Builder.CreateAlignedStore(
        Stored, Slot, DL.getABITypeAlign(Stored->getType()));
    Store->setVolatile(true);
  };

  auto InsertVolatileLoad = [&](AllocaInst *Slot, Instruction *InsertBefore,
                                Twine Name = "") -> LoadInst * {
    IRBuilder<> Builder(InsertBefore);
    LoadInst *Load = Builder.CreateAlignedLoad(
        Slot->getAllocatedType(), Slot, DL.getABITypeAlign(
                                           Slot->getAllocatedType()),
        Name);
    Load->setVolatile(true);
    return Load;
  };

  auto InsertInitialStore = [&](Value *Def, AllocaInst *Slot) {
    if (auto *Inst = dyn_cast<Instruction>(Def)) {
      if (auto *PN = dyn_cast<PHINode>(Inst)) {
        InsertVolatileStore(Def, Slot, &*PN->getParent()->getFirstInsertionPt());
      } else if (auto *II = dyn_cast<InvokeInst>(Inst)) {
        InsertVolatileStore(Def, Slot,
                            &*II->getNormalDest()->getFirstInsertionPt());
      } else {
        assert(!Inst->isTerminator() &&
               "only invoke terminators can define root values");
        InsertVolatileStore(Def, Slot, Inst->getNextNode());
      }
      return;
    }

    assert((isa<Argument>(Def) || isa<Constant>(Def)) &&
           "expected root to be an argument, constant, or instruction");
    InsertVolatileStore(Def, Slot, &*EntryBuilder.GetInsertPoint());
  };

  for (Value *RootValue : RootValues) {
    AllocaInst *Slot = EntryBuilder.CreateAlloca(
        RootValue->getType(), DL.getAllocaAddrSpace(), nullptr,
        suffixed_name_or(RootValue, ".root", "root"));
    Slot->setAlignment(DL.getABITypeAlign(RootValue->getType()));

    StoreInst *InitialStore = EntryBuilder.CreateAlignedStore(
        getOxCamlNonMovingImmediate(RootValue->getType()), Slot,
        DL.getABITypeAlign(RootValue->getType()));
    InitialStore->setVolatile(true);

    AllocaMap[RootValue] = Slot;
    InsertInitialStore(RootValue, Slot);
  }

  for (PartiallyConstructedSafepointRecord &Info : Records) {
    SmallVector<OperandBundleDef, 4> Bundles;
    Info.StatepointToken->getOperandBundlesAsDefs(Bundles);
    bool ChangedThisStatepoint = false;

    for (OperandBundleDef &Bundle : Bundles) {
      if (Bundle.getTag() != "gc-live")
        continue;

      SmallVector<Value *, 64> Inputs;
      SmallPtrSet<Value *, 32> Seen;
      for (Value *Input : make_range(Bundle.input_begin(),
                                     Bundle.input_end())) {
        if (isHandledGCPointerType(Input->getType()))
          Input = RootValueFor(Input);
        if (auto It = AllocaMap.find(Input); It != AllocaMap.end()) {
          Input = It->second;
          ChangedThisStatepoint = true;
        }
        if (Seen.insert(Input).second)
          Inputs.push_back(Input);
      }
      Bundle = OperandBundleDef("gc-live", Inputs);
      break;
    }

    if (!ChangedThisStatepoint)
      continue;

    CallBase *OldCall = Info.StatepointToken;
    CallBase *NewCall = CallBase::Create(OldCall, Bundles, OldCall);
    OldCall->replaceAllUsesWith(NewCall);
    NewCall->takeName(OldCall);
    for (Value *RootValue : RootValues) {
      auto It = StatepointsByRootValue.find(RootValue);
      if (It == StatepointsByRootValue.end())
        continue;
      for (Instruction *&Statepoint : It->second)
        if (Statepoint == OldCall)
          Statepoint = NewCall;
    }
    OldCall->eraseFromParent();
    Info.StatepointToken = cast<GCStatepointInst>(NewCall);
  }

  auto StatepointCanReachItself = [&](Instruction *Statepoint) {
    if (auto *II = dyn_cast<InvokeInst>(Statepoint)) {
      BasicBlock *BB = II->getParent();
      return isPotentiallyReachable(II->getNormalDest(), BB, nullptr, &DT) ||
             isPotentiallyReachable(II->getUnwindDest(), BB, nullptr, &DT);
    }

    Instruction *Next = Statepoint->getNextNode();
    return Next && isPotentiallyReachable(Next, Statepoint, nullptr, &DT);
  };

  auto StatepointIsBeforeInstruction = [&](Instruction *Statepoint,
                                           Instruction *Use) {
    if (Statepoint != Use && DT.dominates(Statepoint, Use))
      return true;
    auto *II = dyn_cast<InvokeInst>(Statepoint);
    if (II) {
      BasicBlock *UseBB = Use->getParent();
      if (UseBB == II->getParent())
        return false;
      return isPotentiallyReachable(II->getNormalDest(), UseBB, nullptr, &DT) ||
             isPotentiallyReachable(II->getUnwindDest(), UseBB, nullptr, &DT);
    }

    Instruction *Next = Statepoint->getNextNode();
    return Next && isPotentiallyReachable(Next, Use, nullptr, &DT);
  };

  auto IsUseAfterRootingStatepoint = [&](Value *RootValue, Instruction *Use) {
    auto It = StatepointsByRootValue.find(RootValue);
    if (It == StatepointsByRootValue.end())
      return false;
    for (Instruction *Statepoint : It->second)
      if (StatepointIsBeforeInstruction(Statepoint, Use) ||
          (Statepoint == Use && StatepointCanReachItself(Statepoint)))
        return true;
    return false;
  };

  auto IsEdgeUseAfterRootingStatepoint = [&](Value *RootValue,
                                             Instruction *EdgeTerminator) {
    auto It = StatepointsByRootValue.find(RootValue);
    if (It == StatepointsByRootValue.end())
      return false;
    for (Instruction *Statepoint : It->second)
      if (StatepointIsBeforeInstruction(Statepoint, EdgeTerminator))
        return true;
    return false;
  };

  SmallVector<Value *, 128> RewriteValues;
  SmallPtrSet<Value *, 32> SeenRewriteValues;
  auto MaybeAddRewriteValue = [&](Value *V) {
    if (!isHandledGCPointerType(V->getType()))
      return;
    if (isa<Constant>(V))
      return;
    Value *Root = RootValueFor(V);
    if (AllocaMap.find(Root) == AllocaMap.end())
      return;
    if (SeenRewriteValues.insert(V).second)
      RewriteValues.push_back(V);
  };

  for (Argument &Arg : F.args())
    MaybeAddRewriteValue(&Arg);
  for (BasicBlock &BB : F)
    for (Instruction &I : BB)
      MaybeAddRewriteValue(&I);

  for (Value *Def : RewriteValues) {
    Value *RootValue = RootValueFor(Def);
    AllocaInst *Slot = AllocaMap[RootValue];
    SmallVector<Instruction *, 20> Uses;
    for (User *U : Def->users())
      if (auto *I = dyn_cast<Instruction>(U))
        Uses.push_back(I);
    llvm::sort(Uses);
    Uses.erase(std::unique(Uses.begin(), Uses.end()), Uses.end());

    for (Instruction *Use : Uses) {
      if (auto *Store = dyn_cast<StoreInst>(Use))
        if (Store->getPointerOperand() == Slot)
          continue;

      if (auto *Phi = dyn_cast<PHINode>(Use)) {
        for (unsigned I = 0, E = Phi->getNumIncomingValues(); I != E; ++I) {
          if (Phi->getIncomingValue(I) != Def)
            continue;
          Instruction *Term = Phi->getIncomingBlock(I)->getTerminator();
          if (!IsEdgeUseAfterRootingStatepoint(RootValue, Term))
            continue;
          LoadInst *Load =
              InsertVolatileLoad(Slot, Term,
                                 suffixed_name_or(RootValue, ".root.load",
                                                  "root.load"));
          Phi->setIncomingValue(I, Load);
        }
        continue;
      }

      if (!IsUseAfterRootingStatepoint(RootValue, Use))
        continue;
      LoadInst *Load =
          InsertVolatileLoad(Slot, Use,
                             suffixed_name_or(RootValue, ".root.load",
                                              "root.load"));
      Use->replaceUsesOfWith(Def, Load);
    }
  }
}

/// Implement a unique function which doesn't require we sort the input
/// vector.  Doing so has the effect of changing the output of a couple of
/// tests in ways which make them less useful in testing fused safepoints.
template <typename T> static void unique_unsorted(SmallVectorImpl<T> &Vec) {
  SmallSet<T, 8> Seen;
  erase_if(Vec, [&](const T &V) { return !Seen.insert(V).second; });
}

/// Insert holders so that each Value is obviously live through the entire
/// lifetime of the call.
static void insertUseHolderAfter(CallBase *Call, const ArrayRef<Value *> Values,
                                 SmallVectorImpl<CallInst *> &Holders,
                                 bool HoldExceptionalDest = true) {
  if (Values.empty())
    // No values to hold live, might as well not insert the empty holder
    return;

  Module *M = Call->getModule();
  // Use a dummy vararg function to actually hold the values live
  FunctionCallee Func = M->getOrInsertFunction(
      "__tmp_use", FunctionType::get(Type::getVoidTy(M->getContext()), true));
  if (isa<CallInst>(Call)) {
    // For call safepoints insert dummy calls right after safepoint
    Holders.push_back(
        CallInst::Create(Func, Values, "", &*++Call->getIterator()));
    return;
  }
  // For invoke safepooints insert dummy calls both in normal and
  // exceptional destination blocks
  auto *II = cast<InvokeInst>(Call);
  Holders.push_back(CallInst::Create(
      Func, Values, "", &*II->getNormalDest()->getFirstInsertionPt()));
  if (HoldExceptionalDest)
    Holders.push_back(CallInst::Create(
        Func, Values, "", &*II->getUnwindDest()->getFirstInsertionPt()));
}

static void findLiveReferences(
    Function &F, DominatorTree &DT, ArrayRef<CallBase *> toUpdate,
    MutableArrayRef<struct PartiallyConstructedSafepointRecord> records) {
  GCPtrLivenessData OriginalLivenessData;
  computeLiveInValues(DT, F, OriginalLivenessData);
  for (size_t i = 0; i < records.size(); i++) {
    struct PartiallyConstructedSafepointRecord &info = records[i];
    analyzeParsePointLiveness(DT, OriginalLivenessData, toUpdate[i], info);
  }
}

// Helper function for the "rematerializeLiveValues". It walks use chain
// starting from the "CurrentValue" until it reaches the root of the chain, i.e.
// the base or a value it cannot process. Only "simple" values are processed
// (currently it is GEP's and casts). The returned root is examined by the
// callers of findRematerializableChainToBasePointer.  Fills "ChainToBase" array
// with all visited values.
static Value* findRematerializableChainToBasePointer(
  SmallVectorImpl<Instruction*> &ChainToBase,
  Value *CurrentValue,
  bool AllowIntegerAddressArithmetic) {
  if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(CurrentValue)) {
    ChainToBase.push_back(GEP);
    return findRematerializableChainToBasePointer(ChainToBase,
                                                  GEP->getPointerOperand(),
                                                  AllowIntegerAddressArithmetic);
  }

  if (CastInst *CI = dyn_cast<CastInst>(CurrentValue)) {
    auto *IntToPtrDstTy = dyn_cast<PointerType>(CI->getType());
    auto *PtrToIntSrcTy = dyn_cast<PointerType>(CI->getOperand(0)->getType());
    bool IsAddrSpace1IntToPtr = AllowIntegerAddressArithmetic &&
                                isa<IntToPtrInst>(CI) && IntToPtrDstTy &&
                                IntToPtrDstTy->getAddressSpace() == 1;
    bool IsAddrSpace1PtrToInt = AllowIntegerAddressArithmetic &&
                                isa<PtrToIntInst>(CI) && PtrToIntSrcTy &&
                                PtrToIntSrcTy->getAddressSpace() == 1;
    if (!IsAddrSpace1IntToPtr && !IsAddrSpace1PtrToInt &&
        !CI->isNoopCast(CI->getModule()->getDataLayout()))
      return CI;

    ChainToBase.push_back(CI);
    return findRematerializableChainToBasePointer(ChainToBase,
                                                  CI->getOperand(0),
                                                  AllowIntegerAddressArithmetic);
  }

  if (AllowIntegerAddressArithmetic) {
    if (auto *BO = dyn_cast<BinaryOperator>(CurrentValue)) {
      if (BO->getOpcode() != Instruction::Add &&
          BO->getOpcode() != Instruction::Sub)
        return BO;

      if (isa<ConstantInt>(BO->getOperand(1))) {
        ChainToBase.push_back(BO);
        return findRematerializableChainToBasePointer(
            ChainToBase, BO->getOperand(0), AllowIntegerAddressArithmetic);
      }

      if (BO->getOpcode() == Instruction::Add &&
          isa<ConstantInt>(BO->getOperand(0))) {
        ChainToBase.push_back(BO);
        return findRematerializableChainToBasePointer(
            ChainToBase, BO->getOperand(1), AllowIntegerAddressArithmetic);
      }

      return BO;
    }

    if (FreezeInst *Freeze = dyn_cast<FreezeInst>(CurrentValue)) {
      ChainToBase.push_back(Freeze);
      return findRematerializableChainToBasePointer(
          ChainToBase, Freeze->getOperand(0), AllowIntegerAddressArithmetic);
    }
  }

  // We have reached the root of the chain, which is either equal to the base or
  // is the first unsupported value along the use chain.
  return CurrentValue;
}

static bool findRematerializableChainToSpecificBase(
    SmallVectorImpl<Instruction *> &ChainToBase, Value *CurrentValue,
    Value *Base, bool AllowIntegerAddressArithmetic) {
  if (CurrentValue == Base)
    return true;

  unsigned OriginalSize = ChainToBase.size();
  auto TryAppendAndRecurse = [&](Instruction *Instr, Value *NextValue) {
    ChainToBase.push_back(Instr);
    if (findRematerializableChainToSpecificBase(
            ChainToBase, NextValue, Base, AllowIntegerAddressArithmetic))
      return true;
    ChainToBase.resize(OriginalSize);
    return false;
  };

  if (auto *GEP = dyn_cast<GetElementPtrInst>(CurrentValue))
    return TryAppendAndRecurse(GEP, GEP->getPointerOperand());

  if (auto *CI = dyn_cast<CastInst>(CurrentValue)) {
    auto *IntToPtrDstTy = dyn_cast<PointerType>(CI->getType());
    auto *PtrToIntSrcTy = dyn_cast<PointerType>(CI->getOperand(0)->getType());
    bool IsAddrSpace1IntToPtr = AllowIntegerAddressArithmetic &&
                                isa<IntToPtrInst>(CI) && IntToPtrDstTy &&
                                IntToPtrDstTy->getAddressSpace() == 1;
    bool IsAddrSpace1PtrToInt = AllowIntegerAddressArithmetic &&
                                isa<PtrToIntInst>(CI) && PtrToIntSrcTy &&
                                PtrToIntSrcTy->getAddressSpace() == 1;
    if (!IsAddrSpace1IntToPtr && !IsAddrSpace1PtrToInt &&
        !CI->isNoopCast(CI->getModule()->getDataLayout()))
      return false;

    return TryAppendAndRecurse(CI, CI->getOperand(0));
  }

  if (AllowIntegerAddressArithmetic) {
    if (auto *BO = dyn_cast<BinaryOperator>(CurrentValue)) {
      if (BO->getOpcode() != Instruction::Add &&
          BO->getOpcode() != Instruction::Sub)
        return false;

      if (isa<ConstantInt>(BO->getOperand(1)))
        return TryAppendAndRecurse(BO, BO->getOperand(0));

      if (BO->getOpcode() == Instruction::Add &&
          isa<ConstantInt>(BO->getOperand(0)))
        return TryAppendAndRecurse(BO, BO->getOperand(1));

      return false;
    }

    if (auto *Freeze = dyn_cast<FreezeInst>(CurrentValue))
      return TryAppendAndRecurse(Freeze, Freeze->getOperand(0));
  }

  return false;
}

static bool areEquivalentRematerializableInstructions(Instruction *A,
                                                      Instruction *B) {
  if (A->getOpcode() != B->getOpcode() || A->getType() != B->getType())
    return false;

  if (auto *AGEP = dyn_cast<GetElementPtrInst>(A)) {
    auto *BGEP = dyn_cast<GetElementPtrInst>(B);
    if (!BGEP || AGEP->getSourceElementType() != BGEP->getSourceElementType())
      return false;
    if (AGEP->getNumIndices() != BGEP->getNumIndices())
      return false;

    auto AIt = AGEP->idx_begin();
    auto BIt = BGEP->idx_begin();
    for (auto AE = AGEP->idx_end(); AIt != AE; ++AIt, ++BIt) {
      auto *AConst = dyn_cast<ConstantInt>(AIt->get());
      auto *BConst = dyn_cast<ConstantInt>(BIt->get());
      if (!AConst || !BConst || AConst->getValue() != BConst->getValue())
        return false;
    }
    return true;
  }

  if (auto *ACast = dyn_cast<CastInst>(A)) {
    auto *BCast = dyn_cast<CastInst>(B);
    return BCast && ACast->getSrcTy() == BCast->getSrcTy() &&
           ACast->getDestTy() == BCast->getDestTy();
  }

  if (auto *ABO = dyn_cast<BinaryOperator>(A)) {
    auto *BBO = dyn_cast<BinaryOperator>(B);
    if (!BBO || ABO->getOpcode() != BBO->getOpcode())
      return false;
    auto SameConstantOperand = [](Value *A, Value *B) {
      auto *AConst = dyn_cast<ConstantInt>(A);
      auto *BConst = dyn_cast<ConstantInt>(B);
      return AConst && BConst && AConst->getValue() == BConst->getValue();
    };
    return SameConstantOperand(ABO->getOperand(0), BBO->getOperand(0)) ||
           SameConstantOperand(ABO->getOperand(1), BBO->getOperand(1));
  }

  if (isa<FreezeInst>(A))
    return isa<FreezeInst>(B);

  return false;
}

static Value *findRematerializablePhiChainToBasePhiImpl(
    SmallVectorImpl<Instruction *> &ChainToBase, PHINode *DerivedPhi,
    PHINode *BasePhi, const PointerToBaseTy &PointerToBase,
    bool AllowIntegerAddressArithmetic, bool &NeedsBaseSelect,
    SmallPtrSetImpl<PHINode *> &ActiveDerivedPhis) {
  auto DebugReject = [&](StringRef Reason, unsigned IncomingIndex,
                         Value *Incoming, Value *Root, Value *IncomingBase,
                         ArrayRef<Instruction *> IncomingChain) {
    if (!DebugOxCamlDerivedRemat)
      return;
    errs() << "rs4gc-oxcaml-phi-chain-reject " << Reason;
    if (IncomingIndex != std::numeric_limits<unsigned>::max())
      errs() << " incoming " << IncomingIndex;
    errs() << "\n  derived-phi: ";
    DerivedPhi->print(errs());
    errs() << "\n  base-phi: ";
    BasePhi->print(errs());
    errs() << "\n";
    if (Incoming) {
      errs() << "  incoming: ";
      Incoming->print(errs());
      errs() << "\n";
    }
    if (Root) {
      errs() << "  root: ";
      Root->print(errs());
      errs() << "\n";
    }
    if (IncomingBase) {
      errs() << "  incoming-base: ";
      IncomingBase->print(errs());
      errs() << "\n";
    }
    errs() << "  incoming-chain-size: " << IncomingChain.size() << "\n";
    if (Incoming) {
      if (auto It = PointerToBase.find(Incoming); It != PointerToBase.end()) {
        errs() << "  incoming-pointer-to-base: ";
        It->second->print(errs());
        errs() << "\n";
      }
    }
    if (Root) {
      if (auto It = PointerToBase.find(Root); It != PointerToBase.end()) {
        errs() << "  root-pointer-to-base: ";
        It->second->print(errs());
        errs() << "\n";
      }
    }
  };
  if (!ActiveDerivedPhis.insert(DerivedPhi).second) {
    DebugReject("cycle", std::numeric_limits<unsigned>::max(), nullptr,
                nullptr, nullptr, {});
    return nullptr;
  }
  auto RemoveActiveDerivedPhi =
      make_scope_exit([&]() { ActiveDerivedPhis.erase(DerivedPhi); });

  NeedsBaseSelect = false;
  if (DerivedPhi->getParent() != BasePhi->getParent() ||
      DerivedPhi->getNumIncomingValues() != BasePhi->getNumIncomingValues()) {
    DebugReject("shape", std::numeric_limits<unsigned>::max(), nullptr,
                nullptr, nullptr, {});
    return nullptr;
  }

  auto RootMatchesIncomingBase = [&](Value *Root, Value *IncomingBase) {
    if (Root == IncomingBase)
      return true;
    auto It = PointerToBase.find(Root);
    if (It != PointerToBase.end() && It->second == IncomingBase)
      return true;
    return isOxCamlBaseEquivalentGCPointer(Root, IncomingBase);
  };
  auto IncomingChainMatchesBase = [&](Value *Incoming, Value *Root,
                                      Value *IncomingBase,
                                      ArrayRef<Instruction *> IncomingChain) {
    if (IncomingChain.empty())
      return Incoming == IncomingBase ||
             isOxCamlBaseEquivalentGCPointer(Incoming, IncomingBase);
    if (RootMatchesIncomingBase(Root, IncomingBase))
      return true;
    // A rematerialized incoming may be based on a relocated copy of
    // IncomingBase.  The rematerialized value itself remains mapped to
    // IncomingBase in PointerToBase, while the root of its syntactic chain is
    // the relocated base value.  Accept that shape and rematerialize the same
    // chain from the current statepoint's relocated base phi.
    auto It = PointerToBase.find(Incoming);
    return !IncomingChain.empty() && It != PointerToBase.end() &&
           It->second == IncomingBase;
  };
  auto InstructionUsesChainOperand = [](Instruction *Instr, Value *Operand) {
    if (auto *GEP = dyn_cast<GetElementPtrInst>(Instr))
      return GEP->getPointerOperand() == Operand;
    if (auto *CI = dyn_cast<CastInst>(Instr))
      return CI->getOperand(0) == Operand;
    if (auto *Freeze = dyn_cast<FreezeInst>(Instr))
      return Freeze->getOperand(0) == Operand;
    if (auto *BO = dyn_cast<BinaryOperator>(Instr))
      return BO->getOperand(0) == Operand || BO->getOperand(1) == Operand;
    return false;
  };
  auto TrimChainToIncomingBase = [&](SmallVectorImpl<Instruction *> &Chain,
                                     Value *&Root, Value *IncomingBase) {
    for (unsigned I = 0, E = Chain.size(); I != E; ++I) {
      if (!InstructionUsesChainOperand(Chain[I], IncomingBase))
        continue;
      Chain.resize(I + 1);
      Root = IncomingBase;
      return true;
    }
    return false;
  };

  SmallVector<Instruction *, 3> RepresentativeChain;
  Value *RepresentativeRoot = nullptr;
  bool SawDerivedChain = false;
  bool SawBaseValueIncoming = false;

  for (unsigned I = 0, E = DerivedPhi->getNumIncomingValues(); I != E; ++I) {
    Value *Incoming = DerivedPhi->getIncomingValue(I);
    Value *IncomingBase = BasePhi->getIncomingValue(I);
    if (DerivedPhi->getIncomingBlock(I) != BasePhi->getIncomingBlock(I)) {
      DebugReject("incoming-block", I, Incoming, nullptr, IncomingBase, {});
      return nullptr;
    }

    if (Incoming == IncomingBase) {
      SawBaseValueIncoming = true;
      continue;
    }

    SmallVector<Instruction *, 3> IncomingChain;
    Value *IncomingRoot = IncomingBase;
    if (!findRematerializableChainToSpecificBase(
            IncomingChain, Incoming, IncomingBase,
            AllowIntegerAddressArithmetic)) {
      IncomingRoot = findRematerializableChainToBasePointer(
          IncomingChain, Incoming, AllowIntegerAddressArithmetic);
      if (!RootMatchesIncomingBase(IncomingRoot, IncomingBase))
        TrimChainToIncomingBase(IncomingChain, IncomingRoot, IncomingBase);
    }
    bool IncomingMatchesByNestedPhi = false;
    if (!IncomingChainMatchesBase(Incoming, IncomingRoot, IncomingBase,
                                  IncomingChain)) {
      auto *IncomingDerivedPhi =
          dyn_cast<PHINode>(Incoming);
      auto *IncomingBasePhi = dyn_cast<PHINode>(IncomingBase);
      if (IncomingDerivedPhi && IncomingBasePhi) {
        SmallVector<Instruction *, 3> NestedIncomingChain;
        bool NestedNeedsBaseSelect = false;
        if (Value *NestedIncomingRoot =
                findRematerializablePhiChainToBasePhiImpl(
                    NestedIncomingChain, IncomingDerivedPhi, IncomingBasePhi,
                    PointerToBase, AllowIntegerAddressArithmetic,
                    NestedNeedsBaseSelect, ActiveDerivedPhis)) {
          IncomingChain = NestedIncomingChain;
          IncomingRoot = NestedIncomingRoot;
          IncomingMatchesByNestedPhi = true;
          NeedsBaseSelect |= NestedNeedsBaseSelect;
        }
      }
    }
    if (!IncomingMatchesByNestedPhi &&
        !IncomingChainMatchesBase(Incoming, IncomingRoot, IncomingBase,
                                  IncomingChain)) {
      DebugReject("incoming-root", I, Incoming, IncomingRoot, IncomingBase,
                  IncomingChain);
      return nullptr;
    }

    if (IncomingChain.empty()) {
      if (Incoming != IncomingBase) {
        DebugReject("empty-chain", I, Incoming, IncomingRoot, IncomingBase,
                    IncomingChain);
        return nullptr;
      }
      SawBaseValueIncoming = true;
      continue;
    }

    if (!SawDerivedChain) {
      RepresentativeChain = IncomingChain;
      RepresentativeRoot = IncomingRoot;
      SawDerivedChain = true;
      continue;
    }

    if (IncomingChain.size() != RepresentativeChain.size())
      return nullptr;
    for (unsigned ChainIndex = 0, ChainEnd = IncomingChain.size();
         ChainIndex != ChainEnd; ++ChainIndex)
      if (!areEquivalentRematerializableInstructions(
              IncomingChain[ChainIndex], RepresentativeChain[ChainIndex]))
        return nullptr;
  }

  if (!SawDerivedChain) {
    DebugReject("no-derived-chain", std::numeric_limits<unsigned>::max(),
                nullptr, nullptr, nullptr, {});
    return nullptr;
  }

  NeedsBaseSelect |= SawBaseValueIncoming;
  llvm::append_range(ChainToBase, RepresentativeChain);
  return RepresentativeRoot;
}

static Value *findRematerializablePhiChainToBasePhi(
    SmallVectorImpl<Instruction *> &ChainToBase, PHINode *DerivedPhi,
    PHINode *BasePhi, const PointerToBaseTy &PointerToBase,
    bool AllowIntegerAddressArithmetic, bool &NeedsBaseSelect) {
  SmallPtrSet<PHINode *, 8> ActiveDerivedPhis;
  return findRematerializablePhiChainToBasePhiImpl(
      ChainToBase, DerivedPhi, BasePhi, PointerToBase,
      AllowIntegerAddressArithmetic, NeedsBaseSelect, ActiveDerivedPhis);
}

static Value *findRematerializablePhiChainToSingleBase(
    SmallVectorImpl<Instruction *> &ChainToBase, PHINode *DerivedPhi,
    Value *Base, const PointerToBaseTy &PointerToBase,
    bool AllowIntegerAddressArithmetic, bool &NeedsBaseSelect) {
  SmallPtrSet<PHINode *, 8> ActiveDerivedPhis;

  auto FindPhiChainToSingleBase =
      [&](auto &&Self, SmallVectorImpl<Instruction *> &OutChainToBase,
          PHINode *Phi, bool &OutNeedsBaseSelect) -> Value * {
    auto DebugReject = [&](StringRef Reason, unsigned IncomingIndex,
                           Value *Incoming, Value *Root,
                           ArrayRef<Instruction *> IncomingChain) {
      if (!DebugOxCamlDerivedRemat)
        return;
      errs() << "rs4gc-oxcaml-phi-single-base-chain-reject " << Reason;
      if (IncomingIndex != std::numeric_limits<unsigned>::max())
        errs() << " incoming " << IncomingIndex;
      errs() << "\n  derived-phi: ";
      Phi->print(errs());
      errs() << "\n  base: ";
      Base->print(errs());
      errs() << "\n";
      if (Incoming) {
        errs() << "  incoming: ";
        Incoming->print(errs());
        errs() << "\n";
      }
      if (Root) {
        errs() << "  root: ";
        Root->print(errs());
        errs() << "\n";
      }
      errs() << "  incoming-chain-size: " << IncomingChain.size() << "\n";
      if (Incoming) {
        if (auto It = PointerToBase.find(Incoming); It != PointerToBase.end()) {
          errs() << "  incoming-pointer-to-base: ";
          It->second->print(errs());
          errs() << "\n";
        }
      }
      if (Root) {
        if (auto It = PointerToBase.find(Root); It != PointerToBase.end()) {
          errs() << "  root-pointer-to-base: ";
          It->second->print(errs());
          errs() << "\n";
        }
      }
    };

    if (!ActiveDerivedPhis.insert(Phi).second) {
      DebugReject("cycle", std::numeric_limits<unsigned>::max(), nullptr,
                  nullptr, {});
      return nullptr;
    }
    auto RemoveActiveDerivedPhi =
        make_scope_exit([&]() { ActiveDerivedPhis.erase(Phi); });

    auto IsBaseValue = [&](Value *V) {
      return V == Base || isOxCamlBaseEquivalentGCPointer(V, Base);
    };
    auto RootMatchesBase = [&](Value *Root) {
      if (IsBaseValue(Root))
        return true;
      auto It = PointerToBase.find(Root);
      return It != PointerToBase.end() && It->second == Base;
    };
    auto IncomingChainMatchesBase =
        [&](Value *Incoming, Value *Root,
            ArrayRef<Instruction *> IncomingChain) {
      if (IncomingChain.empty())
        return IsBaseValue(Incoming);
      if (RootMatchesBase(Root))
        return true;
      // Earlier statepoints can rematerialize PHI incoming values from a
      // relocated copy of Base. The incoming rematerialized value remains
      // mapped to Base even though its syntactic root is the earlier relocate.
      auto It = PointerToBase.find(Incoming);
      return It != PointerToBase.end() && It->second == Base;
    };

    SmallVector<Instruction *, 3> RepresentativeChain;
    Value *RepresentativeRoot = nullptr;
    bool SawDerivedChain = false;
    bool SawBaseValueIncoming = false;

    for (unsigned I = 0, E = Phi->getNumIncomingValues(); I != E; ++I) {
      Value *Incoming = Phi->getIncomingValue(I);
      if (IsBaseValue(Incoming)) {
        SawBaseValueIncoming = true;
        continue;
      }

      SmallVector<Instruction *, 3> IncomingChain;
      Value *IncomingRoot = Base;
      if (!findRematerializableChainToSpecificBase(
              IncomingChain, Incoming, Base, AllowIntegerAddressArithmetic))
        IncomingRoot = findRematerializableChainToBasePointer(
            IncomingChain, Incoming, AllowIntegerAddressArithmetic);

      bool IncomingMatchesByNestedPhi = false;
      if (!IncomingChainMatchesBase(Incoming, IncomingRoot, IncomingChain)) {
        if (auto *IncomingPhi = dyn_cast<PHINode>(Incoming)) {
          SmallVector<Instruction *, 3> NestedIncomingChain;
          bool NestedNeedsBaseSelect = false;
          if (Value *NestedIncomingRoot =
                  Self(Self, NestedIncomingChain, IncomingPhi,
                       NestedNeedsBaseSelect)) {
            IncomingChain = NestedIncomingChain;
            IncomingRoot = NestedIncomingRoot;
            IncomingMatchesByNestedPhi = true;
            OutNeedsBaseSelect |= NestedNeedsBaseSelect;
          }
        }
      }

      if (!IncomingMatchesByNestedPhi &&
          !IncomingChainMatchesBase(Incoming, IncomingRoot, IncomingChain)) {
        DebugReject("incoming-root", I, Incoming, IncomingRoot, IncomingChain);
        return nullptr;
      }

      if (IncomingChain.empty()) {
        SawBaseValueIncoming = true;
        continue;
      }

      if (!SawDerivedChain) {
        RepresentativeChain = IncomingChain;
        RepresentativeRoot = IncomingRoot;
        SawDerivedChain = true;
        continue;
      }

      if (IncomingChain.size() != RepresentativeChain.size()) {
        DebugReject("chain-size", I, Incoming, IncomingRoot, IncomingChain);
        return nullptr;
      }
      for (unsigned ChainIndex = 0, ChainEnd = IncomingChain.size();
           ChainIndex != ChainEnd; ++ChainIndex) {
        if (areEquivalentRematerializableInstructions(
                IncomingChain[ChainIndex], RepresentativeChain[ChainIndex]))
          continue;
        DebugReject("chain-instruction", I, Incoming, IncomingRoot,
                    IncomingChain);
        return nullptr;
      }
    }

    if (!SawDerivedChain) {
      DebugReject("no-derived-chain", std::numeric_limits<unsigned>::max(),
                  nullptr, nullptr, {});
      return nullptr;
    }

    OutNeedsBaseSelect |= SawBaseValueIncoming;
    llvm::append_range(OutChainToBase, RepresentativeChain);
    return RepresentativeRoot;
  };

  NeedsBaseSelect = false;
  return FindPhiChainToSingleBase(FindPhiChainToSingleBase, ChainToBase,
                                  DerivedPhi, NeedsBaseSelect);
}

// Helper function for the "rematerializeLiveValues". Compute cost of the use
// chain we are going to rematerialize.
static InstructionCost
chainToBasePointerCost(SmallVectorImpl<Instruction *> &Chain,
                       TargetTransformInfo &TTI) {
  InstructionCost Cost = 0;

  for (Instruction *Instr : Chain) {
    if (CastInst *CI = dyn_cast<CastInst>(Instr)) {
      assert(CI->isNoopCast(CI->getModule()->getDataLayout()) &&
             "non noop cast is found during rematerialization");

      Type *SrcTy = CI->getOperand(0)->getType();
      Cost += TTI.getCastInstrCost(CI->getOpcode(), CI->getType(), SrcTy,
                                   TTI::getCastContextHint(CI),
                                   TargetTransformInfo::TCK_SizeAndLatency, CI);

    } else if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(Instr)) {
      // Cost of the address calculation
      Type *ValTy = GEP->getSourceElementType();
      Cost += TTI.getAddressComputationCost(ValTy);

      // And cost of the GEP itself
      // TODO: Use TTI->getGEPCost here (it exists, but appears to be not
      //       allowed for the external usage)
      if (!GEP->hasAllConstantIndices())
        Cost += 2;

    } else {
      llvm_unreachable("unsupported instruction type during rematerialization");
    }
  }

  return Cost;
}

static bool AreEquivalentPhiNodes(PHINode &OrigRootPhi, PHINode &AlternateRootPhi) {
  unsigned PhiNum = OrigRootPhi.getNumIncomingValues();
  if (PhiNum != AlternateRootPhi.getNumIncomingValues() ||
      OrigRootPhi.getParent() != AlternateRootPhi.getParent())
    return false;
  // Map of incoming values and their corresponding basic blocks of
  // OrigRootPhi.
  SmallDenseMap<Value *, BasicBlock *, 8> CurrentIncomingValues;
  for (unsigned i = 0; i < PhiNum; i++)
    CurrentIncomingValues[OrigRootPhi.getIncomingValue(i)] =
        OrigRootPhi.getIncomingBlock(i);

  // Both current and base PHIs should have same incoming values and
  // the same basic blocks corresponding to the incoming values.
  for (unsigned i = 0; i < PhiNum; i++) {
    auto CIVI =
        CurrentIncomingValues.find(AlternateRootPhi.getIncomingValue(i));
    if (CIVI == CurrentIncomingValues.end())
      return false;
    BasicBlock *CurrentIncomingBB = CIVI->second;
    if (CurrentIncomingBB != AlternateRootPhi.getIncomingBlock(i))
      return false;
  }
  return true;
}

// Find derived pointers that can be recomputed cheap enough and fill
// RematerizationCandidates with such candidates.
static void
findRematerializationCandidates(PointerToBaseTy PointerToBase,
                                RematCandTy &RematerizationCandidates,
                                TargetTransformInfo &TTI) {
  const unsigned int ChainLengthThreshold = 10;

  for (auto P2B : PointerToBase) {
    auto *Derived = P2B.first;
    auto *Base = P2B.second;
    // Consider only derived pointers.
    if (Derived == Base)
      continue;

    // For each live pointer find its defining chain.
    SmallVector<Instruction *, 3> ChainToBase;
    Value *RootOfChain =
        findRematerializableChainToBasePointer(ChainToBase, Derived);
    bool IsPhiChainToBasePhi = false;
    if ((RootOfChain != Base || ChainToBase.empty()) && isa<PHINode>(Derived) &&
        isa<PHINode>(Base)) {
      ChainToBase.clear();
      bool NeedsBaseSelect = false;
      if (Value *PhiRoot = findRematerializablePhiChainToBasePhi(
              ChainToBase, cast<PHINode>(Derived), cast<PHINode>(Base),
              PointerToBase, /*AllowIntegerAddressArithmetic=*/false,
              NeedsBaseSelect)) {
        if (NeedsBaseSelect)
          continue;
        RootOfChain = PhiRoot;
        IsPhiChainToBasePhi = true;
      }
    }

    // Nothing to do, or chain is too long
    if ( ChainToBase.size() == 0 ||
        ChainToBase.size() > ChainLengthThreshold)
      continue;

    // Handle the scenario where the RootOfChain is not equal to the
    // Base Value, but they are essentially the same phi values.
    if (!IsPhiChainToBasePhi && RootOfChain != PointerToBase[Derived]) {
      PHINode *OrigRootPhi = dyn_cast<PHINode>(RootOfChain);
      PHINode *AlternateRootPhi = dyn_cast<PHINode>(PointerToBase[Derived]);
      if (!OrigRootPhi || !AlternateRootPhi)
        continue;
      // PHI nodes that have the same incoming values, and belonging to the same
      // basic blocks are essentially the same SSA value.  When the original phi
      // has incoming values with different base pointers, the original phi is
      // marked as conflict, and an additional `AlternateRootPhi` with the same
      // incoming values get generated by the findBasePointer function. We need
      // to identify the newly generated AlternateRootPhi (.base version of phi)
      // and RootOfChain (the original phi node itself) are the same, so that we
      // can rematerialize the gep and casts. This is a workaround for the
      // deficiency in the findBasePointer algorithm.
      if (!AreEquivalentPhiNodes(*OrigRootPhi, *AlternateRootPhi))
        continue;
    }
    // Compute cost of this chain.
    InstructionCost Cost = chainToBasePointerCost(ChainToBase, TTI);
    // TODO: We can also account for cases when we will be able to remove some
    //       of the rematerialized values by later optimization passes. I.e if
    //       we rematerialized several intersecting chains. Or if original values
    //       don't have any uses besides this statepoint.

    // Ok, there is a candidate.
    RematerizlizationCandidateRecord Record;
    Record.ChainToBase = ChainToBase;
    Record.RootOfChain = RootOfChain;
    Record.Cost = Cost;
    RematerizationCandidates.insert({ Derived, Record });
  }
}

// Try to rematerialize derived pointers immediately before their uses
// (instead of rematerializing after every statepoint it is live through).
// This can be beneficial when derived pointer is live across many
// statepoints, but uses are rare.
static void rematerializeLiveValuesAtUses(
    RematCandTy &RematerizationCandidates,
    MutableArrayRef<PartiallyConstructedSafepointRecord> Records,
    PointerToBaseTy &PointerToBase) {
  if (!RematDerivedAtUses)
    return;

  SmallVector<Instruction *, 32> LiveValuesToBeDeleted;

  LLVM_DEBUG(dbgs() << "Rematerialize derived pointers at uses, "
                    << "Num statepoints: " << Records.size() << '\n');

  for (auto &It : RematerizationCandidates) {
    Instruction *Cand = cast<Instruction>(It.first);
    auto &Record = It.second;
    Value *Base = PointerToBase[Cand];
    bool ForceOxCamlUseRemat =
        RematAddrSpace1DerivedFromBaseAtAlloc &&
        isOxCamlFunction(*Cand->getFunction()) &&
        isHandledGCPointerType(Cand->getType()) &&
        isHandledGCPointerType(Base->getType());

    if (!ForceOxCamlUseRemat && Record.Cost >= RematerializationThreshold)
      continue;

    if (Cand->user_empty())
      continue;

    if (!ForceOxCamlUseRemat && Cand->hasOneUse())
      if (auto *U = dyn_cast<Instruction>(Cand->getUniqueUndroppableUser()))
        if (U->getParent() == Cand->getParent())
          continue;

    // Rematerialization before PHI nodes is not implemented.
    if (!ForceOxCamlUseRemat &&
        llvm::any_of(Cand->users(),
                     [](const auto *U) { return isa<PHINode>(U); }))
      continue;

    LLVM_DEBUG(dbgs() << "Trying cand " << *Cand << " ... ");

    // Count of rematerialization instructions we introduce is equal to number
    // of candidate uses.
    // Count of rematerialization instructions we eliminate is equal to number
    // of statepoints it is live through.
    // Consider transformation profitable if latter is greater than former
    // (in other words, we create less than eliminate).
    unsigned NumLiveStatepoints = llvm::count_if(
        Records, [Cand](const auto &R) { return R.LiveSet.contains(Cand); });
    unsigned NumUses = Cand->getNumUses();

    LLVM_DEBUG(dbgs() << "Num uses: " << NumUses << " Num live statepoints: "
                      << NumLiveStatepoints << " ");

    if (!ForceOxCamlUseRemat && NumLiveStatepoints < NumUses) {
      LLVM_DEBUG(dbgs() << "not profitable\n");
      continue;
    }

    // If rematerialization is 'free', then favor rematerialization at
    // uses as it generally shortens live ranges.
    // TODO: Short (size ==1) chains only?
    if (!ForceOxCamlUseRemat && NumLiveStatepoints == NumUses &&
        Record.Cost > 0) {
      LLVM_DEBUG(dbgs() << "not profitable\n");
      continue;
    }

    LLVM_DEBUG(dbgs() << "looks profitable\n");

    // ChainToBase may contain another remat candidate (as a sub chain) which
    // has been rewritten by now. Need to recollect chain to have up to date
    // value.
    // TODO: sort records in findRematerializationCandidates() in
    // decreasing chain size order?
    if (Record.ChainToBase.size() > 1) {
      Record.ChainToBase.clear();
      findRematerializableChainToBasePointer(Record.ChainToBase, Cand);
    }

    // Current rematerialization algorithm is very simple: we rematerialize
    // immediately before EVERY use, even if there are several uses in same
    // block or if use is local to Cand Def. The reason is that this allows
    // us to avoid recomputing liveness without complicated analysis:
    // - If we did not eliminate all uses of original Candidate, we do not
    //   know exaclty in what BBs it is still live.
    // - If we rematerialize once per BB, we need to find proper insertion
    //   place (first use in block, but after Def) and analyze if there is
    //   statepoint between uses in the block.
    while (!Cand->user_empty()) {
      Instruction *UserI = cast<Instruction>(*Cand->user_begin());
      if (auto *Phi = dyn_cast<PHINode>(UserI)) {
        for (unsigned I = 0, E = Phi->getNumIncomingValues(); I != E; ++I) {
          if (Phi->getIncomingValue(I) != Cand)
            continue;
          BasicBlock *Incoming = Phi->getIncomingBlock(I);
          Instruction *InsertBefore = Incoming->getTerminator();
          Instruction *RematChain =
              rematerializeChain(Record.ChainToBase, InsertBefore,
                                 Record.RootOfChain, Base);
          Phi->setIncomingValue(I, RematChain);
          PointerToBase[RematChain] = Base;
        }
      } else {
        Instruction *RematChain = rematerializeChain(
            Record.ChainToBase, UserI, Record.RootOfChain, Base);
        UserI->replaceUsesOfWith(Cand, RematChain);
        PointerToBase[RematChain] = Base;
      }
    }
    LiveValuesToBeDeleted.push_back(Cand);
  }

  LLVM_DEBUG(dbgs() << "Rematerialized " << LiveValuesToBeDeleted.size()
                    << " derived pointers\n");
  for (auto *Cand : LiveValuesToBeDeleted) {
    assert(Cand->use_empty() && "Unexpected user remain");
    RematerizationCandidates.erase(Cand);
    for (auto &R : Records) {
      if (R.LiveSet.contains(Cand))
        R.LiveSet.insert(PointerToBase[Cand]);
      R.LiveSet.remove(Cand);
    }
  }

  // Recollect not rematerialized chains - we might have rewritten
  // their sub-chains.
  if (!LiveValuesToBeDeleted.empty()) {
    for (auto &P : RematerizationCandidates) {
      auto &R = P.second;
      if (R.ChainToBase.size() > 1) {
        R.ChainToBase.clear();
        findRematerializableChainToBasePointer(R.ChainToBase, P.first);
      }
    }
  }
}

// From the statepoint live set pick values that are cheaper to recompute then
// to relocate. Remove this values from the live set, rematerialize them after
// statepoint and record them in "Info" structure. Note that similar to
// relocated values we don't do any user adjustments here.
static void rematerializeLiveValues(CallBase *Call,
                                    PartiallyConstructedSafepointRecord &Info,
                                    PointerToBaseTy &PointerToBase,
                                    RematCandTy &RematerizationCandidates,
                                    TargetTransformInfo &TTI,
                                    DominatorTree &DT) {
  // Record values we are going to delete from this statepoint live set.
  // We can not di this in following loop due to iterator invalidation.
  SmallVector<Value *, 32> LiveValuesToBeDeleted;

  for (Value *LiveValue : Info.LiveSet) {
    auto It = RematerizationCandidates.find(LiveValue);
    if (It == RematerizationCandidates.end())
      continue;

    RematerizlizationCandidateRecord &Record = It->second;

    InstructionCost Cost = Record.Cost;
    // For invokes we need to rematerialize each chain twice - for normal and
    // for unwind basic blocks. Model this by multiplying cost by two.
    if (isa<InvokeInst>(Call))
      Cost *= 2;

    // If it's too expensive - skip it.
    if (Cost >= RematerializationThreshold)
      continue;

    if (auto *II = dyn_cast<InvokeInst>(Call))
      if (Info.UsesExplicitExceptionRoots &&
          !isNormalOnlyAfterInvoke(II, LiveValue, DT))
        continue;

    // Remove value from the live set
    LiveValuesToBeDeleted.push_back(LiveValue);

    // Clone instructions and record them inside "Info" structure.

    // Different cases for calls and invokes. For invokes we need to clone
    // instructions both on normal and unwind path.
    if (isa<CallInst>(Call)) {
      Instruction *InsertBefore = Call->getNextNode();
      assert(InsertBefore);
      Instruction *RematerializedValue =
          rematerializeChain(Record.ChainToBase, InsertBefore,
                             Record.RootOfChain, PointerToBase[LiveValue]);
      Info.RematerializedValues[RematerializedValue] = LiveValue;
    } else if (Info.UsesExplicitExceptionRoots) {
      auto *Invoke = cast<InvokeInst>(Call);
      Instruction *NormalInsertBefore =
          &*Invoke->getNormalDest()->getFirstInsertionPt();
      Instruction *NormalRematerializedValue =
          rematerializeChain(Record.ChainToBase, NormalInsertBefore,
                             Record.RootOfChain, PointerToBase[LiveValue]);
      Info.RematerializedValues[NormalRematerializedValue] = LiveValue;
    } else {
      auto *Invoke = cast<InvokeInst>(Call);

      Instruction *NormalInsertBefore =
          &*Invoke->getNormalDest()->getFirstInsertionPt();
      Instruction *UnwindInsertBefore =
          &*Invoke->getUnwindDest()->getFirstInsertionPt();

      Instruction *NormalRematerializedValue =
          rematerializeChain(Record.ChainToBase, NormalInsertBefore,
                             Record.RootOfChain, PointerToBase[LiveValue]);
      Instruction *UnwindRematerializedValue =
          rematerializeChain(Record.ChainToBase, UnwindInsertBefore,
                             Record.RootOfChain, PointerToBase[LiveValue]);

      Info.RematerializedValues[NormalRematerializedValue] = LiveValue;
      Info.RematerializedValues[UnwindRematerializedValue] = LiveValue;
    }
  }

  // Remove rematerialized values from the live set.
  for (auto *LiveValue: LiveValuesToBeDeleted) {
    Info.LiveSet.remove(LiveValue);
  }
}

static bool inlineGetBaseAndOffset(Function &F,
                                   SmallVectorImpl<CallInst *> &Intrinsics,
                                   DefiningValueMapTy &DVCache,
                                   IsKnownBaseMapTy &KnownBases) {
  auto &Context = F.getContext();
  auto &DL = F.getParent()->getDataLayout();
  bool Changed = false;

  for (auto *Callsite : Intrinsics)
    switch (Callsite->getIntrinsicID()) {
    case Intrinsic::experimental_gc_get_pointer_base: {
      Changed = true;
      Value *Base =
          findBasePointer(Callsite->getOperand(0), DVCache, KnownBases);
      assert(!DVCache.count(Callsite));
      auto *BaseBC = IRBuilder<>(Callsite).CreateBitCast(
          Base, Callsite->getType(), suffixed_name_or(Base, ".cast", ""));
      if (BaseBC != Base)
        DVCache[BaseBC] = Base;
      Callsite->replaceAllUsesWith(BaseBC);
      if (!BaseBC->hasName())
        BaseBC->takeName(Callsite);
      Callsite->eraseFromParent();
      break;
    }
    case Intrinsic::experimental_gc_get_pointer_offset: {
      Changed = true;
      Value *Derived = Callsite->getOperand(0);
      Value *Base = findBasePointer(Derived, DVCache, KnownBases);
      assert(!DVCache.count(Callsite));
      unsigned AddressSpace = Derived->getType()->getPointerAddressSpace();
      unsigned IntPtrSize = DL.getPointerSizeInBits(AddressSpace);
      IRBuilder<> Builder(Callsite);
      Value *BaseInt =
          Builder.CreatePtrToInt(Base, Type::getIntNTy(Context, IntPtrSize),
                                 suffixed_name_or(Base, ".int", ""));
      Value *DerivedInt =
          Builder.CreatePtrToInt(Derived, Type::getIntNTy(Context, IntPtrSize),
                                 suffixed_name_or(Derived, ".int", ""));
      Value *Offset = Builder.CreateSub(DerivedInt, BaseInt);
      Callsite->replaceAllUsesWith(Offset);
      Offset->takeName(Callsite);
      Callsite->eraseFromParent();
      break;
    }
    default:
      llvm_unreachable("Unknown intrinsic");
    }

  return Changed;
}

static bool canonicalizeOxCamlAddrSpace1IntToPtrAliases(Function &F,
                                                        DominatorTree &DT) {
  if (!isOxCamlFunction(F))
    return false;

  DenseMap<Value *, SmallVector<IntToPtrInst *, 4>> IntToPtrsByInteger;
  for (BasicBlock &BB : F)
    for (Instruction &I : BB)
      if (auto *I2P = dyn_cast<IntToPtrInst>(&I))
        if (auto *Ty = dyn_cast<PointerType>(I2P->getType()))
          if (Ty->getAddressSpace() == 1)
            IntToPtrsByInteger[I2P->getOperand(0)].push_back(I2P);

  SmallVector<IntToPtrInst *, 16> DeadAliases;
  bool Changed = false;
  for (auto &Entry : IntToPtrsByInteger) {
    SmallVector<IntToPtrInst *, 4> &IntToPtrs = Entry.second;
    if (IntToPtrs.size() < 2)
      continue;

    for (IntToPtrInst *Alias : IntToPtrs) {
      IntToPtrInst *NearestDominatingAlias = nullptr;
      for (IntToPtrInst *Candidate : IntToPtrs) {
        if (Candidate == Alias || Candidate->getType() != Alias->getType() ||
            !DT.dominates(Candidate, Alias))
          continue;

        if (!NearestDominatingAlias ||
            DT.dominates(NearestDominatingAlias, Candidate))
          NearestDominatingAlias = Candidate;
      }

      if (!NearestDominatingAlias)
        continue;

      Alias->replaceAllUsesWith(NearestDominatingAlias);
      DeadAliases.push_back(Alias);
      Changed = true;
    }
  }

  for (IntToPtrInst *Alias : DeadAliases)
    if (Alias->use_empty())
      Alias->eraseFromParent();

  return Changed;
}

static void insertAvailableLiveInValues(BasicBlock *BB,
                                        const SetVector<Value *> &SuccLiveIn,
                                        SetVector<Value *> &LiveOut,
                                        DominatorTree &DT);

struct AddressIntegerLivenessData {
  DenseMap<BasicBlock *, SetVector<Value *>> LiveSet;
  DenseMap<BasicBlock *, SetVector<Value *>> LiveOut;
  DenseMap<BasicBlock *, SetVector<Value *>> LiveIn;
  DenseMap<BasicBlock *, SetVector<Value *>> KillSet;
};

static bool isOxCamlAddrSpace1PointerType(Type *Ty) {
  if (!Ty->isPtrOrPtrVectorTy())
    return false;
  auto *PtrTy = dyn_cast<PointerType>(Ty->getScalarType());
  return PtrTy && PtrTy->getAddressSpace() == 1;
}

static void collectOxCamlAddrSpace1IntToPtrContributingIntegers(
    Function &F, SmallPtrSetImpl<Value *> &AddressIntegers) {
  SmallVector<Value *, 32> Worklist;

  auto Mark = [&](Value *V) {
    if (isa<Constant>(V) || !V->getType()->isIntOrIntVectorTy())
      return;
    if (AddressIntegers.insert(V).second)
      Worklist.push_back(V);
  };

  for (Instruction &I : instructions(F))
    if (auto *I2P = dyn_cast<IntToPtrInst>(&I))
      if (isOxCamlAddrSpace1PointerType(I2P->getType()))
        Mark(I2P->getOperand(0));

  while (!Worklist.empty()) {
    Value *AddressInteger = Worklist.pop_back_val();
    auto *I = dyn_cast<Instruction>(AddressInteger);
    if (!I)
      continue;

    if (auto *BO = dyn_cast<BinaryOperator>(I)) {
      Mark(BO->getOperand(0));
      Mark(BO->getOperand(1));
      continue;
    }

    if (auto *CI = dyn_cast<CastInst>(I)) {
      Mark(CI->getOperand(0));
      continue;
    }

    if (auto *FI = dyn_cast<FreezeInst>(I)) {
      Mark(FI->getOperand(0));
      continue;
    }

    if (auto *PN = dyn_cast<PHINode>(I)) {
      for (Value *Incoming : PN->incoming_values())
        Mark(Incoming);
      continue;
    }

    if (auto *SI = dyn_cast<SelectInst>(I)) {
      Mark(SI->getTrueValue());
      Mark(SI->getFalseValue());
      continue;
    }

    if (auto *EE = dyn_cast<ExtractElementInst>(I)) {
      Mark(EE->getVectorOperand());
      continue;
    }

    if (auto *IE = dyn_cast<InsertElementInst>(I)) {
      Mark(IE->getOperand(0));
      Mark(IE->getOperand(1));
      continue;
    }

    if (auto *SV = dyn_cast<ShuffleVectorInst>(I)) {
      Mark(SV->getOperand(0));
      Mark(SV->getOperand(1));
      continue;
    }
  }
}

static void computeAddressIntegerLiveInValues(
    BasicBlock::reverse_iterator Begin, BasicBlock::reverse_iterator End,
    const SmallPtrSetImpl<Value *> &AddressIntegers,
    SetVector<Value *> &LiveTmp) {
  for (auto &I : make_range(Begin, End)) {
    LiveTmp.remove(&I);

    if (isa<PHINode>(I))
      continue;

    for (Value *V : I.operands())
      if (AddressIntegers.contains(V))
        LiveTmp.insert(V);
  }
}

static void computeAddressIntegerLiveOutSeed(
    BasicBlock *BB, const SmallPtrSetImpl<Value *> &AddressIntegers,
    SetVector<Value *> &LiveTmp) {
  for (BasicBlock *Succ : successors(BB)) {
    for (auto &I : *Succ) {
      PHINode *PN = dyn_cast<PHINode>(&I);
      if (!PN)
        break;

      Value *V = PN->getIncomingValueForBlock(BB);
      if (AddressIntegers.contains(V))
        LiveTmp.insert(V);
    }
  }
}

static SetVector<Value *> computeAddressIntegerKillSet(
    BasicBlock *BB, const SmallPtrSetImpl<Value *> &AddressIntegers) {
  SetVector<Value *> KillSet;
  for (Instruction &I : *BB)
    if (AddressIntegers.contains(&I))
      KillSet.insert(&I);
  return KillSet;
}

static void computeAddressIntegerLiveInValues(
    DominatorTree &DT, Function &F,
    const SmallPtrSetImpl<Value *> &AddressIntegers,
    AddressIntegerLivenessData &Data) {
  SmallSetVector<BasicBlock *, 32> Worklist;

  for (BasicBlock &BB : F) {
    Data.KillSet[&BB] = computeAddressIntegerKillSet(&BB, AddressIntegers);
    Data.LiveSet[&BB].clear();
    computeAddressIntegerLiveInValues(BB.rbegin(), BB.rend(), AddressIntegers,
                                      Data.LiveSet[&BB]);

    Data.LiveOut[&BB] = SetVector<Value *>();
    computeAddressIntegerLiveOutSeed(&BB, AddressIntegers, Data.LiveOut[&BB]);
    Data.LiveIn[&BB] = Data.LiveSet[&BB];
    Data.LiveIn[&BB].set_union(Data.LiveOut[&BB]);
    Data.LiveIn[&BB].set_subtract(Data.KillSet[&BB]);
    if (!Data.LiveIn[&BB].empty())
      Worklist.insert(pred_begin(&BB), pred_end(&BB));
  }

  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.pop_back_val();

    SetVector<Value *> LiveOut = Data.LiveOut[BB];
    const auto OldLiveOutSize = LiveOut.size();
    for (BasicBlock *Succ : successors(BB)) {
      assert(Data.LiveIn.count(Succ));
      insertAvailableLiveInValues(BB, Data.LiveIn[Succ], LiveOut, DT);
    }
    if (OldLiveOutSize == LiveOut.size())
      continue;

    Data.LiveOut[BB] = LiveOut;

    SetVector<Value *> LiveTmp = LiveOut;
    LiveTmp.set_union(Data.LiveSet[BB]);
    LiveTmp.set_subtract(Data.KillSet[BB]);

    assert(Data.LiveIn.count(BB));
    const SetVector<Value *> &OldLiveIn = Data.LiveIn[BB];
    if (OldLiveIn.size() != LiveTmp.size()) {
      Data.LiveIn[BB] = LiveTmp;
      Worklist.insert(pred_begin(BB), pred_end(BB));
    }
  }
}

static void findAddressIntegerLiveSetAtInst(
    Instruction *Inst, const SmallPtrSetImpl<Value *> &AddressIntegers,
    AddressIntegerLivenessData &Data, SetVector<Value *> &Out) {
  BasicBlock *BB = Inst->getParent();

  assert(Data.LiveOut.count(BB));
  SetVector<Value *> LiveOut = Data.LiveOut[BB];
  computeAddressIntegerLiveInValues(BB->rbegin(),
                                    Inst->getIterator().getReverse(),
                                    AddressIntegers, LiveOut);
  LiveOut.remove(Inst);
  Out.insert(LiveOut.begin(), LiveOut.end());
}

static bool isPotentiallyAfterStatepoint(CallBase *Call, Instruction *I,
                                         DominatorTree &DT) {
  BasicBlock *UserBB = I->getParent();

  if (auto *Invoke = dyn_cast<InvokeInst>(Call)) {
    if (UserBB == Invoke->getParent())
      return false;
    return isPotentiallyReachable(Invoke->getNormalDest(), UserBB, nullptr,
                                  &DT) ||
           isPotentiallyReachable(Invoke->getUnwindDest(), UserBB, nullptr,
                                  &DT);
  }

  BasicBlock *CallBB = Call->getParent();
  if (UserBB == CallBB)
    return Call->comesBefore(I);

  return isPotentiallyReachable(CallBB, UserBB, nullptr, &DT);
}

static void collectAddrSpace1IntToPtrUsers(
    Value *V, const SmallPtrSetImpl<Value *> &AddressIntegers,
    SmallPtrSetImpl<Value *> &Visited, SmallVectorImpl<IntToPtrInst *> &Out) {
  for (User *U : V->users()) {
    auto *I = dyn_cast<Instruction>(U);
    if (!I)
      continue;

    if (auto *I2P = dyn_cast<IntToPtrInst>(I)) {
      if (isOxCamlAddrSpace1PointerType(I2P->getType()))
        Out.push_back(I2P);
      continue;
    }

    if (!AddressIntegers.contains(I))
      continue;

    if (Visited.insert(I).second)
      collectAddrSpace1IntToPtrUsers(I, AddressIntegers, Visited, Out);
  }
}

static bool isLikelyOxCamlAllocationPointerInteger(
    Value *V, SmallPtrSetImpl<Value *> &Active) {
  if (!V->getType()->isIntegerTy())
    return false;

  // This is only a heuristic report filter.  Preserve the frontend's
  // allocation-pointer naming as a fallback for optimized phis/selects whose
  // structural origin is no longer obvious to this local walk.
  if (V->hasName() && V->getName().starts_with("alloc"))
    return true;

  if (auto *Arg = dyn_cast<Argument>(V)) {
    Function *F = Arg->getParent();
    return F && isOxCamlFunction(*F) && Arg->getArgNo() == 1;
  }

  auto *I = dyn_cast<Instruction>(V);
  if (!I)
    return false;

  if (!Active.insert(I).second)
    return true;
  auto PopActive = make_scope_exit([&]() { Active.erase(I); });

  if (auto *EV = dyn_cast<ExtractValueInst>(I)) {
    ArrayRef<unsigned> Indices = EV->getIndices();
    if (Indices.size() == 2 && Indices[0] == 0 && Indices[1] == 1)
      if (auto *Call = dyn_cast<CallBase>(EV->getAggregateOperand()))
        return isOxCamlCallingConv(Call->getCallingConv());
    return false;
  }

  if (auto *BO = dyn_cast<BinaryOperator>(I)) {
    Value *LHS = BO->getOperand(0);
    Value *RHS = BO->getOperand(1);
    if (isa<ConstantInt>(LHS))
      return isLikelyOxCamlAllocationPointerInteger(RHS, Active);
    if (isa<ConstantInt>(RHS))
      return isLikelyOxCamlAllocationPointerInteger(LHS, Active);
    return false;
  }

  if (auto *CI = dyn_cast<CastInst>(I))
    return isLikelyOxCamlAllocationPointerInteger(CI->getOperand(0), Active);

  if (auto *FI = dyn_cast<FreezeInst>(I))
    return isLikelyOxCamlAllocationPointerInteger(FI->getOperand(0), Active);

  if (auto *PN = dyn_cast<PHINode>(I)) {
    bool SawIncoming = false;
    for (Value *Incoming : PN->incoming_values()) {
      if (isa<ConstantInt>(Incoming))
        continue;
      SawIncoming = true;
      if (!isLikelyOxCamlAllocationPointerInteger(Incoming, Active))
        return false;
    }
    return SawIncoming;
  }

  if (auto *SI = dyn_cast<SelectInst>(I)) {
    Value *TrueValue = SI->getTrueValue();
    Value *FalseValue = SI->getFalseValue();
    return isLikelyOxCamlAllocationPointerInteger(TrueValue, Active) &&
           isLikelyOxCamlAllocationPointerInteger(FalseValue, Active);
  }

  return false;
}

static bool isLikelyOxCamlAllocationPointerInteger(Value *V) {
  SmallPtrSet<Value *, 16> Active;
  return isLikelyOxCamlAllocationPointerInteger(V, Active);
}

static void heuristicReportOxCamlStatepointCrossingIntToPtrs(
    Function &F, DominatorTree &DT, ArrayRef<CallBase *> ToUpdate) {
  if (!HeuristicReportOxCamlStatepointCrossingIntToPtr || !isOxCamlFunction(F))
    return;

  auto ReachedTotalLimit = [] {
    return HeuristicReportOxCamlStatepointCrossingIntToPtrTotalLimit != 0 &&
           OxCamlHeuristicStatepointCrossingIntToPtrReports >=
               HeuristicReportOxCamlStatepointCrossingIntToPtrTotalLimit;
  };
  auto ReportTotalLimit = [&] {
    if (OxCamlHeuristicStatepointCrossingIntToPtrLimitReported)
      return;
    errs() << "OxCaml heuristic candidate total report limit reached: "
              "further statepoint-crossing inttoptr candidates suppressed\n";
    OxCamlHeuristicStatepointCrossingIntToPtrLimitReported = true;
  };

  if (ReachedTotalLimit()) {
    ReportTotalLimit();
    return;
  }

  SmallPtrSet<Value *, 32> AddressIntegers;
  collectOxCamlAddrSpace1IntToPtrContributingIntegers(F, AddressIntegers);
  if (AddressIntegers.empty())
    return;

  AddressIntegerLivenessData Liveness;
  computeAddressIntegerLiveInValues(DT, F, AddressIntegers, Liveness);

  unsigned ReportedStatepoints = 0;
  unsigned SuppressedStatepoints = 0;
  unsigned SuppressedCandidates = 0;
  for (CallBase *Call : ToUpdate) {
    SetVector<Value *> LiveAddressIntegers;
    findAddressIntegerLiveSetAtInst(Call, AddressIntegers, Liveness,
                                    LiveAddressIntegers);

    for (Value *V : Call->operands())
      if (AddressIntegers.contains(V))
        LiveAddressIntegers.insert(V);

    SmallVector<std::pair<Value *, IntToPtrInst *>, 8> Candidates;
    SmallPtrSet<IntToPtrInst *, 8> SeenIntToPtrs;
    for (Value *V : LiveAddressIntegers) {
      if (isLikelyOxCamlAllocationPointerInteger(V))
        continue;
      SmallPtrSet<Value *, 16> Visited;
      SmallVector<IntToPtrInst *, 4> IntToPtrs;
      collectAddrSpace1IntToPtrUsers(V, AddressIntegers, Visited, IntToPtrs);
      for (IntToPtrInst *I2P : IntToPtrs)
        if (!isLikelyOxCamlAllocationPointerInteger(I2P->getOperand(0)) &&
            SeenIntToPtrs.insert(I2P).second &&
            isPotentiallyAfterStatepoint(Call, I2P, DT))
          Candidates.push_back({V, I2P});
    }

    if (Candidates.empty())
      continue;

    if (ReachedTotalLimit()) {
      ReportTotalLimit();
      return;
    }

    if (HeuristicReportOxCamlStatepointCrossingIntToPtrLimit != 0 &&
        ReportedStatepoints >=
            HeuristicReportOxCamlStatepointCrossingIntToPtrLimit) {
      ++SuppressedStatepoints;
      SuppressedCandidates += Candidates.size();
      continue;
    }

    ++ReportedStatepoints;
    ++OxCamlHeuristicStatepointCrossingIntToPtrReports;
    errs() << "OxCaml heuristic candidate: statepoint-crossing integer "
              "contributes to inttoptr ptr addrspace(1)\n  function: "
           << F.getName() << "\n  statepoint: ";
    Call->print(errs());
    errs() << "\n";
    for (auto [AddressInteger, IntToPtr] : Candidates) {
      errs() << "  crossing integer: ";
      AddressInteger->print(errs());
      errs() << "\n  addrspace(1) inttoptr: ";
      IntToPtr->print(errs());
      errs() << "\n";
    }
  }

  if (SuppressedStatepoints != 0)
    errs() << "OxCaml heuristic candidate report limit reached in function "
           << F.getName() << ": suppressed " << SuppressedStatepoints
           << " statepoints with " << SuppressedCandidates
           << " integer/inttoptr pairs\n";
}

static bool insertParsePoints(Function &F, DominatorTree &DT,
                              TargetTransformInfo &TTI,
                              SmallVectorImpl<CallBase *> &ToUpdate,
                              DefiningValueMapTy &DVCache,
                              IsKnownBaseMapTy &KnownBases) {
#ifndef NDEBUG
  // Validate the input
  std::set<CallBase *> Uniqued;
  Uniqued.insert(ToUpdate.begin(), ToUpdate.end());
  assert(Uniqued.size() == ToUpdate.size() && "no duplicates please!");

  for (CallBase *Call : ToUpdate)
    assert(Call->getFunction() == &F);
#endif

  ExplicitRootSlotMapTy ExplicitRootSlots;
  ExplicitExceptionRootCallSetTy ExplicitExceptionRootCalls;
  ExplicitRootHomeMapTy ExplicitRootHomes;
  OxCamlValueRootSlotMapTy SlotForValue;
  bool UseUnifiedOxCamlVolatileRoots =
      UseOxCamlVolatileRootAllocas && isOxCamlFunction(F);
  if (rematerializeOxCamlDerivedGEPsAtUses(F))
    DT.recalculate(F);
  canonicalizeOxCamlAddrSpace1IntToPtrAliases(F, DT);
  heuristicReportOxCamlStatepointCrossingIntToPtrs(F, DT, ToUpdate);
  if (!UseUnifiedOxCamlVolatileRoots)
    materializeOxCamlExceptionRootSlots(F, DT, ExplicitRootSlots,
                                        ExplicitExceptionRootCalls,
                                        ExplicitRootHomes, SlotForValue,
                                        DVCache, KnownBases);
  SmallVector<Value *, 16> AllExplicitRootSlots;
  collectExplicitRootSlots(ExplicitRootSlots, AllExplicitRootSlots);
  if (isOxCamlFunction(F)) {
    for (CallBase *Call : ToUpdate) {
      auto *II = dyn_cast<InvokeInst>(Call);
      if (!II)
        continue;
      if (getOxCamlTrapRecoveryTargetShapeOnly(II->getUnwindDest()))
        ExplicitExceptionRootCalls.insert(II);
    }
  }

  // When inserting gc.relocates for invokes, we need to be able to insert at
  // the top of the successor blocks.  See the comment on
  // normalForInvokeSafepoint on exactly what is needed.  Note that this step
  // may restructure the CFG.
  for (CallBase *Call : ToUpdate) {
    auto *II = dyn_cast<InvokeInst>(Call);
    if (!II)
      continue;
    normalizeForInvokeSafepoint(II->getNormalDest(), II->getParent(), DT);
    if (!ExplicitExceptionRootCalls.contains(II))
      normalizeForInvokeSafepoint(II->getUnwindDest(), II->getParent(), DT);
  }

  // A list of dummy calls added to the IR to keep various values obviously
  // live in the IR.  We'll remove all of these when done.
  SmallVector<CallInst *, 64> Holders;

  // Insert a dummy call with all of the deopt operands we'll need for the
  // actual safepoint insertion as arguments.  This ensures reference operands
  // in the deopt argument list are considered live through the safepoint (and
  // thus makes sure they get relocated.)
  for (CallBase *Call : ToUpdate) {
    SmallVector<Value *, 64> DeoptValues;

    for (Value *Arg : GetDeoptBundleOperands(Call)) {
      checkNoUnhandledGCPointerType(Arg, "statepoint deopt operands");
      if (isHandledGCPointerType(Arg->getType()))
        DeoptValues.push_back(Arg);
    }

    insertUseHolderAfter(Call, DeoptValues, Holders,
                         !ExplicitExceptionRootCalls.contains(Call));
  }

  SmallVector<PartiallyConstructedSafepointRecord, 64> Records(ToUpdate.size());
  for (size_t I = 0; I < ToUpdate.size(); ++I) {
    llvm::append_range(Records[I].ExplicitRootSlots, AllExplicitRootSlots);
    auto SlotIt = ExplicitRootSlots.find(ToUpdate[I]);
    if (SlotIt != ExplicitRootSlots.end()) {
      SmallPtrSet<Value *, 16> Seen(Records[I].ExplicitRootSlots.begin(),
                                    Records[I].ExplicitRootSlots.end());
      for (Value *Slot : SlotIt->second)
        if (Seen.insert(Slot).second)
          Records[I].ExplicitRootSlots.push_back(Slot);
    }
    auto HomeIt = ExplicitRootHomes.find(ToUpdate[I]);
    if (HomeIt != ExplicitRootHomes.end())
      llvm::append_range(Records[I].ExplicitRootHomes, HomeIt->second);
    Records[I].UsesExplicitExceptionRoots =
        ExplicitExceptionRootCalls.contains(ToUpdate[I]);
    if (isOxCamlCWrapperCallingConv(ToUpdate[I]->getCallingConv())) {
      for (Value *Arg : ToUpdate[I]->args()) {
        checkNoUnhandledGCPointerType(Arg, "OxCaml C-call arguments");
        if (!isHandledGCPointerType(Arg->getType()))
          continue;
        if (!isGCPointerType(Arg->getType()))
          report_fatal_error(
              "OxCaml C-call GC pointer arguments must be scalar pointers");
      }
    }
  }

  // A) Identify all gc pointers which are statically live at the given call
  // site.
  findLiveReferences(F, DT, ToUpdate, Records);

  /// Global mapping from live pointers to a base-defining-value.
  PointerToBaseTy PointerToBase;

  // B) Find the base pointers for each live pointer
  for (size_t i = 0; i < Records.size(); i++) {
    PartiallyConstructedSafepointRecord &info = Records[i];
    findBasePointers(DT, DVCache, ToUpdate[i], info, PointerToBase, KnownBases);
  }
  if (rewriteOxCamlExplicitRootStoresToBases(AllExplicitRootSlots,
                                             PointerToBase, DT)) {
    DVCache.clear();
    KnownBases.clear();
  }
  if (PrintBasePointers) {
    errs() << "Base Pairs (w/o Relocation):\n";
    for (auto &Pair : PointerToBase) {
      errs() << " derived ";
      Pair.first->printAsOperand(errs(), false);
      errs() << " base ";
      Pair.second->printAsOperand(errs(), false);
      errs() << "\n";
      ;
    }
  }

  // The base phi insertion logic (for any safepoint) may have inserted new
  // instructions which are now live at some safepoint.  The simplest such
  // example is:
  // loop:
  //   phi a  <-- will be a new base_phi here
  //   safepoint 1 <-- that needs to be live here
  //   gep a + 1
  //   safepoint 2
  //   br loop
  // We insert some dummy calls after each safepoint to definitely hold live
  // the base pointers which were identified for that safepoint.  We'll then
  // ask liveness for _every_ base inserted to see what is now live.  Then we
  // remove the dummy calls.
  Holders.reserve(Holders.size() + Records.size());
  for (size_t i = 0; i < Records.size(); i++) {
    PartiallyConstructedSafepointRecord &Info = Records[i];

    SmallVector<Value *, 128> Bases;
    for (auto *Derived : Info.LiveSet) {
      assert(PointerToBase.count(Derived) && "Missed base for derived pointer");
      Bases.push_back(PointerToBase[Derived]);
    }

    insertUseHolderAfter(ToUpdate[i], Bases, Holders,
                         !ExplicitExceptionRootCalls.contains(ToUpdate[i]));
  }

  // By selecting base pointers, we've effectively inserted new uses. Thus, we
  // need to rerun liveness.  We may *also* have inserted new defs, but that's
  // not the key issue.
  recomputeLiveInValues(F, DT, ToUpdate, Records, PointerToBase);

  if (PrintBasePointers) {
    errs() << "Base Pairs: (w/Relocation)\n";
    for (auto Pair : PointerToBase) {
      errs() << " derived ";
      Pair.first->printAsOperand(errs(), false);
      errs() << " base ";
      Pair.second->printAsOperand(errs(), false);
      errs() << "\n";
    }
  }

  // It is possible that non-constant live variables have a constant base.  For
  // example, a GEP with a variable offset from a global.  In this case we can
  // remove it from the liveset.  We already don't add constants to the liveset
  // because we assume they won't move at runtime and the GC doesn't need to be
  // informed about them.  The same reasoning applies if the base is constant.
  // Note that the relocation placement code relies on this filtering for
  // correctness as it expects the base to be in the liveset, which isn't true
  // if the base is constant.
  for (auto &Info : Records) {
    Info.LiveSet.remove_if([&](Value *LiveV) {
      assert(PointerToBase.count(LiveV) && "Missed base for derived pointer");
      return isa<Constant>(PointerToBase[LiveV]);
    });
  }

  if (!UseUnifiedOxCamlVolatileRoots)
    assignExplicitRootHomesAndFilterLiveSets(Records, ToUpdate, SlotForValue,
                                             PointerToBase, DT);

  for (CallInst *CI : Holders)
    CI->eraseFromParent();

  Holders.clear();

  if (!UseOxCamlVolatileRootAllocas || !isOxCamlFunction(F)) {
    // Compute the cost of possible re-materialization of derived pointers.
    RematCandTy RematerizationCandidates;
    findRematerializationCandidates(PointerToBase, RematerizationCandidates,
                                    TTI);

    // In order to reduce live set of statepoint we might choose to
    // rematerialize some values instead of relocating them. This is purely an
    // optimization and does not influence correctness.
    // First try rematerialization at uses, then after statepoints.
    rematerializeLiveValuesAtUses(RematerizationCandidates, Records,
                                  PointerToBase);
    for (size_t i = 0; i < Records.size(); i++)
      rematerializeLiveValues(ToUpdate[i], Records[i], PointerToBase,
                              RematerizationCandidates, TTI, DT);
  }

  // We need this to safely RAUW and delete call or invoke return values that
  // may themselves be live over a statepoint.  For details, please see usage in
  // makeStatepointExplicitImpl.
  std::vector<DeferredReplacement> Replacements;

  // Now run through and replace the existing statepoints with new ones with
  // the live variables listed.  We do not yet update uses of the values being
  // relocated. We have references to live variables that need to
  // survive to the last iteration of this loop.  (By construction, the
  // previous statepoint can not be a live variable, thus we can and remove
  // the old statepoint calls as we go.)
  for (size_t i = 0; i < Records.size(); i++)
    makeStatepointExplicit(DT, ToUpdate[i], Records[i], Replacements,
                           PointerToBase);

  DenseMap<InvokeInst *, InvokeInst *> ReplacementInvokeForOriginalInvoke;
  for (size_t I = 0; I < Records.size(); ++I) {
    auto *OriginalInvoke = dyn_cast<InvokeInst>(ToUpdate[I]);
    auto *ReplacementInvoke = dyn_cast<InvokeInst>(Records[I].StatepointToken);
    if (OriginalInvoke && ReplacementInvoke)
      ReplacementInvokeForOriginalInvoke[OriginalInvoke] = ReplacementInvoke;
  }

  ToUpdate.clear(); // prevent accident use of invalid calls.

  for (auto &PR : Replacements)
    PR.doReplacement();

  Replacements.clear();

  for (auto &Info : Records) {
    // These live sets may contain state Value pointers, since we replaced calls
    // with operand bundles with calls wrapped in gc.statepoint, and some of
    // those calls may have been def'ing live gc pointers.  Clear these out to
    // avoid accidentally using them.
    //
    // TODO: We should create a separate data structure that does not contain
    // these live sets, and migrate to using that data structure from this point
    // onward.
    Info.LiveSet.clear();
  }
  if (UseOxCamlVolatileRootAllocas && isOxCamlFunction(F)) {
    volatileRootAllocaViaStatepointLiveSet(F, DT, Records, PointerToBase);
  } else {
    // Do all the fixups of the original live variables to their relocated
    // selves.
    SmallVector<Value *, 128> Live;
    for (size_t i = 0; i < Records.size(); i++) {
      PartiallyConstructedSafepointRecord &Info = Records[i];

      // We can't simply save the live set from the original insertion.  One of
      // the live values might be the result of a call which needs a safepoint.
      // That Value* no longer exists and we need to use the new gc_result.
      // Thankfully, the live set is embedded in the statepoint (and updated),
      // so we just grab that.
      for (Value *V : Info.StatepointToken->gc_args())
        if (isHandledGCPointerType(V->getType()))
          Live.push_back(V);
      for (auto [RootHandle, Slot] : Info.ExplicitRootHomes)
        if (Value *RootValue = RootHandle)
          Live.push_back(RootValue);
#ifndef NDEBUG
      // Do some basic validation checking on our liveness results before
      // performing relocation.  Relocation can and will turn mistakes in
      // liveness results into non-sensical code which is must harder to debug.
      // TODO: It would be nice to test consistency as well
      assert(DT.isReachableFromEntry(Info.StatepointToken->getParent()) &&
             "statepoint must be reachable or liveness is meaningless");
      for (Value *V : Info.StatepointToken->gc_args()) {
        if (!isHandledGCPointerType(V->getType()))
          continue;
        if (!isa<Instruction>(V))
          // Non-instruction values trivial dominate all possible uses
          continue;
        auto *LiveInst = cast<Instruction>(V);
        assert(DT.isReachableFromEntry(LiveInst->getParent()) &&
               "unreachable values should never be live");
        assert(DT.dominates(LiveInst, Info.StatepointToken) &&
               "basic SSA liveness expectation violated by liveness analysis");
      }
#endif
    }
    unique_unsorted(Live);

#ifndef NDEBUG
    // Validation check
    for (auto *Ptr : Live)
      assert(isHandledGCPointerType(Ptr->getType()) &&
             "must be a gc pointer type");
#endif

    relocationViaAlloca(F, DT, Live, Records);
  }
  PointerToBase.clear();
  bool ChangedLateRoots = false;
  if (!UseUnifiedOxCamlVolatileRoots) {
    ExplicitRootSlotMapTy LateExplicitRootSlots;
    ExplicitExceptionRootCallSetTy LateExplicitExceptionRootCalls;
    ExplicitRootHomeMapTy LateExplicitRootHomes;
    // SlotForValue is a ValueMap, so its entries followed the RAUWs and
    // deletions of statepoint rewriting; SSA repair values that merge
    // reloads of an existing slot resolve back to it here.
    ChangedLateRoots = materializeOxCamlExceptionRootSlots(
        F, DT, LateExplicitRootSlots, LateExplicitExceptionRootCalls,
        LateExplicitRootHomes, SlotForValue, DVCache, KnownBases);
    SmallVector<Value *, 16> LateAllExplicitRootSlots;
    collectExplicitRootSlots(LateExplicitRootSlots, LateAllExplicitRootSlots);
    ChangedLateRoots |=
        materializeRemainingOxCamlRecoveryPhis(F, DT, LateAllExplicitRootSlots);
    ChangedLateRoots |=
        appendExplicitRootSlotsToAllStatepoints(F, LateAllExplicitRootSlots);
  }

  // An entry-block init store only matters on paths that reach a slot read
  // (a reload or a statepoint scanning the slot) without passing the
  // defining store.  When the single defining store dominates every load
  // and every statepoint that lists the slot, no such path exists and the
  // init store is dead weight on the hot path.
  if (OxCamlPruneRootSlotInitStores && isOxCamlFunction(F)) {
    DT.recalculate(F);
    for (Instruction &I : F.getEntryBlock()) {
      auto *Slot = dyn_cast<AllocaInst>(&I);
      if (!Slot)
        continue;
      SmallVector<StoreInst *, 2> InitStores;
      StoreInst *DefStore = nullptr;
      bool Prunable = true;
      SmallVector<Instruction *, 8> MustBeDominated;
      for (User *U : Slot->users()) {
        if (auto *Store = dyn_cast<StoreInst>(U)) {
          if (Store->getPointerOperand() != Slot ||
              Store->getValueOperand() == Slot) {
            Prunable = false;
            break;
          }
          if (isOxCamlRootSlotInitStore(Store)) {
            InitStores.push_back(Store);
            continue;
          }
          if (DefStore) {
            Prunable = false; // phi-edge slots store per unwind edge
            break;
          }
          DefStore = Store;
          continue;
        }
        if (auto *Load = dyn_cast<LoadInst>(U)) {
          if (Load->getPointerOperand() != Slot) {
            Prunable = false;
            break;
          }
          MustBeDominated.push_back(Load);
          continue;
        }
        if (auto *Call = dyn_cast<CallBase>(U)) {
          // The collector reads and updates the slot at every statepoint
          // whose gc-live bundle lists it.
          MustBeDominated.push_back(Call);
          continue;
        }
        Prunable = false;
        break;
      }
      if (!Prunable || InitStores.empty() || !DefStore)
        continue;
      if (!llvm::all_of(MustBeDominated, [&](Instruction *Use) {
            return DT.dominates(DefStore, Use);
          }))
        continue;
      for (StoreInst *Store : InitStores)
        Store->eraseFromParent();
    }
  }

  if (OxCamlVerifyRootSlots && isOxCamlFunction(F)) {
    DT.recalculate(F);
    for (Instruction &I : F.getEntryBlock()) {
      auto *Slot = dyn_cast<AllocaInst>(&I);
      if (!Slot)
        continue;
      StoreInst *DefStore = nullptr;
      bool MultipleStores = false;
      for (User *U : Slot->users()) {
        auto *Store = dyn_cast<StoreInst>(U);
        if (!Store || Store->getPointerOperand() != Slot)
          continue;
        if (isOxCamlRootSlotInitStore(Store))
          continue;
        if (DefStore)
          MultipleStores = true;
        DefStore = Store;
      }
      if (MultipleStores)
        continue; // phi-edge slots
      for (User *U : Slot->users()) {
        auto *Load = dyn_cast<LoadInst>(U);
        if (!Load || !Load->getMetadata("oxcaml.exnroot.load"))
          continue;
        bool IsHomeReload = Load->getName().contains(".normal.load");
        if (!DefStore) {
          errs() << "[rs4gc-root-verify] " << F.getName() << ": slot "
                 << Slot->getName() << " has no defining store but load "
                 << Load->getName() << (IsHomeReload ? " (HOME)" : "")
                 << "\n";
          continue;
        }
        if (!DT.dominates(DefStore, Load))
          errs() << "[rs4gc-root-verify] " << F.getName() << ": load "
                 << Load->getName() << (IsHomeReload ? " (HOME)" : "")
                 << " of slot " << Slot->getName()
                 << " not dominated by its defining store in block "
                 << DefStore->getParent()->getName() << "\n";
      }
    }
  }
  return !Records.empty() || ChangedLateRoots;
}

// List of all parameter and return attributes which must be stripped when
// lowering from the abstract machine model.  Note that we list attributes
// here which aren't valid as return attributes, that is okay.
static AttributeMask getParamAndReturnAttributesToRemove() {
  AttributeMask R;
  R.addAttribute(Attribute::Dereferenceable);
  R.addAttribute(Attribute::DereferenceableOrNull);
  R.addAttribute(Attribute::ReadNone);
  R.addAttribute(Attribute::ReadOnly);
  R.addAttribute(Attribute::WriteOnly);
  R.addAttribute(Attribute::NoAlias);
  R.addAttribute(Attribute::NoFree);
  return R;
}

static void stripNonValidAttributesFromPrototype(Function &F) {
  LLVMContext &Ctx = F.getContext();

  // Intrinsics are very delicate.  Lowering sometimes depends the presence
  // of certain attributes for correctness, but we may have also inferred
  // additional ones in the abstract machine model which need stripped.  This
  // assumes that the attributes defined in Intrinsic.td are conservatively
  // correct for both physical and abstract model.
  if (Intrinsic::ID id = F.getIntrinsicID()) {
    F.setAttributes(Intrinsic::getAttributes(Ctx, id));
    return;
  }

  AttributeMask R = getParamAndReturnAttributesToRemove();
  for (Argument &A : F.args())
    if (isa<PointerType>(A.getType()))
      F.removeParamAttrs(A.getArgNo(), R);

  if (isa<PointerType>(F.getReturnType()))
    F.removeRetAttrs(R);

  for (auto Attr : FnAttrsToStrip)
    F.removeFnAttr(Attr);
}

/// Certain metadata on instructions are invalid after running RS4GC.
/// Optimizations that run after RS4GC can incorrectly use this metadata to
/// optimize functions. We drop such metadata on the instruction.
static void stripInvalidMetadataFromInstruction(Instruction &I) {
  if (!isa<LoadInst>(I) && !isa<StoreInst>(I))
    return;
  // These are the attributes that are still valid on loads and stores after
  // RS4GC.
  // The metadata implying dereferenceability and noalias are (conservatively)
  // dropped.  This is because semantically, after RewriteStatepointsForGC runs,
  // all calls to gc.statepoint "free" the entire heap. Also, gc.statepoint can
  // touch the entire heap including noalias objects. Note: The reasoning is
  // same as stripping the dereferenceability and noalias attributes that are
  // analogous to the metadata counterparts.
  // We also drop the invariant.load metadata on the load because that metadata
  // implies the address operand to the load points to memory that is never
  // changed once it became dereferenceable. This is no longer true after RS4GC.
  // Similar reasoning applies to invariant.group metadata, which applies to
  // loads within a group.
  unsigned ValidMetadataAfterRS4GC[] = {LLVMContext::MD_tbaa,
                         LLVMContext::MD_range,
                         LLVMContext::MD_alias_scope,
                         LLVMContext::MD_nontemporal,
                         LLVMContext::MD_nonnull,
                         LLVMContext::MD_align,
                         LLVMContext::MD_type};

  // Drops all metadata on the instruction other than ValidMetadataAfterRS4GC.
  I.dropUnknownNonDebugMetadata(ValidMetadataAfterRS4GC);
}

static void stripNonValidDataFromBody(Function &F) {
  if (F.empty())
    return;

  LLVMContext &Ctx = F.getContext();
  MDBuilder Builder(Ctx);

  // Set of invariantstart instructions that we need to remove.
  // Use this to avoid invalidating the instruction iterator.
  SmallVector<IntrinsicInst*, 12> InvariantStartInstructions;

  for (Instruction &I : instructions(F)) {
    // invariant.start on memory location implies that the referenced memory
    // location is constant and unchanging. This is no longer true after
    // RewriteStatepointsForGC runs because there can be calls to gc.statepoint
    // which frees the entire heap and the presence of invariant.start allows
    // the optimizer to sink the load of a memory location past a statepoint,
    // which is incorrect.
    if (auto *II = dyn_cast<IntrinsicInst>(&I))
      if (II->getIntrinsicID() == Intrinsic::invariant_start) {
        InvariantStartInstructions.push_back(II);
        continue;
      }

    if (MDNode *Tag = I.getMetadata(LLVMContext::MD_tbaa)) {
      MDNode *MutableTBAA = Builder.createMutableTBAAAccessTag(Tag);
      I.setMetadata(LLVMContext::MD_tbaa, MutableTBAA);
    }

    stripInvalidMetadataFromInstruction(I);

    AttributeMask R = getParamAndReturnAttributesToRemove();
    if (auto *Call = dyn_cast<CallBase>(&I)) {
      for (int i = 0, e = Call->arg_size(); i != e; i++)
        if (isa<PointerType>(Call->getArgOperand(i)->getType()))
          Call->removeParamAttrs(i, R);
      if (isa<PointerType>(Call->getType()))
        Call->removeRetAttrs(R);
    }
  }

  // Delete the invariant.start instructions and RAUW undef.
  for (auto *II : InvariantStartInstructions) {
    II->replaceAllUsesWith(UndefValue::get(II->getType()));
    II->eraseFromParent();
  }
}

/// Returns true if this function should be rewritten by this pass.  The main
/// point of this function is as an extension point for custom logic.
static bool shouldRewriteStatepointsIn(Function &F) {
  // TODO: This should check the GCStrategy
  if (F.hasGC()) {
    const auto &FunctionGCName = F.getGC();
    const StringRef StatepointExampleName("statepoint-example");
    const StringRef CoreCLRName("coreclr");
    const StringRef OxCamlName("oxcaml");
    return (StatepointExampleName == FunctionGCName) ||
           (CoreCLRName == FunctionGCName) ||
           (OxCamlName == FunctionGCName);
  } else
    return false;
}

static void stripNonValidData(Module &M) {
#ifndef NDEBUG
  assert(llvm::any_of(M, shouldRewriteStatepointsIn) && "precondition!");
#endif

  for (Function &F : M)
    stripNonValidAttributesFromPrototype(F);

  for (Function &F : M)
    stripNonValidDataFromBody(F);
}

static void collectInputGCLiveRootAllocas(
    Function &F, SmallPtrSetImpl<AllocaInst *> &RootAllocas) {
  for (Instruction &I : instructions(F)) {
    auto *Call = dyn_cast<CallBase>(&I);
    if (!Call)
      continue;

    for (unsigned BundleIndex = 0, E = Call->getNumOperandBundles();
         BundleIndex != E; ++BundleIndex) {
      OperandBundleUse Bundle = Call->getOperandBundleAt(BundleIndex);
      if (Bundle.getTagName() != "gc-live")
        continue;

      for (Value *Input : Bundle.Inputs)
        if (auto *AI = dyn_cast<AllocaInst>(Input->stripPointerCasts()))
          RootAllocas.insert(AI);
    }
  }
}

static bool promoteOxCamlGCPointerAllocas(Function &F, DominatorTree &DT) {
  if (!isOxCamlFunction(F))
    return false;

  SmallPtrSet<AllocaInst *, 32> ExplicitRootAllocas;
  collectInputGCLiveRootAllocas(F, ExplicitRootAllocas);
  SmallVector<AllocaInst *, 64> PromotableAllocas;
  for (Instruction &I : instructions(F)) {
    auto *AI = dyn_cast<AllocaInst>(&I);
    if (!AI)
      continue;
    if (!isHandledGCPointerType(AI->getAllocatedType()))
      continue;
    if (!AI->isStaticAlloca() || !isAllocaPromotable(AI)) {
      if (ExplicitRootAllocas.contains(AI))
        continue;
      errs() << "OxCaml GC pointer alloca is not promotable before late root "
                "discovery in "
             << F.getName() << "\n  alloca: ";
      AI->print(errs());
      errs() << "\n";
      for (User *U : AI->users()) {
        errs() << "  user: ";
        U->print(errs());
        errs() << "\n";
      }
      report_fatal_error(
          "unpromotable ordinary OxCaml GC pointer alloca before RS4GC");
    }
    if (DebugOxCamlGCPointerAllocas) {
      errs() << "rs4gc-oxcaml-gc-pointer-alloca-promote in " << F.getName()
             << ": ";
      AI->print(errs());
      errs() << "\n";
    }
    PromotableAllocas.push_back(AI);
  }

  if (DebugOxCamlGCPointerAllocas && !PromotableAllocas.empty())
    errs() << "rs4gc-oxcaml-gc-pointer-alloca-summary in " << F.getName()
           << ": promotable=" << PromotableAllocas.size() << "\n";

  if (PromotableAllocas.empty())
    return false;

  PromoteMemToReg(PromotableAllocas, DT);
  return true;
}

static bool canonicalizeOxCamlGCPointerAllocaAccesses(Function &F) {
  if (!isOxCamlFunction(F))
    return false;

  const DataLayout &DL = F.getParent()->getDataLayout();
  SmallVector<Instruction *, 64> MemoryInsts;
  for (Instruction &I : instructions(F)) {
    if (auto *LI = dyn_cast<LoadInst>(&I)) {
      if (auto *AI = dyn_cast<AllocaInst>(LI->getPointerOperand())) {
        Type *AllocatedTy = AI->getAllocatedType();
        auto *AllocatedPtrTy = dyn_cast<PointerType>(AllocatedTy);
        auto *LoadIntTy = dyn_cast<IntegerType>(LI->getType());
        if (AI->isStaticAlloca() && isHandledGCPointerType(AllocatedTy) &&
            AllocatedPtrTy && LoadIntTy &&
            LoadIntTy->getBitWidth() ==
                DL.getPointerSizeInBits(AllocatedPtrTy->getAddressSpace()))
          MemoryInsts.push_back(LI);
      }
      continue;
    }

    auto *SI = dyn_cast<StoreInst>(&I);
    if (!SI)
      continue;
    auto *AI = dyn_cast<AllocaInst>(SI->getPointerOperand());
    if (!AI)
      continue;
    Type *AllocatedTy = AI->getAllocatedType();
    auto *AllocatedPtrTy = dyn_cast<PointerType>(AllocatedTy);
    auto *StoredIntTy = dyn_cast<IntegerType>(SI->getValueOperand()->getType());
    if (AI->isStaticAlloca() && isHandledGCPointerType(AllocatedTy) &&
        AllocatedPtrTy && StoredIntTy &&
        StoredIntTy->getBitWidth() ==
            DL.getPointerSizeInBits(AllocatedPtrTy->getAddressSpace()))
      MemoryInsts.push_back(SI);
  }

  bool Changed = false;
  for (Instruction *I : MemoryInsts) {
    if (auto *LI = dyn_cast<LoadInst>(I)) {
      auto *AI = cast<AllocaInst>(LI->getPointerOperand());
      IRBuilder<> B(LI);
      auto *PtrLoad = B.CreateAlignedLoad(
          AI->getAllocatedType(), AI, LI->getAlign(),
          suffixed_name_or(LI, ".gcptr.load", "gcptr.load"));
      Value *AsInt = B.CreatePtrToInt(
          PtrLoad, LI->getType(),
          suffixed_name_or(LI, ".gcptr.int", "gcptr.int"));
      LI->replaceAllUsesWith(AsInt);
      LI->eraseFromParent();
      Changed = true;
      continue;
    }

    auto *SI = cast<StoreInst>(I);
    auto *AI = cast<AllocaInst>(SI->getPointerOperand());
    IRBuilder<> B(SI);
    Value *AsPtr = B.CreateIntToPtr(
        SI->getValueOperand(), AI->getAllocatedType(),
        suffixed_name_or(SI->getValueOperand(), ".gcptr.ptr", "gcptr.ptr"));
    B.CreateAlignedStore(AsPtr, AI, SI->getAlign());
    SI->eraseFromParent();
    Changed = true;
  }

  return Changed;
}

bool RewriteStatepointsForGC::runOnFunction(Function &F, DominatorTree &DT,
                                            TargetTransformInfo &TTI,
                                            const TargetLibraryInfo &TLI) {
  assert(!F.isDeclaration() && !F.empty() &&
         "need function body to rewrite statepoints in");
  assert(shouldRewriteStatepointsIn(F) && "mismatch in rewrite decision");

  auto NeedsRewrite = [&TLI](Instruction &I) {
    if (const auto *Call = dyn_cast<CallBase>(&I)) {
      if (isa<GCStatepointInst>(Call))
        return false;
      if (callsGCLeafFunction(Call, TLI))
        return false;

      // Normally it's up to the frontend to make sure that non-leaf calls also
      // have proper deopt state if it is required. We make an exception for
      // element atomic memcpy/memmove intrinsics here. Unlike other intrinsics
      // these are non-leaf by default. They might be generated by the optimizer
      // which doesn't know how to produce a proper deopt state. So if we see a
      // non-leaf memcpy/memmove without deopt state just treat it as a leaf
      // copy and don't produce a statepoint.
      if (!AllowStatepointWithNoDeoptInfo &&
          !Call->getOperandBundle(LLVMContext::OB_deopt)) {
        assert((isa<AtomicMemCpyInst>(Call) || isa<AtomicMemMoveInst>(Call)) &&
               "Don't expect any other calls here!");
        return false;
      }

      // `musttail` calls wrapped in statepoints fail to verify due to
      // the intrinsic using variadic arguments.
      if (Call->isMustTailCall()) return false;
      
      return true;
    }
    return false;
  };

  // Delete any unreachable statepoints so that we don't have unrewritten
  // statepoints surviving this pass.  This makes testing easier and the
  // resulting IR less confusing to human readers.
  DomTreeUpdater DTU(DT, DomTreeUpdater::UpdateStrategy::Lazy);
  bool MadeChange = removeUnreachableBlocks(F, &DTU);
  // Flush the Dominator Tree.
  DTU.getDomTree();

  MadeChange |= sanitizeOxCamlUndefinedGCPointers(F);
  MadeChange |= canonicalizeOxCamlGCPointerAllocaAccesses(F);
  MadeChange |= promoteOxCamlGCPointerAllocas(F, DT);

  // Gather all the statepoints which need rewritten.  Be careful to only
  // consider those in reachable code since we need to ask dominance queries
  // when rewriting.  We'll delete the unreachable ones in a moment.
  SmallVector<CallBase *, 64> ParsePointNeeded;
  SmallVector<CallInst *, 64> Intrinsics;
  for (Instruction &I : instructions(F)) {
    // TODO: only the ones with the flag set!
    if (NeedsRewrite(I)) {
      // NOTE removeUnreachableBlocks() is stronger than
      // DominatorTree::isReachableFromEntry(). In other words
      // removeUnreachableBlocks can remove some blocks for which
      // isReachableFromEntry() returns true.
      assert(DT.isReachableFromEntry(I.getParent()) &&
            "no unreachable blocks expected");
      ParsePointNeeded.push_back(cast<CallBase>(&I));
    }
    if (auto *CI = dyn_cast<CallInst>(&I))
      if (CI->getIntrinsicID() == Intrinsic::experimental_gc_get_pointer_base ||
          CI->getIntrinsicID() == Intrinsic::experimental_gc_get_pointer_offset)
        Intrinsics.emplace_back(CI);
  }

  // Return early if no work to do.
  if (ParsePointNeeded.empty() && Intrinsics.empty())
    return MadeChange;

  // As a prepass, go ahead and aggressively destroy single entry phi nodes.
  // These are created by LCSSA.  They have the effect of increasing the size
  // of liveness sets for no good reason.  It may be harder to do this post
  // insertion since relocations and base phis can confuse things.
  for (BasicBlock &BB : F)
    if (BB.getUniquePredecessor())
      MadeChange |= FoldSingleEntryPHINodes(&BB);

  // Before we start introducing relocations, we want to tweak the IR a bit to
  // avoid unfortunate code generation effects.  The main example is that we
  // want to try to make sure the comparison feeding a branch is after any
  // safepoints.  Otherwise, we end up with a comparison of pre-relocation
  // values feeding a branch after relocation.  This is semantically correct,
  // but results in extra register pressure since both the pre-relocation and
  // post-relocation copies must be available in registers.  For code without
  // relocations this is handled elsewhere, but teaching the scheduler to
  // reverse the transform we're about to do would be slightly complex.
  // Note: This may extend the live range of the inputs to the icmp and thus
  // increase the liveset of any statepoint we move over.  This is profitable
  // as long as all statepoints are in rare blocks.  If we had in-register
  // lowering for live values this would be a much safer transform.
  auto getConditionInst = [](Instruction *TI) -> Instruction * {
    if (auto *BI = dyn_cast<BranchInst>(TI))
      if (BI->isConditional())
        return dyn_cast<Instruction>(BI->getCondition());
    // TODO: Extend this to handle switches
    return nullptr;
  };
  for (BasicBlock &BB : F) {
    Instruction *TI = BB.getTerminator();
    if (auto *Cond = getConditionInst(TI))
      // TODO: Handle more than just ICmps here.  We should be able to move
      // most instructions without side effects or memory access.
      if (isa<ICmpInst>(Cond) && Cond->hasOneUse()) {
        MadeChange = true;
        Cond->moveBefore(TI);
      }
  }

  // Nasty workaround - The base computation code in the main algorithm doesn't
  // consider the fact that a GEP can be used to convert a scalar to a vector.
  // The right fix for this is to integrate GEPs into the base rewriting
  // algorithm properly, this is just a short term workaround to prevent
  // crashes by canonicalizing such GEPs into fully vector GEPs.
  for (Instruction &I : instructions(F)) {
    if (!isa<GetElementPtrInst>(I))
      continue;

    unsigned VF = 0;
    for (unsigned i = 0; i < I.getNumOperands(); i++)
      if (auto *OpndVTy = dyn_cast<VectorType>(I.getOperand(i)->getType())) {
        assert(VF == 0 ||
               VF == cast<FixedVectorType>(OpndVTy)->getNumElements());
        VF = cast<FixedVectorType>(OpndVTy)->getNumElements();
      }

    // It's the vector to scalar traversal through the pointer operand which
    // confuses base pointer rewriting, so limit ourselves to that case.
    if (!I.getOperand(0)->getType()->isVectorTy() && VF != 0) {
      IRBuilder<> B(&I);
      auto *Splat = B.CreateVectorSplat(VF, I.getOperand(0));
      I.setOperand(0, Splat);
      MadeChange = true;
    }
  }

  // Cache the 'defining value' relation used in the computation and
  // insertion of base phis and selects.  This ensures that we don't insert
  // large numbers of duplicate base_phis. Use one cache for both
  // inlineGetBaseAndOffset() and insertParsePoints().
  DefiningValueMapTy DVCache;

  // Mapping between a base values and a flag indicating whether it's a known
  // base or not.
  IsKnownBaseMapTy KnownBases;

  if (!Intrinsics.empty())
    // Inline @gc.get.pointer.base() and @gc.get.pointer.offset() before finding
    // live references.
    MadeChange |= inlineGetBaseAndOffset(F, Intrinsics, DVCache, KnownBases);

  if (CanonicalizeOxCamlRawHeapMemoryAddresses)
    MadeChange |= canonicalizeOxCamlRawHeapMemoryAddresses(F);
  MadeChange |= exposeGCPointersInAggregates(F, DT);
  checkNoUnsupportedGCPointerAggregateUses(F);

  if (!ParsePointNeeded.empty())
    MadeChange |=
        insertParsePoints(F, DT, TTI, ParsePointNeeded, DVCache, KnownBases);

  return MadeChange;
}

// liveness computation via standard dataflow
// -------------------------------------------------------------------

// TODO: Consider using bitvectors for liveness, the set of potentially
// interesting values should be small and easy to pre-compute.

/// Compute the live-in set for the location rbegin starting from
/// the live-out set of the basic block
static void computeLiveInValues(BasicBlock::reverse_iterator Begin,
                                BasicBlock::reverse_iterator End,
                                SetVector<Value *> &LiveTmp) {
  for (auto &I : make_range(Begin, End)) {
    // KILL/Def - Remove this definition from LiveIn
    LiveTmp.remove(&I);

    // Don't consider *uses* in PHI nodes, we handle their contribution to
    // predecessor blocks when we seed the LiveOut sets
    if (isa<PHINode>(I))
      continue;

    // USE - Add to the LiveIn set for this instruction
    for (Value *V : I.operands()) {
      if (isHandledGCPointerType(V->getType()) && !isa<Constant>(V)) {
        // The choice to exclude all things constant here is slightly subtle.
        // There are two independent reasons:
        // - We assume that things which are constant (from LLVM's definition)
        // do not move at runtime.  For example, the address of a global
        // variable is fixed, even though it's contents may not be.
        // - Second, we can't disallow arbitrary inttoptr constants even
        // if the language frontend does.  Optimization passes are free to
        // locally exploit facts without respect to global reachability.  This
        // can create sections of code which are dynamically unreachable and
        // contain just about anything.  (see constants.ll in tests)
        LiveTmp.insert(V);
      }
    }
  }
}

static void computeLiveOutSeed(BasicBlock *BB, SetVector<Value *> &LiveTmp) {
  for (BasicBlock *Succ : successors(BB)) {
    for (auto &I : *Succ) {
      PHINode *PN = dyn_cast<PHINode>(&I);
      if (!PN)
        break;

      Value *V = PN->getIncomingValueForBlock(BB);
      if (isHandledGCPointerType(V->getType()) && !isa<Constant>(V))
        LiveTmp.insert(V);
    }
  }
}

static void insertAvailableLiveInValues(BasicBlock *BB,
                                        const SetVector<Value *> &SuccLiveIn,
                                        SetVector<Value *> &LiveOut,
                                        DominatorTree &DT) {
  Instruction *Term = BB->getTerminator();

  for (Value *V : SuccLiveIn) {
    if (auto *I = dyn_cast<Instruction>(V)) {
      if (I != Term && !DT.dominates(I, Term))
        continue;
    }

    LiveOut.insert(V);
  }
}

static SetVector<Value *> computeKillSet(BasicBlock *BB) {
  SetVector<Value *> KillSet;
  for (Instruction &I : *BB)
    if (isHandledGCPointerType(I.getType()))
      KillSet.insert(&I);
  return KillSet;
}

#ifndef NDEBUG
/// Check that the items in 'Live' dominate 'TI'.  This is used as a basic
/// validation check for the liveness computation.
static void checkBasicSSA(DominatorTree &DT, SetVector<Value *> &Live,
                          Instruction *TI, bool TermOkay = false) {
  for (Value *V : Live) {
    if (auto *I = dyn_cast<Instruction>(V)) {
      // The terminator can be a member of the LiveOut set.  LLVM's definition
      // of instruction dominance states that V does not dominate itself.  As
      // such, we need to special case this to allow it.
      if (TermOkay && TI == I)
        continue;
      assert(DT.dominates(I, TI) &&
             "basic SSA liveness expectation violated by liveness analysis");
    }
  }
}

/// Check that all the liveness sets used during the computation of liveness
/// obey basic SSA properties.  This is useful for finding cases where we miss
/// a def.
static void checkBasicSSA(DominatorTree &DT, GCPtrLivenessData &Data,
                          BasicBlock &BB) {
  checkBasicSSA(DT, Data.LiveSet[&BB], BB.getTerminator());
  checkBasicSSA(DT, Data.LiveOut[&BB], BB.getTerminator(), true);
  checkBasicSSA(DT, Data.LiveIn[&BB], BB.getTerminator());
}
#endif

static void computeLiveInValues(DominatorTree &DT, Function &F,
                                GCPtrLivenessData &Data) {
  SmallSetVector<BasicBlock *, 32> Worklist;

  // Seed the liveness for each individual block
  for (BasicBlock &BB : F) {
    Data.KillSet[&BB] = computeKillSet(&BB);
    Data.LiveSet[&BB].clear();
    computeLiveInValues(BB.rbegin(), BB.rend(), Data.LiveSet[&BB]);

#ifndef NDEBUG
    for (Value *Kill : Data.KillSet[&BB])
      assert(!Data.LiveSet[&BB].count(Kill) && "live set contains kill");
#endif

    Data.LiveOut[&BB] = SetVector<Value *>();
    computeLiveOutSeed(&BB, Data.LiveOut[&BB]);
    Data.LiveIn[&BB] = Data.LiveSet[&BB];
    Data.LiveIn[&BB].set_union(Data.LiveOut[&BB]);
    Data.LiveIn[&BB].set_subtract(Data.KillSet[&BB]);
    if (!Data.LiveIn[&BB].empty())
      Worklist.insert(pred_begin(&BB), pred_end(&BB));
  }

  // Propagate that liveness until stable
  while (!Worklist.empty()) {
    BasicBlock *BB = Worklist.pop_back_val();

    // Compute our new liveout set, then exit early if it hasn't changed despite
    // the contribution of our successor.
    SetVector<Value *> LiveOut = Data.LiveOut[BB];
    const auto OldLiveOutSize = LiveOut.size();
    for (BasicBlock *Succ : successors(BB)) {
      assert(Data.LiveIn.count(Succ));
      insertAvailableLiveInValues(BB, Data.LiveIn[Succ], LiveOut, DT);
    }
    // assert OutLiveOut is a subset of LiveOut
    if (OldLiveOutSize == LiveOut.size()) {
      // If the sets are the same size, then we didn't actually add anything
      // when unioning our successors LiveIn.  Thus, the LiveIn of this block
      // hasn't changed.
      continue;
    }
    Data.LiveOut[BB] = LiveOut;

    // Apply the effects of this basic block
    SetVector<Value *> LiveTmp = LiveOut;
    LiveTmp.set_union(Data.LiveSet[BB]);
    LiveTmp.set_subtract(Data.KillSet[BB]);

    assert(Data.LiveIn.count(BB));
    const SetVector<Value *> &OldLiveIn = Data.LiveIn[BB];
    // assert: OldLiveIn is a subset of LiveTmp
    if (OldLiveIn.size() != LiveTmp.size()) {
      Data.LiveIn[BB] = LiveTmp;
      Worklist.insert(pred_begin(BB), pred_end(BB));
    }
  } // while (!Worklist.empty())

#ifndef NDEBUG
  // Verify our output against SSA properties.  This helps catch any
  // missing kills during the above iteration.
  for (BasicBlock &BB : F)
    checkBasicSSA(DT, Data, BB);
#endif
}

static void findLiveSetAtInst(Instruction *Inst, GCPtrLivenessData &Data,
                              StatepointLiveSetTy &Out) {
  BasicBlock *BB = Inst->getParent();

  // Note: The copy is intentional and required
  assert(Data.LiveOut.count(BB));
  SetVector<Value *> LiveOut = Data.LiveOut[BB];

  // We want to handle the statepoint itself oddly.  It's
  // call result is not live (normal), nor are it's arguments
  // (unless they're used again later).  This adjustment is
  // specifically what we need to relocate
  computeLiveInValues(BB->rbegin(), Inst->getIterator().getReverse(), LiveOut);
  LiveOut.remove(Inst);
  Out.insert(LiveOut.begin(), LiveOut.end());
}

static void recomputeLiveInValues(GCPtrLivenessData &RevisedLivenessData,
                                  CallBase *Call,
                                  PartiallyConstructedSafepointRecord &Info,
                                  PointerToBaseTy &PointerToBase) {
  StatepointLiveSetTy Updated;
  findLiveSetAtInst(Call, RevisedLivenessData, Updated);

  // We may have base pointers which are now live that weren't before.  We need
  // to update the PointerToBase structure to reflect this.
  for (auto *V : Updated)
    PointerToBase.insert({ V, V });

  Info.LiveSet = Updated;
}
