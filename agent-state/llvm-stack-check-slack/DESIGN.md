Plan: native-style slack stack checks for the AArch64 LLVM backend
Objective for the coding agent

Implement a slack-based stack-check protocol for the OxCaml AArch64 LLVM backend:

Most functions:
  no LLVM machine-prologue stack check

Small leaf functions:
  no stack check at all

Small non-leaf / dynamic-stack functions:
  no prologue check
  ordinary CFG stack check at the CFG-selected point

Large LLVM prologue-frame functions:
  keep special LLVM prologue stack check

This is meant to recover the native backend’s efficiency model in the LLVM backend. The native path already uses CFG/native stack-check placement and emits a compact AArch64 check against Domain_current_stack + stack_ctx + threshold + max_frame_size; the LLVM path currently records CFG stack-check bytes but discards AArch64 CFG checks and instead lets LLVM frame lowering emit a heavy machine-prologue inline-asm check from the legacy "oxcaml-stack-check" attribute.

Core invariant

Use this invariant:

At every OCaml function entry:
  at least entry_slack_bytes are available.

Initial implementation should define:

entry_slack_bytes = Stack_check.stack_threshold_size

Then classify the LLVM-generated machine prologue:

llvm_prefix_bytes =
  final AArch64 frame consumption before any LLVM IR CFG stack check can run
  including realignment padding

A function may skip the special machine-prologue check only when:

llvm_prefix_bytes <= spendable_prefix_bytes

For a first safe implementation:

leaf/no-CFG-check function:
  spendable_prefix_bytes = entry_slack_bytes

function with ordinary CFG stack checks:
  spendable_prefix_bytes = entry_slack_bytes - ordinary_check_slow_path_reserve

Do not hard-code an optimistic reserve blindly. Start conservatively. If using caml_llvm_call_realloc_stack, remember that it pushes at least FP/LR on the OCaml stack before saving registers; the no-stack-using prologue slow path exists specifically because the prologue check can fail when no OCaml stack space is available.

A practical staged policy is:

M1:
  only skip prologue checks for functions with cfg_stack_check_bytes = 0
  and small LLVM prefix

M2/M3:
  lower CFG Stack_check to real LLVM checks
  then skip prologue checks for cfg_stack_check_bytes > 0 only when the
  LLVM prefix leaves enough reserve for the ordinary slow path
Non-negotiable safety rules
Never skip all checks merely because a function is small.
A small non-tail caller must reestablish callee-entry slack before calling another OCaml function.
Do not skip prologue checks for CFG-checked functions until Llvmize.stack_check really emits an AArch64 check.
Current AArch64 Llvmize.stack_check intentionally emits nothing because a normal LLVM IR check is too late for large prologue frames.
Keep -no-cfg-stack-checks legacy-safe.
If the precise "oxcaml-stack-check-bytes" contract is absent, keep current conservative machine-prologue behavior.
Keep -no-stack-checks exact.
No OxCaml stack-check attributes, no _caml_llvm_prologue_realloc_stack, no ordinary stack-check calls.
A large final LLVM frame still needs a prologue check.
CFG checks execute after LLVM has already adjusted sp, so they cannot protect a large unchecked prologue frame.
Milestone 0: strengthen the test harness before changing behavior
Goal

Add tests that can distinguish:

1. CFG stack-check insertion
2. LLVM IR stack-check attributes
3. machine-prologue stack checks
4. ordinary LLVM stack-check slow-path calls
5. runtime Stack_overflow behavior

The existing stack_check_size_contract.sh already compares CFG stack-check bytes with the LLVM IR "oxcaml-stack-check-bytes" attribute and tests -no-cfg-stack-checks / -no-stack-checks; extend it instead of replacing it.

Add generated test functions

Add these to the generated ML test source:

external opaque : 'a -> 'a = "%opaque"

let[@inline never] small_leaf x =
  opaque (x + 1)

let[@inline never] callee x =
  opaque (x + 1)

let[@inline never] small_non_tail_call x =
  callee x + 1

let[@inline never] small_tail_call x =
  callee x

let rec[@inline never] small_recursive_non_tail n =
  if n = 0 then 0 else small_recursive_non_tail (n - 1) + 1

let[@inline never] hot_leaf_cold_call b x =
  if b then opaque (x + 1) else callee x + 1

external noalloc_too_many :
  int -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int -> int ->
  int = "" "noalloc_too_many" [@@noalloc]

let[@inline never] noalloc_outgoing_stack_args x =
  noalloc_too_many x 1 2 3 4 5 6 7
    8 9 10 11 12 13 14 15
    16 17 18 19 20 21 22 23
    24 25 26 27 28 29 30 31
    32 33 34 35 36 37 38 39
    40 41 42 43 44 45 46 47

Later milestones should add a forced-large-frame case.

Add shell helpers

Add helpers to the script:

function_asm() {
  name="$1"
  awk -v name="camlStack_check_size_contract_generated__${name}_" '
    $0 ~ name { in_fn = 1 }
    in_fn { print }
    in_fn && /^\s*\.cfi_endproc/ { in_fn = 0 }
  ' "$asm"
}

has_prologue_realloc() {
  function_asm "$1" | grep -q '_caml_llvm_prologue_realloc_stack'
}

has_ordinary_realloc() {
  function_asm "$1" | grep -q '_caml_llvm_call_realloc_stack'
}

assert_has_prologue_realloc() {
  if ! has_prologue_realloc "$1"; then
    echo "$1: expected LLVM prologue stack check" >&2
    exit 1
  fi
}

assert_no_prologue_realloc() {
  if has_prologue_realloc "$1"; then
    echo "$1: unexpected LLVM prologue stack check" >&2
    exit 1
  fi
}

assert_has_ordinary_realloc() {
  if ! has_ordinary_realloc "$1"; then
    echo "$1: expected ordinary LLVM stack check" >&2
    exit 1
  fi
}

assert_no_ordinary_realloc() {
  if has_ordinary_realloc "$1"; then
    echo "$1: unexpected ordinary LLVM stack check" >&2
    exit 1
  fi
}
Acceptance criteria

Before any behavior change, the extended test should still pass and should clearly report the current state:

small_leaf:
  CFG bytes = 0

small_non_tail_call:
  CFG bytes > 0

noalloc_outgoing_stack_args:
  CFG bytes > 0

-no-stack-checks:
  no "oxcaml-stack-check" attributes
  no _caml_llvm_prologue_realloc_stack

-no-cfg-stack-checks:
  no "oxcaml-stack-check-bytes"
  legacy "oxcaml-stack-check" still present
Milestone 1: split “OxCaml stack protocol” from “needs prologue check”
Goal

Stop treating:

"oxcaml-stack-check"="true"

as meaning:

always emit a machine-prologue stack check

Instead, treat it as:

this function participates in the OxCaml stack-check protocol

The actual prologue-check decision must depend on final or conservatively estimated LLVM frame size and the CFG byte contract.

Files
vendor/llvm-project/llvm/lib/Target/AArch64/AArch64FrameLowering.cpp
backend/llvm/llvm_ir.ml
backend/llvm/llvm_ir.mli
backend/llvm/llvmize.ml
testsuite/tests/llvm-codegen/stack_check_size_contract.sh
Implementation

In AArch64FrameLowering.cpp, replace the single predicate:

static bool needsOxCamlStackCheck(const MachineFunction &MF) {
  return MF.getFunction().hasFnAttribute("oxcaml-stack-check");
}

with helpers like:

struct OxCamlStackCheckAttrs {
  bool Requested = false;
  bool HasCfgBytes = false;
  uint64_t CfgBytes = 0;
};

static OxCamlStackCheckAttrs getOxCamlStackCheckAttrs(
    const MachineFunction &MF);

static bool hasOxCamlStackCheckProtocol(const MachineFunction &MF);

static bool needsOxCamlPrologueStackCheck(
    const MachineFunction &MF,
    uint64_t PrefixBytes,
    bool ConservativeEstimate);

Initial policy:

if (!attrs.Requested)
  return false;

/* Preserve legacy safety when CFG stack-check insertion did not run. */
if (!attrs.HasCfgBytes)
  return true;

/* Safe immediate win: leaf/small functions with no CFG stack-check need. */
if (attrs.CfgBytes == 0)
  return PrefixBytes > StackThresholdBytes;

/* Until ordinary CFG stack checks are lowered, keep the prologue check. */
return true;

Use the new prologue-specific predicate in all places currently controlled by needsOxCamlStackCheck, especially:

emitOxCamlStackCheck call sites
CombineSPBump decision
determineCalleeSaves FP/LR-forcing condition

The current frame lowering disables combined SP bump and forces FP/LR saves based on the broad needsOxCamlStackCheck predicate, which is part of the current inefficiency.

Acceptance criteria

Expected behavior after Milestone 1:

small_leaf:
  cfg_stack_check_bytes = 0
  no _caml_llvm_prologue_realloc_stack if LLVM prefix <= threshold

small_non_tail_call:
  cfg_stack_check_bytes > 0
  still has _caml_llvm_prologue_realloc_stack

noalloc_outgoing_stack_args:
  cfg_stack_check_bytes > 0
  still has _caml_llvm_prologue_realloc_stack

-no-cfg-stack-checks:
  legacy prologue behavior preserved

-no-stack-checks:
  no stack-check attributes or prologue calls

Add a forced-large-leaf test if feasible:

large_leaf:
  cfg_stack_check_bytes = 0
  LLVM prefix > threshold
  still has _caml_llvm_prologue_realloc_stack
Important note

If cfg_stack_check_bytes > 0 and ordinary CFG checks are still not emitted, the prologue check should conservatively request enough space for the larger of:

LLVM prefix bytes
CFG stack-check bytes

That is safer than the current LLVM prologue behavior, which bases the required words on the LLVM stack size alone. The existing CFG contract already computes and exports the byte count.

Milestone 2: lower AArch64 CFG Stack_check to a real LLVM check
Goal

Make this no longer true:

let stack_check _t _i _max_frame_size_bytes =
  match Target_system.architecture () with
  | AArch64 -> ()

Instead, emit an ordinary native-style check at each CFG-selected Stack_check.

Files
backend/llvm/llvmize.ml
backend/llvm/llvm_ir.ml, if new helper IR constructors are needed
runtime/arm64.S, only if a new slow-path ABI is introduced
testsuite/tests/llvm-codegen/stack_check_size_contract.sh
Hot-path shape

Emit logic equivalent to the native AArch64 path:

limit = Domain_current_stack
limit += Domainstate.stack_ctx_words * 8
limit += Stack_check.stack_threshold_size
limit += max_frame_size_bytes

if sp < limit:
  slow path
else:
  continue

The native emitter already does this shape for Lstackcheck: it loads Domain_current_stack, adds the threshold and requested frame size, compares against sp, and branches to local stack reallocation on failure.

Slow path option A: use existing caml_llvm_call_realloc_stack

Emit a call to:

caml_llvm_call_realloc_stack(required_words)

where:

required_words =
  StackThresholdWords + align(max_frame_size_bytes, 8) / 8

The runtime assembly for caml_llvm_call_realloc_stack expects the required space in C_ARG_1.

Use the appropriate OxCaml allocation/runtime calling convention, add the symbol to referenced symbols, and mark the slow call cold. If the existing GC/liveness conventions around calls require live roots, mirror poll/caml_call_gc handling rather than inventing a new convention.

Slow path option B, later optimization

Add or reuse a no-OCaml-stack-push slow path similar to _caml_llvm_prologue_realloc_stack. This would allow a larger spendable prefix because the ordinary slow path would not require emergency stack reserve. Do not make this part of the first correctness patch unless option A proves too conservative.

Acceptance criteria

After Milestone 2, with prologue checks still retained for CFG-checked functions if necessary:

small_leaf:
  no ordinary stack check
  no prologue stack check if small

small_non_tail_call:
  CFG stack_check exists
  assembly contains ordinary slow-path reference/call

noalloc_outgoing_stack_args:
  CFG stack_check exists
  assembly contains ordinary slow-path reference/call

-no-stack-checks:
  no ordinary or prologue stack-check calls

-no-cfg-stack-checks:
  no ordinary CFG checks
  legacy prologue behavior remains

Runtime tests:

let rec f n =
  if n = 0 then 0 else f (n - 1) + 1

let test () =
  try ignore (f very_large_n); false
  with Stack_overflow -> true

Compile with:

-llvm-backend
-stack-checks
small stack settings if available

Expected:

Stack_overflow is catchable
no segfault
no silent corruption
Milestone 3: enable prologue elision for CFG-checked small-prefix functions
Goal

This is the main performance milestone.

Once ordinary CFG checks are real, change the prologue predicate to:

if (!attrs.Requested)
  return false;

if (!attrs.HasCfgBytes)
  return true;  // legacy -no-cfg-stack-checks path

if (attrs.CfgBytes == 0)
  return PrefixBytes > EntrySlackBytes;

return PrefixBytes > SpendablePrefixBytes;

Where:

EntrySlackBytes = Stack_check.stack_threshold_size

SpendablePrefixBytes =
  EntrySlackBytes - OrdinaryCheckSlowPathReserveBytes

Start conservative. Make the reserve a named constant with a comment explaining that it protects caml_llvm_call_realloc_stack if the ordinary check fails after the LLVM prologue consumed part of the entry slack.

Acceptance criteria

Expected behavior:

small_leaf:
  no _caml_llvm_prologue_realloc_stack
  no _caml_llvm_call_realloc_stack

small_non_tail_call:
  no _caml_llvm_prologue_realloc_stack
  has ordinary CFG stack check

small_tail_call:
  no prologue stack check solely due to tail call
  no ordinary stack check unless its own frame requires one

noalloc_outgoing_stack_args:
  no _caml_llvm_prologue_realloc_stack if prefix is small
  has ordinary CFG stack check before/dominating stack-args path

large_prefix_function:
  has _caml_llvm_prologue_realloc_stack

-no-cfg-stack-checks:
  prologue behavior remains conservative

-no-stack-checks:
  still emits no checks

Runtime tests:

small recursive non-tail call:
  raises Stack_overflow cleanly

small mutually recursive non-tail functions:
  raises Stack_overflow cleanly

tail-recursive / tail-call path:
  no new stack-check overhead unless required by frame size

This milestone is complete only when small_non_tail_call has ordinary stack checking but no machine-prologue stack check.

Milestone 4: make ordinary LLVM checks native-quality
Goal

Ensure the ordinary CFG check’s fast path is as close as possible to native AArch64.

Target shape:

ldr   tmp, [x28, #Domain_current_stack]
add   tmp, tmp, #stack_ctx_plus_threshold_plus_required
cmp   sp, tmp
b.hs  .Lok
; cold slow path

The ordinary check must not include:

_caml_plat_pagesize GOT load
_caml_llvm_prologue_realloc_stack
LR save/restore on the hot path
large inline asm blob

The current LLVM prologue check is heavy: it saves/restores LR on the fast path, loads _caml_plat_pagesize through the GOT, computes required words, compares sp, and branches to _caml_llvm_prologue_realloc_stack.

Implementation guidance

If LLVM IR lowering produces poor code, add a late MIR pseudo or controlled inline-asm helper for the ordinary check. The ordinary fast path should be visible enough to assert with assembly tests.

Acceptance criteria

For small_non_tail_call, assembly should contain:

one load of Domain_current_stack or equivalent domain-state load
one compare against sp
one conditional branch
one cold slow-path call/reference
no _caml_llvm_prologue_realloc_stack
no _caml_plat_pagesize

For small_leaf, assembly should contain none of the above stack-check machinery.

Milestone 5: refine check sizes from “whole-function max” to “required from here”
Goal

Avoid overchecking branchy functions.

Current CFG stack-check insertion computes a global max_frame_size and inserts Cfg.Stack_check { max_frame_size_bytes }. The pass identifies blocks needing checks via non-tail calls or max_frame_size >= Stack_check.stack_threshold_size, then chooses a dominating block while avoiding pushing checks into loops; it already notes that duplicating checks may be desirable later.

The first working implementation can keep using the global max. This milestone makes it better.

Change the CFG payload

Extend:

Stack_check of { max_frame_size_bytes : int }

to something like:

Stack_check of
  { max_frame_size_bytes : int;
    required_from_check_bytes : int;
    stack_offset_at_check : int
  }

Or, less invasively:

Stack_check of
  { max_frame_size_bytes : int;
    check_frame_size_bytes : int
  }

Then lower LLVM ordinary checks using the amount needed from the check point, not the whole-function max.

Test cases
Hot leaf path, cold call path
let[@inline never] hot_leaf_cold_call b x =
  if b then opaque (x + 1) else callee x + 1

Expected after CFG placement:

hot leaf path:
  no stack check

cold call path:
  ordinary stack check
Hot small call path, cold large stack path
let[@inline never] hot_call_cold_stack b x =
  if b then callee x + 1
  else noalloc_outgoing_stack_args x

Initial acceptable behavior:

one dominating ordinary check

Optimized behavior:

hot path gets small required check
cold path gets large required check
or cold-only large check if hot path is otherwise covered
Loop case
let[@inline never] loop_calls n =
  let rec loop i acc =
    if i = 0 then acc else loop (i - 1) (callee acc)
  in
  loop n 0

Expected:

do not push stack check into the loop body if a pre-loop check covers it

This matches the current CFG pass’s loop-avoidance policy.

Milestone 6: clean up the rare prologue check path
Goal

Once prologue checks are rare, optimize the remaining cases.

Tasks
6.1 Move LR traffic out of the prologue fast path

Current inline asm begins with:

mov x17, x30
...
mov x30, x17

The fast path should not need to copy/restore LR if it does not clobber x30. Use scratch registers for the fast-path computation and move LR preservation into the cold path immediately before calling _caml_llvm_prologue_realloc_stack.

Acceptance:

large-prefix function still works
fast path no longer contains unconditional mov x17, x30 / mov x30, x17 pair
Stack_overflow remains catchable
6.2 Use CFG bytes in prologue required size

For a large-prefix function with CFG stack checks, the prologue check should request enough for whichever amount the prologue is intended to cover:

required_bytes =
  max(llvm_prefix_bytes, cfg_stack_check_bytes)

or, after precise CFG checks exist:

required_bytes =
  llvm_prefix_bytes + entry_slack_bytes

with ordinary CFG checks left to cover later dynamic stack use.

6.3 Replace inline asm with MachineInstrs

Replace TargetOpcode::INLINEASM with explicit AArch64 MachineInstr emission.

Acceptance:

no INLINEASM for OxCaml prologue stack check
same or better assembly
LLVM verifier passes
unwind/CFI behavior remains valid

The current implementation emits the whole prologue check as an inline asm MachineInstr.

Milestone 7: performance and regression gates
Add a summary script

Add a script that compiles a representative set of files with:

-llvm-backend -S -keep-llvmir -dcfg -dump-into-file

and reports:

number of functions
number of functions with cfg_stack_check_bytes = 0
number of functions with cfg_stack_check_bytes > 0
number of _caml_llvm_prologue_realloc_stack references
number of _caml_llvm_call_realloc_stack references
number of functions where both appear
number of functions with _caml_plat_pagesize in stack-check path
Success target

For the generated tests:

small_leaf:
  0 prologue checks
  0 ordinary checks

small_non_tail_call:
  0 prologue checks
  1 ordinary check

noalloc_outgoing_stack_args:
  0 prologue checks if LLVM prefix is small
  1 ordinary check

large_prefix_function:
  1 prologue check

-no-cfg-stack-checks:
  conservative legacy behavior

-no-stack-checks:
  no checks

For a broader benchmark corpus:

prologue stack-check references should drop sharply
ordinary checks should appear only where CFG stack checks exist
small leaf functions should not force FP/LR saves solely because of stack checks

The frame lowering currently forces FP/LR saves when needsOxCamlStackCheck(MF) is true and estimated stack size is nonzero; after this work, that should happen only for functions that truly need the special prologue check.

Recommended commit sequence for the coding agent
Commit 1: test harness only

Deliverables:

extended stack_check_size_contract.sh
new generated functions
helpers for per-function assembly checks
no behavior changes

Pass criteria:

existing and extended tests pass under current behavior
Commit 2: parse CFG byte attribute in AArch64 frame lowering

Deliverables:

getOxCamlStackCheckAttrs
hasOxCamlStackCheckProtocol
needsOxCamlPrologueStackCheck

Behavior:

only cfg_bytes = 0 small-prefix functions skip prologue checks
cfg_bytes > 0 functions remain conservative
-no-cfg-stack-checks remains legacy

Pass criteria:

small_leaf has no prologue check
non_tail_call still has prologue check
noalloc_outgoing_stack_args still has prologue check
no-stack-checks unchanged
Commit 3: emit ordinary AArch64 LLVM CFG stack checks

Deliverables:

Llvmize.stack_check emits real AArch64 check
ordinary slow path calls caml_llvm_call_realloc_stack or chosen no-stack slow path
tests detect ordinary checks

Behavior:

may still retain prologue checks for cfg_bytes > 0

Pass criteria:

non_tail_call has ordinary check
noalloc_outgoing_stack_args has ordinary check
runtime Stack_overflow tests pass
Commit 4: enable prologue elision for small-prefix CFG-checked functions

Deliverables:

prologue predicate uses spendable_prefix_bytes
ordinary CFG checks now carry semantic responsibility for calls/dynamic stack

Pass criteria:

non_tail_call:
  no prologue check
  ordinary check present

noalloc_outgoing_stack_args:
  no prologue check if prefix small
  ordinary check present

deep non-tail recursion:
  catchable Stack_overflow
Commit 5: improve ordinary check code quality

Deliverables:

native-like fast path
no page-size GOT load
no prologue slow-path call in ordinary checks
assembly assertions

Pass criteria:

ordinary checked function fast path is compact
small leaf functions remain check-free
Commit 6: precise per-check sizes and optional duplication

Deliverables:

extended Stack_check payload or equivalent metadata
branch-local required sizes
optional duplication for hot/cold branches

Pass criteria:

hot leaf / cold call function: hot path has no check
hot small call / cold large stack function: no unnecessary large hot-path check
loop test: no check sunk into loop body
Final definition of done

The feature is complete when all of these are true:

1. AArch64 LLVM no longer emits prologue stack checks just because
   "oxcaml-stack-check"="true".

2. Small leaf LLVM functions with cfg_stack_check_bytes = 0 emit no stack checks.

3. Small non-tail callers emit ordinary CFG stack checks, not machine-prologue
   checks.

4. Large final LLVM prologue frames still emit the special prologue check.

5. -no-cfg-stack-checks remains conservative and safe.

6. -no-stack-checks emits no stack-check attributes or stack-check calls.

7. Stack_overflow remains catchable under small-stack runtime tests.

8. The number of _caml_llvm_prologue_realloc_stack references falls sharply
   across the LLVM codegen test corpus.

9. Ordinary stack checks have native-like hot-path assembly.

The key architectural point for the agent: do not remove stack checks; move them back to the CFG/native slack model, and reserve the LLVM machine-prologue check only for the cases where LLVM’s own frame allocation would consume too much stack before any CFG check can run.