# Stack Check Slack Design

The consensus design is to keep CFG Stack_check as the main/native-like
mechanism, and treat the LLVM machine prologue check only as protection for
stack bytes spent before the first ordinary CFG check can run.

## Suggested Contract

`oxcaml-stack-check` is only a protocol marker. It should not by itself force a
prologue check, force FP/LR saves, or disable combined SP bumps.

`oxcaml-stack-check-bytes=N` is present only when CFG stack-check insertion ran.
`N = 0` means no ordinary CFG check was needed; `N > 0` means ordinary CFG checks
exist and `N` is the maximum ordinary check payload.

`oxcaml-stack-check-before-bytes` should either be explicitly defined as, or
replaced by a clearer attr meaning, unchecked CFG bytes from function entry until
the first ordinary stack check, return, or raise. It should exclude LLVM
machine-prologue bytes, since only LLVM frame lowering knows those.

## Consumer Rule

`P` = final LLVM machine prologue bytes before any IR check can run

`U` = unchecked CFG bytes before first ordinary stack check/return/raise

`R` = named reserve for ordinary slow-path helper stack use

`E` = entry slack, currently `Stack_threshold_size`

With detailed CFG attrs present, omit the LLVM prologue check only if:

```text
P + U + R < E
```

Otherwise emit the prologue check.

Absence of detailed CFG byte attrs should mean unknown, not zero, so that case
should remain conservative.

## Runtime/Prologue Details

Ordinary LLVM checks should keep using the native-shaped limit:

```text
current_stack + Stack_ctx_words * word + Stack_threshold + payload_bytes
```

Prologue checks should use the same stack-base arithmetic, not
`2 * caml_plat_pagesize`; stack-check-enabled runtime stacks use the
`Stack_ctx_words` layout, not guard-page layout.

Validation note: a direct prototype of this change passed the focused
LLVM-codegen, LLVM-stack-check, and frame-pointer qsort tests, but failed
self-stage while compiling `tools/generate_cached_generic_functions.ml`.
Until that failure is reduced, the branch keeps the older conservative
page-based prologue base. The failure means the current `P + U + R` model is
not yet strong enough evidence for changing this part of the prologue check.

Helper `required_space` should remain:

```text
Stack_threshold_words + ceil(payload_bytes / word_size)
```

It should not include `Stack_ctx_words`, page sizes, or the stack-base offset.

## Implementation Order

1. Minimal sound fix: use final `P` plus current or strengthened `U` in
   `getOxCamlPrologueStackCheckBytes`; stop unconditional prologue-check elision
   for `CfgBytes != 0`.
2. Performance fix: split protocol/prologue/frame-record predicates, re-enable
   combined SP bumps when legal, and stop stack-check-driven FP/LR forcing.
3. Cleanup: lower prologue checks with proper AArch64 MachineInstrs rather than
   Darwin-shaped inline asm.

Tests should encode the real safety condition: matrix cases for
`P + U + R < E`, `P + U + R = E`, and `P + U + R > E`, for both
`cfg_bytes = 0` and `cfg_bytes > 0`, plus malformed/missing-attribute cases that
must not silently elide the prologue check.

## Frame-Record Experiment

Native AArch64 OCaml frames generally do not maintain an x29 frame-pointer
chain. Ordinary frames subtract the stack frame and save LR when the function
contains calls. LLVM currently does more: `oxcaml_fpcc` plus the
stack-check-protocol path saves an x29/LR pair in nearly every stack-using
function.

A focused experiment narrowed LLVM's explicit x29 setup to functions with an
LLVM personality, and treated LR-only `oxcaml_fpcc` frames like no-FP frames for
the saved-LR offset. This passed focused stack-check and frame-pointer tests and
reduced most generated `mov x29, sp` instructions, but it did not remove the
broad `stp x29, x30` saves because the `oxcaml_fpcc` callee-saved list and call
preserved mask still globally promise x29 preservation.

A second experiment changed the global `oxcaml_fpcc` save list and preserved
mask to the no-FP variant. That is not safe as a blunt change:
`tests/llvm-stack-checks/challenges.ml` segfaulted and
`tests/llvm-stack-checks/compile_challenges.ml` bus-errored. The likely issue is
that some generated code still depends on the caller/callee x29 contract when
stack growth or trap recovery is involved. The proper design needs an explicit
contract split, not a global preserved-mask change:

- ordinary OCaml calls should not force x29 frame records just because
  stack-check protocol is present;
- functions or call sites that really need x29 across an active trap/unwind must
  say so explicitly;
- the call-preserved mask must match that contract, or LLVM may keep x29 live
  across calls that can clobber it.
