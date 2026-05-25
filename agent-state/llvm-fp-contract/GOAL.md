# Goal

Implement LLVM backend floating-point contraction parity with the native arm64 backend for the common multiply-add and multiply-subtract patterns that native already fuses.

The motivating issue is that LLVM currently leaves some hot float arithmetic as separate multiply/subtract instructions even after the normal LLVM cleanup pipeline. Native arm64 emits a fused instruction for the same source pattern. The first target case is the `float_ref_loop` benchmark from the slow-case report:

```ocaml
let[@inline never] black_box_int (x : int) = Sys.opaque_identity x

let n =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1000

let reps =
  if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 1000

let[@inline never] run n reps =
  let acc = ref 1.0 in
  for _ = 1 to reps do
    for i = 1 to n do
      let x = float_of_int i *. 0.000001 in
      acc := (!acc +. x) *. 1.00000001 -. x
    done
  done;
  !acc

let () = Printf.printf "%.17g\n%!" (run (black_box_int n) (black_box_int reps))
```

Background evidence is in:

- `/Users/julesjacobs/git/oxcaml-llvm/agents/llvm-fast-path-roots/oxcaml/_bench_llvm_slow_cases/report.html`
- `/Users/julesjacobs/git/oxcaml-llvm/agents/llvm-fast-path-roots/oxcaml/_bench_llvm_slow_cases/llvmir_opt/float_ref_loop.opt.ll`

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Allowed OxCaml source areas: LLVM backend implementation under `backend/llvm`, focused tests under `testsuite`, and small helper scripts only if needed for reproduction.
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Vendored LLVM policy: do not edit vendored LLVM unless the OxCaml-side implementation is proven insufficient and the evidence is recorded here first.
- Expected output: LLVM IR should carry contraction permission, or otherwise express a fused FP operation, only for patterns that native arm64 already treats as fusible.

## Required Semantics

- Do not introduce broad fast-math behavior.
- Preserve `Sys.opaque_identity` as a barrier, matching the intent of `testsuite/tests/asmcomp/prevent_fma.ml`.
- Prefer matching native arm64's selected fused patterns first, rather than adding `contract` to every floating-point operation.
- Include both positive tests where LLVM should be able to emit fused instructions and negative tests where contraction must not happen.

Candidate source patterns to inspect against native arm64 before implementing:

- `x +. (y *. z)`
- `(y *. z) +. x`
- `x -. (y *. z)`
- `(y *. z) -. x`
- negated variants only if native arm64 also fuses them under the same barrier rules.

## Validation Targets

- Add focused codegen/expect coverage for fused and non-fused cases.
- Verify optimized LLVM IR after standard cleanup passes, not only raw `-keep-llvmir` output.
- Re-run a small version of the motivating `float_ref_loop` comparison and record whether LLVM now emits the fused instruction and whether the runtime gap shrinks.

## Branches

- OxCaml: `jujacobs/llvm-fp-contract`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/17
