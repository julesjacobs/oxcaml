# Loop-Invariant Root Microbenchmarks

This is a focused native-vs-LLVM benchmark for values that are loop-invariant
and live across an OCaml call inside the loop.

It generates two cases:

- `loop_invariant_int_across_call`: non-GC integer value live across the call.
- `loop_invariant_gc_across_call`: GC string value live across the call.

Run from the OxCaml checkout:

```sh
python3 agent-state/test-suite-29e4cd/loop_invariant_microbench/run.py
```

The script uses the installed compiler at `_install/bin/ocamlopt.opt`, builds
both cases with `-O3 -unbox-closures`, and uses `../clang-wrapper` for the LLVM
backend. It writes timings to `results.json` and asm/LLVM IR artifacts under
`inspect/`.

Useful environment overrides:

- `OCAMLOPT=/path/to/ocamlopt.opt`
- `LLVM_PATH=/path/to/clang-or-wrapper`
- `OCAMLLIB=/path/to/lib/ocaml`
- `N=...`
- `REPS=...`
- `SAMPLES=...`
