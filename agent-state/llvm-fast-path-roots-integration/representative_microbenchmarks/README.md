# Representative Microbenchmarks

This suite contains additional native-vs-LLVM backend microbenchmarks for
compiler-like shapes. It complements `compiler_slowdown_benchmarks` by varying
one axis at a time: string lookup, local handlers, closure calls, mutation, and
controls.

Run:

```sh
eval "$(../../../scripts/agent-tmp-env)"
PAIRS=3 LLVM_PATH="$LLVM_PATH" \
  agent-state/llvm-fast-path-roots-integration/representative_microbenchmarks/run.sh
```

Use `CASES=a,b,c` to run a subset. Use `LLVM_EXTRA_FLAGS` to pass additional
backend flags to the LLVM build, for example:

```sh
LLVM_EXTRA_FLAGS="-mllvm -oxcaml-regalloc-allocate-regmask-split-remainders"
```

The runner writes generated sources, binaries, assembly, and `summary.json`
under `.build/`.

Saved numbers and interpretation are in `RESULTS.md`.
