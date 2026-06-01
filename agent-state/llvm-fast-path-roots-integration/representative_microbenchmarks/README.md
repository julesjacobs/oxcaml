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

Use `CASES=a,b,c` to run a subset. By default, the runner passes the intended
OxCaml regalloc flag:

```sh
LLVM_EXTRA_FLAGS="-mllvm -oxcaml-regalloc-call-split-remainders"
```

Set `LLVM_BACKEND_FLAGS=` or `LLVM_EXTRA_FLAGS=` to intentionally benchmark
without extra backend flags, or set either variable to another string for
experiments. `LLVM_EXTRA_FLAGS` wins when both variables are set.

```sh
LLVM_EXTRA_FLAGS="-mllvm -oxcaml-regalloc-allocate-regmask-split-remainders"
```

The runner writes generated sources, binaries, assembly, and `summary.json`
under `.build/`.

Saved numbers and interpretation are in `RESULTS.md`.
