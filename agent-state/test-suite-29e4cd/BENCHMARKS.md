# Benchmark Harnesses

This directory contains reusable benchmark harnesses used for LLVM backend
comparisons. Generated binaries, IR, assembly, logs, and result JSON files are
ignored inside each harness directory.

## Microbenchmarks

- `exception_microprobe/run.py`: focused native-vs-LLVM microbenchmarks for
  exception-heavy and small codegen shapes.
- `exception_microprobe/src/*.ml`: standalone source copies of the main
  microbench cases.
- `exception_microprobe/sweep/src/*.ml`: generated sweep cases for closure
  environment size and try/no-try variants.
- `loop_invariant_microbench/run.py`: focused loop-invariant GC-vs-int value
  benchmarks. The source text is embedded in the runner and regenerated under
  `loop_invariant_microbench/src/`.

## Minibenches

- `minibench_suite/run.py`: fixed native-vs-LLVM harness for vendored
  js_of_ocaml execution benchmarks plus local numeric, hash, and finance cases.
- Standard minibench sources come from
  `external/js_of_ocaml/benchmarks/sources/ml/`.
- Extra local sources live in `minibench_suite/local_src/*.ml`.

## Benchmarks Game

- `benchmarksgame_ocaml/run.py`: native-vs-LLVM harness for selected OCaml
  Benchmarks Game programs.
- `benchmarksgame_ocaml/src/*.ml`: representative source copies for the chosen
  benchmark set.
- `benchmarksgame_ocaml/probe_all/src/*.ml`: broader source snapshot used while
  selecting useful Benchmarks Game cases.

## Compiler Benchmark

- `run_compiler_bench.py`: compares selected compiler module compile times
  between `_native_install/bin/ocamlopt.opt` and
  `_llvm_self_stage_install/bin/ocamlopt.opt`.

## Stage Evidence

- `slowdown_vs_native_stage_artifacts_20260608_current/`: tracked stage evidence
  for representative slowdowns, including source, before/after RS4GC IR, MIR,
  LLVM assembly, and native assembly.

