# Minibench Suite

This suite runs a fixed set of vendored `js_of_ocaml` OCaml execution
benchmarks with the native backend and the LLVM backend.
Each benchmark is compiled with `ocamlopt.opt -O3 -unbox-closures` for both
backends.

Benchmarks:

- `almabench`
- `bdd`
- `binary_trees`
- `boyer`
- `boyer_no_exc`
- `fannkuch_redux`
- `fannkuch_redux_2`
- `fft`
- `hamming`
- `kb`
- `kb_no_exc`
- `nucleic`
- `quicksort`
- `raytrace`
- `soli`
- `splay`

Example:

```sh
OCAMLOPT="$PWD/_install/bin/ocamlopt.opt" \
OCAMLLIB="$PWD/_install/lib/ocaml" \
LLVM_PATH="$PWD/../clang-wrapper" \
SAMPLES=3 \
python3 agent-state/test-suite-29e4cd/minibench_suite/run.py
```

The harness writes:

- `build/<case>.native` and `build/<case>.llvm`
- `inspect/<case>.native.s`
- `inspect/<case>.llvm.s`
- `inspect/<case>.llvm.ll`
- `results.json`
