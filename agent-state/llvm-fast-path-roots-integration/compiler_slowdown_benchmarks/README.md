# Reduced Compiler Slowdown Benchmarks

These two cases isolate the exception/trap shape seen in the compiler profile,
especially `Env.find_same_without_locks` in `typing/env.ml`.

Both cases install a local `try ... with Miss` handler inside the hot loop and
call a small lookup helper. This is the same shape as:

```ocaml
try Ident.find_same id tbl.current with Not_found -> ...
```

`try_lookup_hit.ml` exercises the hit path: the handler is installed, but the
helper returns normally.

`try_lookup_miss.ml` exercises the miss path: the helper raises and the local
handler catches it.

`no_try_lookup_hit.ml` is the control: same hit-only lookup helper call, but no
local handler in the hot loop.

`string_tree_find.ml` is a separate non-trap case. It models the
`Ident.find_same` pattern: recursive tree lookup with `String.compare` at each
node.

`string_compare_loop.ml` isolates the same string comparison overhead without
tree recursion.

`string_map_find.ml` uses `Map.Make(String).find`, which is close to the
lookup-heavy compiler data-structure style but still small enough to inspect.

The tiny `black_box_int` wrapper on the command-line inputs is intentional. It
keeps the benchmark close to the larger exploratory harness and prevents the
hit-path case from becoming too easy for the optimizer.

Run from the OxCaml checkout root:

```sh
export PATH=/Users/julesjacobs/.opam/oxcaml-5.4.0+oxcaml/bin:$PATH
unset OCAMLLIB OCAMLPARAM
export LLVM_PATH=/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper
agent-state/llvm-fast-path-roots-integration/compiler_slowdown_benchmarks/run.sh
```

The runner compiles each source twice with the same flags except for
`-llvm-backend -llvm-path $LLVM_PATH`, then checks outputs and times both
executables.
