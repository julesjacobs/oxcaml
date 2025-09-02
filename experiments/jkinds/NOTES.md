This directory contains local, ad-hoc test files created while experimenting with the experimental jkind solver hook.

Purpose
- Provide tiny, self-contained OCaml snippets to quickly validate that the compiler can build simple modules when the experimental solver is toggled on/off via the `OXCAML_JKIND_SOLVER` environment variable.

Important
- These files are not part of the official build or test suite and should be deleted before upstreaming or opening a PR.
- They are intended only for local development and smoke tests.

Suggested use
- Build with `make install`.
- Try compiling files with and without the flag:
  - `_install/bin/ocamlc -c simple.ml`
  - `OXCAML_JKIND_SOLVER=6 _install/bin/ocamlc -c simple.ml`

Files
- `simple.ml`: `type t = int` and `let v = 3` — a minimal sanity-check.
- `A.mli`/`A.ml`: simple abstract type exposure and definition.
- `B.mli`/`B.ml`: abstract type in the interface with a concrete definition in the implementation.
- `tmp_decl.ml`/`tmp_test.ml`: scratch files used during early experiments.

Please remove this directory and its contents before submitting upstream.

