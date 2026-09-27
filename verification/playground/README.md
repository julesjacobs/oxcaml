# Vox playground

A static web page where a visitor edits a Vox program and checks it. The
check runs entirely in the browser: the compiler's front end (parsing, typing
with modes, refinements and ghost checks) and the Vox verifier are compiled to
JavaScript with js_of_ocaml, and Z3 4.16.0, the version the native compiler
uses, is the official WebAssembly build from npm (`z3-solver`). No server-side
code is involved.

## Build

From a configured checkout (see `AGENTS.md`), with the `oxcaml-5.4.0+oxcaml`
opam switch (which provides js_of_ocaml 6.3.2), node and npm:

```sh
verification/playground/build.sh
python3 verification/playground/serve.py     # http://localhost:8769/
```

`build.sh` writes the site to `_build/playground/site`. It runs
`make boot-compiler runtime-stdlib` (about a minute from scratch), not the full
compiler build: the checker is built in Dune's boot context, whose compiler is
the opam switch's, so the switch's js_of_ocaml accepts the bytecode. The
standard library interfaces come from the runtime_stdlib context, built by
that front end; they are identical to the installed ones. Options:

- `--out DIR`: write the site elsewhere.
- `--catalogue-url URL`: where the "Demonstrations" link points (default
  `../catalogue/index.html`).
- `--library-prefix PREFIX`: also ship the verified library's interfaces from
  `PREFIX/lib/ocaml/vox` (installed by `verification/library/build.sh PREFIX`),
  so that programs can use `Vox_sequence` and the rest. The examples do not
  need them, and they are large: 125 interfaces, 19 MB (4.6 MB compressed),
  against 2 MB for the standard library.

## Hosting

Copy the site directory to any static host. Z3's WebAssembly build uses
threads (`SharedArrayBuffer`), which browsers allow only on cross-origin
isolated pages, so the pages should be served with

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

`serve.py` sends them. On a host without header control (GitHub Pages, for
example), the included `coi-serviceworker.min.js` adds them from a service
worker; that needs HTTPS (or localhost) and reloads the page once on the first
visit. The page loads nothing from other origins. Serving `.wasm` as
`application/wasm` with compression is recommended; the files are listed in
the build output.

## How it works

- `vox_playground.ml` is the driver: it does what
  `ocamlc -extension refinement_types -c NAME.ml` does up to Lambda, on a
  file in js_of_ocaml's in-memory file system, and returns the status and the
  compiler's messages as ocamlc prints them. Each check starts from the
  typer's state at startup (`Local_store`), so results do not depend on
  earlier checks.
- `vox_smt_solver.ml` implements the interface of the native
  `../vox_smt_solver.mli`, sending SMT-LIB text to Z3 through
  `Z3_eval_smtlib2_string`. The call is synchronous, so the verifier
  (`../vox_vc.ml`, `../runtime/vox_verify.enabled.ml`) runs unchanged. Both
  runners share the response protocol in `../vox_smt_response.ml`.
- `web/checker.js` loads Z3 and the checker. Every query gets a fresh Z3
  context, because an API context, unlike a Z3 process, does not restart its
  resource count at `(reset)`. Z3's timer threads are started before any
  check, since a worker cannot start one while it runs synchronous code.
- `web/worker.js` runs the checker in a worker; `web/app.js` is the page
  (CodeMirror 5, the example picker, diagnostics marked in the editor).
- `examples/` holds the examples and `examples/index.json` their order.

## Limitations

- js_of_ocaml's `int` and `nativeint` have 32 bits. The verifier models the
  target's 63-bit integers with `Int64` and is unaffected, but the typer reads
  integer literals with the host's `int_of_string`. A program with an `int`
  literal outside [-2^31, 2^31) (or a hexadecimal one at or above 2^31), or
  such a `nativeint` literal, is not checked: the page says so and gives no
  verdict. `max_int`, `min_int` and arithmetic are not affected.
- Only single files are checked, with the flags above; there is no
  `-principal`, no `.mli` and no other compilation unit.
- Z3's WebAssembly binary is 34 MB (8 MB compressed), and it needs cross-origin
  isolation.
- Deep recursion in the checker can exhaust the JavaScript stack; the page
  then reports a limitation of the browser build, not a verdict.

## Differential check

```sh
python3 verification/playground/differential/run.py --native _install
```

compares the built site (run in Node) with the native compiler installed in
`_install` on the examples, the integer boundary cases in
`differential/cases`, and every phrase of the expect tests listed in
`differential/sample.txt`. It prints each disagreement and each file the
browser build declines to check. With `--browser-bundle`, it also writes
`differential/browser.html` into the site, which repeats the comparison in a
real browser.
