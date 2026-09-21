# Executable Vox tutorial

The real Vox frontend, verifier and bytecode compiler run in a WebAssembly
build of Vox's C bytecode interpreter. Z3 runs in a separate browser worker.
Successfully verified programs execute in a fresh interpreter worker. Source
code and solver queries stay on the device.

## Run

From this directory, with generated assets already present:

```sh
npm ci
npm run dev
```

For the production build:

```sh
npm run build
npm run preview
```

Open the URL printed by Vite. Both servers supply the COOP/COEP headers
required by Z3 and the synchronous compiler/solver bridge. A production host
must serve HTTPS (localhost is allowed), `Cross-Origin-Opener-Policy:
same-origin` and `Cross-Origin-Embedder-Policy: require-corp`. Serve `.wasm`
files as `application/wasm`. Deploy the whole `dist/` directory at the origin
root; arbitrary base paths are not supported yet.

## Build the compiler assets

Prerequisites: the repository's native build dependencies, Python 3, Node.js,
and the official Emscripten SDK (tested with **6.0.9**). Activate the SDK's
`emsdk_env.sh` or set `EMCC` to its `emcc` executable.

```sh
npm ci
npm run build:compiler
npm run build
npm test
```

`build:compiler` configures this checkout with a local `_install` prefix and
runs `make install` before building the Wasm runtime. Do not run another
Make/Dune command in this checkout concurrently. To reuse an installation
that is already up to date:

```sh
npm run build:compiler -- --skip-native
```

The script copies tracked working files, including uncommitted changes,
into `.runtime-source/`. Generated assets, dependencies and build trees are
ignored by Git. `npm run build` rebuilds the JavaScript workers; changes to
OCaml or C sources require `build:compiler` as well. The compiler build must
come from a Vox development checkout, not upstream OxCaml without Vox.

## Implementation

- Emscripten `MEMORY64=2` lowers 64-bit pointer operations to wasm32 memory.
  OCaml values retain their 63-bit integer representation. Compiling this
  compiler through a 31-bit OCaml runtime changes source integer semantics.
- The browser executable links the existing parsing, typing, termination,
  refinement, bytecode generation and linking libraries. `compiler/vox_verify.ml`
  replaces only the native process-based solver transport.
- The compiler worker waits on a per-query `SharedArrayBuffer`. The page
  forwards its query to the Z3 worker, which records the result and wakes the
  compiler. Only `unsat` discharges an obligation. Counterexamples, unknown,
  cancellation and timeouts cannot lead to execution.
- Editing terminates compiler/execution workers, interrupts the active Z3
  query, clears results, and starts checking after 500 ms. Revision numbers
  reject stale messages. Proofs have a 3 s solver timeout, checking has a
  30 s wall-clock limit, and execution has a 3 s limit plus an output cap.
- The runtime is single-domain. The Emscripten memory commit shim zeroes
  committed regions; decommit retains linear memory until worker disposal.
- CodeMirror provides highlighting, editing, search, undo, lexical completion,
  compiler type hovers, local definition navigation and inline errors.
  Hovers and definitions are available after successful verification.
- The Proof goals panel expands generated intermediate bindings and simplifies
  Boolean bookkeeping for display. It shows assumptions, the goal, proof status,
  and source navigation. The original SMT query remains under Solver details.
  Countermodels use source names, signed 63-bit integers, exact mathematical
  integers, and constructor names. Derived values remain in Solver details.
  After a failed proof, a separate 250 ms search tries source integer inputs
  between -1 and 1; failure to find a small model preserves the original model.
  These are solver countermodels, not replayed runtime tests. Opaque functions
  are identified when missing facts could explain the failure.
- Edits persist per lesson in localStorage. Reset restores the example;
  Download code saves the current source.

## Current scope

Five executable chapters cover refinement types, testing and verification,
termination checking, lemmas and induction, and ghost code/data. A sixth
playground checks integer boundaries. The revised tutorial's module interface,
borrowing, heap ownership, concurrency and complexity chapters still need to
be integrated. This is a working foundation, not the complete tutorial.

Completion is lexical, not Merlin semantic completion. Multi-file projects,
external packages, input, and parallel-domain execution are not supported.
Compiler warnings currently go to the developer console. Large programs may
hit browser memory limits; this has been tested in the Codex in-app browser,
not yet across Firefox, Safari and mobile browsers.

The Z3 npm package is pinned to 5.2.0; its bundled engine identifies itself as
5.1.0.0. Its Wasm asset is about 33 MB and reserves 2 GiB of shared linear
memory. Compiler bytecode is about 7 MB and the packed standard library about
15 MB, before HTTP compression. Enable compression and caching on a deployed
host. Startup is intentionally lazy at the page level, not per chapter.

## Checks

`npm test` compiles all examples with the Wasm compiler, discharges their
actual queries with the package's Wasm Z3, and executes the resulting
bytecode. It checks negative proofs, totality, ghost-to-real rejection,
unknown/timeout solver responses, runtime input checks, 63-bit boundaries,
GC/allocation, readable goals, countermodel decoding and small-model fallback. Browser UI checks additionally cover the worker bridge,
live editing, failing tests, execution limits and recovery.

The canonical slide deck is separate and unchanged.
