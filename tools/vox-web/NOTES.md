# vox-web: purely browser-based vox editor — feasibility notes

Goal (as assigned): a purely browser-based vox editor — the OxCaml
compiler (with the vox verifier) AND the Lean solver both running
client-side, with NO server round-trips for `/check`.

Reference (server-based) editor: `tools/vox-editor/` in this tree.
Pipeline recap (from `server.py` / `vc_index.py` / `lean_bridge.py`):

  * `/check` runs the built `ocamlc` TWICE on a temp `input.ml`:
      1. `ocamlc -c -vox-dump-vc-provenance -vox-dry-run input.ml`
         — pure elaboration, dumps every VC (goal/hyps/kind/spans) to
         stderr. **No Lean is invoked.** This is the "shape" pass.
      2. `ocamlc -c -vox-solver-path <lean> input.ml`
         — real verify; shells out to the `lean` binary; first failed
         proof becomes a single structured error. Success = all proved.
    plus `lean_bridge.capture_generated` to dump the full Lean text.
  * `/goal` uses the Lean LSP for live proof state (slow, explicit).

So `/check` = { fast pure-OCaml elaboration } + { slow Lean proof }.
That split is the crux of everything below.

---

## VERDICT

**Purely browser-based (Tier 3, "full in-browser") is NOT achievable**,
for two INDEPENDENT reasons, either of which alone is decisive:

1. **Lean cannot run client-side.** There is no browser-wasm build of
   Lean 4.31.0. The pinned toolchain here is **2.7 GB** (the `lean`
   binary is a 400-byte wrapper into a 2.7 GB `lib/` of oleans + the
   elaborator). `lean4web`, the canonical "Lean in the browser" project,
   runs Lean **server-side** and streams results to a Monaco client — it
   is not wasm. Proof verdicts (`/check` step 2 and `/goal`)
   fundamentally require Lean, so "no server round-trips for /check" is
   impossible on the proof half regardless of the compiler.

2. **The compiler-in-browser half is blocked in this environment**
   (details below): js_of_ocaml is not installed and cannot be
   installed here, and OxCaml emits a fork-specific bytecode magic that
   a stock js_of_ocaml would not recognize.

**Best architecturally-sound tier = Tier 2** ("compiler in browser via
jsoo for instant elaboration/VC shapes; Lean proofs via a small remote
endpoint"). This is a real win — offline, instant VC feedback — but it
could not be *demonstrated running* here because jsoo is unavailable.
See "What WOULD work" for the design and the exact drop-in point.

**What was actually built + tested tonight:** the achievable slice —
a static (server-less-to-serve) front end whose check pipeline lives in
a web worker behind a pluggable `CheckBackend`, with a `MockBackend`
that makes the whole UI run with **zero server**, and a `RemoteBackend`
that talks to the existing `/check`+`/goal`. The `InBrowserBackend`
stub marks precisely where a jsoo compiler + remote-Lean would slot in.
See `tools/vox-web/`.

---

## FINDINGS (each with the exact command + verdict)

### Environment / tooling
- `which opam ocamlfind js_of_ocaml node` →
  opam `/home/jujacobs/.dispatch/bin/opam`, ocamlfind in switch
  `5.4.0`, **node `/usr/bin/node` v18.20.8**, **js_of_ocaml: ABSENT**.
- opam switches (`ls ~/.opam`): `4.14.2`, `5.4.0`. No jsoo in either
  (`ls ~/.opam/5.4.0/lib/js_of_ocaml*` → none;
  `ocamlfind list | grep js_of` → none).
- js_of_ocaml absent from nix too (`ls /nix/store/*js_of_ocaml*` → none).

### js_of_ocaml install: BLOCKED here
- `opam install js_of_ocaml-compiler --dry-run` → **permission denied**
  by the harness policy.
- `opam install -y js_of_ocaml-compiler` → **permission denied** (real
  install, not just dry-run — settled).
- Web search / `WebFetch` for a prebuilt artifact → **denied** by policy
  (both directly and via subagent). No way to download jsoo either.
- Net: jsoo cannot be obtained in THIS environment. (In a normal opam
  env `opam install js_of_ocaml-compiler` is routine — this is an
  environment restriction, not a jsoo problem.)

### OxCaml bytecode magic: fork-specific (compat risk even with jsoo)
- `utils/config.common.ml`: `exec_magic_number = Caml1999X581`
  (cmi `Caml1999I581`, cmo `Caml1999O581`, …).
- `build-aux/ocaml_version.m4`: `MAGIC_NUMBER__PREFIX = Caml1999`,
  `MAGIC_NUMBER__VERSION = 581` — a fork-chosen 3-byte version so
  OxCaml artifacts never mix with stock OCaml's.
- Consequence: `js_of_ocaml-compiler` reads the executable's magic and
  maps it (via its `Magic_number` table) to a known OCaml version to
  pick the right bytecode/runtime handling. `Caml1999X581` is not in a
  stock jsoo's table, so a stock jsoo would reject OxCaml bytecode
  ("not a bytecode executable" / unknown magic). A working jsoo would
  have to be built against OxCaml's compiler-libs (bigger undertaking),
  and its JS runtime would have to cover any OxCaml-specific C
  primitives the driver references. For the *driver* (stdlib+Unix-ish)
  the primitive risk is moderate but unverified.

### Bytecode compiler IS buildable (removes one worry)
- `Makefile:281` installs `$(prefix)/bin/ocamlc.byte`. So `make install`
  produces a bytecode `ocamlc.byte` — the input jsoo needs. (The
  `boot-compiler` target only produced native `.opt` binaries:
  `_build/_bootinstall/bin/ocamlc.opt -> main_native.exe`.)

### Latency budget (measured with the reference clone's native ocamlc.opt on examples/reverse.ml, 178 lines)
- Dry-run VC-shape pass (`-vox-dump-vc-provenance -vox-dry-run`, NO
  Lean): **~0.02 s** native, x3 runs. Pure elaboration.
  - Under jsoo, expect ~10–30x native for compute-bound OCaml → order
    **0.2–0.6 s** in-browser. Interactive. This is the payoff of Tier 2:
    every VC's location/goal/hypotheses/spans, live, offline.
- Full solve (`-vox-solver-path <lean>`): **~1.1–1.7 s**, Lean-dominated.
  Cold ~1.7 s, warm ~1.1 s. This is the part that must stay off the
  client.

### Size budget (shippable virtual-FS payload)
- stdlib `.cmi`s the compiler needs to typecheck user code:
  **~2.0 MB total across 100 files** (`~/…/_install/lib/ocaml/*.cmi`).
  Trivially shippable in a browser virtual FS. (The full
  `_install/lib/ocaml` is 1.4 GB, but that is native `.a/.cmx/.o` etc.
  the *bytecode typechecker* does not need — only the `.cmi`s.)
- jsoo'd `ocamlc.js`: NOT measured (jsoo unavailable). For reference,
  try-ocaml-style jsoo'd toplevels land around a few MB gz; a full
  compiler driver would be larger but is a one-time download.
- Lean toolchain: **2.7 GB** — the reason (1) is decisive.

---

## What WOULD work (Tier 2 design; the drop-in point)

`/check`, split honestly by what needs Lean:

  A. ELABORATION / VC SHAPES  — 100% client-side, no Lean, ~0.2–0.6 s:
     jsoo'd `ocamlc.byte` runs `-vox-dump-vc-provenance -vox-dry-run`
     in a Web Worker over a virtual FS preloaded with the ~2 MB of
     stdlib `.cmi`s. Parse stderr with the SAME logic as
     `vc_index.parse_dump` (port to JS, or run the existing Python via
     Pyodide — but plain JS is lighter). Emits the identical
     `regions`/`vcs`/`spans` JSON `app.js` already consumes → UI
     unchanged. This alone gives instant, offline "does it elaborate,
     here are the proof obligations and their spans" feedback.

  B. PROOF VERDICTS — Lean, must be remote (or skipped):
     keep a tiny endpoint that runs `ocamlc -vox-solver-path` (or Lean
     directly on dumped VC text) and returns pass/fail+counterexample.
     Async: the UI shows VC shapes instantly (A), then fills in
     proved/failed badges when (B) returns. `/goal` (live proof state)
     stays remote too.

The vox-specific obstacle the team flagged — "the verifier shells out
to `lean`, and jsoo has no `Unix.create_process`" — is SOLVED by option
(A)+(B): the dry-run pass never calls Lean, so the jsoo compiler never
needs a subprocess. Lean is reached only by the remote endpoint (B).
(The alternative — a jsoo runtime shim that turns the subprocess into a
sync call into a Lean worker — is moot because there is no Lean worker:
no Lean wasm.)

Drop-in point in the code built tonight: `backends.js`
`InBrowserBackend.check()` — replace its `throw` with
`{ elaborate via jsoo worker (A); fetch proof verdicts from remote (B) }`.

## Open risks if someone pursues Tier 2 in a permissive env
1. Build jsoo against OxCaml compiler-libs (the `Caml1999X581` magic);
   confirm `js_of_ocaml ocamlc.byte -o ocamlc.js` accepts it.
2. Confirm the driver references no OxCaml C primitive missing from the
   jsoo runtime (link-time `Missing primitive` errors are the tell).
3. Virtual FS: preload `.cmi`s; point `Config.standard_library` at the
   FS root (the native binary hard-codes `$(pwd)/_install/lib/ocaml`).
4. Port `vc_index` VC parsing to JS (small, regex-based) or ship Pyodide.
</content>
</invoke>
