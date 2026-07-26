# vox2 IDE

A local, single-file browser editor for vox2 OCaml. It runs the actual
`ocamlc.opt` built in this worktree and reports parse/type errors, expression
types, the inferred module signature, and — as you edit — live refinement
**verification** feedback: refined obligations are discharged by Lean and
their outcome is surfaced in the buffer. All browser assets are vendored; the
Python server uses only the standard library.

Verification is not a separate compiler pass: the vox2 compiler runs
`Vox_verify.verify_structure` unconditionally inside the ordinary `-c`
type-check, discharging each obligation with Lean, and a discharge failure is
an ordinary located compiler error whose message begins `Refinement
verification failed`. So one compile yields both type diagnostics and
verification results; the IDE classifies them and styles them distinctly.
(Lean must be reachable — see below — because for refined code a missing
solver is a hard failure, not a skipped check.)

## Run

```sh
cd $VOX2/voxide
export TMPDIR="$HOME/tmp"        # somewhere with room; not /tmp
export PATH="$HOME/.opam/5.4.0/bin:$PATH"
export NO_PROXY=127.0.0.1
export VOXIDE_SMT_SOLVER="z3 -in"   # or an absolute path to your z3
WORKTREES=$VOX2/worktrees
python3 server.py --port 8471 \
  --ocamlc "$WORKTREES/consolidate-r3/_install/bin/ocamlc.opt"
```

Open <http://127.0.0.1:8471/>. Keep `VOXIDE_SMT_SOLVER` in the launch
environment: without it the z3 backend is unavailable even though the server
starts normally.

The server prefers `../_install/bin/ocamlc.opt`, then falls back to
`../_build/_bootinstall/bin/ocamlc.opt`. Override this with either
`--ocamlc /absolute/path/to/ocamlc.opt` or the `VOX2_OCAMLC` environment
variable. The listener is always bound to `127.0.0.1`.

### Lean

Verification needs the Lean solver the compiler was pinned against (4.31.0).
The compiler locates it via `VOX_LEAN`, then its pinned nix path, then `lean`
on `PATH`, so an ordinary environment usually needs no configuration. If
refined buffers report `Refinement verification failed (solver error)`, Lean
is not reachable by the `ocamlc.opt` subprocess.

## Architecture

The layout mirrors the vox1 IDE while omitting its Lean/VC pipeline:

1. `compiler.py` writes the current buffer to an isolated temporary directory.
   It runs `ocamlc.opt -c -annot input.ml` for real parse/type feedback and
   expression types, then (on success) `ocamlc.opt -i input.ml` for the module
   signature. Compiler locations are normalized to CodeMirror's 0-based
   coordinates at this boundary.
   The `-c` run is what discharges refinement obligations, so its subprocess
   budget (`_CHECK_TIMEOUT_SECONDS`, 60 s) sits above the compiler's own 30 s
   per-obligation Lean timeout: a slow discharge then reports a real
   `Refinement verification failed (solver error)` instead of being killed.
   Each error is classified `kind: "type"` or `kind: "verification"` (the
   latter when its message begins `Refinement verification failed`), and a
   `verification` summary — `verified` / `failed` / `blocked` / `none` — is
   derived from the errors and whether the buffer carries refinements.
2. `server.py` is a threaded `http.server`. `POST /check` accepts
   `{source, revision, backend}` and returns diagnostics, types, signature,
   verification summary, and the translated VC dump together. The dump and
   annotations come from one authoritative `-c` pass. Static serving is
   restricted to this directory.
3. `index.html`, `app.js`, and `style.css` provide the thin browser layer.
   Checking and verification run automatically after edits; there are no
   separate Check or Verify controls. Rounds are debounced,
   single-flight/coalesced, and guarded by the buffer revision so a stale
   response cannot overwrite newer feedback. The header shows exactly the
   completed round's latency as `(N ms)`; it clears while a newer revision is
   pending. `/config` supplies backend capabilities before the first check, so
   the selector is present from load and chooses configured oxsmt by default.
   Type errors
   become red wavy squiggles; verification failures a distinct amber squiggle
   and an amber `verify` badge in the diagnostics list; `-annot` drives type
   at cursor; `-i` drives the signature pane. While a round is in flight, old
   diagnostics, marks, and proof rows are cleared and the header says
   `checking…` (typing never blocks — the compile runs in a server thread).
4. `vox-mode.js` and `vendor/codemirror/` are copied from the vox1 IDE so the
   editor works offline and understands refinement braces and holes.

Compiler scratch directories follow `TMPDIR` and are deleted after each
request. The compiler is invoked directly without a shell and without linking.

## Proof pane, file explorer, and personalization (slice 3)

### File explorer and curated examples

`workspace.py` exposes a single read-only root — the curated `examples/`
directory — behind two `GET` endpoints. `/ls` returns a small collapsible
tree; `/file?path=<id>` serves one file's text. A path id is
`examples/<relpath>`; `workspace.resolve` maps it back to an absolute path with
strict traversal protection (the symlink-resolved target must stay inside the
real root) and an extension allowlist (`.ml`, `.mli`, `.md`), so neither `..`
segments nor a symlink can escape and only source files are reachable. `/ls`
carries each example's title and its expected-verification flag so the tree can
label a deliberately-failing example with `✗`. `/examples` serves the curated
`index.json` (titles, descriptions, the `default`, and a per-example teaching
`cursor` line the pane opens on). The `examples/*.ml` set is authored in vox2
surface syntax and each verifying example was checked through the actual
compiler (see the report). Multi-root browsing with stdlib dependency staging,
as vox1 has, is out of scope for this slice.

The sidebar is collapsible (persisted), highlights the active file, colors
files by kind, and guards against discarding hand edits: switching files while
the buffer differs from what was last loaded asks for confirmation first. The
tree is keyboard-operable with a roving tabindex (one item in the tab order at
a time, so Tab enters the tree once): Up/Down move focus among the visible
items, Left/Right collapse/expand a directory, and Enter/Space activate. A `✗`-labelled example spells out "expected outcome: does not verify"
in its tooltip so a curated expected-failure is not mistaken for a regression.

### Read-only documentation viewer

A second root, `docs/`, lists the curated `docs/*.md` notes behind the same
`/ls` + `/file` endpoints and traversal guard. Opening a `.md` shows it in a
rendered, read-only viewer that replaces the editor; it is never compiled (the
automatic round is suppressed while a doc is open), and switching to a doc
clears the previous buffer's
diagnostics, signature, type, verification, and proof state so none of it
lingers. Markdown is rendered by a small, self-contained subset renderer
(headings, paragraphs, fenced code, lists, blockquotes, rules, and inline
emphasis / code / links) built entirely with DOM APIs — never `innerHTML` — so
doc text cannot inject markup; a `javascript:`/`data:` link degrades to plain
text, and a link to a curated file (`docs/x.md`, `examples/y.ml`) opens that
file in the explorer. The last file opened is remembered in `localStorage` and
reopened on reload.

### Proof pane and per-VC marks

The right-hand **Proof pane** follows the cursor and renders the innermost
obligation covering the caret as a **proof state**: a named hypothesis list,
one per line (`n : n >= 0`, or a positional `h0`/`h1` when the dump carries no
binder name), a dividing rule, then the turnstile goal `⊢ …` in monospace, with
a verdict badge. A hypothesis whose source origin is known is a link — clicking
it jumps to and briefly highlights where it entered scope. The raw app-syntax
predicate and the generated Lean theorem are demoted to disclosures; the
solver's diagnostic is shown for a failure. Each obligation's source span is
painted with a status-colored underline, and a verdict legend appears only
while some obligation failed, reusing the same marker classes so it cannot
drift from the underlines. The compact/full toggle adds the obligation kind,
the raw-predicate disclosure, and the solver detail. Hypotheses the proof did
not use are always dimmed. The compact preference and light/dark theme persist
in `localStorage` and apply before first paint.

All of this is driven through **one adapter function** (`adaptVcs` in
`app.js`), the sole integration point between the compiler's per-obligation
dump and the UI. The normal `POST /check` compiles the live buffer with
`-annot -vox-dump-vc-json`; `compiler.check_source` translates that document
into the structured shape below in the same response. (`POST /vcs` remains a
compatibility/debug endpoint, not an editor round.) Real data feeds every
buffer — curated example or hand-edited scratch.
On a buffer that fails to verify, the
compile aborts at the first disproved obligation; the obligations discharged
before the abort are still shown (the sidecar is written at process exit).

Both the translation and the renderer consume schema **v2** (each predicate's
source-like `display`, and each fact's `origin` = binder `name` + source
`span`) when present, and **fall back to schema v1** (raw `text`, no origin)
otherwise, so the pane works unchanged against either compiler. The VC fields
in `/check` are 0-based line/column throughout:

```json
{ "revision": 12,
  "unavailable": false,
  "hidden": 0,
  "vcs": [
    { "id": 0,
      "status": "proved | disproved | unproved | solver-error",
      "kind": "contract | annotation | seal",
      "span": { "start": {"line": 10, "col": 12}, "end": {"line": 10, "col": 22} },
      "goal": { "display": "_ > 0", "raw": "(app[Stdlib!.>] _ 0)" },
      "hypotheses": [
        { "name": "y", "display": "y > 0", "raw": "(app[Stdlib!.>] y 0)",
          "span": { "start": {"line": 11, "col": 6}, "end": {"line": 11, "col": 7} },
          "used": true } ],
      "counterexample": ["..."],
      "detail": "grind failed ...",
      "generated_lean": "theorem vc_0 : ..." } ] }
```

`unavailable` is `true` when the check did not complete (the compile could not
run, or the dump was missing, unreadable, or malformed) — distinct from a
completed dump with an empty `vcs` (a genuine "no obligations"), which the pane
must not be shown as. `hidden` counts obligations the dump reported but that
have no placeable source span (ghost-only), noted in the pane so the visible
count never silently shrinks. An unrecognized `status` is normalized to
`unknown` and fails closed (shown as an anomaly, not "not yet checked").

Under schema v1, `display` equals `raw` (the instantiated app-syntax, e.g.
`(app[Stdlib!.>] 7 0)`), `name` is `null` (positional), and `span` is `null`
(no link). The obligation's own `span` is its `program_point` (the call site
for a contract argument, the binding for an annotation), converted from the
schema's 1-based line / byte column to the editor's 0-based line / UTF-16
column. Predicate text is preserved, not whitespace-collapsed, so the goal
keeps its structure. `counterexample` is usually `null` — a disproved
obligation's solver output lands in `detail` instead. When the selected backend
reports per-hypothesis usage, `used: false` facts are always dimmed; missing
usage remains unknown rather than being treated as unused. A VC
whose only source spans are ghost (synthesized, unplaceable) is dropped; a
hypothesis with a ghost origin span keeps its text but is not a link. An empty
buffer, or any failure to run, dump, or parse, yields an empty `vcs` list, so
the pane shows its placeholder and no stale marks linger.

### Deeper proof interaction

Three refinements make the pane read more like a real proof experience, all
driven from the VC data in the unified `/check` response:

**Known at this point.** When the caret is not on any obligation the pane is no
longer dead: it shows an approximate "known at this point" view — the facts
introduced textually above the caret, drawn from the obligations the dump
already carries, rendered as the same named rows the obligation view uses (no
goal, no turnstile). This is deliberately conservative. Only facts with a binder
name are shown, because a branch condition always arrives without one; the dump
gives a branch's condition span but not the extent of the branch it governs, so
placing a branch fact by text position would assert it somewhere it does not
hold. Branch facts are therefore omitted off-obligation, and the panel says so.
Some monotone facts without a name (an annotation result, an indirect call
result) are omitted too, so the view can under-report. It is an approximation,
sound only for a single flat scope: a named binding introduced inside a branch
or other nested scope is placed by text position, so a caret below that scope —
where the binding is no longer live — can also over-report a now-closed-scope
fact. The panel labels the view approximate and says as much, so it reads as a
hint, not a guarantee. On an obligation, every hypothesis is still shown, branch
facts included, since there the branch fact genuinely holds.

**Honest counterexamples.** A disproved obligation always gets a counterexample
section. When the solver echoed a concrete witness it is shown, but labelled a
*candidate* over Lean's unbounded-`Int` model — a value it assigns need not fit
a machine int, and a genuine overflow refutation would never be found under that
model, so it is never presented as an established runtime fact. When no witness
came back — the common case, since `grind` refutes a goal without reliably
producing a model — the pane says so explicitly ("the solver refuted this goal
but produced no concrete witness") rather than looking empty. Disproved (the
goal is false) is kept sharply distinct from unproved (automation gave up): an
unproved obligation carries a prominent note that no counterexample was found
and the goal may still hold, and never a counterexample.

**Generated-Lean escape hatch.** The generated theorem is a complete,
self-contained Lean file (no imports; opaque declarations inlined), so the
disclosure that shows it also offers to copy it, download it as a `.lean`, or
open it in a new tab — dropping the exact check, success or failure, into any
real Lean. The solver's own diagnostic sits beside it for a failure.

## Tests

```sh
cd $VOX2/voxide
export TMPDIR="$HOME/tmp"        # somewhere with room; not /tmp
export PATH="$HOME/.opam/5.4.0/bin:$PATH"
python3 -m unittest discover -s tests -v
```

`test_compiler.py` pins pure location/error/annotation/signature parsing, the
error classification and verification-status helpers, and drives good and bad
buffers through the built vox2 compiler when it is available — including a
provable refinement (verified clean), a false annotation (a located
verification failure), and an impure predicate (a totality rejection).
`test_server.py` performs HTTP round trips over `/check`, `/verify`, `/ls`,
`/file`, `/examples`, and `/vcs`, including static-file and explorer
traversal containment (examples and docs roots), with fake compiler and
VC-dump adapters so the transport layer stays focused. `test_compiler.py` also
pins the VC translation: pure schema-v1/v2 → frontend-shape cases (span choice,
status/kind mapping, v1 raw-text fallback and v2 display/origin, ghost fallback
and drop, detail/Lean, fail-closed unknown status) and the availability state
(a completed dump is available even when empty; a run that cannot start is
unavailable), plus, when the compiler is built, real-dump cases that assert
each obligation's span underlines the exact source text it claims and that
pre-abort obligations survive a failing buffer. `test_workspace.py` pins
`workspace.resolve`'s allowlist and traversal protection, the tree's
title/verifies metadata, the docs root, and the malformed-`index.json`
hardening.

`test_frontend.js` (run with `node tests/test_frontend.js`) shims the DOM,
CodeMirror, `localStorage`, and `fetch`, loads the real `app.js`, and drives
it headlessly: the Markdown renderer and its injection safety, the read-only
doc viewer (compile suppression, result clearing, editor restore), last-file
persistence, the unsaved-edits guard across a doc detour, and keyboard tree
navigation.

`test_view_harness.py` (skipped if `node` is absent) drives the textual-view
tool in `tools/voxide-view.js` against a fake-checker server, so the headless
rendering of the editor, proof pane, diagnostics, and toggles is exercised as
plain text without needing a compiler.

## Manual test script

1. Start the server with the command above and load
   <http://127.0.0.1:8000/>.
2. Before the first result completes, confirm the header already shows the
   backend dropdown. After the automatic round, read the header left-to-right:
   backend, `✓ verified · 1/1`, then a parenthesized duration such as `(230 ms)`.
3. Move the cursor onto `7` in the preloaded **Sixty seconds** example. Confirm
   the PROOF zone shows `⊢ 7 > 0` and the CURSOR zone shows its inferred type.
4. Open **When you're wrong**. After the debounce, confirm `need_one 2` gets an
   **amber** squiggle, the STATUS zone shows a `verify`-badged diagnostic, and
   the header reads `✗ 1 disproved` with the completed round's `(N ms)`.
5. Change the argument to `true`. Confirm it gets a **red** type-error
   squiggle, the verification detail says verification waits on the type
   errors, and the signature is unavailable until it is fixed.
6. Put an impure call in a predicate, e.g.
   `let z = (read_int () : int{ _ = read_int () })`. Confirm it is rejected
   with a totality error ("`read_int` is partial but is expected to be
   total") — a type/mode rejection, red, not a Lean discharge failure.
7. Edit repeatedly while a slow backend is checking. Confirm typing remains
   responsive, old marks disappear immediately, the header shows `checking…`
   and `(—)`, and only the newest buffer's result and duration appear.

## Known gaps / notes

- Verification is folded into every `-c` check because the compiler performs
  it in the same pass; there is no type-only fast path for refined code (a
  missing Lean is a hard failure, not a skip). Live feedback stays responsive
  via debounce, single-flight, and a server thread per request; a genuinely
  slow discharge shows the `checking…` pending state.
- On a clean compile the `-i` signature run re-drives verification a second
  time (a small redundant cost paid only on success).
- The compiler stops at its first error, so one check normally returns one
  diagnostic. Partial `.annot` data is still surfaced.
- Each `/check` is a single in-memory implementation buffer written to a fresh
  scratch dir. The file explorer is read-only (browse the curated examples);
  there is no save, multi-file workspace, stdlib dependency staging,
  completion, formatting, or authentication.
- The proof-pane VC data is emitted by the same `-c -annot
  -vox-dump-vc-json` pass as the rest of `/check`. A superseded response is
  dropped as a unit, including its pane data and latency. `/vcs` remains only
  as a compatibility/debug endpoint.
- Real goals and hypotheses are the compiler's instantiated predicate text
  (e.g. `(app[Stdlib!.>] 7 0)`), not the source-like `_ > 0`; a disproved
  obligation usually reports its solver diagnostic in `detail` rather than a
  discrete counterexample.
