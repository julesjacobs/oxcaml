# voxide-view -- a headless textual mirror of the vox2 IDE

`voxide-view.js` renders, as plain text, what a user would see in the vox2
browser IDE, and lets an agent drive that UI without a browser. It exists so
that any agent can check the IDE's user-visible behaviour headlessly: open a
file, move the cursor, edit the buffer, toggle a preference, wait for the
automatic check+verify round, and read back the editor (with its underlines
shown inline), the proof
pane, the diagnostics, the type and signature panes, the verification banner,
the active toggles, and any confirmation guard -- all as deterministic text
suitable for asserting against or recording as a golden file.

## Synopsis

```sh
export NO_PROXY=127.0.0.1
node tools/voxide-view.js --server http://127.0.0.1:PORT [options] [command ...]
```

It needs a **running** `server.py` (the real IDE server) to talk to; start one
the usual way (see `../README.md`) on a port you own, then point `--server` at
it. Node 18+ is required (it uses the built-in `fetch`).

If a managed sandbox forbids localhost sockets, use
`--compiler /path/to/ocamlc.opt` instead. The harness then sends one-shot
requests through `server.py`'s real dispatcher and compiler wrapper without
binding a port.

## How it works (and what is real)

The tool loads the **real frontend** and runs it against the **real server**:

- `index.html` is parsed for its element set, so the panes the tool reads are
  exactly the elements the browser builds (add an element with an `id` and the
  tool sees it with no change here).
- the vendored CodeMirror core, the `mllike` OCaml mode, and `vox-mode.js` are
  loaded and executed, so the **tokenizer is the real one** (the header reports
  `tokenizer=real`).
- `app.js` is executed **verbatim**. Every user-visible decision it makes runs
  as the real code: which source span gets which severity underline, which
  obligation the proof pane shows for the caret, the status text, the unified
  `/check` VC adapter normalisation, and the discard-edits guard.
- requests to `/check`, `/workspace-check`, `/ls`, `/file`, and `/examples` are
  real
  HTTP calls to the server you pointed at, so the diagnostics, expression
  types, inferred signature, and verification outcome are produced by the
  actual worktree compiler and Lean.

The **one** thing modelled rather than executed is CodeMirror's on-screen
editor *widget*. There is no browser or jsdom on this box, so `fromTextArea`
is replaced by a faithful document/marker model: it holds the buffer lines and
cursor, and records every `markText`/`setBookmark` call app.js makes. The tool
then renders those recorded marks inline under the source -- which is precisely
the decoration the user sees (an underline over that range, with that
severity). The tokenizer, the adapter, and all of app.js's logic still run for
real; only the pixels are absent.

The modelled seam is narrow: app.js calls only 13 simple line/`ch` CodeMirror
methods (`getValue`/`setValue`/`getLine`/`lineCount`/`getCursor`/`setCursor`/
`markText`/`setBookmark`/`on`/`addKeyMap`/`focus`/`getOption`/`setOption`) and
never `posFromIndex` or any offset/measurement API, so the document/marker
model reproduces the whole surface app.js depends on. The known divergences are
CR/CRLF handling (the model treats the buffer as LF) and that `setValue` does
not itself reset the cursor -- neither affects the LF example buffers or the
panes.

This choice was made because the box is offline (no `jsdom`/`linkedom`/browser
is installed and none can be fetched) and because a textual view does not need
CodeMirror's DOM measurement -- it needs the marks, which app.js produces
regardless of whether a real editor paints them. A prior claim that an earlier
lane had a headless app.js runner did not check out (none exists in the
slice-3 tip or the neighbouring IDE worktrees), so this is built fresh but
reuses the real frontend as required.

## Options

| option | meaning |
| --- | --- |
| `--server URL` | base URL of a running `server.py` (default `http://127.0.0.1:8000`) |
| `--compiler PATH` | socket-free bridge through `server.py --one-shot` using this compiler |
| `--replay FILE` | replay a captured compiler response; optional file/tree metadata can reproduce doc and explorer navigation |
| `--frontend DIR` | the `voxide` dir with `index.html`/`app.js` (default: the parent of `tools/`) |
| `--theme dark\|light` | initial persisted theme, applied before first paint |
| `--compact on\|off` | initial "compact" proof-pane toggle |
| `--sidebar shown\|hidden` | initial explorer visibility |
| `--confirm yes\|no` | default answer to a discard-edits `confirm()` guard |
| `--no-tokenize` | skip loading the real tokenizer (the *text* is identical; only colour, which text cannot show, is affected) |
| `--redact` | print `<server>`/`<frontend>` in the header instead of the real values, for stable golden files |
| `--section NAME` | render only one section (`editor`, `proof`, `diagnostics`, `type`, `signature`, `verification`, `explorer`, `legend`, `dialogs`, `header`) |
| `-e "CMD"` | a command (repeatable); `;` or newline separates commands |
| `--script FILE` | read commands from `FILE` (`-` for stdin) |

The theme/compact/sidebar options set the persisted preference the way it
would be in `localStorage`, so they apply *before first paint* -- the same path
the real UI uses.

## Commands

Commands run in order. Each command that changes state waits for the frontend
to settle (debounce timers fire and the in-flight unified `/check` completes)
before the next runs. If no `render` command is given, the full view is printed
once at the end; otherwise `render` prints wherever you place it, so a single
run can show the UI evolving.

| command | effect |
| --- | --- |
| `open <name>` | open an example (`open abs`) or a path (`open examples/abs.ml`) from the explorer; honours the discard-edits guard |
| `open! <name>` | open it even if the buffer has unsaved edits (skip the guard) |
| `openfile <path>` | open any allowlisted path directly, bypassing the tree |
| `source <text>` | replace the buffer with one line of text (edits the current buffer) |
| `source-file <file>` | replace the buffer with a local file's contents (edits the current buffer) |
| `cursor L:C` | move the caret to line `L`, column `C` (1-based, as the UI reports them) |
| `backend lean\|z3\|oxsmt\|cross` | select the verification backend and re-run the current buffer |
| `toggle theme\|compact\|sidebar` | flip a toggle exactly as a click would |
| `confirm yes\|no` | answer the *next* discard-edits guard this way (overrides `--confirm` once) |
| `render [section]` | print the view now (optionally one section) |

`source`/`source-file` model in-place editing, so the current file association
is unchanged -- there is no user action in
the IDE that loads arbitrary text as a brand-new pathless buffer, so the tool
does not synthesize one.

## Reading the output

The view is a sequence of labelled sections:

- **header** -- the toggles (theme, compact, sidebar, and whether the real
  tokenizer loaded), followed by the actual control order: backend dropdown,
  status text, and the completed round's parenthesized `(N ms)` duration.
- **File explorer** -- the curated tree; the active file is marked `(active)`
  and a deliberately-failing example is prefixed `✗`, exactly as in the sidebar.
- **Editor** -- the buffer with line numbers; the cursor line is marked `*` in
  the gutter and a `^ (cursor)` row points at the column. Under each line, one
  row per mark shows the underlined columns and the mark's kind and tooltip:
  `[type]` / `[verify]` for compiler diagnostics (a `~` run for a squiggle, a
  `^`/`!` for a point), and `[vc:<status>]` for a per-obligation underline.
- **Proof pane** -- the cursor-following obligation in prover style: its named
  hypotheses (`name : predicate`, or a positional `h0`/`h1`; an unused one is
  always prefixed `(unused)`), then the goal behind a `⊢`
  turnstile with a `[status]` badge. Full mode also shows solver detail on a
  failure and the generated Lean. In a read-only doc the pane shows a
  "Documentation (read-only)." placeholder (no check is issued).
- **Verdict legend** -- shown only while some obligation failed (the same
  condition as in the UI).
- **Diagnostics** -- the diagnostics list, each entry `[type]`/`[verify]` with
  its 1-based location and message.
- **Type at cursor**, **Inferred signature**, **Verification** -- the three
  text panes, verbatim.
- **Dialogs / guards** -- any `confirm()`/`alert()` the frontend raised since
  the last render, with the answer given.

Exit status is non-zero if app.js raised an error or the frontend failed to
settle, so a run is safe to gate a test on.

## Examples

Show the default view (opens the `overview` example, verifies it clean):

```sh
node tools/voxide-view.js --server http://127.0.0.1:8940
```

Open a failing example and read just the proof pane and diagnostics:

```sh
node tools/voxide-view.js --server http://127.0.0.1:8940 \
  -e "open counterexample; cursor 10:13; render proof; render diagnostics"
```

Read a doc, then switch back to an example:

```sh
node tools/voxide-view.js --server http://127.0.0.1:8940 \
  -e "openfile docs/welcome.md; render; open! overview; render proof"
```

Recorded transcripts live in `tools/transcripts/` (generated with `--redact`
against the real worktree compiler, which now emits the schema-v2 dump with
source-like `display` predicates):

- `realcheck.txt` -- a unified automatic `/check` type error shown inline, the
  diagnostics entry, the unavailable signature, and the gated verification
  detail.
- `clean.txt` -- the `overview` example verifying clean: the compact real proof
  pane shows `⊢ 7 > 0 ✓`.
- `failure.txt` -- the `counterexample` example: a real disproved obligation
  (`⊢ 2 = 1 ✗`) and its amber verification diagnostic.
- `docs.txt` -- the read-only doc viewer and the real proof pane together:
  opening `docs/welcome.md` renders the doc and shows the pane's
  "Documentation (read-only)." placeholder (no check issued), then switching
  back to `overview` restores the editor and the real proved obligation.

## Limitations

- **Colour and layout are not shown.** The textual view captures *which* span
  carries *which* severity/verdict, and all text content, but not syntax-
  highlight colour, font style (the italic refinement body), or pixel layout.
  Because the real tokenizer runs, `--no-tokenize` produces identical *text*.
- **The editor widget is modelled, not painted.** Marks are rendered from the
  `markText`/`setBookmark` calls app.js makes, which is what determines the
  user-visible decoration; but CodeMirror's own rendering (bracket matching,
  scroll, selection painting) is not exercised.
- **The VC dump is real.** The proof pane fills from the compiler's real
  per-obligation data carried by the same `/check` response (schema v2:
  source-like `display` predicates and per-fact origins).
- **`source`/`source-file` do not create a pathless scratch buffer** (see
  above); they edit the current buffer.

## Tests

`../tests/test_view_harness.py` drives this tool against a `server.py` started
with a deterministic in-test unified `/check` provider (so it needs neither
the compiler nor Lean), and asserts the real-`/check` error view, the
prover-style proof pane (a named hypothesis, the turnstile goal), the
read-only doc viewer with its suppressed proof pane, the doc-to-editable
restore, and that a declined discard-edits guard is reported cleanly. It is
skipped entirely if `node` is not on `PATH`.

---

# voxide-pane -- show *precisely* the IDE proof pane, by cursor

`voxide-pane.js` shows exactly what a user sees in the IDE's right (proof) pane
for a source file, queryable by exact cursor position. Unlike `voxide-view`
(which renders the whole IDE and needs a running server), `voxide-pane` is
focused on the proof pane and renders it from the IDE's **own** pane logic --
`../pane_model.js`, the single shared model that both the browser (`app.js`) and
this tool consume. There is no second copy of the pane's decision or content
logic, so the terminal output cannot drift from what the user sees. That
property is locked by `../tests/test_pane_fidelity.js` (below).

## Modes

- **`--map` (primary)** -- a single static *cursor→pane map* of a whole file: a
  per-column glyph ruler under each source line, plus a global legend of the
  unique panes. The ruler glyph **is** the legend id directly (one hop:
  `glyph → legend entry`), so an agent reads the whole landscape at a glance --
  which pane every caret position would show. `·` is the empty/placeholder pane;
  real panes take `0-9`, then `a-z`, then `A-Z`, assigned in order of first
  appearance. Identical pane content always maps to the same glyph; a run like
  `0000` marks a region where the pane does not change.
- **`--line L --col C`** -- a point query: the pane at caret line `L`, column `C`
  (1-based, exactly as the editor reports the cursor). This is the building
  block; each map cell is one of these.

## Obtaining the obligations

```sh
# offline / deterministic: a captured /vcs (or /workspace-check) payload
node tools/voxide-pane.js examples/abs.ml --vcs-json tests/fixtures/abs.vcs.json --map

# live: spins a THROWAWAY server on an ephemeral port (never the user's :8471)
VOX2_OCAMLC=/path/to/ocamlc.opt TMPDIR=/usr/local/home/jujacobs/tmp \
  node tools/voxide-pane.js examples/abs.ml --map

# live against an already-running server you own
node tools/voxide-pane.js examples/abs.ml --server http://127.0.0.1:PORT --map
```

For a multi-file workspace, feed a `/workspace-check` payload and select the
active unit with `--file NAME`; only obligations tagged for that unit reach the
pane, exactly as the browser filters by the active tab.

## Output is SAVED by default

The map/pane is a durable artifact agents read, so the tool **writes to a file
by default** and prints the path to stderr:

- `--map` → `<file>.panemap.txt` next to the source; a point query →
  `<file>.pane-L<L>C<C>.txt` (`.json` with `--json`). `--out PATH` overrides.
- The saved file is plain text (ANSI stripped), so it is stable to commit or
  diff. `--stdout` also echoes to the terminal (coloured when a TTY); `--no-file`
  suppresses the file and prints to stdout only.

```sh
node tools/voxide-pane.js examples/abs.ml --vcs-json tests/fixtures/abs.vcs.json --map
# -> voxide-pane: wrote examples/abs.ml.panemap.txt   (on stderr)
```

## Options

| option | meaning |
| --- | --- |
| `--map` | the static cursor→pane map of the whole file (primary mode) |
| `--line L --col C` | point query at 1-based line `L`, column `C` |
| `--vcs-json FILE` | read a captured `/vcs`/`/workspace-check` payload (offline) |
| `--server URL` | POST to a running server's `/vcs` (must not be the live editor) |
| `--ocamlc PATH` | compiler for live mode (else `$VOX2_OCAMLC`); a throwaway server is used |
| `--file NAME` | multi-file: show the pane for unit `NAME` |
| `--section body\|mode\|legend\|all` | which surface to print (default `all`) |
| `--json` | emit the raw view-model (point query) or the map model |
| `--runs` | map: run-length ruler instead of per-column (dense files) |
| `--compact on\|off` | proof-pane "compact" toggle (default on, as in the UI) |
| `--no-color` | plain text (no ANSI); also auto-off when stdout is not a TTY |
| `--out PATH` | write to `PATH` instead of the derived default path |
| `--stdout` | also echo the output to stdout |
| `--no-file` | do not write a file (stdout only) |

## Fidelity (why "precisely what I see")

The tool shows a **readable, block-aware projection of the pane's visible
layout**, locked to the real browser DOM. The `--section body`/`mode`/`legend`
text, ANSI-stripped, is **byte-for-byte** a block-aware, chrome-stripped
serialization of the rendered `#pane-body` / `#pane-mode` / `#legend` -- what
the user visually reads:

- each hypothesis on its own line (`name : predicate`);
- the goal on its own line with the `⊢` turnstile (the browser draws it via
  CSS; the tool restores it);
- the generated Lean as a delimited `[generated Lean]` block (header + the
  theorem text);
- `+N more here`, the status note, and the counterexample on their own line(s).

This is deliberately **not** raw DOM `.textContent` (which mashes every block
into one run and drags in chrome). Colour is layered on top and strips cleanly.
The map's ruler alignment assumes monospace BMP text; the columns are
CodeMirror `ch` units, matching the caret.

**Enumerated deviation -- the tool shows pane CONTENT and drops these
interactive-only affordances** (buttons, not content; they carry the classes in
`pane_model.js`'s `CHROME_CLASSES`):

- the generated-Lean **copy** / **download .lean** / **open in new tab**
  buttons (the Lean text itself is shown verbatim under `[generated Lean]`);
- the Lean disclosure's tooltip help prose ("The exact, self-contained theorem
  ...").

Nothing else is added or removed; the `raw predicate` / `generated Lean`
disclosures appear as `[raw predicate]` / `[generated Lean]` headers over their
content.

Honesty carries over exactly: a disproved-without-witness obligation is
labelled "refuted, no witness" (never a fake counterexample), the generated
Lean is verbatim, and the off-obligation "known at this point" view keeps its
single-scope-approximation caveat and never surfaces a branch condition. The
tool adds nothing the pane does not display.

## Recorded transcript

`tools/transcripts/voxide-pane-map.txt` is a real `--map` run over `abs.ml`,
`guard.ml`, and `recursion.ml`, plus a disproved-obligation point query on
`counterexample.ml`. It is reproducible offline from the committed fixtures
(the command is in the file header) and is byte-identical to a live run.

## Tests

`../tests/test_pane_fidelity.js` is the anti-drift lock. Over the example
fixtures and **every caret** in each file, in both compact and full modes, it:

1. loads the real `app.js` in a vm sandbox, feeds the fixture, sets the cursor,
   lets `renderProofPane` build the DOM, and derives the ground truth by a
   **block-aware, chrome-stripped serialization** of the rendered pane (newline
   at each block boundary, chrome subtrees skipped, the `⊢` turnstile restored)
   -- what the user visually sees, not raw `.textContent`;
2. asserts each equals the shared model's readable text (what this CLI prints,
   ANSI-stripped);
3. proves the re-plumb is **byte-identical**: the pre-extraction `app.js` (the
   pinned pre-extraction sha) and the current `app.js` produce identical
   `#pane-body` innerHTML / `#pane-mode` / `#legend` across the whole grid --
   the browser pane is provably unchanged;
4. proves the map is consistent: every ruler glyph resolves (via the legend) to
   a pane text equal to the point-query pane at that `(line, col)`;
5. spot-checks the actual CLI binary end-to-end (its `--section` output,
   ANSI-stripped, equals the block-aware DOM projection).
