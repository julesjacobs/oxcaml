# vox editor

A prototype web editor for the **vox** verifier (the Lean-backed
refinement-type extension of OCaml on the `vox` branch). CodeMirror on
the left; a **proof pane** on the right that shows, at the cursor:

- the **goal and hypotheses** of a verification condition (VC), with its
  proved / failed status and any counterexample; and
- inside an embedded `[%%vox.lean]` block, the **Lean proof state** —
  either the statically-parsed theorem goal (instant) or the real
  intermediate state from Lean itself (on demand).

The design goal was to make every piece of logic programmatically
testable and keep the DOM/UI layer thin. See "What is tested how" below.

## Running it

Prerequisites: a **built** vox compiler in this clone
(`_build/_bootinstall/bin/ocamlc.opt`, produced by `make -s boot-compiler
&& make -s install`) and a **Lean 4.31** binary.

```sh
cd tools/vox-editor
export TMPDIR=/path/on/a/big/disk        # Lean writes scratch here
python3 server.py --port 8000            # auto-detects ocamlc + pinned lean
#   (or: python3 server.py --ocamlc /abs/ocamlc.opt --lean /abs/lean)
#   (or: python3 server.py --no-lean     # VC shapes only, no solver)
```

Then open <http://127.0.0.1:8000/>. Type a program — the pane's
goals and hypotheses follow the buffer as you type (a fast no-Lean
compile pass, ~250ms debounce; verdicts of unchanged obligations are
carried over by content) and the full Lean check follows once typing
pauses (also on **Check** / Ctrl-Enter). Move the cursor onto a
refinement or into a `[%%vox.lean]` block to drive the pane; inside a
block, click **Get live Lean goal at cursor**.

> If a local HTTP proxy is set in the environment, start the browser
> with `NO_PROXY=127.0.0.1` so it reaches the server directly.

## Architecture (bottom-up, each layer tested before the next)

| Layer | File | Responsibility |
|------|------|----------------|
| 1 | `vc_index.py` | Compile a source file with the built `ocamlc`; parse `-dump-vc -vox-dry-run` output (VC shapes) and the verification-failure error format (goal / hypotheses / counterexample) into JSON. |
| 2 | `lean_bridge.py` | In-block goals. Recover the generated Lean via a `-vox-solver-path` wrapper, rewrite it self-contained (inline VoxCore), map a block-source cursor to the generated position by verbatim substring search, and query `lean --server`'s `$/lean/plainGoal`. Also a static tier that parses the enclosing theorem. |
| 3 | `server.py` | stdlib `http.server`. `POST /check` → unified 0-based **regions** (VCs, static block theorems, block outlines) + errors + generated Lean; `fast:true` skips the Lean solve for the as-you-type pass. `POST /goal` → live proof state. `GET` → static assets. |
| 4 | `selection.js` | Pure cursor→region logic: innermost enclosing region, else nearest preceding on the line, else nearest above; block/theorem regions route to the Lean path. Runs in node and the browser. |
| 5 | `index.html`, `app.js`, `style.css` | The UI. The pane follows the cursor **client-side** (selection.js); server round-trips only on Check / idle / the explicit live-goal button. |
| 6 | `browser_test.js` | Headless-Chrome smoke test of the assembled page. |

Locations: `vc_index` uses the compiler's convention (1-based line,
0-based column); `server.py` normalises everything to **0-based line,
0-based column** (CodeMirror) at the HTTP boundary.

## What is tested how

Run everything: `./run_tests.sh`.

- **`vc_index`** (`test_vc_index.py`): the parser is pinned **byte-for-byte**
  against real compiler output copied from the vox mechanics suite
  (`mechanics/refines_kind.ml`, `mechanics/lean_refines_fact.ml`), plus
  end-to-end tests that compile fixtures through the built compiler and
  the real solver.
- **`lean_bridge`** (`test_lean_bridge.py`): pure tests for block
  extraction, offset mapping, self-containment, and the static parse;
  **live LSP** tests that (a) a VoxCore-using block elaborates after
  inlining and (b) a full source→goal query returns the real proof
  state.
- **`server`** (`test_server.py`): `build_check_response` /
  `build_goal_response` end-to-end, plus a real HTTP round-trip (urllib)
  including static-file serving and path-traversal rejection.
- **`selection`** (`test_selection.js`): 8 node assertions over the
  enclosing / preceding / above cases and lean-routing.
- **Browser** (`browser_test.js`): spawns the server, drives the real
  page in headless Chrome (puppeteer-core), and asserts the sample
  verifies, a VC shows its goal, a block theorem shows its static goal,
  and a live Lean proof state is fetched from inside the block.

The DOM/UI layer (`app.js`) is exercised **only** by the browser test;
all other logic is covered by the layer tests.

## Known gaps

- **Live goals need a referenced block.** A `[%%vox.lean]` block is only
  spliced into the solver input when a VC actually uses it (a reflected
  function / spec function it defines appears in a refinement). For an
  unreferenced lemma block the compiler never sends anything to Lean, so
  there is no live state — the **static** tier still shows the declared
  goal/hypotheses.
- **Single file only.** A block whose generated Lean imports another
  unit's sig module (`VoxSig_*`, i.e. cross-unit `.mli` blocks) is
  reported `unsupported` for live goals; the prototype edits one file.
- **VoxCore inlining is a copy.** `lean_bridge._voxcore_body` mirrors
  `typing/vox_module.ml`; if the compiler's base theory changes
  incompatibly, the VoxCore live-block test fails loudly (by design).
- **Failure status is single-VC.** A real solver run stops at the first
  failed VC, so on failure exactly that VC is marked `failed` and the
  rest stay `unknown` (successful runs mark all `proved`).
- No editing of multiple files, no persistence, no auth — it is a
  prototype.
