# voxide right-pane redesign — INDEPENDENT verification report (lane 2)

Verifier: independent (dual-verify lane 2). Every claim below was re-derived from
the spec, the shared model source, the live tool, the live :8471 server, and the
four suites — not from the implementing lane's self-report.

**OVERALL VERDICT: CONFIRM.** All six checkpoints pass. One non-blocking cosmetic
recommendation (item 5). One design-nuance note on item 1(c). No honesty hole, no
anti-drift break, no source-like regression.

## Retarget addendum — verified AT tip 339bfb65d9

Lead retargeted verification to tip `339bfb65d9` (the test-only 5th commit). Confirmed:
- `git diff --stat 5a4058add0..339bfb65d9` = `voxide/tests/test_frontend.js` ONLY
  (+141 lines). No client-file change; live `app.js`/`pane_model.js` byte-identical
  to the worktree (md5 match) — the redesign is already de-facto live from these
  static files. My CONFIRM gates the PR/branch push, not the deploy.
- The 6 STATUS-zone fail-closed assertions run against the REAL app.js: the test
  loads the actual `app.js` + `pane_model.js` via `vm.runInContext` (test_frontend.js:
  527-531), drives it with `cm.setValue` + source-keyed `/check`+`/vcs` mocks, and
  asserts on `registry["status-verdict"].textContent` — the DOM node the production
  `renderStatusVerdict` → `statusRollup` path writes. Not a re-implementation.
- All 6 pass (live run):
  ```
  STATUS roll-up fail-closed honesty:
    ok - (a) all proved + compiles -> 'verified · 2/2'
    ok - (b) compile error -> STATUS 'type error'
    ok - (b) compile-error STATUS never reads verified/proved
    ok - (b) PROOF pane reads obligations unavailable (never a verdict)
    ok - (c) disproved -> STATUS '1 disproved'
    ok - (c) disproved STATUS never reads verified
  ```
This directly strengthens item 1(a): the STATUS honesty surface — the one
user-facing zone outside the PROOF anti-drift lock — now has dedicated real-app.js
regression coverage. Overall verdict unchanged: **CONFIRM at tip 339bfb65d9.**

## Tree under review

- Reviewed tip named by lead: `5a4058add0` (4 commits on `74c7603628`).
- Actual worktree HEAD: `339bfb65d9` = `5a4058add0` + one commit
  "add STATUS-zone fail-closed honesty tests (outside the PROOF fidelity lock)".
  The extra commit touches ONLY `voxide/tests/test_frontend.js` (+141 lines of
  tests). The reviewed substance (`app.js`, `pane_model.js`, `tools/voxide-pane.js`,
  `style.css`) is **byte-identical to 5a4058add0** (`git diff --stat 5a4058add0..HEAD`
  = test_frontend.js only). The extra commit is a strict improvement (it tests the
  one surface that sits outside the fidelity lock — see item 2).
- LIVE :8471 serves the reviewed tree byte-for-byte: `md5sum` of served `app.js`
  and `pane_model.js` == the worktree copies. `index.html` carries the 3 zones:
  `status-zone`/`status-verdict`, `proof-section`/`pane-body`/`pane-mode`/`legend`,
  `cursor-zone`/`cursor-type`/`signature`.

---

## Item 1 — HONESTY invariants (end-to-end, compact AND full)

### (a) STATUS fails closed — CONFIRM
`statusRollup` (pane_model.js:154) is fail-closed by construction:
- `!compiles` returns early → `✗ type error`, `detail: obligations unavailable`,
  BEFORE any verified/disproved branch is reachable.
- `disproved > 0` → `✗ N disproved`; `other > 0` (unproved/failed/solver-error/
  **unknown**) → `⚠ N unproved`; only then, `total>0` all-proved → `✓ verified · N/N`.
- `adaptVcs` normalizes every status through `normalizeStatus` → any unrecognized
  verdict folds to `"unknown"`, which is in `other`; so the verified branch is
  reachable ONLY when proved == total. No status can slip past into false-green.

Wiring (app.js:642-645): `lastErrorCount` counts only `kind !== "verification"`
errors, so `compiles` keys off genuine parse/type errors — a *verification*
failure (disproved/unproved obligation on a buffer that compiles) correctly shows
`✗ N disproved` / `⚠ N unproved` from the vcs, NOT "type error".

Live-server proof (broken buffer `let x : int = "hello"`):
- `/check` → `ok=false`, one error `kind="type"`, `verification.status="blocked"`.
- `/vcs`  → `vcs=[]`, `unavailable=true`.
- Therefore STATUS = `✗ type error`; PROOF (unavailable mode) reads
  `Obligations unavailable — this buffer did not compile.` (never proved/verified).

Now also covered by browser-DOM tests (commit 339bfb65d9): `(a)` all-proved+
compiles → `verified · 2/2`; `(b)` compile error → `type error`, never verified/
proved, PROOF reads `unavailable`; `(c)` disproved → `1 disproved`, never green.

### (b) DISPROVED-no-witness welded, both views — CONFIRM
`obligationModel` (pane_model.js:489-494) welds `· no witness` (or `· witness` iff
`vc.counterexample` is a non-empty array) into the depth-0 token via
`verdictTokenText`. Depth 0 is emitted in BOTH compact and full. Live tool:
compact and full both open `✗ DISPROVED · no witness`. The refutation note
("…refuted this goal but produced no concrete witness.") is the depth-1 expansion,
shown in full. No counterexample heading appears in compact, so a witness is never
implied. Witness-present path (label "candidate counterexample (unbounded-int
model)") is logic-verified; no fixture exercises it (grind yields no model on the
corpus).

### (c) unproved ≠ disproved — CONFIRM (with a design nuance)
The tokens are unmistakably distinct in BOTH views: `⚠ UNPROVED` vs
`✗ DISPROVED · no witness` (different glyph AND different word). The explanatory
NOTE ("Unproved: automation gave up. No counterexample was found, so the goal may
still hold.") is depth-1 → shown in FULL only, hidden in compact.

Nuance: the lead's checklist phrasing is "distinct word + note, **both views**".
The *note* is full-only by the selected disclosure design (the disclosure spec's
own compact mockup (b) shows the badge without the note; the note is the depth-1
caveat). The load-bearing guard against skimming unproved-as-disproved — the
distinct glyph+word — IS in both views. This matches the selected design and is
not a regression; flagging only so the "both views" wording is understood as
"distinct word both views; explanatory note in full".

### (d) off-obligation CONTEXT carries "approximate", both views — CONFIRM
Context token `◦ CONTEXT · approximate` is depth-0 (welded, always visible). Live
tool compact shows exactly `◦ CONTEXT · approximate`. Full adds the facts
(`n : n >= 0`) and the verbatim multi-line "Approximate: facts introduced textually
above the cursor … a hint, not a guarantee" caveat at depth 1.

### (e) generated-Lean escape hatch reachable, both views — CONFIRM
`[generated Lean]` (+ `[raw predicate]`, `[solver detail]`) are depth-2 and ALWAYS
emitted (pane_model.js:795-807), independent of compact. Live tool shows the Lean
theorem in both compact and full for proved/disproved/unproved.

---

## Item 2 — ANTI-DRIFT (tool == browser) — CONFIRM

`node tests/test_pane_fidelity.js` → **22 checks, all pass**.

The fidelity surface is the per-section `body` / `mode` / `legend` outputs
(test_pane_fidelity.js:658-660 compare `stripAnsi(runCli(section))` to the DOM's
`bodyReadable`/`modeText`/`legendReadable`). Crucially the test loads the REAL
`app.js` (not a re-implementation), drives its render at EVERY caret position
(line × col) across 6 fixtures, in BOTH compact levels (MODES = {compact:true},
{compact:false}), and asserts app.js-DOM == shared model (paneBodyReadable/
paneModeText/legendReadable) AND CLI == that DOM. So the chain app.js render ==
model == tool is locked byte-for-byte.

Scope note (not a defect): the tool mirrors the PROOF pane only. The STATUS zone
(`#status-verdict`) and CURSOR zone (`#cursor-type`/`#signature`) are user-facing
browser surfaces that are NOT projected by the tool and NOT in the fidelity lock.
This is by design (the tool's purpose is the cursor-driven proof pane), and it is
pre-existing, not introduced by this redesign. The one honesty-critical outside
surface — STATUS — is derived purely from the shared `statusRollup` and is now
guarded by dedicated browser-render honesty tests (339bfb65d9). Recommend keeping
that STATUS-test coverage as the standing guard for that surface.

---

## Item 3 — COMPACT vs FULL match the spec depth model — CONFIRM

`paneBodyLines` (pane_model.js:749): depth 0 always; depth 1 emitted iff `!compact`
(the compact checkbox IS the collapse — it DROPS depth-1 lines, not merely hides);
depth 2 always emitted as a one-click disclosure in both. Verified against the
live tool (`--compact on` vs `--compact off`):
- Compact = depth 0 (token, goal, anchor) + depth-2 disclosures.
- Full adds depth 1: kind tag ("annotation obligation"), hypotheses, status note,
  refutation heading/note, facts, "+N more here".
- Depth 2 (raw predicate / solver detail / generated Lean) present in both.

(Flag on my own method: `--compact` takes an argument `on|off` and defaults to
`on`; a bare `--compact` consumes the next token. Use `--compact off` for full.)

---

## Item 4 — SOURCE-LIKE display, no regression — CONFIRM

- Automated scan of ALL 13 fixtures for raw-AST patterns (`app[`, `constructor[`,
  `Stdlib!`, `Tvar`, `__vc`, `#row`) in every non-`raw` string field → **0 leaks**.
- Goal renders source-like: `⊢ 7 > 0`, `⊢ 2 = 1`, `⊢ x * x >= 0`.
- Live `/check` types & signatures render source-like refinement types:
  `int{ _ > 0 } -> int`, `int{ _ >= 0 }`, `int{ _ = n }` — 0 raw leaks across
  overview/abs/dependent.
- Raw AST `(app[Stdlib!.>] 7 0)` appears ONLY behind the explicit `[raw predicate]`
  disclosure (depth 2). Lean/grind text behind `[generated Lean]`/`[solver detail]`
  is legitimately verbatim solver output, not the app[]/constructor[] AST.

---

## Item 5 — TOOL HEADER ODDITY (`mode: (none)`) — assess: mildly misleading, OUTSIDE fidelity surface; recommend FIX (non-blocking)

The redesign blanked `paneMode` (the old "mode:" header line was cut — the verdict
token now carries the mode). `paneModeText` is therefore ALWAYS `""`. In
`--section all` the tool prints `mode: (none)` (voxide-pane.js:413) for EVERY pane,
including a PROVED obligation.

- Misleading? Mildly. It prints a now-meaningless "mode:" field that always reads
  `(none)`, even when the body immediately below shows `✓ PROVED ⊢ …`. A careful
  agent won't be truly confused (the body disambiguates), but it is dead chrome
  that can read as "no mode / not on an obligation".
- Inside or outside the fidelity surface? OUTSIDE. `--section all` is explicitly
  "Not byte-compared" (voxide-pane.js:409-410); the fidelity lock compares
  `--section mode`, which returns `paneModeText` = `""` and matches the DOM
  `#pane-mode` textContent (also `""`). So this does not break anti-drift.
- Recommendation: FIX (cheap, tool-only, no fidelity impact) — drop the `mode:`
  header from the `--section all` human view, since `paneMode` is dead post-redesign
  and the verdict token is the mode. Non-blocking; the true fidelity surface is
  already correct.

---

## Item 6 — Suite results (real counts)

| suite | command | result |
|---|---|---|
| pane fidelity (anti-drift) | `node tests/test_pane_fidelity.js` | **22 checks, all pass** |
| pane regressions | `node tests/test_pane_regressions.js` | **21 checks, all pass** |
| frontend (incl. new STATUS honesty) | `node tests/test_frontend.js` | **79 ok, 0 failures** |
| python | `python3 -m unittest discover -s tests -p 'test_*.py'` | **106 tests, OK** |

Note: bare `python3 -m unittest` from `voxide/` discovers 0 tests (the tests live
in `tests/`); use `discover -s tests` (above) or run per-file. All ran with
`TMPDIR=/usr/local/home/jujacobs/tmp` and
`VOX2_OCAMLC=…/scratch-h3/_install/bin/ocamlc.opt`.

---

## Pasted live tool output (compact + full)

### PROVED — overview.ml @11:22 (`--section all`)
COMPACT and FULL both:
```
mode: (none)

✓ PROVED
⊢ 7 > 0
11:22
[raw predicate]
(app[Stdlib!.>] 7 0)
[generated Lean]
set_option autoImplicit false

theorem vc_0 : (decide (7 > 0) = true) := by
  grind
```
(depth-1 kind tag/hyps are empty for this VC; the compact/full difference shows on
VCs that have depth-1 content — see disproved/unproved/context below.)

### DISPROVED (no witness) — counterexample.ml @10:22 (`--section body`)
COMPACT:
```
✗ DISPROVED · no witness
⊢ 2 = 1
10:22
[raw predicate]
(app[Stdlib!.=] 2 1)
[solver detail]
… grind failure …
[generated Lean]
theorem vc_0 : (decide (2 = 1) = true) := by
  grind
```
FULL (adds depth-1 kind + refutation note):
```
✗ DISPROVED · no witness
⊢ 2 = 1
10:22
contract obligation
refutation
Disproved: the solver refuted this goal but produced no concrete witness.
[raw predicate]
(app[Stdlib!.=] 2 1)
…
```

### UNPROVED — unproved.ml @8:40 (`--section body`)
COMPACT:
```
⚠ UNPROVED
⊢ x * x >= 0
8:40
[raw predicate] … [solver detail] … [generated Lean] …
```
FULL (adds depth-1 kind + "may still hold" note):
```
⚠ UNPROVED
⊢ x * x >= 0
8:40
annotation obligation
Unproved: automation gave up. No counterexample was found, so the goal may still hold.
[raw predicate] …
```

### OFF-OBLIGATION (context) — recursion.ml @8:15 (`--section body`)
COMPACT:
```
◦ CONTEXT · approximate
```
FULL (adds depth-1 facts + verbatim caveat):
```
◦ CONTEXT · approximate
n : n >= 0
Approximate: facts introduced textually above the cursor, derived from nearby
obligations. Branch conditions are omitted, and a binding introduced inside a
branch or other nested scope may still appear below that scope where it is no
longer in scope. Treat this as a hint, not a guarantee of what holds here.
```

### COMPILE ERROR (unavailable) — broken buffer, `unavailable:true` (`--section all`)
COMPACT and FULL both:
```
mode: (none)

Obligations unavailable — this buffer did not compile.
```
(Live `/check` on `let x : int = "hello"` → STATUS `✗ type error`; `/vcs` →
`vcs=[]`, `unavailable=true`. No obligation verdict anywhere; reads "unavailable".)

---

## Bottom line

CONFIRM. Honesty invariants hold end-to-end in compact and full; the anti-drift
lock (app.js == model == tool) is real and passes at every caret in both views;
compact/full match the spec depth model; source-like display did not regress;
STATUS fails closed (and is now browser-tested). The only actionable item is the
cosmetic `mode: (none)` header in the tool's `--section all` view (item 5) —
non-blocking, outside the fidelity surface. Design nuance on 1(c): the
unproved/disproved explanatory note is full-only by the selected disclosure design;
the distinct token/word (the load-bearing guard) is in both views.

---

# DELTA-VERIFY — minimal-compact + hypotheses stage (339bfb65d9 → ad640b0a4e)

Independent lane-2 delta verification of the two new commits on vox2-ide-integrated:
- `f1524e7657` minimal compact pane = token + goal + hypotheses (vox1 rule)
- `ad640b0a4e` rebaseline pane-fidelity purity ref (tip)

Delta is client-only: `git diff --stat 339bfb65d9..ad640b0a4e` = `voxide/app.js`
(+15/-9), `voxide/pane_model.js` (+42/-31), `voxide/tests/test_pane_fidelity.js`
(+5/-5). No compiler change. LIVE :8471 serves the reviewed tree byte-for-byte
(md5 of served app.js + pane_model.js == worktree).

**DELTA VERDICT: CONFIRM.** The new compact rule is honest and the anti-drift lock
holds; the escape hatches correctly moved to full-only (still reachable in full);
FULL still carries everything, organized. No regressions.

## New depth model (verified against the shared model + live tool)
- COMPACT (depth 0, always): verdict token WITH welded honesty qualifiers, the
  `⊢ goal`, and the hypotheses (sequent rows, `faded` flag preserved for unused).
  Nothing else.
- FULL (adds depth 1): the code anchor, the kind tag (both between goal and hyps as
  a metadata header), then status note / refutation / counterexample / `+N here`,
  then the three escape-hatch disclosures `[raw predicate]` / `[solver detail]` /
  `[generated Lean]`. app.js `renderVc` and `paneBodyLines` mirror each other line
  for line (both gate on `full`; hyps emitted unconditionally at depth 0).

## Per-item verdicts

1. **COMPACT is EXACTLY token/goal(/hyps), nothing else — CONFIRM.** Rendered all
   five cases via the live tool (`--section body`): no anchor, no kind tag, and
   none of the three depth-1 disclosures appear in any compact view. (See pasted
   output below.)
2. **Hypotheses in compact, goal-FIRST then hyp rows, unused faded — CONFIRM.**
   `multi_arg` @8:68 compact renders `⊢ a + b > 0` THEN `a : a > 0` / `b : b > 0`.
   Order (pane_model.js:757-780 / app.js renderVc): token → goal → (full: anchor,
   kind) → hyps. Goal is above hyps in BOTH views. The `faded: row.faded` flag is
   carried on the depth-0 hyp row (pane_model.js:773), so unused hyps render (faded)
   in compact rather than being dropped.
3. **FULL still contains everything, organized — CONFIRM.** Full shows anchor, kind
   tag, hyps, refutation heading + no-witness note (disproved), status note
   (unproved), `+N here` (unchanged depth-1 block), and all three disclosures.
   Order is headline → metadata → sequent → reason → escape hatches.
4. **HONESTY preserved in the NEW compact — CONFIRM.**
   - Disproved compact: `✗ DISPROVED · no witness` — welded qualifier intact.
   - Unproved compact: `⚠ UNPROVED` — distinct glyph+word from disproved (the
     "may still hold" note is now full-only, consistent with the minimal rule; the
     distinct token — the load-bearing guard — is in both views).
   - Off-obligation compact: `◦ CONTEXT · approximate` — welded qualifier intact.
   - Compile-error (unavailable) compact & full: `Obligations unavailable — this
     buffer did not compile.` — no verdict, never proved/verified.
   - Generated-Lean: now FULL-only (acceptable per brief); confirmed present in
     full for proved-with-hyps and disproved.
5. **ANTI-DRIFT green, baseline correctly re-pinned — CONFIRM.**
   `test_pane_fidelity.js` = 22 checks pass at this tip. The test drives the real
   app.js at every caret in BOTH compact levels and locks app.js DOM == shared
   model == CLI (sections 1/5/6, independent of the purity sha). The app.js-purity
   baseline was re-pinned `2bc5a0bfee → f1524e7657` — i.e. to the commit that
   introduced the minimal-compact layout, so the reference IS the reviewed layout.
   This is a standard intentional-layout rebaseline, NOT a loosening: the true
   anti-drift property (tool == browser) does not depend on this sha and still
   passes byte-for-byte at every caret.
6. **Suites (real counts at ad640b0a4e):** fidelity **22 pass**, regressions
   **21 pass**, frontend **79 ok / 0 fail** (incl. the 6 STATUS honesty asserts),
   python **106 OK** (`discover -s tests`).
7. **Cosmetic `mode: (none)` header — STILL PRESENT** in the tool's `--section all`
   view on every pane (incl. PROVED). Unchanged by this delta. Outside the fidelity
   surface (`--section all` is not byte-compared; `--section body/mode/legend` are).
   Non-blocking; recommendation to drop it still stands.

## Pasted live tool output (compact + full), 5 cases

### 1 PROVED (no hyps) — overview.ml @11:22
COMPACT:
```
✓ PROVED
⊢ 7 > 0
```
FULL:
```
✓ PROVED
⊢ 7 > 0
11:22
contract obligation
[raw predicate]
(app[Stdlib!.>] 7 0)
[generated Lean]
set_option autoImplicit false

theorem vc_0 : (decide (7 > 0) = true) := by
  grind
```

### 2 PROVED-WITH-HYPS — multi_arg.ml @8:68 (goal-first then hyps)
COMPACT:
```
✓ PROVED
⊢ a + b > 0
a : a > 0
b : b > 0
```
FULL:
```
✓ PROVED
⊢ a + b > 0
8:68
annotation obligation
a : a > 0
b : b > 0
[raw predicate]
(app[Stdlib!.>] (app[Stdlib!.+] a b) 0)
[generated Lean]
… theorem vc_0 (v_0 v_1 …) := by grind
```

### 3 DISPROVED (no witness) — counterexample.ml @10:22
COMPACT:
```
✗ DISPROVED · no witness
⊢ 2 = 1
```
FULL:
```
✗ DISPROVED · no witness
⊢ 2 = 1
10:22
contract obligation
refutation
Disproved: the solver refuted this goal but produced no concrete witness.
[raw predicate]
(app[Stdlib!.=] 2 1)
[solver detail]
… grind failure …
[generated Lean]
… theorem vc_0 : (decide (2 = 1) = true) := by grind
```

### 4 OFF-OBLIGATION (context) — recursion.ml @8:15
COMPACT:
```
◦ CONTEXT · approximate
```
FULL:
```
◦ CONTEXT · approximate
n : n >= 0
Approximate: facts introduced textually above the cursor, derived from nearby
obligations. Branch conditions are omitted, … a hint, not a guarantee …
```

### 5 COMPILE ERROR (unavailable) — broken buffer
COMPACT and FULL both:
```
Obligations unavailable — this buffer did not compile.
```

## Bottom line (delta)
CONFIRM at tip ad640b0a4e. Compact is now the minimal vox1 sequent (token + goal +
hyps, welded honesty qualifiers intact); anchor/kind/escape-hatches are full-only
and reachable; full is complete and organized; anti-drift lock is green with a
legitimate (non-loosening) baseline re-pin. Only the pre-existing cosmetic tool
`mode: (none)` header remains (non-blocking).

---

# DELTA-VERIFY — refinement type-at-cursor CLIENT wiring (72ed9b00d1 → c882bc651d)

Independent lane-2 verification of the CURSOR-zone refinement-predicate
type-at-cursor wiring. Tip `c882bc651d` (child of published `72ed9b00d1`), 1
commit, 4 files: `app.js`, `compiler.py`, `tests/test_compiler.py`,
`tests/test_frontend.js`. Verified with the new
`refine-types-emit/_install/bin/ocamlc.opt` binary. Re-derived from source +
live behavior + suites.

Aside: the parent commit `72ed9b00d1` already dropped the cosmetic
`mode: (none)` header from `voxide-pane --section all` — my prior delta
recommendation, actioned.

**DELTA VERDICT: CONFIRM.** Purely additive, honest (fail-closed + drops
ghost/malformed/typeless), correct end-to-end, and it does not touch the shared
PROOF model or its anti-drift lock. Gates the live :8471 restart onto the
refine-types-emit binary + the fork push.

## Per-item verdicts

1. **ADDITIVE — CONFIRM.** A no-refinement buffer (`let g x = x + 1`) yields
   `refinement_types: []`; the `/vcs` payload gains ONLY that one field
   (`vcs`/`hidden`/`unavailable` unchanged). The diff does not touch
   `check_source` or `parse_annot`, so the `.annot` expression types and their
   spans are byte-for-byte unchanged. `renderCursorType` uses `expressionTypes`
   alone when `refinementTypes` is empty (identical to prior behavior); it only
   `concat`s the refinement ranges when present.
2. **HONESTY — CONFIRM.** `refinement_types()` drops every non-trustworthy
   entry (unit-checked against a crafted document): ghost location, empty type,
   `null` type, and non-dict (malformed) location are ALL dropped; only the one
   well-formed entry survives. Fail-closed: `_vcs_unavailable` returns
   `refinement_types: []`, and the client seeds `refinementRanges = []` then only
   overwrites on a successful, non-superseded `/vcs` (stale predicate types never
   linger). Reset on every context change — `clearResults`, `applyWorkspaceView`
   (workspace path not wired; explicitly cleared so single-buffer types can't
   leak), and `switchTab` all set `refinementTypes = []`. The frontend test
   "off the predicate, no refinement type is invented" passes — nothing invented
   off a predicate subterm.
3. **CORRECTNESS end-to-end — CONFIRM.** `vcs_for_source` with the new binary on
   `let f (x : int{ _ > 0 }) = x` emits 4 source-like ranges (0-based/UTF-16,
   parse_annot shape):
   ```
   cols 16-17  `_`       -> int
   cols 18-19  `>`       -> int -> int -> bool
   cols 20-21  `0`       -> int
   cols 16-21  `_ > 0`   -> bool
   ```
   All source-like (no raw app-syntax). Smallest-span-wins: a caret on a subterm
   picks the tightest node (`_`/`>`/`0`); the whole-predicate `bool` is reachable
   where only the enclosing span contains the caret (the parenthesized form, per
   the passing frontend test "caret on the whole `(_ > 0)` predicate shows bool").
4. **ANTI-DRIFT UNAFFECTED — CONFIRM.** The delta does not touch `pane_model.js`
   or `tools/voxide-pane.js` (git diff --stat = empty for both). `renderCursorType`
   writes the app.js-only CURSOR zone (`#cursor-type`), not the shared PROOF
   model. `test_pane_fidelity` stays **22/22**; the PROOF pane is unchanged.
5. **SUITES (real counts at c882bc651d, new binary):**
   - `test_pane_fidelity.js` — **22 pass**
   - `test_pane_regressions.js` — **21 pass**
   - `test_frontend.js` — **84 ok / 0 fail** (+5 over the prior 79; incl. the two
     new refinement type-at-cursor tests)
   - `python3 -m unittest discover -s tests` — **109 tests, OK, 1 skipped**
     (+3 over the prior 106; new: `test_entries_become_zero_based_editor_ranges`,
     `test_ghost_and_malformed_entries_are_dropped`,
     `test_missing_or_non_list_field_is_empty`). Note: the impl report's "88 OK/3
     skipped" reflects a narrower discovery scope; full `discover -s tests` gives
     109 with 0 failures.
6. **No `@ mode` leaked — CONFIRM.** grep of the delta shows no `@ mode` /
   `paneMode` / arbitrary-expression-mode additions; that is the separate future
   Phase 2 stage and is absent here.

## Bottom line (delta)
CONFIRM at tip c882bc651d. The refinement type-at-cursor client wiring is
additive, fail-closed, drops ghost/malformed/typeless entries, and is correct
end-to-end on the new binary (`_`→int, `>`→int->int->bool, `0`→int,
`_ > 0`→bool, smallest-span-wins). It leaves the PROOF model and its anti-drift
lock untouched. Green to restart :8471 onto the refine-types-emit binary and push.

---

# DELTA-VERIFY — compact polish (fb5225d930 → 4dd62bc07d)

Independent lane-2 verification of the compact polish: verdict moved onto the
goal line (loud token + swatch removed), subtler zones, header carries the
count. Polish = `0ed2eafeeb` (compact polish) + `4dd62bc07d` (fidelity
rebaseline) on top of `fb5225d930` (fade compiler.py — orthogonal/ignored) /
`e5696c7cb3` (#157 labels). Client-only pane change (app.js, index.html,
pane_model.js, style.css, tools, tests). LIVE :8471 serves the reviewed tree
byte-for-byte (md5 match on app.js + pane_model.js). Re-derived from source +
shared model + live tool + suites.

**DELTA VERDICT: CONFIRM.** The verdict now rides the goal line (glyph + colour +
welded qualifier); honesty is preserved in both views (fail-closed, welded
`· no witness` in compact, glyph-distinct unproved, compile-error unavailable);
the cuts match the brief; anti-drift is green with a legitimate rebaseline.

## Per-item verdicts

1. **Verdict on the goal line; loud token + swatch GONE — CONFIRM.** The
   standalone `verdict-token` div and the `tok-swatch` element are removed from
   `renderVc`, from `CHROME_CLASSES`, and from `voxide-view`'s `htmlToText`;
   `pane_model` emits `goalGlyph`/`goalQualifier` instead of a `token`. The goal
   line is `<glyph> ⊢ <goal>[ · <qualifier>]`, tinted by verdict in BOTH views.
   Confirmed ANSI colours on the goal line (tool `--color always`):
   - PROVED  → `ESC[1m ESC[32m ✓ ⊢ 7 > 0` (bold GREEN — was blue, now `--ok`
     #3fb950/#1a7f37)
   - DISPROVED → `ESC[1m ESC[31m ✗ ⊢ 2 = 1 · no witness` (bold RED)
   - UNPROVED → `ESC[1m ESC[33m ⚠ ⊢ x * x >= 0` (bold AMBER)
   - CONTEXT (full) → grey `◦ CONTEXT` (`STATUS_COLOR.context = gray`)
   CSS mirrors this: `.goal-proved`→`--ok`, `.goal-disproved/failed/unknown`→
   `--error`, `.goal-unproved/solver-error`→`--warning`.
2. **HONESTY in both views — CONFIRM.**
   - Green ONLY when proved: the goal colour is keyed on the per-VC status
     straight from the compiler dump (`normalizeStatus`), so only a compiler-
     reported `proved` is green; unknown/unrecognized normalizes to `unknown`
     (red in CSS, magenta in tool — never green). Fail-closed.
   - Disproved keeps the welded `· no witness` IN COMPACT (`✗ ⊢ 2 = 1 · no
     witness`), not dropped.
   - Unproved is distinct from disproved by GLYPH (`⚠` vs `✗`), not hue alone —
     colour-blind-safe.
   - Compile-error shows NO verdict in either view: `Obligations unavailable —
     this buffer did not compile.` (never proved/verified).
3. **COMPACT cuts — CONFIRM.** Off-obligation compact shows NOTHING in the PROOF
   zone (the CONTEXT token moved to depth 1); FULL shows the grey CONTEXT token +
   facts + the verbatim approximate caveat riding WITH the facts (confirmed).
   `#signature-box` gained the `depth-1` class (signature full-only); anchor,
   kind tag, and the three escape-hatch disclosures are all full-only.
4. **HEADER — CONFIRM.** The standalone `#status-verdict` line is removed from
   `index.html`; `renderStatusVerdict` now writes the fail-closed roll-up (glyph
   + `verified · N/N`) to the top header pill `#status`, using the SAME
   `statusRollup(vcs, {compiles, errorCount})` (green only when it compiles AND
   every obligation proved; `compiles` still keys off the non-verification error
   count). Header stays fail-closed.
5. **#157 labels intact — CONFIRM.** `hypLabel` falls back name → kind →
   positional: `branch`→"branch condition" (not h3), `annotation`→"annotation",
   `contract-argument`→"argument", `application`→"result", unnamed binder →
   positional `h1`, `_`→`h2`. Anonymous facts are labelled from their kind, not
   h0/h1.
6. **ANTI-DRIFT — CONFIRM.** `test_pane_fidelity` = 22/22 at the tip (drives the
   real app.js at every caret, both compact levels, locking app.js DOM == model
   == CLI). The rebaseline commit `4dd62bc07d` re-pins the app.js-purity ref
   `f1524e7657 → 0ed2eafeeb` (the polish commit) — a legitimate intentional-
   layout re-pin, NOT a loosening: the true tool==browser property is independent
   of the pinned sha and holds byte-for-byte.
7. **Suites (real counts at 4dd62bc07d):** fidelity **22 pass**, regressions
   **27 pass** (+6), frontend **84 ok / 0 fail**, python `discover -s tests`
   **109 OK / 1 skipped / 0 fail**. (The impl report's "88 OK/3 skipped" reflects
   a narrower discovery scope; full discover is 109 with 0 failures.)

Trivial cosmetic nit (non-blocking, outside the text fidelity surface): the
`unknown` status maps to magenta in the tool but red in CSS. Both are non-green,
so honesty is unaffected; it is a colour-only edge for an off-corpus status.

## Bottom line (delta)
CONFIRM at tip 4dd62bc07d. The verdict rides the goal line (green proved / red
disproved / amber unproved, distinguished by glyph too), the loud token + swatch
are gone, honesty holds in both views (fail-closed, welded `· no witness` in
compact, compile-error unavailable), the header pill carries the fail-closed
count, #157 labels are intact, and the anti-drift lock is green with a legitimate
rebaseline. Green to push the fork.

---

# DELTA-VERIFY — #160 flash fix (4dd62bc07d → efb55b8e5b)

Independent lane-2 verification of the STATUS-header flash fix — the last piece
before the fork bundle push. Delta is client-only: `app.js` (+17) and
`tests/test_frontend.js` (+35). LIVE :8471 serves the reviewed tree byte-for-byte
(md5 match on app.js). Re-derived from source + a differential test run + suites.

Bug: on refresh, `/vcs` can return before the first `/check`; `renderStatusVerdict`
then ran `statusRollup` off the initial `lastCompiles=false` and flashed a false
`✗ type error`. Fix: a `firstCheckDone` gate makes `renderStatusVerdict`
early-return (header stays pending "checking…") until a COMPLETED check lands; set
in `applyCheck` / `applyWorkspaceView`, reset in `clearResults` on buffer switch.

**DELTA VERDICT: CONFIRM.** The gate removes the false flash without weakening any
fail-closed semantics, resets honestly across buffers, does not touch the PROOF
model or the anti-drift lock, and is covered by a genuine differential test.

## Per-item verdicts

1. **Pre-first-check: no false verdict — CONFIRM.** The new test drives
   `refreshVcs()` (i.e. `/vcs`) alone before the first check and asserts the
   header (`#status`) never contains "type error" and stays "checking…".
   **Differential (the strong evidence):** I ran the NEW test against the OLD
   `app.js` (git show 4dd62bc07d:voxide/app.js, no `firstCheckDone`) in an
   isolated temp copy — it FAILS both pre-check assertions ("no false 'type
   error'" and "stays pending 'checking…'"), i.e. the old code really did flash
   `type error`. Against the fixed tree all three pass. The bug is real, the test
   catches it, and the fix resolves it.
2. **After a completed check: fail-closed UNCHANGED — CONFIRM.** The STATUS
   honesty asserts still pass at this tip: (a) all-proved+compiles → `verified ·
   2/2`; (b) compile error → `type error`, never verified/proved, PROOF reads
   `unavailable`; (c) disproved → `1 disproved`, never verified. The gate only
   blocks BEFORE `firstCheckDone`; once a completed check sets it, `statusRollup`
   runs exactly as before — a REAL error verdict is never suppressed.
3. **Reset on buffer/workspace switch — CONFIRM.** `clearResults` sets
   `firstCheckDone = false`; the #160 test opens a fresh editable file (which
   routes through `clearResults`) and confirms the header is honestly pending
   until its own check completes, then clears. `applyWorkspaceView` sets the flag
   for the active unit's completed workspace check.
4. **No regression to compact-polish / goal-line verdict — CONFIRM.** The delta
   touches only `renderStatusVerdict` + the `firstCheckDone` global; `renderVc` /
   `paneBodyLines` (goal-line verdict) are untouched. The compact-polish header
   and goal-line tests remain green.
5. **ANTI-DRIFT unaffected — CONFIRM.** The delta does NOT touch `pane_model.js`,
   `tools/`, or `test_pane_fidelity.js` (git stat empty). The STATUS header
   (`#status`) is outside `#pane-body`, so no rebaseline was needed and none was
   done. `test_pane_fidelity` = 22/22.
6. **Suites (real counts at efb55b8e5b):** fidelity **22 pass**, regressions
   **27 pass**, frontend **87 ok / 0 fail** (+3: the three #160 assertions),
   python `discover -s tests` **109 OK / 1 skipped / 0 fail**. (The impl report's
   "88 OK/3 skipped" reflects a narrower discovery scope; full discover is 109
   with 0 failures.)

## Bottom line (delta)
CONFIRM at tip efb55b8e5b. The `firstCheckDone` gate cleanly removes the transient
false "type error" on refresh (differential-tested: fails pre-fix, passes
post-fix), preserves every fail-closed verdict after a completed check, resets
honestly on buffer switch, and leaves the PROOF model + anti-drift lock untouched.
Green to push the fork bundle (compact polish + labels + fade compiler.py + this
fix).

---

# DELTA-VERIFY — 3 visual tweaks (efb55b8e5b → 5b7782784c)

Independent lane-2 verification of #161 glyph-right, #162 vox1 failed-goal
underline, #149 nested-goal wash. Polish = `60f7b00eb5` (tweaks) + `5b7782784c`
(fidelity rebaseline). Client-only (app.js, pane_model.js, style.css, tests). LIVE
:8471 serves the tree byte-for-byte (app.js/pane_model.js/style.css all IDENTICAL
to the worktree). Re-derived from source + shared model + live tool + suites.

**DELTA VERDICT: CONFIRM.** All three tweaks are correct and honest; anti-drift is
green with a legitimate rebaseline. One NON-BLOCKING cleanup nit (a harmless dead
`diagnostic-squiggle-verify` branch in the view-harness tool — see item 2).

## Per-item verdicts

1. **#161 glyph moved to the RIGHT — CONFIRM.** Model + DOM + harness all render
   `⊢ <goal>[ · qual]  <glyph>` (glyph at line end, two-space gap) in BOTH views.
   Live tool:
   - PROVED  compact & full: `⊢ 7 > 0  ✓`
   - DISPROVED compact: `⊢ 2 = 1 · no witness  ✗`  (welded `· no witness` stays
     with the goal, glyph to its right — present in compact)
   - UNPROVED compact: `⊢ x * x >= 0  ⚠`
   HONESTY: colour-blind-safe distinct glyphs (`✓`/`✗`/`⚠`); whole line tinted by
   verdict (ANSI: proved bold GREEN, disproved bold RED incl. the `· no witness`
   + `✗`, unproved bold AMBER); green ONLY when the compiler reports `proved`
   (goal colour keyed on per-VC status). CSS `.goal-mark { float: right; }` places
   the glyph; `.goal-turn`/`.goal-qual` keep their weight.
2. **#162 vox1 failed-goal underline (no wavy) — CONFIRM (+ 1 non-blocking nit).**
   `renderDiagnostics` now gates the wavy mark behind `else if (!isVerify)`, so a
   VERIFICATION failure gets NO `diagnostic-squiggle`; its editor underline is the
   status-aware `vc-<status>` mark drawn by `markVcs` (`className: "vc-" +
   vc.status`) — CSS `.vc-disproved` solid red, `.vc-unproved` dashed. A real
   parse/TYPE error still gets the wavy `diagnostic-squiggle`. The dead
   `.diagnostic-squiggle-verify` CSS rule is removed. Span-less verification
   errors still get the amber `diagnostic-point-verify` "!" widget (correct).
   NIT (non-blocking): one dangling reference to the removed class survives at
   `tools/voxide-view.js:620` (`markKind`: `if (className.includes(
   "diagnostic-squiggle-verify")) return "verify"`). It is harmless DEAD code —
   app.js emits that class 0 times now, so the branch never fires, and the
   fallthrough still classifies type errors as "type" and verification marks via
   the `vc-<status>` regex. Recommend deleting the dead branch; it does not affect
   behavior, honesty, or any passing test.
3. **#149 nested-goal wash — CONFIRM.** `--vc-goal-bg` added (dark
   `rgba(88,166,255,0.07)` / light `rgba(11,95,191,0.08)`); the base `.vc-*`
   underline mark gets `background-color: var(--vc-goal-bg)`. Nested obligation
   spans nest CodeMirror mark spans, so the washes STACK (deeper = more opaque).
   Purely visual: it is an EDITOR-mark background, touches no pane text/model, so
   the anti-drift text surface is unaffected (pane_model change in this commit is
   only the #161 glyph move). Low alpha keeps a single goal subtle and does not
   fight the underline or code legibility.
4. **ANTI-DRIFT — CONFIRM.** `test_pane_fidelity` = 22/22; CLI == DOM including
   the two-space gap before the glyph (the goal text with the trailing glyph is
   byte-identical between tool and browser). The rebaseline `5b7782784c` re-pins
   the app.js-purity ref `0ed2eafeeb → 60f7b00eb5` (the glyph-right commit) — a
   legitimate intentional-layout re-pin, NOT a loosening (the tool==browser
   property is independent of the pinned sha and holds byte-for-byte). The
   view-harness test was updated to assert the new layout
   (`⊢ 0 0 · no witness  ✗` with glyph to the right).
5. **Suites (real counts at 5b7782784c):** fidelity **22 pass**, regressions
   **27 pass**, frontend **87 ok / 0 fail**, python `discover -s tests` **109 OK /
   1 skipped / 0 fail** (incl. the updated `test_view_harness.py`). (The impl
   report's "88 OK/3 skipped" reflects a narrower discovery scope; full discover
   is 109 with 0 failures.)

## Bottom line (delta)
CONFIRM at tip 5b7782784c. The glyph rides the right end of the verdict-coloured
goal line (welded `· no witness` intact in compact, colour-blind-safe glyphs,
green only when proved); verification failures use the vox1 `vc-<status>`
underline (solid/dashed red) instead of the wavy squiggle, which is now reserved
for type errors; the nested-goal wash is a subtle visual-only editor stack; and
anti-drift holds with a legitimate rebaseline. Green to push. One non-blocking
cleanup: remove the dead `diagnostic-squiggle-verify` branch in voxide-view.js.

---

# DELTA-VERIFY — #163 compact-label-drop + provenance hover (5b7782784c → 1f90810453)

Independent lane-2 verification of #163: compact drops the hypothesis label
(bare predicate only), full keeps `label : predicate`; plus vox1-style provenance
hover (pane row ↔ editor source span). Delta = `ff4f09c3d0` (impl) + `1f90810453`
(fidelity rebaseline). Client-only. LIVE :8471 serves the tree byte-for-byte
(app.js/pane_model.js/style.css all IDENTICAL). Re-derived from source + live
tool + suites.

**DELTA VERDICT: CONFIRM.** Compact hides only the LABEL (predicate always shown);
honesty markers untouched; the hover correlation is browser-only interactive
chrome that never enters the shared model, so the anti-drift lock holds.

## Per-item verdicts

1. **Compact drops label, full keeps it — CONFIRM.** Model change:
   `pushLines(out, full ? row.label + " : " + row.display : row.display, ...)` —
   compact emits the bare predicate, full the labelled row; depth stays 0 (shown
   in compact). Live tool on binder.ml @8:4 (hyps `annotation`, `x`):
   - COMPACT: `⊢ x > 0  ✓` / `7 = 7` / `x = 7`  (bare predicates, no label)
   - FULL:    `⊢ x > 0  ✓` / 8:4 / annotation obligation / `annotation : 7 = 7`
              / `x : x = 7`
   Multi-hyp confirmed (two hyps). HONESTY: only the LABEL is hidden — the
   predicate is ALWAYS shown; the DISPROVED welded qualifier is untouched
   (`⊢ 2 = 1 · no witness  ✗` in compact); off-obligation compact still shows
   NOTHING (context facts + caveat remain full-only). A new regression test
   asserts exactly this ("compact hides the LABEL, never the fact").
2. **Hover correlation, both directions — CONFIRM** (one minor coverage note).
   (a) pane row → editor: a delegated `mouseover` on `#pane-body` (survives
   re-renders) calls `paintHoverSpan`, which draws a transient `.prov-hl`
   *editor* mark (`cm.markText`) for the goal's VC span (`paneVc.start/end`) or
   the hyp's `hyp.span` (`data-hyp` index); cleared on `mouseout` (with a
   same-row guard). Tested: "hovering hyp 0 paints one prov-hl mark", "mouseout
   clears", "hovering the goal paints its obligation span (0:4-0:8)", "mouseout
   after a goal hover clears". (b) editor → pane: a `mousemove` on the CM wrapper
   calls `editorHoverToPane`, which maps the pointer via `coordsChar`, picks the
   SMALLEST shown span containing it (goal + each hyp), scoped to `paneVc`, and
   toggles `.prov-active` on the matching row; cleared on `mouseleave` and before
   every re-render. Direction (b) is code-verified sound (smallest-span-wins,
   scoped, cleared) but has no automated test — the harness CM stub lacks a real
   `getWrapperElement`/`coordsChar`, and the wiring is guarded for exactly that
   (`if (cmWrapper && cmWrapper.addEventListener)`). Minor coverage gap, not a
   defect.
3. **ANTI-DRIFT / CHROME — CONFIRM.** The hover classes never touch the shared
   model or the serialized pane text: `.prov-hl` is a CodeMirror editor mark (not
   pane DOM at all); `.prov-active` is a runtime `classList` toggle applied only
   while hovering an existing row, and `renderProofPane` clears both hover states
   before rebuilding, so serialized `#pane-body` innerHTML never contains them.
   `CHROME_CLASSES` is UNCHANGED (not in the diff), and `pane_model.js` has zero
   `prov-` references. `test_pane_fidelity` = 22/22 (tool text == browser DOM at
   every caret, both compact levels). The rebaseline re-pins the app.js-purity
   ref `60f7b00eb5 → ff4f09c3d0` (the compact-drop-label commit) — a legitimate
   intentional-DOM-change re-pin, not a loosening.
4. **Suites (real counts at 1f90810453):** fidelity **22 pass**, regressions
   **30 pass** (+3: the #163 label-drop assertions), frontend **92 ok / 0 fail**
   (+5: the provenance-hover assertions + label-drop), python `discover -s tests`
   **109 OK / 0 fail**. (The impl report's "88 OK/3 skipped" reflects a narrower
   discovery scope; full discover is 109 with 0 failures.)

## Bottom line (delta)
CONFIRM at tip 1f90810453. Compact now shows bare predicates (label a hover away),
full keeps `name : predicate`, and the predicate is never hidden; the DISPROVED /
off-obligation honesty is intact. The provenance hover is browser-only chrome
(editor mark + runtime class toggle, cleared on mouse-out and on re-render) that
leaves the shared model and the tool==browser anti-drift lock untouched. Green to
push. (Prior non-blocking nit — the dead `diagnostic-squiggle-verify` branch in
voxide-view.js — is unrelated to this delta and still open.)

---

# DELTA-VERIFY — #165 no-witness gating (1f90810453 → eea0d1c1bf)

Independent lane-2 verification of #165: the welded disproved `· no witness`
headline qualifier is shown only when a witness would be MEANINGFUL. MODEL-ONLY
delta (`pane_model.js obligationModel` + `test_pane_regressions.js`); no
rebaseline. LIVE :8471 serves the tree byte-for-byte (pane_model.js IDENTICAL;
app.js untouched by the delta). Re-derived from source + live tool + suites.

Gate: `witnessRelevant = vc.hypotheses.length > 0 || /\bv_\d+\b/.test(vc.lean||"")`;
`tokenQualifier = disproved ? (counterexample ? "witness" : witnessRelevant ? "no
witness" : null) : null`.

**DELTA VERDICT: CONFIRM.** The vacuous `· no witness` is dropped from ground
disproved goals; the verdict glyph, the full-view refutation note, the free-var
`· no witness`, the concrete `· witness`, and unproved are all unchanged; honesty
holds (never a false witness claim).

## Per-item verdicts

1. **GROUND disproved — CONFIRM.** `counterexample.ml` `2 = 1` has 0 hyps and a
   Lean theorem with no `v_N`, so `witnessRelevant=false`. Live tool COMPACT:
   `⊢ 2 = 1  ✗` — NO `· no witness`. The `✗` verdict/glyph is untouched. FULL
   still shows the depth-1 refutation: `refutation` / "Disproved: the solver
   refuted this goal but produced no concrete witness." — only the vacuous
   HEADLINE qualifier drops.
2. **FREE-VAR disproved — CONFIRM.** A disproved VC with a `v_N` binder in its
   Lean (no hyp) → `⊢ x > 0 · no witness  ✗` (kept); a disproved VC with a
   hypothesis (no `v_N`) → `⊢ x > 0 · no witness  ✗` (kept). Both witness-relevant
   paths preserved.
3. **CONCRETE WITNESS — CONFIRM.** A disproved VC with a non-null
   `counterexample` → `⊢ x > 0 · witness  ✗`. The gate never suppresses a real
   witness (the `counterexample ? "witness"` arm is checked first, independent of
   `witnessRelevant`).
4. **HONESTY — CONFIRM.** No path ever implies a witness that does not exist:
   `null` and `no witness` both assert the absence, and `witness` appears only
   with a real counterexample model. Unproved stays distinct: `⊢ x * x >= 0  ⚠`
   (glyph `⚠`, no witness qualifier) vs disproved `✗`. Only change is dropping the
   vacuous `no witness` on witness-irrelevant (ground) goals.
5. **NO REBASELINE — CONFIRM (justified).** The qualifier is computed in
   `pane_model.js`; both `app.js` and `voxide-pane` render `ob.goalQualifier`, so
   `app.js` is byte-identical (empty diff), `test_pane_fidelity.js` is untouched,
   and `OLD_APP_REF` remains `ff4f09c3d0`. The section-4 app.js-purity check
   passes as-is; fidelity = 22/22.
6. **Suites (real counts at eea0d1c1bf):** fidelity **22 pass**, regressions
   **34 pass** (+4: the #165 witness-gating assertions — ground→dropped, hyp→kept,
   `v_N`→kept, concrete→witness), frontend **92 ok / 0 fail**, python
   `discover -s tests` **109 OK / 0 fail**. (The impl report's "88 OK/3 skipped"
   reflects a narrower discovery scope; full discover is 109 with 0 failures.)

NOTE (non-blocking, as flagged): a GROUND goal that carries a free-var HYPOTHESIS
(e.g. goal `2 = 1` with a hyp `a > 0`) still keeps `· no witness` (because
`hypotheses.length > 0`). Confirmed live: `⊢ 2 = 1 · no witness  ✗`. This is
HONEST over-inclusion — it states that no concrete witness exists (always true
here), and can NEVER produce a false witness claim; it merely shows the qualifier
in a borderline case. Not a regression; the heuristic errs toward the honest
message.

## Bottom line (delta)
CONFIRM at tip eea0d1c1bf. Ground disproved goals drop the vacuous headline
`· no witness` (verdict glyph and full-view refutation note intact); free-var
`· no witness` and concrete `· witness` are unchanged; unproved stays distinct;
honesty is preserved (over-inclusion is toward, never against, the truth). No
rebaseline needed (app.js byte-identical). Green to push.

---

# DELTA-VERIFY — #173 nested-wash depth-stacking (eea0d1c1bf → 9b9ee61667)

Independent lane-2 verification of #173: the nested-goal wash now deepens by
EXPLICIT containment depth. VISUAL-ONLY delta (`app.js markVcs` editor-mark
classes + `style.css` per-depth classes + `test_frontend.js`); no rebaseline.
LIVE :8471 serves the tree byte-for-byte (app.js/style.css IDENTICAL). Re-derived
from source + an independent depth replica + suites.

**DELTA VERDICT: CONFIRM.** Depth counts DISTINCT enclosing span geometries
(dedup handles the #144 duplicate-span case correctly), the per-depth alphas
strictly increase, and the change touches only editor marks + CSS, so the
anti-drift text surface is untouched and no rebaseline is needed.

## Per-item verdicts

1. **DEPTH CORRECTNESS — CONFIRM.** `spanStrictlyContains(outer, inner)` requires
   containment AND a strict inequality, so two obligations sharing the EXACT span
   do not contain each other. `markVcs` counts DISTINCT enclosing span geometries
   via a `Set` keyed on `line:col-line:col`, then `Math.min(size, 3)`. I
   independently replicated the logic on `recursion.ml`:
   - id0 outer `9:3–9:50` → **d0** (no encloser)
   - id1 inner `9:29–9:36` → **d1** (enclosing geoms = {`8:2-8:49`}) — the
     duplicate outer span (id0 AND id3 both `8:2–8:49`) DEDUPES to ONE level, so
     it is d1, NOT d2.
   - id2 inner `9:43–9:50` → **d1**; id3/id4/id6 (whole-span siblings) → d0;
     id5 → d1.
   Clamp at d3 confirmed (`Math.min(containers.size, 3)`). The frontend test
   asserts exactly this, including "the duplicate outer span does not inflate
   depth".
2. **STACKING VISIBLE — CONFIRM.** `vc-goal-d0..d3` map to strictly increasing
   alpha: dark `0.11 / 0.20 / 0.30 / 0.42`, light `0.12 / 0.22 / 0.34 / 0.46`.
   Base d0 (`0.11`, bumped from the prior `0.07`) is clearly visible. Because the
   depth is computed EXPLICITLY per obligation and baked into the class, the
   nesting reads as increasing opacity regardless of how CodeMirror splits/
   flattens overlapping mark segments (deepest class wins on a shared segment).
3. **VISUAL-ONLY / NO REBASELINE — CONFIRM.** The `app.js` change is confined to
   `markVcs` (the `cm.markText` className now appends `vc-goal-dN`) plus the new
   `spanStrictlyContains` helper — `renderVc` / `#pane-body` / `#pane-mode` /
   `#legend` are untouched, and `pane_model.js` + `test_pane_fidelity.js` are not
   in the delta. So the anti-drift TEXT surface is unchanged: `test_pane_fidelity`
   = 22/22 and `OLD_APP_REF` remains `ff4f09c3d0` (no rebaseline). No honesty or
   verdict change — the `vc-<status>` class and all verdict markers are intact;
   only a translucent wash class is added.
4. **Suites (real counts at 9b9ee61667):** fidelity **22 pass**, regressions
   **34 pass**, frontend **95 ok / 0 fail** (+3: the nested-wash depth assertions
   — outer→d0, singly-nested→d1 with the dedup note, doubly-nested→d2), python
   `discover -s tests` **109 OK / 0 fail**. (The impl report's "88 OK/3 skipped"
   reflects a narrower discovery scope; full discover is 109 with 0 failures.)

## Bottom line (delta)
CONFIRM at tip 9b9ee61667. The nested-goal wash now deepens by explicit
containment depth (distinct enclosing geometries, dedup-correct for the #144
shared-span pair, clamped at d3), with strictly increasing per-depth alpha and a
clearly-visible base. It is an editor-mark + CSS change only: the shared model,
the pane text surface, and the tool==browser anti-drift lock are all untouched, so
no rebaseline was needed. Green to push.
