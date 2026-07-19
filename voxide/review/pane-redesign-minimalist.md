# voxide right-pane redesign — RADICAL MINIMALISM / single-focus

## Philosophy

The right pane exists to answer **one** question at a time: *what is true at the
cursor?* Concretely, when I put the caret somewhere, I want to know instantly —

1. Is the whole buffer OK? (one glyph, always visible)
2. Is there an obligation **here**? Is it proved? What's the goal?

Everything else — hypotheses detail, generated Lean, solver diagnostics, the raw
predicate, the type at cursor, the inferred signature, the verdict legend, the
five section headings — is *secondary* and is either hidden behind a single click
or cut entirely. A proof IDE's default surface should be a **status line + one
focus block**, not five stacked panels each with its own `<h2>`.

The design is two things stacked, nothing more:

```
[ STATUS STRIP ]   one line, always present — the buffer's overall verdict
[ FOCUS       ]    the cursor's one thing — badge + goal (+ honesty payload)
```

Compact = exactly those two, the focus collapsed to a **single dense line**. Full
= the same, with **at most one** layer of detail expanded beneath the focus. No
third level lives on the surface; deep material (Lean, solver dump, signature)
stays behind disclosures reachable from either view.

---

## 1. Component inventory — KEEP / CUT / MERGE / MOVE

| # | Current element | Verdict | Why |
|---|---|---|---|
| — | 5 section `<h2>`s ("Diagnostics", "Verification", "Proof pane", "Type at cursor", "Inferred signature") | **CUT** | Headings restate what the content already says. One focus surface needs no labels. |
| 1 | Diagnostics: "No parse or type errors." | **CUT** | Silence = health. The green dot in the status strip already says this; a whole line to say "nothing wrong" is pure filler. |
| 1 | Diagnostics: error list (with squiggles/marks) | **MERGE** | When errors exist they *become* the focus block (see error mockup); marks in the editor stay. Not a standing panel. |
| 2 | Verification: "Verified ✓ — …" / "failed" / "No refinements…" / "Not run." | **MERGE** | Collapses into the one-line status strip as a glyph + tally. Prose forms are cut. |
| 3 | Proof pane: `mode:` line (obligation/context/placeholder) | **CUT** | The badge + goal already tell you the mode. "mode: obligation" above "[proved] ⊢ …" is redundant. |
| 3 | Status badge `[proved]`/`[disproved]`/`[unproved]`/`[solver error]` | **KEEP** | This is *the* one thing. It becomes the focus headline. |
| 3 | Goal `⊢ …` | **KEEP** | The second essential; shares the headline line in compact. |
| 3 | `kind` label ("annotation obligation" / "contract obligation") | **CUT** | Low-value taxonomy; the goal and its source underline say where it came from. |
| 3 | Hypotheses `name : predicate` | **MOVE** | Behind the focus disclosure. Compact shows a count only; full expands them (one layer). |
| 3 | Faded/unused hypotheses | **CUT (compact) / KEEP dimmed (full)** | The model already flags them *unused* → lowest-value rows; drop them from compact entirely, dim in full. |
| 3 | Refutation heading (the word "refutation") | **CUT** | The `[disproved]` badge already means "refuted". The heading adds a word, not information. |
| 3 | Witness / "no concrete witness" note | **KEEP** | Honesty-critical and the actionable payload of a disproved goal. Inlined into the focus line in compact. |
| 3 | `[solver detail]` (grind diagnostics) | **KEEP, disclosure** | Already collapsed; stays reachable in full only. Deep-debug material. |
| 3 | `[raw predicate]` | **CUT** | Redundant since the printer fix made the goal display source-like; the `app[…]` form no longer adds anything a user reads. |
| 3 | `[generated Lean]` | **KEEP, disclosure** | Hard constraint: escape hatch must remain reachable. Behind a `⋯` in compact, a disclosure in full. |
| 3 | `+N more obligations here` | **KEEP, terse** | Shortened to `+N`. Honest signal that the span overlaps siblings. |
| 3 | State-at-cursor "known at this point" rows | **CUT (compact) / KEEP (full)** | Compact shows nothing (implying nothing → honest). Full shows facts under a click. |
| 3 | State-at-cursor approximation caveat | **KEEP whenever facts show** | Load-bearing honesty; may be tightened but never dropped when the facts are visible. |
| 3 | Verdict-key legend | **MOVE** | Behind a `?` affordance on the status strip. Static reference, learned once. |
| 4 | Type at cursor | **MOVE** | To a single on-demand line at the bottom of full view; cut from the always-on surface. It's reference, not proof focus. |
| 5 | Inferred signature | **MOVE, disclosure** | Collapsed at the very bottom of full view. Rarely needed mid-proof; "unavailable" state folds into the error focus. |

---

## 2. ASCII mockups (whole right pane)

Legend of glyphs: `●` proved/verified · `✗` disproved/failed · `?` unproved
(no witness) · `!` solver error · `‼` compile error · `·` neutral/off-obligation ·
`⌄` expandable · `⋯` escape-hatch menu (holds generated Lean).

### (a) Cursor on a PROVED obligation — `abs.ml`, caret in `x >= 0`

**COMPACT**
```
● verified · 2/2 proved                              ?
─────────────────────────────────────────────────────
● proved   ⊢ x >= 0                              1 fact ⌄  ⋯
```

**FULL**
```
● verified · 2/2 proved                              ?
─────────────────────────────────────────────────────
● proved
⊢ x >= 0

facts
  x >= 0
⌄ generated Lean
⌄ inferred signature
```

### (b) Cursor on a DISPROVED (no-witness) obligation — `counterexample.ml`, caret in `need_one 2`

**COMPACT**
```
✗ 1 disproved · 0/1 proved                           ?
─────────────────────────────────────────────────────
✗ disproved   ⊢ 2 = 1   · no witness                     ⋯
```

**FULL**
```
✗ 1 disproved · 0/1 proved                           ?
─────────────────────────────────────────────────────
✗ disproved
⊢ 2 = 1
refuted — solver found no concrete witness
⌄ solver detail
⌄ generated Lean
```
*(If a candidate witness exists, the line reads instead:*
`false when (candidate; Lean unbounded-Int model — may not be a valid machine int): x = 2`
*— the candidate caveat is never dropped.)*

### (c) OFF any obligation (state-at-cursor) — `recursion.ml`, caret on `fib`'s binder

**COMPACT**
```
● verified · 8/8 proved                              ?
─────────────────────────────────────────────────────
· no obligation at cursor                        known here ⌄
```

**FULL**
```
● verified · 8/8 proved                              ?
─────────────────────────────────────────────────────
known here — approximate
  n : n >= 0
Facts introduced textually above the caret, from nearby obligations; branch
conditions omitted and a nested binding may show out of scope. A hint, not a
guarantee of what holds here.
```

### (d) File with a compile ERROR

**COMPACT**
```
‼ 1 type error                                       ?
─────────────────────────────────────────────────────
‼ line 8, col 5 — Unbound value foo
```

**FULL**
```
‼ 1 type error                                       ?
─────────────────────────────────────────────────────
type · line 8, col 5
  Unbound value foo
(signature & obligations unavailable until errors are fixed)
```

---

## 3. Section order, grouping, and the compact↔full rule

**Order (top → bottom), always the same two zones:**

1. **Status strip** — one line, always present. Left: overall glyph + verdict +
   `proved/total` tally (e.g. `● verified · 8/8 proved`, `✗ 1 disproved · 0/1
   proved`, `‼ 1 type error`). If obligations are hidden or data is unavailable it
   says so (`· 3 hidden`, `· data unavailable`). Right: a `?` that reveals the
   verdict legend on demand. **The strip mirrors the compiler's authoritative
   overall verdict** — it is never computed optimistically from the visible VCs
   alone; a type error or a hidden failing unit downgrades it.
2. **Focus** — the cursor's one thing. Its shape depends on cursor state
   (obligation / off-obligation / error), never on a heading.

**Compact drops/collapses:**
- Focus renders as a **single line**: `<badge> ⊢ <goal>  <payload?>  <count ⌄>  ⋯`.
- Hypotheses → a count (`1 fact ⌄`), unused ones excluded from the count.
- State-at-cursor facts → **not shown** (a neutral "no obligation" line; a `known
  here ⌄` reveals them on click, with the caveat).
- Solver detail, generated Lean, signature, type-at-cursor, verdict legend →
  hidden; generated Lean still reachable via `⋯`, legend via `?`.
- Compile error → collapsed to `‼ line L, col C — <message>` (first error; `+N`
  if more).

**Full adds exactly one layer beneath the focus:**
- Hypotheses expanded (used bold, unused dimmed).
- Disprove/unprove honesty payload spelled out (witness/no-witness/"may still
  hold").
- `⌄ solver detail`, `⌄ generated Lean`, `⌄ inferred signature` as collapsed
  disclosures; a single type-at-cursor line when present.
- State-at-cursor facts + the full approximation caveat.

No element that exists in compact is *absent* from full, and nothing in full is
more than one expand-click from the focus.

---

## 4. Rationale + top-3 clutter cuts

**Rationale.** The current pane is five co-equal panels; the eye has to scan all
five to find "is this proved?". Minimalism makes the *answer* the layout: a glyph
you never lose (strip) and a single block that reshapes to the cursor. Detail
isn't deleted — it's demoted below the fold, one click away, so the 95% case (glance,
read verdict, move on) costs zero scanning and the 5% case (debug a failure) costs
one click. Honesty is untouched: every cut removes *chrome* (headings, "mode:",
"No errors.", the raw predicate) or *demotes* detail behind a disclosure — no cut
ever makes the pane imply more than the compiler established. The three honesty
invariants are explicitly preserved: (i) a disproved goal always shows "no
witness"/candidate-caveat, (ii) off-obligation facts always carry "approximate …
not a guarantee", (iii) the strip never says "verified" when a unit errored or an
obligation is hidden.

**Top 3 clutter-cuts:**

1. **All five `<h2>` headings and the `mode:` line.** ~6 lines of chrome that
   restate what the content beneath them already says. The status strip + a
   self-describing focus block replace them.
2. **Type-at-cursor and Inferred-signature as standing panels.** Reference
   material, not proof focus — moved to a single on-demand line and a bottom
   disclosure. This reclaims the bottom third of the pane for nothing lost.
3. **The filler prose + verdict legend:** "No parse or type errors.", "No
   refinements to verify.", "Not run.", the raw-predicate block, and the
   always-on verdict key. Replaced by one glyph (strip) and a `?` affordance.

## 5. Purity note (anti-drift lock)

Everything above is a pure function of `(vcs, cursor)` plus the existing
`verification` verdict already threaded to the pane, so it fits `pane_model.js`
unchanged in contract:
- The status strip's tally is `count(status=proved)/count(vcs)` folded with the
  authoritative `verification.status` and `hidden`/`unavailable` flags — all
  already in the model.
- Compact/full stays a single boolean gate over which model fields the sink emits
  (as today), so the terminal tool and browser render identically; the tool's
  `--section` set (mode/body/legend) maps onto strip/focus/legend with no new
  data source.
