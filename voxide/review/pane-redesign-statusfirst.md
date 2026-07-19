# voxide right-pane redesign — STATUS-FIRST / CODE-ANCHORED

## Philosophy

One question dominates every glance at this pane: **"is the thing under my cursor
proved, and if not, why not?"** So the pane leads with a single loud **verdict
token** — colored, carrying a swatch of the exact underline the editor draws for
that span — with the **goal beside it**. Everything else is subordinate detail,
and every piece of it is pinned to **where in the code it applies** (`file:line:col`,
a code slice, a `→ :line` jump) rather than explained in sentences.

Two design rules follow:

1. **Verdict + goal are the headline; prose is near-zero.** Labels like
   "mode: obligation", "refutation", "annotation obligation", and the verdict-key
   legend are all restatements of what the token already says in color. They go.
   What remains is symbols, badges, code, and `file:line` anchors.
2. **Compact = the headline band alone.** The verdict token, its underline swatch,
   the goal, the anchor, and a row of collapsed chips (counts + disclosure carets).
   Nothing else. Full adds the facts, the reason, type/signature, and the file
   strip — still terse, still code-anchored.

**Honesty is load-bearing in a status-first design**, because a bold badge is
exactly what a skimmer over-reads. So the token vocabulary is strict and the
qualifiers are welded to the token, never dropped by compact:

- The token *never* claims more than the compiler established. `CONTEXT` (off an
  obligation) is grey and is **not** a verdict — it can never read as "proved".
- `DISPROVED` always carries `· no witness` when the solver produced no model; a
  witness badge appears *only* when a concrete counterexample exists.
- `CONTEXT` always carries `approx` and the "omits branch conditions" qualifier.
- A compile error shows **no obligation verdict at all** — obligations read
  `unavailable`, not "proved".
- The file-level chip and the cursor-obligation token are visually distinct
  regions so a green file chip can never be misread as "this obligation proved".

---

## 1. Component inventory

| # | Current element | Action | Why |
|---|---|---|---|
| 1 | `mode: obligation/context/placeholder` line | **CUT** | The verdict token already encodes mode (`PROVED`/`CONTEXT`/`ERROR`). Pure restatement. |
| 2 | status badge `[proved]`/`[disproved]`/… | **KEEP → PROMOTE** | Becomes *the* headline verdict token: big, colored, underline swatch. |
| 3 | kind label (`annotation obligation` / `contract obligation`) | **MERGE + MOVE** | Demote to a small grey tag on the band, full-only. |
| 4 | goal `⊢ …` | **KEEP → PROMOTE** | The other half of the headline; sits directly under the token. |
| 5 | hypotheses (`name : pred`, faded if unused) | **KEEP → MOVE** | Becomes the full-only "facts" strip; compact shows a count chip. |
| 6 | `refutation` heading | **CUT** | The red `DISPROVED` token is the heading. |
| 7 | witness / "no concrete witness" note | **KEEP (terse)** | Honesty-critical. Rendered as an inline `· no witness` qualifier welded to the token. |
| 8 | `[solver detail]` disclosure | **KEEP** | Collapsed disclosure; auto-open only on the active failing obligation. |
| 9 | `[raw predicate]` disclosure | **KEEP → MOVE** | Collapsed disclosure, full-only. |
| 10 | `[generated Lean]` disclosure | **KEEP** | Hard constraint — must stay reachable. Collapsed disclosure. |
| 11 | `+N more here` | **KEEP (terse)** | Becomes a chip `+N here → :line` with a jump anchor. |
| 12 | off-obligation state-at-cursor facts | **KEEP + MERGE** | Same facts strip, under a grey `CONTEXT` token. |
| 13 | approximation caveat (long prose) | **KEEP (compressed)** | Honesty-critical but 3 lines → a `approx · omits branch conds` tag + `▸ why approximate` disclosure holding the full caveat verbatim. |
| 14 | verdict-key legend | **CUT from body → MOVE** | Behind a `?` on the token. It restates token meanings; only shown on demand. |
| 15 | Diagnostics section | **MERGE** | Folded into the bottom FILE strip as a count + list; surfaced at top only for a compile error. |
| 16 | Type at cursor | **KEEP → MOVE** | One terse line in the OBLIGATION region, full-only. |
| 17 | Inferred signature | **KEEP → MOVE** | Into the FILE strip behind `▸ signature`; it is file-level and long. |
| 18 | Verification (overall verdict) | **MERGE** | Becomes the FILE strip's single chip (`✓ verified` / `✗ 1 disproved` / `▲ errors`). |

Net: five stacked sections collapse into **two regions** — a dominant
**OBLIGATION** band (cursor-anchored) and a thin **FILE** status bar
(workspace-anchored).

---

## 2. Layout & section order

```
╭─ OBLIGATION ─ <file:line:col> ─ `<code slice>` ─╮   ← anchor header
│  <VERDICT TOKEN>  <underline swatch>   <kind>   │   ← THE headline
│  ⊢ <goal>                                       │
│  <reason / witness qualifier>  (disproved only) │   ← full
│  facts (N):  name : pred   name : pred          │   ← full
│  type:  <type at cursor>                        │   ← full
│  <chips: +N here→:L   ▸raw   ▸Lean   ▸detail>   │
├─ FILE ──────────────────────────────────────────┤   ← thin status bar
│  <FILE CHIP>  · N diagnostics                    │
│  <diagnostic line → :L:C>            (if any)    │
│  ▸ signature                                     │   ← full
╰──────────────────────────────────────────────────╯
```

**Underline swatches** (a copy of the editor underline for that span, so the
badge and the code agree at a glance): proved `───`, disproved `～～～`,
unproved `···`, solver-error `╌╌╌`. Context has no swatch (not a verdict).

Order rationale: verdict → goal → why → where-else, then the file bar last so the
per-cursor answer is always top-of-pane and the file answer is a stable footer.

---

## 3. Compact vs full rule

**Compact keeps only the headline band + the chip row + the file chip.** It drops
(does not delete — everything stays reachable):

- the facts strip → replaced by a `N facts ▸` chip,
- the kind tag, the type line, the reason prose,
- the diagnostic list and the signature (chip-only in FILE).

Compact **never** drops an honesty qualifier: `· no witness`, `approx`, and
`unavailable` ride on the token/chip in both views. A disclosure caret means the
detail is one click away, never gone.

---

## 4. ASCII mockups

Legend for the mockups: `✓`green `✗`red `?`amber `◦`grey `▲`red. `▸` = collapsed
disclosure. Tokens are shown in CAPS; color noted in brackets on first use.

### (a) Cursor on a PROVED obligation — `overview.ml:11:13`, `positive 7`

**FULL**
```
╭─ OBLIGATION ─ overview.ml:11:13 ─ `positive 7` ─╮
│  ✓ PROVED   ───            annotation           │   [green token, green swatch]
│  ⊢ 7 > 0                                         │
│  facts (0): none needed                          │
│  type: int{ _ > 0 } -> int                       │
│  +1 here → :11    ▸raw predicate   ▸generated Lean│
├─ FILE ───────────────────────────────────────────┤
│  ✓ verified   · 0 diagnostics                     │   [green chip]
│  ▸ signature                                       │
╰───────────────────────────────────────────────────╯
```

**COMPACT**
```
╭─ OBLIGATION ─ overview.ml:11:13 ─ `positive 7` ─╮
│  ✓ PROVED   ───                                  │
│  ⊢ 7 > 0                                          │
│  0 facts   +1 here→:11   ▸Lean                    │
├─ FILE ───────────────────────────────────────────┤
│  ✓ verified · 0 diag                              │
╰───────────────────────────────────────────────────╯
```

### (b) Cursor on a DISPROVED (no-witness) obligation — `counterexample.ml:10:22`

**FULL**
```
╭─ OBLIGATION ─ counterexample.ml:10:22 ─ `need_one 2` ─╮
│  ✗ DISPROVED   ～～～   · no witness    contract      │   [red token, red swatch]
│  ⊢ 2 = 1                                              │
│  refuted; solver produced no concrete witness         │
│  ▸solver detail   ▸raw predicate   ▸generated Lean    │
├─ FILE ─────────────────────────────────────────────────┤
│  ✗ 1 disproved   · 1 diagnostic                        │   [red chip]
│  verify: refinement failed (disproved) → :10:22        │
│  ▸ signature   (unavailable until fixed)               │
╰─────────────────────────────────────────────────────────╯
```

**COMPACT**
```
╭─ OBLIGATION ─ counterexample.ml:10:22 ─ `need_one 2` ─╮
│  ✗ DISPROVED   ～～～   · no witness                   │
│  ⊢ 2 = 1                                              │
│  ▸detail   ▸Lean                                      │
├─ FILE ─────────────────────────────────────────────────┤
│  ✗ 1 disproved · 1 diag                                │
╰─────────────────────────────────────────────────────────╯
```

> Honesty: `· no witness` is welded to the token in both views. Were a concrete
> model available, the token would instead carry `· witness ▾` opening the model —
> the qualifier's presence/word distinguishes the two cases and can never be dropped.

### (c) Cursor OFF any obligation (state-at-cursor) — `recursion.ml:9:5`

**FULL**
```
╭─ CONTEXT ─ recursion.ml:9:5 ─ approx ──────────╮
│  ◦ context      (not an obligation)             │   [grey token, no swatch]
│  facts known above here (5):                     │
│    n : n >= 0                                     │
│    n : n - 1 >= 0                                 │
│    fib : fib (n - 1) >= 0                         │
│    … +2                                           │
│  approx · omits branch conditions   ▸ why approx │
├─ FILE ───────────────────────────────────────────┤
│  ✓ verified   · 0 diagnostics                     │
│  ▸ signature                                       │
╰───────────────────────────────────────────────────╯
```

**COMPACT**
```
╭─ CONTEXT ─ recursion.ml:9:5 ─ approx ──────────╮
│  ◦ context   5 facts ▸   · omits branch conds    │
├─ FILE ───────────────────────────────────────────┤
│  ✓ verified · 0 diag                              │
╰───────────────────────────────────────────────────╯
```

> Honesty: the token is grey and reads `context (not an obligation)` — never a
> verdict. `approx` and `omits branch conditions` are pinned to the header and the
> facts line in both views; the full caveat text lives verbatim behind
> `▸ why approx`.

### (d) A file with a compile ERROR — `broken.ml:7:11`

**FULL**
```
╭─ OBLIGATION ─ broken.ml:7:11 ────────────────────╮
│  ▲ COMPILE ERROR                                  │   [red token]
│  This expression has type int but was expected    │
│  of type string                        → :7:11    │
│  obligations: unavailable until errors fixed       │
├─ FILE ─────────────────────────────────────────────┤
│  ▲ 1 error                                          │   [red chip]
│  type: line 7, col 11 — type mismatch  → :7:11     │
│  ▸ signature   (unavailable until fixed)           │
╰─────────────────────────────────────────────────────╯
```

**COMPACT**
```
╭─ OBLIGATION ─ broken.ml:7:11 ────────────────────╮
│  ▲ COMPILE ERROR                    → :7:11        │
│  obligations: unavailable                          │
├─ FILE ─────────────────────────────────────────────┤
│  ▲ 1 error                                          │
╰─────────────────────────────────────────────────────╯
```

> Honesty: no obligation verdict token is shown — nothing was verified. Both
> obligations and signature read `unavailable`, never "proved"/"verified".

---

## 5. Rationale + top-3 clutter cuts

**Why status-first works here.** The pane is consulted mid-edit, dozens of times a
minute, to answer one binary-ish question. A design that makes the answer the
largest, most colorful thing on screen — and co-locates the goal so you see *what*
was answered — turns each consultation into a sub-second glance. Anchoring every
detail to `file:line:col` and a code slice means the pane is read *against the
editor*, not as standalone prose: the eye bounces between a fact and the span it
constrains, which is how a programmer actually reasons about a proof obligation.

**Top-3 clutter cuts (each justified):**

1. **The `mode:` line, the `refutation` heading, and the kind label as prose.**
   All three are restatements of the verdict token. The colored token +
   underline swatch says "obligation, disproved" more loudly than three lines of
   text; kind survives only as a 1-word grey tag in full. *Removes ~3 lines from
   every obligation view.*
2. **The verdict-key legend in the body.** It's a glossary that duplicates the
   token vocabulary; it belongs behind a `?` on the token, shown on demand, not
   stapled under every failing obligation. *Removes a 4-line block.*
3. **The multi-sentence approximation caveat.** Compressed to a pinned
   `approx · omits branch conditions` tag with the full, verbatim caveat one click
   away behind `▸ why approx`. Honesty is fully preserved (the qualifier is always
   visible; the guarantee-level wording is never implied), while the state-at-cursor
   view drops from ~4 prose lines to one tag. *Removes ~3 lines whenever off an
   obligation.*

All three cuts remove *restatement*, not *information* — every fact and every
honesty qualifier is either still on screen or one disclosure away, satisfying the
"compact may hide, never mislead" constraint. The whole design remains a pure
function of `(vcs, cursor)`: tokens, swatches, chips, and anchors are all derivable
from the existing obligation dump and the cursor, so `pane_model.js` stays the
single source and the terminal tool mirrors it unchanged.
