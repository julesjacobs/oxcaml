# Right-pane redesign — the "prover goal window"

Design philosophy: make the right aside read like a mature proof-assistant goal
window (Coq's goals panel, the Lean infoview). Those tools converged on a layout
their users already carry in their heads — a status of the whole proof, then the
current goal, then the context of hypotheses in scope, with messages and
signatures pushed to clearly separated subordinate zones. We adopt that fixed,
familiar skeleton and let it carry the meaning, so the pane can shed almost all of
its headings and restated labels. The result should feel calm and standard: you
glance once and know "is the file green? am I on a goal? is it proved? what is the
goal?" without reading any chrome.

This is design only. No code.

---

## 1. Component inventory (current element -> disposition)

| Current element | Disposition | Why |
|---|---|---|
| Toolbar status pill ("typechecks ✓" / "verification failed ✗") | MERGE -> file-status band | Same information as the Verification section; one authoritative whole-file verdict, at the top of the aside. |
| `Diagnostics` h2 heading | CUT | The layout (an error list) is self-evident; a conventional messages zone needs no title. |
| "No parse or type errors." | CUT | Silence is the conventional "clean" signal; the file-status band already asserts the positive state, so an empty errors zone simply vanishes. |
| Diagnostics error/verify items | KEEP | The actionable payload; promoted to fill the goal window when a compile error means there is no proof state. |
| `Verification` h2 + verdict text | MERGE -> file-status band | The whole-file verdict becomes the persistent top band (scope = file). |
| `Proof pane` h2 heading | CUT | The goal window is the star; it needs no label. |
| `mode: obligation` / `mode: (none)` line | CUT | Pure chrome. The presence of a goal + verdict says "on an obligation"; the placeholder says "off one". |
| `annotation obligation` / `contract obligation` kind line | MOVE -> tag on the status line | Provenance is minor; it rides the verdict as `proved · annotation`, saving a line. |
| Status badge `[proved]` etc. | KEEP | Becomes the goal-window status line (scope = cursor), distinct from the file band. |
| Hypotheses / context rows (with fade-unused) | KEEP | The CONTEXT zone of the goal window. Fade-unused toggle unchanged. |
| Turnstile dividing rule | KEEP | The familiar goal/context separator; shown only when context is non-empty. |
| Goal `⊢ …` | KEEP | The star. Top of the goal-window content. |
| `raw predicate` disclosure | MERGE -> "internals" disclosure group | Low value since predicates now print source-like; folded next to generated Lean, full only. |
| Status note (unproved / solver-error prose) | KEEP | Honesty. Compressed to a status-line tag in compact; full sentence in full. |
| `refutation` heading + witness / "no concrete witness" | KEEP | Honesty core. Its own zone under the goal when disproved. |
| `[solver detail]` disclosure | KEEP | Full only, collapsed. |
| `[generated Lean]` + copy/download/open + hint | KEEP | The escape hatch; full only, collapsed. Hint trimmed to one line. |
| `+N more obligation(s) here` | KEEP | Real navigational fact; kept in both views. |
| Hidden-count ("N obligations with no source location") | KEEP | Honesty (don't imply completeness). |
| Cross-unit ("Obligations in other units") | KEEP | Subordinate navigational zone. |
| State-at-cursor `known at this point` heading | KEEP (relabel) | Reuses the CONTEXT zone, labelled `context · approximate`. |
| State-at-cursor facts | KEEP | Full only. |
| State-at-cursor "Approximate: …" 3-sentence caveat | CUT the always-on paragraph -> persistent `approximate` tag + `▸ why?` disclosure | Honesty preserved (the word "approximate" is always on the zone header); the explanation is on demand. **Boldest cut — flagged for lead sign-off.** |
| Collapsed "verdict key" legend | CUT from the pane -> `?` affordance on the status band | proved/disproved/unproved/solver-error is standard vocabulary; meaning stays reachable, not stacked. |
| `type at cursor` h2 + content | KEEP content, CUT heading | Subordinate reference zone with a tiny inline `type` label; full only. |
| `Inferred signature` h2 + content | KEEP content, CUT heading | Subordinate reference zone with a tiny inline `sig` label; full only. |

---

## 2. Section order and grouping

Fixed top-to-bottom order (calm and standard beats reflowing):

1. **FILE STATUS BAND** — a thin full-width coloured strip. The whole-file
   verdict, always present. Carries a `?` that reveals the verdict-key legend.
   Scope = file.
2. **GOAL WINDOW** — the star:
   - status line: per-obligation badge + kind tag + honesty tags (`disproved · no witness`). Scope = cursor.
   - `⊢ goal`
   - `── turnstile rule ──` (only when context is non-empty)
   - CONTEXT: hypotheses (on an obligation) or `context · approximate` facts (off one)
   - REFUTATION zone (only if disproved): witness, or an explicit no-witness note
   - internals disclosures (full, collapsed): solver detail, generated Lean, raw predicate
   - navigational notes: `+N more here`, hidden-count, cross-unit
3. **ERRORS** — only when present; when a compile error blocks proof state the
   goal window collapses to a one-line placeholder and this zone holds the payload.
4. **REFERENCE** (full only) — `type` at cursor, `sig` inferred signature.

### Compact vs full rule

- **COMPACT = status + goal.** File-status band, plus the goal-window status line
  and `⊢ goal` (with honesty tags inline). Everything else is hidden: context,
  refutation detail, all internals disclosures, the reference zone, and the errors
  zone *when clean*. Two honesty exceptions that compact never hides: the file
  band's true state, and — when non-empty — the ERRORS zone (you must see why the
  band is red). Off an obligation, compact shows only the band + `no obligation at
  cursor`.
- **FULL = status + goal + context + subordinate zones.** Adds the turnstile rule,
  the context/approximate facts, the refutation zone, the internals disclosures
  (including the generated-Lean escape hatch), the navigational notes, and the
  reference zone.

Generated Lean stays reachable in every case: it is a collapsed disclosure in
full, and reachable from compact by unchecking `compact`.

---

## 3. ASCII mockups (whole right aside)

`▸` = a collapsed disclosure. `═` band = coloured status strip.

### (a) Cursor on a PROVED obligation  (abs.ml, on the `if` expression)

COMPACT
```
════════════════════════════════════════════ ?
 ✓ VERIFIED · all 2 obligations discharged
════════════════════════════════════════════
 proved · annotation
   ⊢ x ≥ 0
 +1 more here
```

FULL
```
════════════════════════════════════════════ ?
 ✓ VERIFIED · all 2 obligations discharged
════════════════════════════════════════════
 proved · annotation
   ⊢ x ≥ 0
 ────────────────────────────────────────────
   h0  x ≥ 0
 ▸ generated Lean
 ▸ raw predicate
 +1 more here
 ────────────────────────────────────────────
 type  x : int
 sig   val abs : int -> int{ _ >= 0 }
```

### (b) Cursor on a DISPROVED obligation, no witness  (counterexample.ml, on `need_one 2`)

COMPACT
```
════════════════════════════════════════════ ?
 ✗ NOT VERIFIED · 1 obligation disproved
════════════════════════════════════════════
 disproved · no witness
   ⊢ 2 = 1
```

FULL
```
════════════════════════════════════════════ ?
 ✗ NOT VERIFIED · 1 obligation disproved
════════════════════════════════════════════
 disproved · contract
   ⊢ 2 = 1
 refutation
   No concrete witness — the solver refuted
   this goal without producing a model.
 ▸ solver detail
 ▸ generated Lean
 ▸ raw predicate
 ────────────────────────────────────────────
 type  need_one : int{ _ = 1 } -> int{ _ = 1 }
 sig   val need_one : int{ _ = 1 } -> int{ _ = 1 }
```
(No context here, so no turnstile rule is drawn.)

### (c) Off any obligation — state at cursor  (recursion.ml, in `fib`'s scope)

COMPACT
```
════════════════════════════════════════════ ?
 ✓ VERIFIED · all 4 obligations discharged
════════════════════════════════════════════
 no obligation at cursor
```

FULL
```
════════════════════════════════════════════ ?
 ✓ VERIFIED · all 4 obligations discharged
════════════════════════════════════════════
 no obligation at cursor
 ────────────────────────────────────────────
 context · approximate                   ▸ why?
   n  n ≥ 0
 ────────────────────────────────────────────
 type  n : int
 sig   val fib : int{ _ >= 0 } -> int{ _ >= 0 }
```
`▸ why?` reveals the full caveat: facts introduced textually above the cursor,
branch conditions omitted, a nested binding may still appear below its scope —
treat as a hint, not a guarantee.

### (d) File with a compile ERROR

COMPACT
```
════════════════════════════════════════════ ?
 ✗ TYPE ERROR · 1
════════════════════════════════════════════
 no proof state
 errors
   type  L7 C10: This expression has type int
         but an expression was expected of
         type string
```

FULL
```
════════════════════════════════════════════ ?
 ✗ TYPE ERROR · 1
════════════════════════════════════════════
 no proof state — fix errors below
 errors
   type  L7 C10: This expression has type int
         but an expression was expected of
         type string
 ────────────────────────────────────────────
 type  (unavailable)
 sig   unavailable until the errors are fixed
```

**Other verdicts follow the same pattern**, honestly tagged on the status line and
in the note zone: `unproved · no witness — may still hold` and `solver error · no
verdict`, each with the same note zone and (in full) solver-detail / Lean
disclosures. Because the file band is scope=file and the status line is
scope=cursor, a proved obligation in a file that has an unproved one elsewhere
reads honestly: band `✗ 3 of 4 discharged`, status line `proved`.

---

## 4. Rationale + top-3 clutter cuts

**Rationale.** A proof assistant's goal window is a solved UI problem: users
already know to look top-for-status, then goal, then hypotheses. By committing to
that skeleton we can delete the scaffolding the current pane uses to explain its
own structure — five section headings, a `mode:` line, a repeated "obligation"
word, a stacked legend — and let familiar layout do the work. Honesty is not
weakened but sharpened: two verdicts with muddled scope today become one file band
(scope = file) and one status line (scope = cursor), so "am I green overall" and
"is this goal proved" can never be confused. Compact is the goal window at its
most reduced (status + goal); full is the same window with its context and
subordinate zones opened — a difference of depth, not of a different screen.

**Top 3 clutter cuts.**

1. **Kill the five headings and the mode line.** `Diagnostics`, `Verification`,
   `Proof pane`, `Type at cursor`, `Inferred signature`, plus `mode: obligation`
   and the duplicated "obligation" word, all go — replaced by one thin status band
   and the conventional goal-window layout. Roughly seven lines of chrome removed
   from every render.

2. **One verdict, not two-plus.** The whole-file `Verification` section and the
   toolbar status pill collapse into a single top band; the per-obligation badge
   becomes the goal-window status line with a clear, separate scope. One place to
   learn "is the file green," one place to learn "is this goal proved."

3. **On-demand honesty prose.** The always-on 3-sentence state-at-cursor caveat
   becomes a persistent `approximate` tag + `▸ why?` (the word "approximate" is
   never absent, so nothing is misled); the always-on "No parse or type errors."
   disappears when clean; the stacked verdict-key legend becomes a `?` on the band.
   Verbose text is available on demand instead of stacked on every render.
