# voxide right-pane redesign — PROGRESSIVE DISCLOSURE

Design philosophy: **one uniform disclosure hierarchy.** Every section shows a
tight, honest one-line summary; everything else is one predictable expand away.
Nothing is ever dropped — the default *depth* is shallow and consistent, so the
pane never dumps. Compact and full are not two layouts; they are the same layout
at two default depths of the same collapse mechanism.

---

## 1. The layer model (the whole idea)

There is exactly ONE disclosure primitive — the collapse triangle `▸`/`▾` — and
every piece of pane content is assigned a **depth**:

| depth | name        | what lives here                                                            | compact | full |
|-------|-------------|----------------------------------------------------------------------------|---------|------|
| **0** | *summary*   | the single honest takeaway of a section (verdict+goal, roll-up, type)       | shown   | shown |
| **1** | *reasoning* | why: hypotheses, counterexample, the unproved/approximate caveat, diag list | `▸`     | shown |
| **2** | *evidence*  | machine artifacts: raw predicate, grind diagnostics, generated Lean, sig    | `▸`     | `▸`   |

The compact toggle sets the default depth: **compact = depth 0** (depth ≥1
collapsed), **full = depth 0+1** (depth 2 collapsed). That is the entire rule.
Depth-2 (Lean, grind, raw predicate, full signature) is *always* one more click,
in both views — the escape hatches stay reachable and stay out of the way.

"Summary" is mode-appropriate but always the same *principle*: the one thing you
must not miss. For an obligation that is the verdict + goal; off an obligation it
is the (approximate) facts; for buffer status it is the roll-up; for the cursor
it is the type.

---

## 2. Section order + grouping

Three sections, most-orienting first. The five generic noun-headings collapse
into three **status-bearing** summary lines (the heading *is* the summary).

1. **STATUS** — buffer roll-up (merges old Diagnostics + Verification).
   depth 0: `✓ verified · N/N` (fail-closed, see honesty). depth 1: the error
   list + full tally. depth 2: `▸ verdict key` (only when something failed).
2. **PROOF** — the cursor-driven working surface (the star; old Proof pane).
   depth 0: badge + goal (or, off an obligation, the approximate facts).
   depth 1: hypotheses / counterexample / caveats / cross-unit list.
   depth 2: `▸ raw predicate` `▸ solver detail` `▸ generated Lean`.
3. **CURSOR** — local reference (merges Type at cursor + Inferred signature).
   depth 0: type at cursor. depth 2: `▸ signature`.

Grouping rationale: STATUS answers "is my buffer OK?", PROOF answers "what's
under my caret and does it hold?", CURSOR answers "what are the types here?" —
buffer → caret-obligation → caret-types, widest to narrowest.

---

## 3. Compact vs full rule (one sentence)

**Compact shows depth 0; full shows depth 0 and 1; depth 2 is collapsed in
both.** Each collapsed group names its contents (`▸ details — 1 hyp, Lean`) so
you always know what an expand would reveal before you click.

One deliberate asymmetry, stated for honesty: in *obligation* mode the depth-0
headline is the **verdict + goal** (hypotheses are depth-1 support); off an
obligation the depth-0 headline is the **approximate facts themselves** (there is
no goal, so the facts are the headline). Both modes therefore always show their
one essential takeaway in compact.

---

## 4. Component inventory (KEEP / CUT / MERGE / MOVE)

| current element | verdict | where it lands / why |
|---|---|---|
| `## Diagnostics` heading | **CUT** | generic noun; replaced by the STATUS summary line, which carries the actual verdict |
| "No parse or type errors." | **CUT** | silence is the signal — the `✓` roll-up already implies a clean parse/type |
| error items (type/verify badge + loc + msg) | **KEEP · MOVE** | into STATUS depth-1 (list on expand / inline in full); the actionable payload |
| `## Verification` heading | **CUT** | merged into the STATUS line |
| "Verified ✓ — All refinement obligations discharged." | **MERGE · CUT prose** | becomes `✓ verified · N/N obligations`; the sentence restates the count |
| "Not run." / "No refinements to verify." | **KEEP · terser** | STATUS states `… checking` / `— no obligations` |
| `## Proof pane` heading | **CUT** | the badge + goal are self-identifying; the noun adds nothing |
| verdict-key legend (collapsed) | **KEEP · MOVE** | to STATUS depth-2 footer, still collapsed, content unchanged |
| pane-mode word "obligation" | **CUT** | the `[proved]` badge + `⊢` already say "you are on an obligation" |
| pane-mode "context" | **KEEP · reword** | `context · approximate` — the word "approximate" must survive even collapsed |
| kind label ("annotation"/"contract") | **KEEP · MERGE** | dim tag on the badge line, full-only (`[proved]  annotation`); cut in compact |
| status badge `[proved]`… | **KEEP** | depth 0 — the core verdict token |
| hypotheses (`name : pred`, faded) | **KEEP · MOVE** | depth 1 (inline full, `▸` compact); proof support, not headline |
| goal `⊢ …` | **KEEP** | depth 0 — the most important single line |
| `▸ raw predicate` | **KEEP** | depth 2, both views (low value, kept reachable) |
| status note (unproved / solver-error) | **KEEP** | depth 1 — the honesty reason (badge word carries it at depth 0) |
| counterexample heading + witness / "no witness" | **KEEP** | depth 1 — the verdict's reason; the honest no-witness note is preserved verbatim |
| `▸ solver detail` (grind) | **KEEP** | depth 2, both views |
| `▸ generated Lean` | **KEEP** | depth 2, both views (hard-constraint escape hatch) |
| "+N more obligation here" | **KEEP** | depth 0 — tells you the caret covers siblings |
| hidden-count note | **KEEP** | depth 1 — honesty (obligations without a source location) |
| state-at-cursor facts + approximate caveat | **KEEP** | facts = depth 0 (context headline); full caveat = depth 1; "approximate" also in the header |
| placeholder "Move the cursor onto a marked obligation." | **KEEP · terser** | `— cursor not on an obligation` |
| cross-unit list | **KEEP · MOVE** | PROOF depth 1 |
| `## Type at cursor` heading | **CUT** | the type line stands alone under CURSOR |
| type-at-cursor text | **KEEP** | depth 0 of CURSOR |
| `## Inferred signature` heading | **CUT** | becomes the `▸ signature` expander label |
| signature text | **KEEP · MOVE** | CURSOR depth 2 (`▸ signature`; inline in full) |

---

## 5. ASCII mockups (whole right aside)

Frame: `▸` collapsed, `▾` expanded, `·` separator, `────` section divider. The
slim top line is the app header pill + the compact toggle.

### (a) On a PROVED obligation — `abs.ml`, caret in the `if` (2 obligations)

**COMPACT**
```
┌ vox2  ✓ verified            [x] compact ┐
│ ✓ verified · 2/2                        │
│ ─────────────────────────────────────  │
│ [proved]  ⊢ x >= 0                      │
│ +1 more here                            │
│ ▸ details — 1 hyp, Lean                 │
│ ─────────────────────────────────────  │
│ int{ _ >= 0 }                           │
│ ▸ signature                             │
└─────────────────────────────────────────┘
```

**FULL**
```
┌ vox2  ✓ verified            [ ] compact ┐
│ ✓ verified · 2/2 obligations            │
│ ─────────────────────────────────────  │
│ [proved]  annotation                    │
│ ⊢ x >= 0                                │
│   h0 : x >= 0                           │
│ +1 more obligation here                 │
│ ▸ raw predicate                         │
│ ▸ generated Lean                        │
│ ─────────────────────────────────────  │
│ int{ _ >= 0 }                           │
│ ▾ signature                             │
│   val abs : int -> int{ _ >= 0 }        │
└─────────────────────────────────────────┘
```

### (b) On a DISPROVED (no-witness) obligation — `counterexample.ml`

**COMPACT**  (badge alone is honest; no witness is implied because none is shown)
```
┌ vox2  ✗ 1 disproved         [x] compact ┐
│ ✗ 1 disproved · 0 proved                │
│ ▸ 1 diagnostic · verdict key            │
│ ─────────────────────────────────────  │
│ [disproved]  ⊢ 2 = 1                    │
│ ▸ details — refutation, grind, Lean     │
│ ─────────────────────────────────────  │
│ int{ _ = 1 }                            │
│ ▸ signature                             │
└─────────────────────────────────────────┘
```

**FULL**  (the honest "no concrete witness" note is depth-1, shown inline)
```
┌ vox2  ✗ 1 disproved         [ ] compact ┐
│ ✗ 1 disproved · 0 proved                │
│ ▾ diagnostics                           │
│   [verify] L10 C13: obligation 2 = 1    │
│           was disproved                 │
│ ▸ verdict key                           │
│ ─────────────────────────────────────  │
│ [disproved]  contract                   │
│ ⊢ 2 = 1                                 │
│ refutation                              │
│   the solver refuted this goal but      │
│   produced no concrete witness.         │
│ ▸ raw predicate                         │
│ ▸ solver detail                         │
│ ▸ generated Lean                        │
│ ─────────────────────────────────────  │
│ int{ _ = 1 }                            │
│ ▸ signature                             │
└─────────────────────────────────────────┘
```
(An *unproved* obligation reads identically but with `[unproved]` and the note
"automation gave up; no counterexample found, so the goal may still hold." — the
badge word and note keep it from ever being skimmed as disproved.)

### (c) OFF any obligation — `recursion.ml`, state-at-cursor

Depth-0 headline here is the facts (there is no goal); the header always carries
`approximate`, so compact cannot imply a guarantee.

**COMPACT**
```
┌ vox2  ✓ verified            [x] compact ┐
│ ✓ verified · 1/1                        │
│ ─────────────────────────────────────  │
│ context · approximate                   │
│   n : n >= 0                            │
│ ▸ why "approximate"                     │
│ ─────────────────────────────────────  │
│ int                                     │
│ ▸ signature                             │
└─────────────────────────────────────────┘
```

**FULL**  (the full caveat is depth-1, restored inline)
```
┌ vox2  ✓ verified            [ ] compact ┐
│ ✓ verified · 1/1 obligation             │
│ ─────────────────────────────────────  │
│ context · approximate                   │
│   n : n >= 0                            │
│   Approximate: facts introduced         │
│   textually above the cursor, from      │
│   nearby obligations. Branch conditions │
│   are omitted; a binding from a nested  │
│   scope may still appear below it. A    │
│   hint, not a guarantee of what holds.  │
│ ─────────────────────────────────────  │
│ int                                     │
│ ▸ signature                             │
└─────────────────────────────────────────┘
```

### (d) File with a compile ERROR

Type error dominates and fails closed: no green, no VC content (VCs are not
trustworthy until the buffer compiles), signature unavailable.

**COMPACT**
```
┌ vox2  ✗ type error          [x] compact ┐
│ ✗ type error                            │
│ ▸ 1 error                               │
│ ─────────────────────────────────────  │
│ — verification unavailable              │
│ ─────────────────────────────────────  │
│ int                                     │
│ ▸ signature                             │
└─────────────────────────────────────────┘
```

**FULL**
```
┌ vox2  ✗ type error          [ ] compact ┐
│ ✗ type error                            │
│ ▾ diagnostics                           │
│   [type] L8 C21: This expression has    │
│          type int but was expected      │
│          type bool                      │
│ ─────────────────────────────────────  │
│ — verification unavailable until the    │
│   errors above are fixed                │
│ ─────────────────────────────────────  │
│ No inferred type at the cursor.         │
│ ▾ signature                             │
│   Unavailable until the errors are      │
│   fixed.                                │
└─────────────────────────────────────────┘
```

---

## 6. Honesty invariants (unchanged, load-bearing)

- **STATUS fails closed.** `✓ verified` appears only when every obligation is
  proved *and* the buffer compiles. Any type/parse error → `✗ type error`
  dominates and PROOF shows `verification unavailable`. Any disproved →
  `✗ N disproved`; any unproved/solver-error (none disproved) → `⚠ …`. Never
  green on a mixed or errored buffer.
- **No implied witness.** Disproved-no-witness shows the badge and, at depth 1,
  the verbatim "refuted … no concrete witness" note. Compact hides the note but
  shows no witness and no counterexample heading, so it cannot imply one exists.
- **Unproved ≠ disproved.** The badge word plus the "may still hold" note keep
  the two distinct at every depth.
- **Approximate stays approximate.** The context header always reads
  `context · approximate`, even in compact; the full caveat is depth 1.
- **Escape hatch reachable.** Generated Lean, solver detail, raw predicate are
  depth-2 disclosures present in both compact and full.
- **Pure function of (vcs, cursor).** All of the above is decided in
  pane_model.js (add a per-line `depth` to `paneBodyLines`, and roll STATUS from
  the same vcs + errors), so the terminal tool and browser stay byte-identical.

---

## 7. Top 3 clutter cuts

1. **Five generic noun-headings → three status-bearing summary lines.**
   "Diagnostics / Verification / Proof pane / Type at cursor / Inferred
   signature" are five titles that restate the obvious and cost vertical space;
   the replacement lines each carry the actual verdict (`✓ verified · 2/2`), so
   the heading does real work.
2. **Verbose prose → dense tokens.** "No parse or type errors.", "Verified ✓ —
   All refinement obligations discharged.", and the redundant "obligation" word
   next to a badge all vanish into `✓ N/N` and the badge itself; the long
   approximate caveat drops to depth 1 while the word "approximate" stays.
3. **The always-open dump → depth-gated default.** Today every section and (in
   full) every inline detail renders at once. Progressive disclosure pushes the
   heavy artifacts — grind diagnostics, generated Lean, full signature — below
   the fold by default in *both* views, so the pane presents a scannable spine
   instead of a wall.
