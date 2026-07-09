# vox bool representation: Prop vs Bool vs hybrid — a probe-grounded design study

*Design study, 2026-07-07. Base: origin/vox @ 04f02386d (branch
vox-boolstudy in the vox-kindsfix clone; everything landed, incl. the
deep-pattern + ground-negation machinery). Probes in `scratch_bool/`.
Compiler `_install/bin/ocamlc.opt -vox-solver-path <lean4-4.31.0>`.*

## The question

Should vox reflect OCaml `bool` to Lean `Bool` instead of `Prop`? The
user's worry: `if x < 2 then …` would put `(x < 2) = true` in goals. The
working hypothesis to attack: a **hybrid** — comparisons/boolean ops in
condition/predicate position keep the direct `Prop` translation (zero
noise, the reflect fast path never materializes a `Bool`); `bool` as a
**stored value** (constructor/record fields, tuple slots, pattern
scrutinees, refined bools) sorts at Lean `Bool` with `decide`-style
bridges (`b = true`, `b = true ↔ P`) at the boundary.

## TL;DR verdict

**The hybrid is the right design, and it is a genuine improvement, not a
wash.** Status-quo Prop is what makes conditions clean (probe `base_cond`:
`if x = 5` threads the bare fact `x = 5`, no `= true`), and that fast path
must be kept — an all-Bool design taxes *every* comparison and condition
with `= true` and forces the whole ↔-based equality machinery to become
`decide`-normalization. But Prop is exactly wrong for a `bool` that is
*stored* or *matched*, which is where #7's two warts live:

- **Wart (a) [confirmed, probe `wart_a_field`]:** a bool constructor/record
  field emits as Lean `Prop` inside the inductive/structure. The model
  cannot case on it — `def score | { live := true } => …` fails with
  *"Dependent elimination failed"* (Prop is proof-irrelevant, no
  `DecidableEq`, no `true`/`false` to match). This is why vmap shipped
  `MOpt` instead of bool pairs.
- **Wart (b) [confirmed, probe `wart_b_pos`]:** a bool-literal match arm
  does not refine on the POSITIVE side. `match b with true -> … | false
  -> …` at result `bool{ _ = b }`: the `true`-arm gets goal `true = b`
  with **no hypotheses** (DISPROVED, counterexample `b = false`), while
  the `false`-arm *does* get `not (b = true)` from the landed
  ground-negation machinery. The `if` form (`base_if_refbool`,
  `wart_b_pos_if`) works — it threads `b` / `not b`. So on 04f02386d wart
  (b) is a HALF-fix: negatives flow, the positive literal equation does
  not.

Both warts are precisely the "stored value / scrutinee" positions the
hybrid moves to `Bool`. The condition/predicate fast path — the only
place Prop is *better* — the hybrid keeps. So the hybrid dominates
status-quo on expressiveness at no cost to the clean-condition property,
and dominates all-Bool by not taxing conditions.

## 1. Inventory: every S_bool touchpoint (base 04f02386d, typing/vox_verify.ml)

| # | Site | Role | Bool→ |
|---|---|---|---|
| 1 | `lean_sort` (4526) | universal sort→Lean type name | `Prop` |
| 2 | `lean_datatype_decl` (4574; fields at 4608/4620 via `lean_sort`) | inductive ctor arg / structure field | `Prop` — **wart (a)** |
| 3 | `boolish` (4640) | decides when `=`/`<>` emits as `↔` vs `=` | true for `Pbool`, comparisons, `Pand/Por/Pnot/Pimp`, `Pvar`/`Pglobal` at S_bool, bool `Pfield`, `Pis`, `Pquant`, bool tuple component |
| 4 | `lean_of_pred` Eq/Neq (4762/4765) | equality of boolish operands | `↔` |
| 5 | `lean_rsort` `Rbool` (4804) | reflected (`total_`) fn bool param/result | `Prop` |
| 6 | `total_` sig `sort` (5615) | bool param/result of a spec fn | `Prop` |
| 7 | model enumeration (6243) | counterexample search domain for bool | `[false; true]` |
| 8 | `structural_sort`/`equality_sort` (1750/1758) | runtime-check faithfulness | bool is "structural" |
| 9 | condition path (`Texp_ifthenelse`, gap-#32) | threads condition as a **Prop** fact (`b`, `not b`, `x = 5`) — the clean path | Prop atom |
| 10 | pattern path (`Tpat_construct` true/false, ground-negation 2873/2943) | sibling arms get `not (b = true)`; positive literal arm gets nothing — **wart (b)** | — |

Single most load-bearing choke point: `lean_sort S_bool = "Prop"` (1),
which every stored/field/param position routes through. `boolish` (3) is
the co-choke point that makes the *value/Prop coercion* implicit at
equality.

## 2. The three designs, probed

### Baseline: status-quo Prop (probes `base_cond`, `base_if_refbool`, `wart_a_field`, `wart_b_pos`)

Conditions are clean (see `base_cond` dump: hyps `x = 5` / `not (x = 5)`).
Refined bools work as `↔` (`base_if_refbool`: `b`, `b = true` in scope,
verifies). Warts (a) and (b) as above. **Verdict: clean where bool is a
CONTROL predicate; broken where bool is DATA.**

### All-Bool: `bool` → Lean `Bool` everywhere

Reasoned from the dumps + the `boolish`/`↔` machinery (full-suite
all-Bool prototype NOT run — marked UNPROBED-at-scale, see §3):
- Every condition fact gains `= true`: `base_cond`'s `x = 5` becomes
  `(x = 5) = true`, `base_if_refbool`'s `b` becomes `b = true`. Ground
  comparisons in goals (`_ > x`, `_ = n`) all become `(… ) = true`.
- The entire `↔` equality machinery (site 4) collapses into `Bool`
  equality + `decide`/`Bool.decide` normalization; grind must discharge
  the `decide`-bridges on every boolean goal. The 182→184-test suite's
  proof power depends on grind's `decide` normalization holding at that
  scale — **UNPROBED-at-scale**; the risk is real because many suite
  proofs are ↔-shaped today.
- **Verdict: taxes the majority position (control predicates) to fix the
  minority (stored data). Rejected.**

### Hybrid: field/scrutinee/stored → `Bool`; condition/predicate → `Prop`

Prototype (this study, surgical): `lean_field_sort` emits S_bool **fields**
at `Bool` in `lean_datatype_decl`; everything else unchanged.

- **Wart (a) model side — FIXED (probe `wart_a_field` under prototype):**
  with the field at `Bool`, `def score | { live := true } => … | { live
  := false } => …` elaborates cleanly. The "Dependent elimination failed"
  error is gone: a `Bool` field is decidable and case-able. So the
  stored-value half of the hybrid demonstrably fixes wart (a)'s model
  side.
- **Boundary cost — a bridge is REQUIRED and is localized (same probe):**
  the client `get_score = if c.live then c.v else 0` at `int{ _ = score
  c }` now fails to VERIFY (goal `c.v = score c`, hyp `c.live`, grind
  gives up). The condition path threads `c.live` as a **Prop** atom
  (fast path, unchanged), but `score c` cases on the **Bool** field
  `c.live`; nothing connects them. This is exactly the decide-bridge the
  hybrid calls for: at a bool-field projection feeding a condition/
  predicate, emit `(c.live = true) ↔ c.live_prop` (or normalize the
  condition on a Bool field to `c.live = true` directly). The prototype
  proves the bridge is (i) necessary and (ii) confined to the
  Bool→Prop boundary — conditions not involving stored bools
  (`base_cond`, `base_if_refbool`) are unaffected (both still verify
  under the prototype).

## 3. Migration cost per design

- **Status-quo Prop:** zero. Warts stay; #7 stays open (point-fixable in
  Prop-world, see §5).
- **All-Bool:** high + risky. Rewrites site 1 and deletes/rewrites the
  `boolish`/`↔` machinery (3,4); every bool-touching test expectation
  churns (`= true` everywhere); grind proof-power at suite scale
  UNPROBED. TCB/encoder surface grows (decide-normalization is now
  load-bearing on every boolean goal).
- **Hybrid:** moderate, localized. New `lean_field_sort` (fields), a
  `Bool→Prop` bridge at projection/scrutinee boundaries (`b = true ↔ P`),
  and a decide-bridge for refined stored bools. Conditions and the ↔
  machinery are untouched, so condition-heavy tests DON'T churn. Churn is
  confined to datatypes-with-bool-fields and bool-scrutinee matches (few
  today, precisely because the wart discouraged them).

## 4. #7 disposition

**FIXED-BY-DESIGN under the hybrid, for both warts — not a point-fix in
Prop-world.** The two warts are the same root cause (bool-as-stored-data
modeled as Prop) seen from two positions; the hybrid addresses the cause.

- Wart (a): fixed by `lean_field_sort` (fields at `Bool`) + the projection
  bridge. Prototype confirms the model side; the bridge is the remaining
  build.
- Wart (b): a `Bool` scrutinee `b` matched against the literal `true`/`false`
  yields the decidable equation `b = true` / `b = false`, which rides the
  ALREADY-LANDED deep-pattern + ground-negation machinery (task #38/#22)
  directly — the positive-arm equation the current Prop scrutinee cannot
  express (probe `wart_b_pos`) simply appears, because `b : Bool` has
  literal constructors to match on and `Pis`/positive facts become honest
  `= true`. This is why the team lead's "bool-as-Bool patterns would
  immediately ride the deep-pattern machinery" is right: the machinery is
  built; only the sort at the scrutinee is wrong.

A Prop-world point-fix for wart (b) is *possible* (mint the positive
`b = true`/`Pis` fact for a bool-literal arm the way the negative side
already does), and it is the correct STOPGAP if the hybrid is deferred —
but it does nothing for wart (a), which is fundamentally a "Prop field
isn't data" problem no Prop-world tweak resolves.

## 5. Recommendation (ranked)

**R1 — Adopt the hybrid (recommended).** Keep Prop as the sort of a bool
in condition/predicate position (the fast path — probe `base_cond` shows
why: clean `x = 5` facts, no `= true`). Move bool to Lean `Bool` in the
four *stored/matched* positions: (i) datatype fields — `lean_field_sort`,
prototyped, fixes wart (a) model side; (ii) tuple components; (iii) match
scrutinees — fixes wart (b) by riding the landed deep-pattern machinery;
(iv) refined stored bools (`bool{ … }` as a value, not a condition).
Boundary rules (the bridges, prototype-confirmed necessary and localized):

- *Bool→Prop (a stored bool used as a condition/predicate atom):* at the
  projection/scrutinee-into-condition site, emit `b = true` (decidable)
  and let grind/`decide` bridge to the Prop atom, i.e. the invariant
  `(b = true) ↔ P_b`. This is the only new obligation and it fires only
  when a stored bool flows into control position.
- *Prop→Bool (a condition result stored/returned as a bool value):*
  reflect via `decide`/`Bool.decide`, so `if x < 2 then …` returning a
  stored bool records `decide (x < 2)`.
- *gap-#32 condition-naming survives:* the `if b` machinery still threads
  `b` as a Prop atom when `b` is a plain bool binder in control position;
  when `b` is a STORED bool (field/scrutinee) at `Bool`, the threaded
  fact becomes `b = true` and the bridge above reconciles it. Probes
  `base_cond`/`base_if_refbool` verify unchanged under the field
  prototype, so control-position naming is not disturbed by the
  field-position change; the &&/|| `Pand`/`Por` guards compose as before
  (they are control-position, Prop).

Lean encoding sketch: `structure … where (live : Bool)`; a bool field
projection in a predicate emits `c.live` at `Bool`, with equality to a
literal as `c.live = true`; a condition on it emits `c.live = true` and
the ↔-bridge lets the Prop fast path consume it. Refined stored bool
`b : bool{ _ = P }` at a value position: `b : Bool` with obligation
`b = true ↔ P` (a `decide` bridge), versus today's `b ↔ P` at Prop.

Migration: moderate, localized (§3). Churn is confined to datatypes with
bool fields and bool-scrutinee matches — few today precisely because the
wart discouraged them, so low blast radius on the 184-test suite.
UNPROBED: full hybrid (bridge emission) not implemented; suite-scale
churn is an estimate, not a measured run.

**R2 — If the hybrid is deferred: Prop-world point-fix for wart (b)
only.** Mint the positive `b = true` equation for a bool-literal match
arm (symmetric to the landed negative `not (b = true)`). Cheap, unblocks
bool-scrutinee matches, leaves wart (a) open. A stopgap, not a
resolution.

**R3 — Reject all-Bool.** It taxes the common case (control predicates)
with `= true` and forces the entire ↔ machinery into decide-normalization
with UNPROBED suite-scale proof-power risk, to fix a minority position the
hybrid fixes without that tax.

## Probe index (`scratch_bool/`)

`base_cond.ml` (clean Prop conditions), `base_if_refbool.ml` (gap-#32 if
on refined bool), `wart_b_match.ml` (match under a refinement — passes via
negative fact), `wart_b_pos.ml` (positive-arm equation MISSING — wart b),
`wart_b_pos_if.ml` (if-form works), `wart_a_field.ml` (bool field → Prop,
model casing fails).
