# Vresult — language-needs notes

Leaf exposed-ADT module (dual of Voption: two int-carrying constructors).
Verified end to end with the real solver; zero trust items. Ships 4 reduction
laws (`vr_is_ok_ok`, `vr_not_ok_error`, `vr_get_ok_ok`, `vr_get_err_err`), all
confirmed LIVE. One SOUNDNESS finding (below) plus ergonomic friction.

### Vresult · SOUNDNESS — `@[grind, expose]` on non-recursive defs makes every law DEAD
- **site:** vox_stdlib/vresult.mli:19-33 (model defs + the 4 reduction laws)
- **milestone/gap:** new (Phase-C soundness finding; feeds backlog "lint for
  silently-dead block theorems")
- **what I tried:** the vopt_b exemplar's `@[grind, expose] public def` for the
  model vocabulary, with definitional laws (`vr_is_ok_ok`, …) as inline public
  theorems.
- **error:** no compiler error — that is the hazard. `expose` publishes the
  non-recursive def *body* across the unit boundary, so a client's grind
  unfolds `vr_is_ok (.Vok x)` (etc.) definitionally and NEVER consults a law.
  Probe: deleting ALL FOUR inline theorems from the exposed `.mli` left the
  smoke client GREEN — the laws were inert. A §6.7 "shipped law consumed"
  check that only asks "does smoke pass?" is satisfied vacuously here.
- **workaround used:** make the model defs `@[grind] public` **without**
  `expose`. The body then stays inside the unit (the inline proofs and the .ml
  op VCs still discharge, since the def is transparent WITHIN its unit), but a
  client can reason only via the exported reduction laws. LIVENESS RE-PROVEN:
  deleting any one law now makes the matching smoke goal fail (exit 2), for all
  four laws. Also added `vr_get_err_err : vr_get_err d (.Verror e) = e` — with
  the defs unexposed, `get_err_or` is otherwise unusable by a client (no law
  characterizes its result on the error branch).
- **removed by:** (a) a lint that flags an exported reduction law whose LHS is
  dischargeable by client-side unfolding of an `expose`d def (the law is dead
  by construction); and/or (b) making `expose` opt-in per-law rather than a
  blanket door that silently kills the module's algebra.
- **severity:** MAJOR — silent soundness-of-evidence hole: the module *looks*
  verified with a full algebra, but clients get their guarantees from raw
  unfolding, not the audited laws, and a reviewer eyeballing "smoke green"
  is fooled. This is the single most important note from the module.

### Vresult · constructor application in a dependent-argument position
- **site:** vox_stdlib/clients/smoke_vresult.ml:11 (and every op call in the smoke client)
- **milestone/gap:** C1 (named-call-result / non-variable injection)
- **what I tried:** `Vresult.is_ok (Vresult.Vok x)` — pass a bare constructor
  application straight to an op whose parameter is named in its result
  refinement (`is_ok : (r : t) -> bool{ _ = vr_is_ok r }`).
- **error:** `vox: the argument for a dependent parameter must be a variable or
  a pure expression the logic can name (let-bind it first)`
- **workaround used:** `let r = Vresult.Vok x in Vresult.is_ok r` — let-bind the
  constructor first, then pass the variable. The binder threads `r = .Vok x`
  correctly (this is the exposed-ADT constructor case, NOT the via-value #31
  case: the smoke client verifies against cmi+olean with sources deleted).
- **removed by:** letting vox auto-name pure argument expressions (constructor
  applications are pure and nameable) so a dependent op accepts them directly.
- **severity:** MAJOR-ERGONOMIC — bites at *every* call site of any dependent
  op, so client code cannot use the natural nested-application style.

### Vresult · model theory authored in both .mli and .ml
- **site:** vox_stdlib/vresult.mli:18-25 and vox_stdlib/vresult.ml:5-12
- **milestone/gap:** model-dup
- **what I tried:** define the model vocabulary (`vr_is_ok`, `vr_get_ok`,
  `vr_get_err`) once. The seal re-elaborates the interface block against the
  implementation, so the three defs must be restated verbatim in the .ml block.
- **error:** (none — silent duplication requirement; drift would be caught by
  the seal, but the copies are hand-maintained.)
- **workaround used:** copy the 3 `def`s into the .ml block, dropping `public`
  (3 defs × 2 blocks).
- **removed by:** a `.ml`-side `open`/`include` of the interface block's model
  theory, so model defs are authored once and inherited by the seal context.
- **severity:** MINOR — 3 short defs; mechanical, but it is real per-module tax.

### Vresult · KEY FINDING — inline public theorems pay ZERO M1 tax (may change the §4 leaf default)
- **site:** vox_stdlib/vresult.mli:27-34 (4 inline `public theorem`s); vox_stdlib/vresult.ml:1-13 (defs-only block, NO law copies)
- **milestone/gap:** M1 (client law statement typed twice) — MEASURED to be 0 here
- **what I tried:** author each client-facing law once.
- **error:** none.
- **probe evidence (A/B, both re-run green with the real solver in a private temp dir):**
  - VARIANT A (shipped): `.mli` has 3 inline `public theorem`s (`vr_is_ok_ok`,
    `vr_not_ok_error`, `vr_get_ok_ok`); `.ml` block carries ONLY the 3 model
    `def`s and NO theorem restatements. → `.mli` MLI_OK, `.ml` ML_OK, seals green.
  - VARIANT B (control): identical, but the `.ml` block ALSO restates the 3
    theorems (the vopt_b exemplar's shape). → also green.
  - Conclusion: the `.ml` theorem copies in variant B are REDUNDANT. Unlike a
    `public axiom` (an obligation the seal re-demands as a same-named `.ml`
    theorem), a `public theorem` is proved once in the interface and is NOT
    re-demanded. The seal re-elaborates the `.mli` block (theorems included)
    against the `.ml`'s restated model defs, so def drift is still caught — the
    3 model defs (model-dup) are the only things that must appear in both blocks.
- **A/B vs the sibling module:** Voption ships the SAME exposed-ADT / definitional-law
  shape in OBLIGATION form (axiom-in-.mli + theorem-in-.ml), paying M1 = N laws
  typed twice + model-dup. Vresult (inline) pays M1 = 0 + model-dup. Same
  soundness, same client-visible laws; inline is strictly leaner on the M1 axis.
  Phase C therefore has a clean A/B for the §4 obligation-vs-inline default.
- **orthogonal to the expose fix:** the inline-vs-obligation (M1) axis and the
  expose-vs-not (dead-law soundness) axis are independent. The corrected module
  is inline-theorems AND non-exposed defs: M1 = 0 (this note) and laws LIVE (the
  soundness note above). The seal-doesn't-redemand-a-theorem discovery holds
  regardless of exposure.
- **workaround used:** §4 inline exception; `.ml` block is defs-only.
- **removed by:** n/a — this is *evidence to widen the §4 inline default* for
  definitional leaf ADTs (no scaffolding, one-line proofs): obligation form
  buys no extra safety there but doubles the law-typing cost.
- **severity:** MINOR (load-bearing house-rule data point, not a complaint).

### Vresult · exposed-ADT constructor must be dotted in a block law
- **site:** vox_stdlib/vresult.mli:27-34 (`.Vok x`, `.Verror e`)
- **milestone/gap:** new (documented house gotcha; load-bearing here)
- **what I tried:** the natural `vr_is_ok (Vok x)` (bare constructor) in a law.
- **error:** `Function expected at Vok ... unknown` (per the blueprint's vopt_b
  probe; same failure shape).
- **workaround used:** dot every exposed-ADT constructor in every law and its
  `grind_pattern` (`.Vok`, `.Verror`).
- **removed by:** resolving a bare constructor name against the unit's exposed
  ADT sort inside a block law (elaborator already knows the expected type).
- **severity:** MINOR — one-character fix, but a silent trap without the docs.

### Vresult · no higher-order ops (map / bind / map_err) can be modelled
- **site:** vox_stdlib/vresult.mli (ops absent by necessity)
- **milestone/gap:** N2 / new (higher-order function arguments unmodelled)
- **what I tried:** the natural Result API wants `map (f:int->int) r`,
  `bind (f:int->t) r`, `map_err`. vox does not model function-valued arguments
  in specs, so these cannot carry a refinement relating output to `f`.
- **error:** (not attempted to compile — same limitation flagged for Voption §3.)
- **workaround used:** ship only first-order ops (`is_ok`, `is_error`,
  `get_ok_or`, `get_err_or`); defer the combinators.
- **removed by:** a spec vocabulary for applying a modelled/reflected function
  symbol to a value (so `map`'s result can be `{ _ = mapspec f r }`).
- **severity:** MAJOR-ERGONOMIC — the map/bind combinators are the *point* of a
  result type in client code; their absence is the biggest gap in the surface.

### Vresult · *unknownN* placeholder in the VC (readability)
- **site:** observed in the failing-goal VC dump (`*unknown1* = vr_is_ok r`)
- **milestone/gap:** C3 (`*unknownN*` placeholders)
- **what I tried:** read a rejected goal to confirm the rejection layer.
- **error:** the result atom prints as `*unknown1*` rather than a readable name.
- **workaround used:** none needed (only affects human reading of VCs).
- **removed by:** naming the op-result atom after the value binding / return.
- **severity:** COSMETIC.

### Vresult · `not` applied to a Prop-valued def in a refinement works (positive)
- **site:** vox_stdlib/vresult.mli:37 / vox_stdlib/vresult.ml:17 (`is_error : ... bool{ _ = not (vr_is_ok r) }`)
- **milestone/gap:** new (positive data point)
- **what I tried:** state `is_error`'s contract as `not` of the Prop-valued
  `vr_is_ok` (the §3 Voption `is_none` shape, which the vopt_b probe did NOT
  exercise).
- **error:** none — compiles and seals. The bool/Prop coercion in `{ _ = P }`
  extends to `{ _ = not P }` for a Prop `P`.
- **workaround used:** none.
- **removed by:** n/a — recorded so a later builder does not needlessly avoid it.
- **severity:** COSMETIC.

## HOF surface (WP-1, 2026-07-08)

map (on Ok) / map_error (on Error) / bind / fold / to_option via the HOF kit.
Exposed ADT -> exact output available (smoke: map_ok_exact / fold_ok_exact).
to_option enumerates into a Voption, so Vresult DEPENDS ON Voption. Substrate
comes from the shared Vhof module (open Vhof); Vresult declares none of its own
(see notes/vhof.md). Spec params [@vox.total]. All verify; smoke green; negatives
fail closed.

### Vresult · imports both Vhof (substrate) and Voption (to_option target)
- **site:** vox_stdlib/vresult.mli (`open Vhof` + `open Voption`)
- **milestone/gap:** new (multi-import HOF module)
- **what I tried:** ship map/bind/fold using the substrate + a to_option bridging
  to the Voption model.
- **error:** none — `open Vhof` supplies IntRel/rHolds/r3Holds; `open Voption`
  supplies Vox_Voption_t/.Vsome/.Vnone for vr_to_opt_rel. Both VoxSig oleans are
  staged.
- **workaround used:** n/a (works). Recorded as the reference for a HOF module
  that imports the substrate AND another container.
- **removed by:** n/a.
- **severity:** none.
