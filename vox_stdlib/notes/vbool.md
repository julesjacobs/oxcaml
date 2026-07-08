# Vbool — language/compiler needs (blueprint §5 format)

Module: boolean ops + a bool-as-DATA demonstration. **The first stdlib module
authored against Variant V** (bool VALUES sort at Lean `Bool`; condition/
predicate positions stay `Prop`; a hybrid bridge reconciles them). Vbool is a V
ACCEPTANCE exercise; the findings below are the first-consumer report.

**V-acceptance verdict.** Bool-as-data DELIVERED for the value/parameter case
(`to_int` fully works, in-module and cross-unit) and for wart-(b) match positive
facts (`bnot` via a bool `match` verifies). The wart-(a) *model side* is fixed —
a def can case on a bool field (`if c.live`) without the old "Dependent
elimination failed" — but its LAW/BLOCK surface is unavailable in the context
where stdlib modules actually live (a VoxSig-importing unit). See F-V3.

Op inventory: `bnot` (via match, wart-b), `band`/`bor`/`bxor`/`bequal` (native
boolish, inline specs), `to_int` (bool-as-data via `vb_toint : Bool -> Int`),
`of_int` (native nonzero test). Laws: `vb_toint_true`/`_false`/`_cases`.

### Vbool · F-V1 — bool-as-DATA param to a `Bool` model fn WORKS, but needs BOTH true/false laws + the V bridge
- **site:** Vbool.mli (`vb_toint`, `to_int`); probes vA/vA2 (scratch)
- **milestone/gap:** Variant V acceptance (wart-(a) value side)
- **what I tried:** `to_int (b : bool) : int{ _ = vb_toint b }` with body
  `if b then 1 else 0`, `vb_toint (b : Bool) : Int := if b then 1 else 0`.
- **error:** (first attempt) with only `vb_toint_true`, the else branch fails:
  goal `0 = vb_toint b`, hyp `not b` (the condition path threads `b` as a **Prop**
  atom), and grind cannot connect `not b` (Prop) to `vb_toint b` (which cases on
  the **Bool** value). This is exactly the hybrid Bool↔Prop bridge the bool study
  predicted would be needed.
- **workaround used:** ship BOTH `vb_toint_true (b = true -> = 1)` and
  `vb_toint_false (b = false -> = 0)`. grind then case-analyses the Bool value and
  the V bridge reconciles the Prop condition; BOTH an `if`- and a `match`-bodied
  caller verify. So the bridge is real and landed — it just requires the two-sided
  characterization (a one-sided law leaves the other branch stuck).
- **removed by:** n/a — this is the intended V shape. Recorded so downstream
  authors ship the true/false PAIR for any bool-cased model fn.
- **severity:** COSMETIC (guidance; the mechanism works).

### Vbool · F-V2 — wart-(b) positive match arm WORKS
- **site:** Vbool.ml (`bnot` implemented as `match b with true -> false | false -> true`)
- **milestone/gap:** Variant V wart-(b)
- **what I tried:** implement `bnot : bool{ _ = not b }` by a bool-scrutinee match.
- **error:** none. The `true` arm now refines to `b = true` (the positive fact the
  pre-V Prop scrutinee did not mint), so both arms discharge. `is_true` (`match b
  with true -> true | false -> false : bool{ _ = b }`) verifies likewise.
- **workaround used:** none needed — wart-(b) is fixed. bnot ships via match as the
  in-module exercise.
- **removed by:** n/a — wart-(b) works as designed.
- **severity:** COSMETIC (positive evidence).

### Vbool · F-V3 — wart-(a) bool FIELD: model DEF fixed, but its block/law surface is unavailable under a VoxSig import
- **site:** clients/smoke_Vbool.ml (record `cell = { live : bool; v : int }`);
  probes vB0/vB1/vBfull/vLit/vVar (scratch)
- **milestone/gap:** Variant V wart-(a) (the headline "bool fields are case-able data")
- **what I tried:** a record with a bool field, a model `vc_score c := if c.live
  then c.v else 0` that CASES on the field, ambient `@[grind]` laws over the field,
  and abstract- and literal-cell clients. Also a variant `Box of bool * int` cased
  by pattern.
- **error:** (precise, several layers)
  1. In a STANDALONE `.ml` (no VoxSig import) the model def `if c.live` ELABORATES
     — the pre-V "Dependent elimination failed" is gone. So V genuinely made the
     bool field case-able (wart-(a) model side FIXED).
  2. But in a `[%%vox.lean]` block inside a VoxSig-IMPORTING unit (a real stdlib
     `.mli`, OR a client that `open`s a module and imports its VoxSig), the field
     projection `c.live` is rejected: `invalidField: Field projection operates on
     types of the form C ... where C is a constant`. So the same def that
     elaborates standalone FAILS to elaborate where stdlib modules live.
  3. An ambient `@[grind]` law whose hypothesis projects the field (`c.live =
     true`) is rejected by grind's pattern indexer (same invalidField) even in a
     non-importing unit; a plain (non-`@[grind]`) theorem projecting the field is
     fine standalone but hits (2) under an import.
  4. Constructor-literal-keyed laws (`vc_score { live := true, v := v } = v`)
     REGISTER, but only fire on literal-constructor applications, never on an
     abstract `vc_score c`.
  5. A VARIANT carrying a bool arg cannot be cased by the equation compiler:
     `def f | .Box true v => ...` fails (`Invalid pattern: Expected a
     constructor`), even standalone — bool-in-constructor-pattern is not matchable.
- **workaround used:** the WORKING wart-(a) client shape is plain-OCaml literal
  construction: `let c : cell = { live = true; v } in if c.live then c.v else 0`
  verifies `int{ _ = v }` under an import (construction supplies the field facts;
  the branch on the Bool field is exact). smoke_Vbool.ml ships this. The model
  block/law surface over the field is NOT used (it can't be, under an import).
- **removed by:** (a) allowing record-field projection inside a solver block in a
  VoxSig-importing context (the emitted structure has real projections — the
  block env just can't name them); (b) a grind pattern indexer that accepts a
  field projection in a hypothesis; (c) bool-literal patterns inside constructor
  patterns in the equation compiler. Any of these would let a bool-field
  datatype's model be consumed the way its int-field cousins are.
- **severity:** MAJOR-ERGONOMIC (V fixed the model side, but the fix is not
  reachable from the stdlib-module/client position — bool-as-data is usable as a
  VALUE/param (F-V1) and as a match scrutinee (F-V2), NOT as a datatype field
  whose laws you want to ship).

### Vbool · native boolean algebra needs no block
- **site:** Vbool.mli (`bnot`/`band`/`bor`/`bxor`/`bequal`, `of_int`)
- **milestone/gap:** none (positive)
- **what I tried:** `band a b : bool{ _ = (a && b) }` etc. over bool binders.
- **error:** none — `&&`/`||`/`not`/`<>`/`=` over bool binders are refinement-native
  boolish connectives, so each op ships an EXACT spec with no Lean def and no law
  (like Vint's succ/pred/even/odd). Nothing to keep live, nothing dead.
- **workaround used:** none — this is the intended free-algebra shape.
- **removed by:** n/a.
- **severity:** COSMETIC (informational — the algebra is free).
