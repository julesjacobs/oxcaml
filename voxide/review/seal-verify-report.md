# Independent verification — seal-obligation re-anchoring (commit d0d506a52c)

**Lane:** lane 2 (independent). Re-derived from binaries + source; did not rely on the impl report's measurements.

**Under review:** `d0d506a52c` on branch `seal-anchor`
  binary `worktrees/seal-anchor/_install/bin/ocamlc.opt` (built 15:40).
**Baseline:** `0fff71f089` (= the change's parent, current live compiler)
  binary `worktrees/fade-unused/_install/bin/ocamlc.opt`.

**Overall verdict: CONFIRM.** The change is location + display-subject only.
Soundness-neutrality proved empirically (byte-identical `generated_lean`,
unchanged discharge verdict, byte-identical error message) and corroborated at
the source level. New `.ml` anchoring is correct for covariant and
contravariant seals; `related_spans` unchanged; suite green (23/23) with the
sole test-artifact change being the expected `vc_dump_file_seal.reference`
promotion.

## Method

Four purpose-built repros, each compiled with **both** binaries under
`-vox-dump-vc-json` (which discharges — the seal error path fires, so exit codes
are real verify results), plus a fallback probe and the in-tree suite.

| repro | shape | files |
|---|---|---|
| A | covariant/return, **proving** (impl `_>0` ⊢ iface `_>=0`) | `lib.mli: val f:int->int{_>=0}`, `lib.ml: let f (x:int):int{_>0}=1` |
| B | covariant/return, **failing** (impl `_>=0` ⊬ iface `_>0`) | `lib.mli: val f:int->int{_>0}`, `lib.ml: let f (x:int):int{_>=0}=1` |
| C | **contravariant/argument** seal | `lib.mli: val f:int{_>0}->int`, `lib.ml: let f (x:int{_>=0}):int=1` |
| D | no-seal (no `.mli`), refinement annotations only | `main.ml` two annotated bindings |
| E | fallback probe (inferred/propagated impl refinement) | `lib.mli: val y:int{_>=0}`, `lib.ml: let x=(1:int{_>0}) let y=x` |

Exit codes matched base↔fix on every repro (A=0, B=2, C=0, D=0).

## Item 1 — SOUNDNESS-NEUTRAL (the crux): **CONFIRMED**

Field-level JSON diff (base vs fix), per VC:

- **Repro D (no-seal):** whole VC-JSON **byte-for-byte identical** (3-way: file
  sizes equal, `diff` empty). The non-seal path is untouched.
- **Repros A & B — the `annotation` (non-seal) VC in the same file:**
  **byte-identical** (every flattened field equal).
- **Seal VC (A, B, C):** the *only* differing fields are
  - subject string `_seal_value` → `value` (in `goal.text/display` and
    `facts[0].text/display`),
  - `location`, `program_point`, `provenance.source_span` (ghost `0:-1` →
    real `.ml` span),
  - `goal.source_span` (`.mli` → `.ml`).
  In every case: **`generated_lean` byte-identical**, and **`discharge.status`
  unchanged** (A proved→proved, B not-proved→not-proved, C proved→proved).
- **Repro B extra field:** `discharge.detail` differs only in the random temp
  path `…/vox2-vcXXXXXX.lean` (nondeterministic across *any* two runs); the
  `grind` counterexample body (`v_0`, `h_0`, assignment `v_0 := 0`) is
  identical. Not caused by the change.
- **Failing-seal error message (B):** byte-identical modulo that temp path —
  still raised `File "lib.ml", line 1: Error: … at module seal for value "f"
  (not-proved)` with sub-messages at the `.mli` interface site and the `.ml`
  impl binding. So the compiler diagnostic is unchanged; only IDE metadata moved.

Source corroboration: emitted Lean names free `Rbound` vars positionally
`"v_" ^ string_of_int index` (`typing/vox_lean.ml:678`), never `Ident.name`, so
renaming the subject cannot perturb the term. `record_vc`/`Vox_lean.discharge`
consume `condition` (facts+goal), whose logical content is unchanged; the new
`goal.rexp_loc = anchor`, subject `~loc`, and `~program_point` feed only
JSON/display. Consistent with the observed byte-identity.

## Item 2 — CORRECT `.ml` ANCHORING: **CONFIRMED**

The anchor is `rso_implementation_predicate_location =
refinement1.ref_pred.rexp_loc` recorded at the single `(Trefine,Trefine)` seal
site in `moregen` (`typing/ctype.ml:6591`). `refinement1` is `t1`, the
**implementation** side (moregen convention: `t1` ⊆ `t2` = impl fulfils
interface, per `includemod.ml:264-275`), independent of variance:

- Repro A (return): anchor `lib.ml 1:21-26` = the `.ml` return predicate `_ > 0`
  (non-ghost). Goal `value >= 0`, hyp `value > 0` — entailment `impl ⊢ iface`.
- Repro C (argument): anchor `lib.ml 1:16-22` = the `.ml` **arg** predicate
  `_ >= 0` (non-ghost). Confirms the anchor tracks the impl predicate even when
  variance flips (goal `value >= 0` = impl arg, hyp `value > 0` = iface arg,
  the correct contravariant direction).
- `goal.source_span` now equals the anchor (`.ml`), so an editor click lands in
  the `.ml`, not the ghost `.mli` line-0 the baseline emitted.
- `subject` = `value`, entailment-framed. Matches the intended
  `value > 0 ⊢ value >= 0` reading.

**Fallback path:** when the impl predicate loc is ghost (inferred refinement),
anchor falls back to `rso_implementation_location` = impl `val_loc`
(`includemod.ml:275`), which is always the `.ml` binding — correct-by-
construction. I could not force this branch empirically (repro E's inferred
`let y = x` did not emit a seal VC — that shape is handled by a different,
non-seal path), matching the impl report's "if reachable" caveat. The fallback
is a defensive `.ml`-side default and cannot regress to the `.mli`.

## Item 3 — `related_spans` carries both `.mli` and `.ml`: **CONFIRMED**

Byte-identical between base and fix on all seal repros:
`[{interface, lib.mli 1:0}, {implementation, lib.ml 1:4}]`. Nothing dropped;
the `.mli` interface site remains reachable.

## Item 4 — Suite: **CONFIRMED**

`make -s test-one DIR=refinement` (full final-compiler rebuild, real run):
**23 considered, 23 passed, 0 failed, 0 errors, exit 0.** `vc_dump_file_seal.ml`
and `vc_dump.ml` both pass. The commit's only test-artifact change is
`vc_dump_file_seal.reference`, now `.ml`-anchored:
`VC seal-implication at vc_dump_file_seal.ml:18:25-30 / value = 1 / |- value > 0`
(was `ml:0:-1--1 / _seal_value`). All other refinement source files are
untouched by the diff (git show: only `typing/ctype.{ml,mli}`,
`typing/vox_verify.ml`, and that one `.reference`).

## Bottom line

CONFIRM. Safe to swap the live compiler to `d0d506a52c` and publish to PR#65.
The seal VC now surfaces on the `.ml` refinement annotation with subject
`value`, framed as `impl ⊢ interface`; discharge, emitted Lean, error output,
and every non-seal VC are provably unchanged.
