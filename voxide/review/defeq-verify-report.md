# Definitional equations (`let[@vox.def]`) — adversarial verification (lane 2)

**Verdict: REJECT.** One critical, defeq-introduced soundness hole: the
verify-skip keys off a **user-spellable attribute** (`[@vox.def.axiom]`) with no
provenance check, so any user can inject an arbitrary *unverified* trusted axiom
and prove False (ex falso → any refinement). The rest of the mechanism held up
under attack (faithful lowering, fail-closed gates, manual-only deposit, correct
substitution); the only other divergence found (integer overflow) is
pre-existing and shared with the baseline.

- Feature commit `4453b5bc0795dc17ec118cd8d7026ed7caefaff8` (branch `defeq-build`, base `ebedc4dec1`).
- defeq binary: `worktrees/defeq-build/_install/bin/ocamlc.opt`
- baseline binary: `worktrees/refine-types-emit/_install/bin/ocamlc.opt`
- All probes run with `ocamlc.opt -c` (real batch compilation), `TMPDIR=/usr/local/home/jujacobs/tmp`.

---

## CRITICAL — user-spellable `[@vox.def.axiom]` = arbitrary trusted-axiom injection

`Vox_verify.is_def_axiom_binding` (`typing/vox_verify.ml:597`) decides whether to
**skip verifying a binding's body** by scanning `binding.vb_attributes` for the
literal string `vox.def.axiom` (`Vox_defeq.axiom_attribute`). There is **no
check that Vox_defeq generated the binding** — and OCaml attributes are ordinary
surface syntax a user can write. So the verify-skip that the design intends only
for the compiler-generated companion lemma is available to any source program.

Exploit (compiles **clean, no warning** on defeq; **rejected** on baseline):

```ocaml
let bad_axiom (x : int) = (() : unit{ 0 = 1 }) [@@vox.def.axiom]
let () = bad_axiom 0
let a = (7 : int{ _ = 99 })   (* accepted on defeq; disproved on baseline *)
let b = (0 : int{ _ = 1 })    (* accepted *)
let c = (0 : int{ _ <> 1 })   (* also accepted — inconsistent *)
```

- **Without** the attribute, the `unit{ 0 = 1 }` lemma body is verified and
  **disproved** (rejected) on both compilers — the body check normally catches
  it. The attribute is exactly what turns off that check.
- **With** the user-written attribute, defeq skips verification, registers the
  false equation `0 = 1` as a fact (via the same `check_application` path the
  generated lemma uses), and every subsequent refinement is provable. Baseline
  (which does not know the attribute) still verifies the body and rejects.

This defeats the entire trust model: soundness must not depend on users not
spelling an internal marker, and there is no opt-in flag, no warning, no
provenance gate. It is **defeq-introduced** (baseline rejects the identical
program). Per the task's bar ("REJECT if you find ANY false-axiom path"), this
is decisive.

**Fix direction:** do not gate the verify-skip on a spellable attribute. Track
generated-lemma provenance out of band (a node/marker not expressible in surface
syntax, or a per-compilation nonce), or have the expander register the equation
through an internal channel rather than an attribute that survives into
user-writable territory. Localized to `is_def_axiom_binding` +
`Vox_defeq.make_lemma_binding`.

---

## Per-item findings

### 1. Faithfulness of `rhs` lowering vs OCaml semantics — MOSTLY CONFIRMED, one pre-existing divergence

The generated axiom asserts `f x = <lowering of rhs>`; soundness needs
`lowering(rhs) = OCaml-eval(rhs)`. I audited every predicate-lowerable construct:

- **Interpreted ops** are exactly `primitive_builtin` (vox_lean.ml:260): `+ - *`,
  `= <> < <= > >=`, `&& || not`. Everything else (`mod`, `/`, `land/lor/lxor`,
  `lsl/lsr/asr`, `~-`/`succ`/`pred`) is **opaque** to the solver → can only
  produce *not-proved* (incomplete), never a false conclusion. Verified: opaque
  bodies are safe.
- **Comparisons / bool ops / equality**: faithful. Function equality is
  explicitly rejected (`vox_lean.ml:725`); `expect_sort` forces operand sorts to
  match. Structural `=` on int/bool/data matches OCaml.
- **let / if / tuples / constructors / field access / lambdas**: structural,
  faithful. Verified let-shadowing is correct — `let[@vox.def] f x = let x = x+1
  in x+x` proves `f 5 = 12` (correct) and **disproves** `f 5 = 10` (the naive
  no-shadow answer).
- **Argument substitution**: faithful and non-commutative-safe — `sub x y = x-y`
  with `sub_def 10 3` proves `sub 10 3 = 7`, disproves `= -7` (args not swapped);
  complex total args (`sub_def (5+5) 3`) substitute correctly.

**Divergence (pre-existing, NOT defeq-specific): integer overflow.** OCaml `int`
is lowered to Lean **unbounded `Int` (ℤ)** (`vox_lean.ml:106`), and `+/-/*` emit
ℤ ops. So `max_int + 1` proves `> 0` though it wraps to `min_int` (< 0) at
runtime. This holds on the **baseline** for an inline predicate
(`(4611686018427387903 + 1 : int{ _ > 0 })` accepted on both binaries; runtime
value = `-4611686018427387904`). defeq inherits it uniformly via the shared
lowering — it adds no new reachable false conclusion (the same falsehood is
already writable inline). The suite even has an accepted `integer_model.ml`,
i.e. the ℤ model is a deliberate, tested choice. Flagged as a known limitation,
not a defeq gate.

### 2. Discharge an unsound equation — CONFIRMED via the CRITICAL hole above
Via `[@vox.def.axiom]` I proved both a VC and its negation (`0=1` and `0<>1`) and
a concretely-wrong result (`7=99`). Absent that hole, I could not construct a
false discharge from the *generated* axiom except through the pre-existing
overflow model.

### 3. Fail-closed gates — CONFIRMED (for the generated path)
- `let[@vox.def] bad_div x = 100 / x` → `(/) is partial` (Part 1). ✓
- `if b then raise Not_found else 0` → `raise` partial. ✓
- `let[@vox.def] rec f x = f x` → rejected "recursive binding". ✓
- `let[@vox.def] c = 42` → rejected "requires a function binding". ✓
- `match x with ...` body → rejected "match not yet supported in refinements". ✓
- Float body (`x +. 1.0`): accepted at definition (float ops are total), but
  **fail-closed at use** — any VC touching the fact returns `solver-error`
  (float has no Lean sort); same as baseline float refinements. Not unsound. ✓
- Non-structure-level `let[@vox.def] ... in e` is silently *ineffective* (no
  expansion, no axiom) — safe, and already noted as a limitation in the report.

### 4. Manual-only deposit — CONFIRMED
Without a `let () = f_def a b` application, `f` stays opaque: `f 5 = 10` is
*not-proved*. Merely having `f_def` in scope, or referencing it unapplied
(`let _ = f_def`), or partially applying it (`sub_def 10`), deposits **nothing**.
The fact reaches the solver only through a full application via
`check_application`. No auto-unfold.

### 5. Part 1 classification — CONFIRMED, exactly 2 intended flips
`/` and `mod` now partial (rejected `@ total` and in predicates); `+ * land`
total; `raise`/`List.hd` unchanged-partial. `defeq_totality.ml` (new) and the
two `printer_source_like.ml` mod/div predicate cases (now rejected) are the only
verdict changes; the bitwise printer cases are retained.

### 6. Verdict-invariance / emit-additive — CONFIRMED for non-`[@vox.def]` code
`expand_structure` is identity on any structure with no `[@vox.def]` binding
(guarded `List.exists binding_has_def_attribute`), and Part 1 only tightens
div/mod totality. Suite deltas match (only the two Part-1 flips).

### 7. Suites (real counts, this binary)
`make test-one`: refinement **24/0**, refinement-lean **4/0** (incl.
`defeq_reflection.ml`), refinement-acceptance **14/0**. Matches the author's
numbers. Note: the happy-path suites do **not** cover the user-written-attribute
hole — that is precisely the untested gap.

---

## Bottom line
The core generated-axiom mechanism is well-built and survives faithfulness /
fail-closed / manual-only attacks. But it ships a soundness-critical verify-skip
gated on a user-spellable attribute, which is a complete ex-falso escape hatch.
**REJECT** until the verify-skip is bound to compiler provenance rather than a
surface attribute. (The integer-overflow divergence is pre-existing and out of
scope for this gate, but worth tracking.)
---

# Round 2 — provenance fix re-verification (SHA `bdc38e6a7b`)

**Verdict: CONFIRM.** The round-1 critical hole is closed and I could not
reopen it. The verify-skip is now keyed on the physical identity (`==`) of a
fresh ghost `Location.t` the expander mints and records in
`Vox_defeq.generated_lemma_locations`; the user-writable `[@vox.def.axiom]`
attribute is gone. Forgery is dead, no location-aliasing false-positive path was
found (analytically or empirically), and every round-1 CONFIRM still holds.

## The fix (diff `4453b5bc07..bdc38e6a7b`)
- `vox_defeq.ml`: removed `axiom_attribute`; added
  `generated_lemma_locations : Location.t list ref` and
  `is_generated_lemma_loc loc = List.exists (fun r -> r == loc) !...`.
  `make_lemma_binding` mints `lemma_loc = { loc with loc_ghost = true }`
  (a fresh heap object), records it, and builds the lemma `Vb.mk ~loc:lemma_loc`.
- `vox_verify.ml`: `is_def_axiom_binding b = Vox_defeq.is_generated_lemma_loc b.vb_loc`.

Identity is preserved end-to-end: `Typecore` sets `vb_loc = pvb.pvb_loc`
(typecore.ml:11866) and the later value_binding rebuild (5624-5629) passes
`vb_loc` through unchanged, so the recorded ghost object reaches `Vox_verify`
intact for the lemma, while every user binding keeps its own distinct location
object. A user cannot obtain a reference to a recorded object from source syntax,
so the channel is unforgeable.

## 1. Forgery — DEAD (was the round-1 REJECT)
All rejected now (verified normally → disproved), vs accepted-clean before:
- `let bad (x:int) = (() : unit{0=1}) [@@vox.def.axiom]; let () = bad 0; (7:int{_=99})` → **disproved**.
- `let[@vox.def.axiom] bad ...` (attribute on the let) → **disproved**.
- `[@@vox.def]`, `[@@vox.def.axiom "payload"]` variants → **disproved**.
No attribute spelling skips verification; the `0=1` fact is never deposited.

## 2. Location-aliasing (the real risk) — no false-positive path found
The dangerous direction is a *user* binding's `vb_loc` becoming physically `==`
a recorded ghost loc (→ wrongly skipped → unverified axiom admitted). Ruled out:
- Recorded objects are fresh `{loc with loc_ghost=true}` records, assigned ONLY
  to the generated `f_def` binding; no user construct references them.
- `==` is heap identity, so structural coincidence (same span/ghost flag) never
  matches — only object sharing would, and there is none.
- Recorded objects are never the shared `Location.none` sentinel (they are fresh
  records), so even synthesized `vb_loc=Location.none` user bindings don't match.

Empirical stress (all user bindings correctly VERIFIED → rejected):
- Interleaved genuine `[@vox.def]` + hand-written `unit{0=1}` / `unit{1=2}` → disproved.
- User-defined `foo_def` colliding by name with the generated lemma, refined
  `unit{0=1}` → disproved (provenance is by object, not name).
- User `sneaky (x:int) = (() : unit{ g x = 999 })` next to a real defeq `g` →
  not-proved (the user lemma is NOT trusted; the follow-on `g 5 = 999` never lands).
- Nested `module M = struct ... end` and `module F (X:sig end) = struct ... end`,
  each with a defeq + a user `unit{false}` → user ones disproved (provenance
  survives `type_structure` re-entry).

## 3. Set lifetime / scoping — sound; one non-blocking hygiene note
`generated_lemma_locations` is a process-global ref, never cleared. Not a
soundness issue: every parse allocates fresh location objects, so a lemma loc
recorded in one unit/phrase can never be `==` a binding in another unit/phrase.
The toplevel/REPL path (many phrases, one process) is exercised by the
`defeq_reflection.ml` expect suite (passes). **Non-blocking follow-up:** the ref
grows unbounded and `is_generated_lemma_loc` is an O(n) linear scan over all
lemmas ever compiled in the process — a minor leak/scaling cost for long-lived
processes (merlin, toplevel), never a correctness problem. A per-unit reset (or
storing the marker on the Typedtree node) would tidy it.

## 4. Positives — genuine lemmas still skipped correctly
- `let[@vox.def] double x = x+x` alone → `double 5 = 10` **not-proved** (opaque,
  no deposit, body-skip working — otherwise the `()`-proves-`double x=x+x` VC
  would itself error).
- After `let () = double_def 5` → `double 5 = 10` **proved**, `double 5 = 11`
  **disproved**. Deposit + faithful equation intact.

## 5. Regression — all round-1 CONFIRMs hold
- Fail-closed: `/`, `mod`, `raise`, `rec`, non-function bodies all rejected with
  the same messages.
- Part 1 unchanged (div/mod partial; `+ * land` total).
- Faithful lowering, manual-only deposit, argument substitution: untouched by the
  diff (only the attribute→provenance swap changed), and spot-re-checked.
- Integer overflow (`inc max_int > 0` proves) still present — PRE-EXISTING /
  shared with baseline, unchanged by this fix, out of scope for this gate.

## 6. Suites (real counts, `bdc38e6a7b` binary)
`make test-one` on the `bdc38e6a7b` binary:
- refinement: **24 passed / 0 failed / 0 skipped**
- refinement-lean: **4 passed / 0 failed** (incl. `defeq_reflection.ml`)
- refinement-acceptance: **14 passed / 0 failed**

## Round-2 bottom line
The provenance fix closes the round-1 ex-falso hole cleanly and I found no way to
reopen it: forgery via any attribute spelling is verified normally, and no
location-aliasing false-positive path exists (physical identity of fresh ghost
objects, unforgeable from source). Genuine lemmas still skip correctly; all
fail-closed / faithfulness / Part-1 behaviours are preserved; suites green.
The only residuals are the pre-existing integer-overflow ℤ model (shared with
baseline, out of scope) and a non-blocking unbounded-growth/O(n) hygiene note on
the global provenance list. **CONFIRM** — clears the defeq soundness gate for
merge onto the live compiler + PR#65.
