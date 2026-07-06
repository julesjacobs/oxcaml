# vox demo-modernization quest — notes (2026-07-06)

Refactoring vox's existing demos/library artifacts, one by one, to use the
language features that landed this week, keeping only genuine improvements.

## Outcome

One accepted refactor; every other target was already best-practice for its
role and skipped with a reason (below). The trust ledger across all lib/demo
artifacts is already **zero** trusted assumptions (no `assume_unchecked_`, no
`.ml` `axiom`s), so the modernization axis here was clarity/exhibition, not
trust reduction.

Full vox suite after the change: **167 passed / 0 failed**.

## Accepted

- **lib/cfold.ml** (commit `4fcca805e`): dropped 7 dead peephole rule lemmas
  (`pr_add_zero`..`pr_mul_two`). They carried no `@[grind]` and no
  `grind_pattern`, so they were never fed to any obligation; the module
  verifies identically without them (grind's linear arithmetic already closes
  every peephole arm). Corrected the comment that claimed they were
  load-bearing. 140 -> 129 lines, no trust change, honest story. `lean_cfold.ml`
  passes.

## Skipped (already exemplary for their role)

- **demo/lean_seal + lib/step*** — the flagship mli-axiom pattern: one sig, two
  implementations (step_incr/step_double), a fail fixture (step_bad). The
  identical client verifies against either impl. Nothing to improve.
- **lib/oset** — the deliberate *opaque-sort* full-abstraction exhibit (own
  sort `Vox_Oset_t`, model constants + laws as interface obligations the `.ml`
  seal discharges). Its axioms are NOT TCB (they're obligations proved in the
  `.ml`). Converting it to a `via` model would collapse the intended
  three-point modeling spectrum (opaque `oset` -> inductive-via `via_set` ->
  extensional-via `xset`) and duplicate `via_set`. The question the task posed
  ("could its axioms become proved public theorems via a via model?") — the
  considered answer is no: keeping it opaque is the exhibit.
- **lib/htbl, lphtbl, mhtbl** — exposed-model containers: the full model theory
  is proved as `public theorem`s in the `.mli` and rides the `.cmi` to clients;
  the `.ml`s carry no block. That is the correct split for an exposed model
  (the axiom-*obligation* split is the complementary pattern, appropriate for
  the abstract interfaces seal/oset use). Both are best-in-class for their
  abstraction level. `mhtbl` correctly reuses htbl's model with no block.
- **lib/rbt** — deliberately exhibits "the interface carries the entire
  verified model + algebra; the implementation is pure OCaml with no local
  block/prelude." The task's `[@@vox.lemma]` framing does not apply: rbt's
  lemmas are Lean-model theorems (about `balance`/`ins`/`invc`... proved by
  induction/case-split on Lean datatypes), not recursive OCaml functions, which
  is what `[@@vox.lemma]` reifies. Splitting the internal scaffolding lemmas out
  of the client-facing `.mli` into a `.ml` block would trade rbt's clean
  model/code separation for a marginally smaller client surface and is
  high-risk against a green suite — a net story loss, so not done.
- **lib/peano, lib/bignum** — canonical `via`-native (`Nat`) exhibits. The
  remaining `refine_`s are the required image-binder unpacks; the `zero`-last
  ordering and the B0/B1-instead-of-bool representation are documented
  workarounds for OPEN gaps (#31 inline-via value binding, #44 bool fields as
  Prop), not stale spellings. Nothing to modernize until those gaps close.
- **lib/triset** — the `.ml`'s re-declaration of `ISet`/`smem` alongside
  `elemset`/`sunion` + the two bridge theorems is inherent to the via-seal
  mechanism (the impl must register the model under the same solver name to
  define the abstraction function and prove the bridge). Its bridge lemmas are
  Lean-model theorems, not `[@@vox.lemma]` candidates. Well-split as-is.
- **lib/utf8** — already an exemplary `.mli`-block exhibit with an explicit,
  minimal audit surface (~11 lines) and public roundtrip/soundness theorems;
  `.ml` carries no block. Nothing to improve.
- **demo/lean_reflect_prim + reflectbits** — deliberately complementary per the
  reflect design doc: `lean_reflect_prim` reflects a genuine primitive
  (`%andint` -> `bland`) whose laws are *assumed axioms* (a faithful bitwise Int
  model is out of scope); `reflectbits` reflects a value whose laws are *proved
  theorems* to exhibit the cross-unit reflect-binding-rides-the-cmi story plus
  `total_` composition in the client. Changing reflectbits' reflected op to a
  bit op would force its laws back to axioms and break the client's `dmin`
  composition — a regression against the doc's intent.
- **Editor examples** (`tools/vox-editor/examples_src/`) — the override files
  (nth, tuples, mutable, deadcode, counterexample) are already modernized to the
  direct-spelling annotation form (explicit result `refine_`s already removed).
  The editor compiles single plain files, so the `.mli`-axiom pattern cannot
  fit; the block-based manifest examples (fib, reverse) deliberately keep their
  embedded `[%%vox.lean]` blocks so the editor's live-Lean-goal pane has
  something to show. No single-buffer mli/lemma upgrade applies.

## Compiler-gap follow-up (revealed by the cfold refactor)

A `[%%vox.lean]` block `theorem` with neither `@[grind]` nor a `grind_pattern`
and not referenced by any other proof is **silently dead**: it is checked (it
must still prove), but it never enters the solver's fact set, so it contributes
nothing to any obligation — and nothing warns about it. cfold shipped 7 such
lemmas with a comment asserting they were fed to grind. A lint (or a
`-vox`-diagnostic) that flags a block theorem which is neither attributed
(`@[grind]`/`grind_pattern`) nor cited by another block declaration would catch
this class of decorative-but-inert proof. Low priority, but it is the reason a
whole "rule table" could ride along doing nothing.
