# DT sat-model completeness fix (task #62, +30 Barrett structural unknowns)

Branch `task/dt-model`, based on trunk `4fbcb66657`. Own worktree worktrees/dt-model.

## Summary

The 30 QF_DT Barrett structural unknowns (census logs/dt-gap-census.md §5-L3, list
logs/dt-structural-unknowns.tsv) are NOT "the builder cannot construct a model" as the
census framed it. Instrumentation shows the DT theory DOES build a constructor-tree model,
but `Dt_model_check` correctly REJECTS it — an original assertion evaluates FALSE under the
built model. The checker is right; the bug is the model BUILDER
(`Dt.check_model`/`constructor_model_gen` in smt/theories/dt/dt.ml). Per the charter: fix =
reconstruct correctly, never touch the checker.

Root cause: the builder violated disequalities between two WITNESSED classes. All 30
degrade this way. Recursive witness dumps of the exemplars:
- v10l40001: `16 = succ(succ FREE36)` != `41 = succ FREE40` — same constructor, DIFFERENT
  wrap depths. The two free descendants got consecutive spine values (succ³ / succ⁴), so
  the succ-wrapped parents both collapsed to succ⁵(zero).
- v1l50071: `cons(leaf zero, null)` != `cons(leaf FREE33, null)` — the diseq only reduces
  to `zero != FREE33`, which was never propagated, so FREE33 was base-completed to zero.

The pre-existing completion seeded its `forbidden` set only from FULLY-CONSTRAINED
disequality sides (`constrained_value`, which returns None as soon as a field is a free
class) and propagated only through SINGLE-field constructors with a concrete value (the code
comment admits "multi-field propagation is skipped"). So a disequality between two witnessed
classes with free descendants was silently unenforced and the completion could reproduce it.

## Fix (smt/theories/dt/dt.ml, `constructor_model_gen`)

Replaced the `forbidden`/`constrained_value` machinery with the model-construction half of
the Barrett abstract decision procedure:

1. `rep` map: a representative term per class, so completion can materialize a disequal
   peer's tree by name.
2. Class-pair DISEQUALITY CLOSURE (`dis`): seeded from the asserted disequalities (over
   classes), then pushed DOWN through same-constructor witness chains — two classes with the
   same top constructor are disequal iff a field differs, and a genuine diseq guarantees ≥1
   distinct field-class pair (injectivity), so require the first distinct-class field
   position to differ unless a position is already guaranteed distinct (field pair already in
   `dis`, or two different-constructor field witnesses). Different top constructors need no
   propagation (heads already differ). Bounded fixpoint, deterministic (ascending class
   order).
3. Free-class completion consults `dis`: materialize each disequal peer's tree (memoized;
   the `Uninterp k` placeholder breaks cycles) and pick the least `distinct_base` index
   whose tree avoids them all. On a self-recursive sort this always succeeds (distinct-length
   spines); a non-self-recursive sort (e.g. mutually-recursive `tree` with no direct self
   field) yields one value, so a forced-distinct peer there degrades to a sound `unknown`.

Soundness backstop unchanged: `Dt_model_check` gates every DT `sat` (session.ml commit_sat),
so any residual builder imprecision only degrades to `unknown`, never a wrong `sat`. The
checker was NOT modified.

Diff: dt.ml only (~+80/-37 vs trunk after the blowup-guard follow-up). (session.ml and dt_model_check.ml were touched only by transient
debug instrumentation, fully reverted.)

## Diagnosis method

Env-gated `eprintf` (all removed before commit): confirmed the `Cdclt.model` Degrade sites
never fire and commit_sat is reached; the DT commit's `Dt_model_check.check` returns false;
a per-conjunct evaluator localized the false conjunct to a disequality; a per-diseq-pair
tree dump + recursive witness-chain dump pinned the exact colliding classes and their
witness structure (the data above).

## Tests (RED-verified against trunk 4fbcb66657)

Two new goldens under tests/dt-goldens-sat/ (driven by `make dt-sat-gate`, which runs each
through the real Session and asserts a CHECKED sat — Session returns Sat only after
`Dt_model_check` validates the constructor-tree model):
- `dt_witnessed_diseq_nested_sat.smt2` (the v1l50071 nested tree/list/nat shape).
- `dt_witnessed_diseq_depth_sat.smt2` (the v10l40001 deep-nat shape, reduced to 8 used
  declarations).
Both: trunk `unknown` -> fix checked-`sat`; z3 `sat`.

## Measurement

The 30 census structural unknowns (all `unknown` on trunk), 2s wall, z3 4.8.5 cross-check:
- **30/30 resolved to checked-sat.** 0 label mismatches vs `:status`, 0 disagreements vs z3.
  Matches the census upper bound exactly.

Regression + soundness sweep, 350-file Barrett sample (every 12th file, tests + typed),
fix vs trunk vs z3:
- **0 disagreements** vs z3 (179 sat + 171 unsat, all agree).
- **0 regressions** (no file that trunk solved now degrades).
- +1 bonus conversion in the sample (a trunk-`unknown` now checked-sat beyond the listed 30),
  so the corpus-wide gain is ≥30.

## Gates (by exit code)

- `make dt-sat-gate`: 0 (21 checks, incl. the 3 new goldens + the wrong-tree discrimination).
- `make check-frozen`: 0 (no frozen .mli touched).
- `make test`: 0 (harness, combine-test, euf-test, wiring-test, chrono, satpre).
- `dune build @fmt`: dt.ml clean under the real ocamlformat (a pre-existing
  smt/core/array_defs.ml drift, another lane's, left untouched).

## Reviewer steer points (team-lead)

1. TERMINATION. The class-pair disequality closure is a BOUNDED FIXPOINT, not open
   recursion: each round takes a snapshot of the current `dis` pairs and can only ADD pairs
   (never remove); there are finitely many class pairs (≤ #classes²), so the number of
   rounds that make progress is finite, and a `rounds < 100_000` fuel cap backstops any
   pathological chain (hitting it leaves the closure incomplete → some diseq unenforced →
   the checker degrades that model to `unknown`, sound). `witness_ca`/`guaranteed` are
   non-recursive lookups. The completion `tree_of` is memoized per class with an `Uninterp k`
   placeholder installed before recursing (breaks cycles), plus the pre-existing
   `depth > 10_000` caps in `base_tree`/`distinct_base` and the `tries < 256` cap in `pick`.
   So neither the closure nor the completion can loop.

2. FINITE-DOMAIN leaves (Bool / enum). The completion never forces an unsound assignment
   over a finite sort; it fails to `unknown` via the checker. Verified empirically
   (all match z3 or degrade soundly, none wrong-sat):
   - PLAIN Bool 3-distinct (`(distinct p q r)`, p q r : Bool) → `unsat` — the SAT/Bool layer
     refutes it directly, never reaching DT completion (fable's log-nit correction).
   - The datatype-FIELD variant (3 boxes each holding one Bool field, `(distinct a b c)` over
     `box(v Bool)`) → sound `unknown`: the free-class completion cannot produce 3 distinct
     Bool-field values, so it collides and Dt_model_check rejects → unknown, never wrong-sat.
     2 such boxes distinct → sat. Bool leaves flow through the existing `bool_completion`
     (B1), not the datatype `tree_of` path.
   - 3 enum colors distinct → sat; 4 → `unsat` (the theory refutes over-capacity enums
     before model build; `distinct_base`'s enum arm + the `pick` exhaustion + checker
     backstop the rest). The closure does not assume fresh values exist — exhaustion over a
     finite sort degrades to `unknown`, never a forced wrong value.
   Known completeness-only limitation (sound): the multi-field pick chooses the lowest-index
   distinct-class field even if it is a finite sort where an infinite-sort sibling would
   separate more easily; and a non-self-recursive sort (mutually-recursive `tree` with no
   direct self field) yields one `distinct_base` value. Both only ever degrade to `unknown`;
   neither bit the 30 files or the 350-file regression sample.

3. MULTI-FIELD CHOICE DETERMINISM. The pick is the LOWEST index with distinct field classes
   (`for i = 0 to n-1 ... if not !picked`); closure pairs are processed in ascending order
   (`List.sort compare pairs`), peer forbidden-trees in ascending class order
   (`List.sort compare peers`), and `next_idx` advances in the tag-ordered `dt_terms`
   traversal. Verified: 5 identical runs of the exemplar (byte-identical output).

## Straggler (opportunistic look — genuine wall, not converted)

The lone Barrett-typed raw-count straggler is `typed/v1/typed_v1l50016.cvc.smt2`
(logs/dt-addressable-deficit.tsv) — z3=UNSAT, oxsmt times out at 2s. It is NOT a
model-completeness case (unsat builds no model, so this fix is orthogonal). At 12s oxsmt
solves it `unsat` in 2 conflicts / 2 decisions — so it is not a search-throughput wall
either. The cost is a **41.7 MB assertion** (1M+ tester applications, 680K `children`,
298K `car`, deeply nested); ~9s wall is dominated by parse / preprocess / internalization of
that giant term. Converting it needs a ~5x frontend/setup throughput win on a 41 MB term —
outside this lane (the DT model path) and not a cheap convert. Noted and left; the
sub-corpus stays at 7,999 raw / +3 correct vs z3 as the census projects.

## Fix-before-land: base_tree / distinct_base node blowup (codex item (e))

RED-verified: `dt_binary_tree_distinct_sat.smt2` (Tree = node(Tree,Tree) | leaf, 30
pairwise-distinct free Trees) HANGS on the pre-fix code (killed at 8s, no verdict) because
`distinct_base(idx)` recursed BOTH self-sorted fields of `node` → a 2^idx-node tree. After the
fix it completes in ~78ms, `sat`, matching z3.

Fix (dt.ml, in the model-completion block): (1) `distinct_base` spines only the FIRST
self-sorted field, basing the rest — distinct-length spines are still pairwise-distinct, now
O(idx) nodes; (2) `base_tree` memoized per sort (context-free) with a cycle-breaking
placeholder; (3) a 2,000,000 total-node budget (`Too_big` → `None` → `unknown`) as a belt for
any residual pathological shape — the checker never sees a partial tree, so completeness-only.
The new golden is a permanent regression guard (dt-sat-gate: 21 checks, was 20). The 30-file
sweep re-ran clean after the fix (30/30 checked-sat, 0 disagreements).

## Probe (codex [SUSPECTED] checker gap — NOT a fix, per instructions): SOUND, presentation proceeds

Claim: `Dt_model_check` keys an underspecified WRONG-CONSTRUCTOR selector's value by syntactic
term (dt_model_check.ml:162), so `sel(a)`/`sel(b)` could get distinct values when `a=b` is
entailed-but-underived, yielding a wrong-sat if the results are asserted distinct.

Built five gadgets driving the real Session (verdict cross-checked vs z3 4.8.5 and the true
verdict by construction): entailment of `a=b` via (i) constructor injectivity (`is A` on both +
equal A-field), (ii) 2-constructor exhaustiveness (`not (is B)`), (iii) 3-constructor
elimination exhaustiveness, (iv) direct constructor-equality (`a = B e0`, `b = B e0`), each with
a WRONG-constructor selector applied to `a`,`b` and its results asserted distinct — the true
verdict is UNSAT in every case. **All five returned `unsat` (matching z3); none produced a
wrong-sat.**

Mechanism that saves it (documented for the record): oxsmt's DT theory DERIVES these equality
entailments (constructor injectivity: same constructor + merged fields → merge; exhaustiveness:
tester assertions determine the constructor) and MERGES the two terms into one e-class. Once
merged, a wrong-constructor selector on them is congruent (same class → one value), so the
checker's per-term env lookup returns the SAME value for both — no spurious distinctness. Two
terms left in DIFFERENT e-classes are, by that same completeness, NOT entailed-equal, and
`sel_C` of a `D`-value (`C≠D`) is genuinely underspecified by SMT-LIB, so distinct values there
are a legal model, not a wrong-sat. The checker independently evaluates the tester assertions
structurally, rejecting any model whose tree violates a `(_ is C)` constraint, so the
"case-exhaustiveness the theory doesn't merge" shape is also caught model-side. I could not
construct a triggering case. Residual (unchanged, pre-existing, not this commit's scope): a
genuine DT-theory INCOMPLETENESS that left two truly-entailed-equal terms unmerged AND was
invisible to structural tester/field evaluation could still bite; none of the natural
injectivity/exhaustiveness entailments do. No wrong-sat found → the beat-z3 presentation holds.
