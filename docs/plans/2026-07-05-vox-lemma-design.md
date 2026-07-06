# `[@@vox.lemma]`: lemmas as functions

A recursive OCaml function whose refined result is a PROPOSITION over its
parameters is a proof by induction: the recursive call's refined result is
the induction hypothesis. `[@@vox.lemma]` EXPORTS that proposition as an
ambient grind fact — `forall (params), (parameter-contracts) -> Q` — so
downstream VCs (and the same unit's later code) discharge it with NO
explicit call at the use site.

The statement is built from the function's dependent-arrow type: each
parameter becomes a universally-bound binder at its skeleton sort
(int/bool/simple-variant), each parameter's contract refinement becomes a
hypothesis, and the result refinement `Q` is the goal (it must be a
proposition over the parameters — `unit{ ... }` — not a constraint on the
return value). A `grind_pattern` on the first outermost spec-function
application of `Q` makes the fact fire by E-matching.

## Soundness is the solver's

Ordinary vox recursion is only PARTIALLY correct: a `unit{ false }`
self-call verifies its own body (the recursive call's contract is the
induction hypothesis, and divergence makes the postcondition vacuous at
every terminating use). So a naive universal export would register a false
fact. The export is therefore RE-PROVED in Lean, never asserted as an
`axiom`; a false or non-terminating "lemma" has no Lean proof and fails
CLOSED at the solver.

## v1 (tactic re-proof) — now the FALLBACK

v1 emitted `theorem <name> (params) (hyps) : Q := by first | grind |
(induction p <;> grind)... | (fun_induction f p <;> grind)...` and a
`grind_pattern`. Structural `induction` is well-founded and exhaustive
over constructors (catching partial-match false lemmas); `fun_induction f`
borrows the reflected function `f`'s Lean-checked termination.

The problem the user flagged: a blind `induction` gives the WRONG
induction hypothesis for a lemma whose recursion carries an ACCUMULATOR
(`induction` on the recursion variable fixes the accumulator), and there
may be no reflected function with the lemma's recursion for `fun_induction`
to borrow. Such a lemma VERIFIES at the vox body level yet v1 cannot
export it — "a fake feature."

## v2 (proof-carrying recursive definition) — the primary path

v2 translates the OCaml lemma body into a genuine Lean recursive proof
term that MIRRORS it, so a lemma whose body already verified never fails to
export (given Lean accepts termination):

- each `match` on a variable -> a Lean `match` (dependent motive
  specialises the goal per arm);
- each `if` -> a dependent `if _h : c then ... else ...` (the path fact is
  in scope for `grind`);
- each value `let x = e in ...` -> the substitution `x := e` inlined into
  later call arguments (so a constructor accumulator like `Cons (h, acc)`
  reaches the recursive call directly);
- each recursive / other-lemma call `f a1 ... an` -> `have _ih := f a1
  ... an (by grind) ... (by grind)` at the EXACT argument instantiation the
  body used, with one `(by grind)` per precondition of `f` (the same VC vox
  already proved for the body). This is the crucial difference from
  `induction`: the IH arrives where the body used it — accumulators,
  non-first-argument recursion, and int accumulators all work.
- each arm's residual -> `by grind`, with the arm's constructor/path facts
  and the emitted IHs in scope (the same hypotheses the body VC had).
- termination: structural recursion needs nothing (Lean infers it);
  int-indexed recursion emits `termination_by (metric).toNat` /
  `decreasing_by` from `[@@vox.decreases e]`, exactly as a `total_`
  definition does.

Completeness: if every body VC verified and Lean accepts termination, the
emitted def elaborates — the per-arm `grind` goals are the same goals as the
body VCs, so there is no new failure surface.

### Routing to the fallback

The choice is STATIC (shape-based), decided in OCaml, and observable under
`-dump-vc` (`vox: [@vox.lemma] <name> exported via structural | fallback
translation`). v2 raises `Lemma_v2_unsupported` — routing that lemma to the
v1 tactic re-proof — for shapes it does not cover, notably:

- a call to a non-lemma function, a non-variable match scrutinee, a `when`
  guard, a labelled/optional parameter, or a `function`-cases body;
- an `if`-controlled (int-indexed) recursion WITHOUT `[@@vox.decreases]`:
  Lean cannot show termination structurally, but v1's `fun_induction`
  can, so it is handled there (this is why `lemma_dbl` without a metric
  still exports).

## Scope / limitations (v0)

- Parameters sort at int, bool, or a simple variant.
- Result is a proposition over the parameters (`unit{ P }`).
- Same-unit only (no `.cmi` export of lemma facts yet).
- Single `grind_pattern` trigger (first outermost spec-fn application of
  `Q`).

## Tests

- `mechanics/lean_lemma.ml` — structural + int-indexed lemmas (v2 + a
  fallback dbl).
- `mechanics/lean_lemma_accum.ml` — the two accumulator lemmas that FAIL
  v1 and pass v2 (datatype accumulator `Cons (h, acc)`; int accumulator
  `acc + 1`; both recurse on the second argument).
- `mechanics/lemma_path.ml` — pins the export path (structural vs fallback)
  under `-dump-vc -vox-dry-run`.
- `mechanics/lean_lemma_false_fail.ml` (`unit{ 1 = 2 }` self-call),
  `lean_lemma_partial_fail.ml` (partial-match false claim),
  `lean_lemma_baddecr_fail.ml` (`[@@vox.decreases 0]` that does not
  decrease) — all rejected, fail-closed at the solver.
