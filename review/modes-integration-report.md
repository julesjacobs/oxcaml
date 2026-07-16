# Modes integration: total + logical predicate checking

This is the final implementation stage of the refinement plan. It replaces the stubbed
predicate-checking step with the real total + logical mode discipline, so that a refinement
predicate is checked at mode `total` with the values it mentions viewed at `logical`.

## Mechanism

A refinement type `t{ p }` carries a boolean predicate `p` that is an ordinary OCaml expression.
The predicate is elaborated inside `with_refinement_typing_frame` / `type_refinement`
(`typing/typecore.ml`). Four pieces make the discipline real:

1. **Total closure lock.** The predicate body is elaborated under a ghost total const closure lock
   (`Env.add_const_closure_lock ~ghost:true` with a comonadic const that pins totality to `Total`
   and leaves the other comonadic axes at `max`), followed by the existing region lock. This makes
   the predicate a total closure: a captured partial value — an effectful/IO value, a partial
   function, `raise`, a dereference primitive, `while`/`for` — fails the totality boundary, and the
   lock presents the captured values at `logical`. So a captured `ref` may be *mentioned* but not
   *dereferenced* (`!` rejects: "this value is logical but is expected to be physical"), which is
   the logical view of captures.

2. **Self bound at logical.** The refined value (the `_` hole) is added to the environment at a
   logical mode, then given its type-directed crossing computed from the skeleton's jkind. A self
   whose type crosses logicality (an immediate such as `int`, or an immutable type) can therefore
   still be read by a comparison, while a self of a non-crossing type (a `ref`, or a polymorphic
   type not known to cross) stays logical. The skeleton's jkind is used directly rather than the
   type's principality-sensitive crossing because refinement skeletons are not generalized at this
   point.

3. **Predicate checked at total.** The predicate expression is typed at an expected mode whose
   totality is `Total`. The closure lock does the capture enforcement; pinning the expected mode is
   additionally needed so that a predicate inside a top-level binding is fixed total at its own
   phrase rather than left as an inference variable that a later consumer can no longer constrain
   (the top-level structure-boundary default — see the anchor note below).

4. **Predicate-scoped admission of comparison primitives.** Comparison primitives
   (`%equal`, `%notequal`, `%lessthan`, `%lessequal`, `%greaterthan`, `%greaterequal`) are `partial`
   everywhere in ordinary program code — this is the deliberately-deferred total-comparisons
   decision, unchanged here. Inside a predicate they are admitted as total, because a predicate is a
   denotational context and these are exactly the pure, deterministic proposition constructors the
   Lean backend models. The admission is a single list
   (`primitive_is_refinement_comparison`, kept in sync with `Vox_lean.primitive_builtin` — same
   audited family), consulted at the primitive-mode lookup in `type_ident` only while a nesting-safe
   `refinement_predicate_context` flag is set (raised and restored by
   `with_refinement_typing_frame`). It changes primitive *classification* within predicate
   elaboration; it does not touch the mode axes, and it is not a totality snapshot — the totality
   enforcement is the closure lock's submode constraints, not a momentary boolean check. Outside a
   predicate a comparison stays partial (see the scoping-guard test).

The boolean connectives `&&`/`||`/`not` and integer arithmetic were already total, so only the six
comparisons are newly admitted, and only within predicates.

## Marker dispositions

All four modes-integration source markers are gone; the marker string greps to zero across the
whole tree (including the historical `review/v2-merge-report.md`, whose prose mentions were reworded
without changing its meaning).

### The two documented flips (each judged individually)

- **`refined_in_total_closure` (refinement-acceptance/refined_annotation_in_total.ml): REJECT →
  ACCEPT.** A refined annotation `(2 : int{ _ > 0 })` inside a closure required `total` is now
  accepted: the comparison is admitted inside the predicate, so the predicate's `>` no longer makes
  the host closure partial. The closure is written `let refined_in_total @ total = ...` rather than
  relying on the later `expects_total` consumer, because under `-principal` a top-level binding's
  totality is defaulted at the structure boundary before a later consumer can constrain it — the
  same late-inference class this campaign closed earlier. The annotation states the intended
  condition directly and passes in both normal and `-principal` checking; a comment records why.

- **`fp_impure_expr_in_pred` (refinement-acceptance/fact_pollution.ml): ACCEPT → REJECT.** The
  predicate `(read_int () : int{ _ = read_int () })` now rejects: `read_int` is a partial IO value,
  a predicate is total, and a total predicate cannot call it. This closes the last residual
  stub-era unsoundness (two `read_int ()` occurrences were previously identified as equal and the
  bogus obligation proved).

### New scoping-guard test

`refinement-acceptance/refinement_comparison_scoping.ml` pins the scope of the admission: outside a
predicate, `fun (x : int) -> x > 0` fed to a `total` consumer still rejects ("closes over the value
(>) ... which is partial"). This guards that comparison stays partial in ordinary program code.

### Stage-move class (six tests, same cause, same unlock)

Making predicates genuinely total surfaces a class of tests whose predicates call a *partial user
function or prelude wrapper* rather than an admitted primitive. In each the verdict is unchanged
(still REJECT) and the marker is unchanged; only the rejection *stage* moved, from a verification
failure ("not-proved" / "cannot yet be represented") to a totality mode error ("the value X is
partial but is expected to be total"), and the prose was rewritten to describe that. These are
`refinement-examples/abs.ml`, `fib.ml`, `max.ml`, `list_length.ml`, `sealed_module.ml` (all via
`Vox_spec.int_ge` / `int_le` / `list_length`) and `refinement-lean/identity_guards.ml` (via a user
`add`). Their `unlocks` tags already record the dependency (`total-comparisons` /
`recursive-totality` / `verification`): once that feature makes the wrapper total-annotatable, the
predicate will again flow through to verification-stage coverage. For the total-comparisons /
recursive-totality lane: these six flip back to verification-stage behaviour when the wrappers
become total; nothing else about them changes.

### Documented restriction (elaboration.ml)

`refinement/elaboration.ml` gains a documented-restriction case and keeps its coverage:
- `type 'a reentrant = 'a{ (_ : 'a) = _ }` now REJECTS ("logical but expected physical"). A
  polymorphic self is viewed logical and is not known to cross logicality, so it cannot be compared
  in its own predicate. This is sound and per-design; it is deferred, unlocked by the same
  kind-constrained-declarations feature that unlocks total comparisons (a `('a : immediate)` self
  would cross and work). A comment states the restriction.
- `type int_reentrant = int{ (_ : int) = _ }` is added and ACCEPTS, preserving the original
  parametric-refinement elaboration coverage with a concrete immediate self. The pre-existing `rich`
  case (int-self comparisons) continues to accept.

## Verification record

Built from source; every suite run via `make test-one` (normal and `-principal`), boot-compiler
clean, whole-tree marker grep zero, no snapshot-era totality mechanism present.

| suite | passed | failed |
|---|---|---|
| refinement | 12 | 0 |
| refinement-acceptance | 14 | 0 |
| refinement-lean | 2 | 0 |
| refinement-examples | 6 | 0 |
| typing-modes | 37 | 0 |
| typing-objects | 21 | 0 |
| comprehensions | 10 | 0 |
| typing-modal-kinds | 5 | 0 |
| typing-jkind-bounds | 71 | 0 |
| implicit-types | 4 | 0 |
| typing-modules | 54 | 0 |
| parsetree | 7 | 0 |

refinement-acceptance is 14 (the twelve pre-existing acceptance files plus the two anchors already
present, plus the new scoping guard). Every non-refinement suite matches its pre-merge count exactly
— the admission flag is inert outside predicate elaboration, so there are no regressions in the mode
machinery itself.

Direct probes (installed compiler, normal and `-principal`): a total closure mentioning a captured
`ref` is accepted; dereferencing it rejects ("logical but expected physical"); `read_int` in a
predicate rejects; `>` in an ordinary total closure still rejects.
