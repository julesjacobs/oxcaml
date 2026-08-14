# Vox refinement flow: assumptions in, obligations out

The type-formers piece made `t{ p }` a real type and left it inert: rigid,
unconsumable, carrying no rules. This piece gives it the two rules that make
refinements mean something, and nothing else.

The rules are dual, and the duality is the whole design:

- **A refinement on a value is an assumption.** When our code *receives* a
  value it is a value: fully evaluated, sitting in a register, with a
  determinate identity. Its refinement is a known fact about that identity.
  Receiving therefore strips the refinement from the type and records a fact.
- **A refinement on a position is an obligation.** When a value *leaves* — into
  a function, into the heap, out through a return — it has to satisfy whatever
  the destination demands. Leaving therefore strips the refinement from the
  expected type and records an obligation.

In SSA terms: a refinement attaches to an SSA value, and holds for that value
from its definition onward. Positions are not SSA values; a position is a
requirement. So the same syntactic construct `t{ p }` is an assumption when it
describes something we hold and an obligation when it describes somewhere we are
putting something.

Neither rule *checks* a predicate. This piece emits no verification conditions,
calls no solver, and proves nothing. It makes the typed tree the artifact a later
pass can read facts and obligations off. That later pass is a separate piece.

## What "the type checker never checks a predicate" buys

Refinements are already rigid: `int{ p }` and `int` do not unify. Left alone,
that makes every refined program a type error — you cannot pass a refined value
anywhere, and you cannot supply anything to a refined parameter. Stripping is
what lets refined programs type-check at all, and doing it *only* at the two
points above is what keeps the type checker honest about what it has and has not
established.

The alternative — teaching unification to relate `t{ p }` and `t` by proving
`p` — would put the solver inside inference. Predicate proving would then run at
unification order, on types that are still being solved for, inside
backtracking. That is a much worse system, and it is not what we are building.

## The two hooks

### Elimination: one funnel

Every expression that reaches a known expected type goes through one place. We
add a single hook there:

```
strip_expected exp expected_ty =
  match get_desc exp.exp_type, get_desc expected_ty with
  | Trefine { ref_payload; _ }, (concrete and not Trefine) ->
      (* use is at the payload; the predicate is a fact for the later pass *)
      { exp with exp_type = instance ref_payload }
  | _, Trefine _ -> exp   (* imposition site: obligation, keep it rigid *)
  | _, (Tvar _ | Tunivar _) -> exp   (* undetermined; see the decision below *)
  | _ -> exp
```

vox2 has exactly this function (`weaken_refinement_occurrence`,
`typecore.ml:1529`, called from `unify_exp`) and its three-way guard is the
valuable part. The cases are not symmetric and each has a reason:

- **Expected type concrete and unrefined** — strip. The value's predicate is
  forgotten *at this use*, which is the sound direction: forgetting a fact
  cannot make a program wrong.
- **Expected type refined** — do not strip. This is where a predicate is being
  demanded, so the refinement must survive into the tree for the later pass to
  turn into an obligation. Weakening here would silently discharge it.
- **Expected type an unsolved variable** — do not strip. **Settled:** we strip
  only when the expectation is known. This does make the outcome depend on
  inference order, and that is accepted rather than worked around: there is no
  general fix, because the question "is this expectation refined" genuinely has
  no answer yet at that point. Deferring is the honest response to an
  undetermined expectation, and the alternatives are worse — stripping eagerly
  decides a question that has not been asked, and rejecting turns every
  not-yet-solved expectation into an error.

  The practical consequence is worth stating so nobody treats it as a bug: in
  `let y = if c then refined_thing else other`, whether the refinement reaches
  `y` depends on when the variable for `y`'s type is solved. Annotate to make it
  determinate. This is a known, bounded imprecision, not a soundness question —
  a missed strip leaves a rigid refinement that fails loudly at the next
  unification, it does not silently discharge anything.

Critically, stripping rewrites *this occurrence*, never the declared type. The
arrow keeps its refined domain, the record field keeps its refined type, the
signature keeps its refined value. vox2 states the reason well
(`typecore.ml:13096`): "The arrow retains the refined domain as the durable
contract record. The supplied expression is checked at its carrier."

That invariant is what removes the need for any side table. The later pass does
not need us to mark obligation sites, because it can re-derive every one: an
application whose function type has a refined domain owes an obligation at that
argument, a record construction against a refined field owes one there, a
function whose declared result is refined owes one at each return. **Our job is
to strip uses without ever damaging a contract.** That is a checkable invariant
and it should be stated as one.

### Introduction: at the binder

A name enters scope carrying a refined type; we install it at the payload and
record the fact. vox2 does this in `add_pattern_variables` with a
`~strip_refinement` flag (`typecore.ml:1850`) plus
`Env.add_refinement_program_values` to register the idents.

We do the same, with one difference: the flag is not optional. vox2's defaults to
`false` and only three call sites turn it on (`Pexp_let`, match cases,
comprehension bindings), which means any new binding form silently gets the wrong
behaviour. Ours strips at every binder unless the binder is in the exempt set
below, so a new binding form is right by default and a deliberate exemption is
visible.

Exempt from stripping:

- **Mutable bindings.** A mutable variable is not an SSA value; its content can
  change after the fact is recorded, so the refinement is not a fact about it.
  vox2 exempts `Val_mut _` in both its strip sites and is right to. What a
  refinement on a mutable binding *should* mean is a question for a later piece;
  here it stays rigid, which fails closed.

## The positions, enumerated

The two hooks cover these. The list is here so that a reviewer can check
coverage rather than infer it, and so that anything missing is visibly missing.

**Receiving (assumption; strip and record):**

- a function parameter, when the arrow's domain is refined
- a `let`, `match`, `function`, `try` or comprehension binder
- the result of an application, when the arrow's codomain is refined
- a field projection, when the field's declared type is refined
- a constructor argument recovered by a pattern
- a module-level value whose signature type is refined

**Releasing (obligation; strip the expectation and leave the refinement):**

- an argument at a refined parameter
- an expression at a refined field in a record construction or update
- a returned expression, when the enclosing function's result is refined
- a value at a refined type annotation or coercion
- a value at a refined field of a signature during inclusion checking

The user framing is worth keeping in the code comments: *values in registers
carry facts, values going to the heap or across a call boundary owe proofs.*

## Where a single funnel is not enough

vox2 has 294 refinement mentions in `typecore.ml` across roughly 35 distinct
sites. Most are accidental, but not all, and the ones that are not share a
shape: **they construct a type instead of checking against one.** There is no
incoming expected type to weaken, so the checker has to invent the type, and the
invention needs a rule.

The clearest case is `if`/`else` when the expected type is unknown
(`typecore.ml:10351`). vox2 types the first arm against a fresh variable, and if
it comes back refined it takes the *payload* as the join type, checks the second
arm against that, and gives the whole expression the payload:

```ocaml
| Trefine refinement ->
  let join_type = instance refinement.ref_skeleton in
  let ifso = weaken_refinement_occurrence ifso join_type in
  let ifnot = type_expect env expected_mode sifnot (mk_expected join_type) in
```

Two things about this are right and we should keep them.

**The join is the payload, never a disjunction.** vox2 never forms `p ∨ q`, and
should not. The later pass reads the typed tree and reconstructs the path
condition itself — `c` holds in one arm, `¬c` in the other — so a disjunction
computed here would be both redundant and weaker than what the verifier can
derive. Predicate algebra does not belong in the type checker.

**The first arm's predicate must not become an expectation for the second.**
Otherwise `if c then (x : int{ x > 0 }) else y` imposes `> 0` on `y` purely
because of the order the arms were written in. vox2's comment is exact: "A
neutral join does not make the first arm's predicate an expectation for the
second arm."

One thing about it is wrong and we should fix it. The refined-join path triggers
on the *first* arm only. Write the refined arm second and you fall through to the
ordinary path, where the second arm gets stripped against the first arm's
already-unrefined type. Same destination, different route, and the asymmetry is
luck rather than design. Ours computes the join from both arms: if either is
refined, the join is the common payload.

When the expected type *is* known, no special rule is needed at all — each arm
meets the expectation independently through the funnel, so a refined expectation
distributes to both arms as two obligations and an unrefined one strips both.
That is the behaviour we want and it falls out.

Other constructed-type sites to handle explicitly, for the same reason: `match`
case joins, `try`/`with` joins, and list and array literal element types. Each
gets the same rule — the join is the common payload — and each gets a test.

## What we take from vox2, and what we leave

**Take:**

- the occurrence-weakening hook and its three-way guard
- keeping the refinement on declared types as the durable contract, so no side
  table of obligation sites is needed
- payload-not-disjunction joins, and the reason
- mutable-binding exemption
- payload extraction that also expands a `Tconstr` revealing a refinement, so
  `type u = int{ p }` strips like the refinement it is. vox2 caches this
  (`refinement_alias_cache`), which tells us the lookup is hot enough to matter;
  we should expect to need the same and should measure rather than assume
- a scope check so a refinement cannot escape the scope of the program variables
  it names (`check_refinement_scope`, applied to function types). A predicate
  mentioning a parameter must not outlive the parameter
- **occurrence-side stripping for values that arrive from a signature**
  (`typecore.ml:9359`). **Settled: imports do not strip.** `Env.find_value`
  keeps the signature's refined type, and the strip happens at the `Texp_ident`
  occurrence instead. The reason is that inclusion checking needs the refined
  signature type intact, so an import-time strip would have to keep a second,
  unstripped copy anyway — two representations either way, and this way the
  environment stays faithful to what the user wrote. vox2's comment describes the
  same split: it "projects module-level refined values while leaving their
  interface descriptions intact"

**Leave:**

- 35 scattered sites. The two hooks plus an enumerated, tested list of
  constructed-type joins is the whole surface. If a third kind of site appears,
  that is a design event, not a patch
- the opt-in `~strip_refinement:false` default
- asymmetric `if`/`else`

Note that binder-side and occurrence-side stripping are *not* duplicate
mechanisms, which was my first reading of vox2 and was wrong. They cover disjoint
populations: the binder path handles names this compilation unit binds, the
occurrence path handles names that arrive already typed from a signature and
never pass through a binder. Both are needed. What we do add over vox2 is a test
that pins which population each one serves, so a later change cannot quietly
make one of them dead code.

## Scope of this piece

**In:** the two hooks; the enumerated join rules; the binder exemptions; the
invariant that declared types are never damaged; tests for all of it.

**Out:** verification condition generation, the logical context, any solver
call, and any error message about an unproved predicate. Also out: refinements on
mutable bindings, and inference of refinements (they remain never inferred).

A consequence worth stating plainly, because it looks alarming and is intended:
**after this piece, a refined program type-checks and nothing is verified.**
`let f (x : int{ x > 0 }) = x` applied to `-1` compiles. The piece is the
plumbing that a verifier reads; it is not a verifier, and pretending otherwise by
adding a partial check here would be worse than the honest gap.

## Settled decisions

**Strip only when the expectation is known.** Recorded in the funnel above,
including the accepted inference-order dependence.

**Imports do not strip.** Recorded in the take-list above: the environment keeps
the signature's refined type and the occurrence strips.

## Open decisions

**Does the arrow binder scope over obligations?** The type-formers piece put
an optional `Ident.t` on the arrow so `x:int{ x > 0 } -> int{ _ >= x }` can
mention `x` in the codomain. When we emit an obligation for the codomain, `x`
must be bound to the actual argument. That substitution is the later pass's job,
but this piece must not destroy the binder while stripping. Needs a test that a
dependent arrow survives an application with its binder intact.

## Tests

All under `testsuite/tests/vox/`, following the other pieces.

- `refinement_flow.ml` — expect test for the two hooks: a refined value used at
  an unrefined expectation strips; at a refined expectation stays; a refined
  parameter binds at the payload; a refined result is received at the payload.
- `refinement_flow_joins.ml` — `if`/`else` both ways round, `match`, `try`, list
  and array literals; the join is the payload in every case and the arms are
  symmetric.
- `refinement_flow_contracts.ml` — the invariant that declared types are
  undamaged: after an application, the function's arrow still prints its refined
  domain; after a projection, the field type is still refined; a `.cmi` round
  trip preserves both.

The contract test is the important one. Everything the later pass can do depends
on the refinements still being there to read, so a regression that over-strips
would be invisible in the other two files and fatal to the next piece.

Where this piece changes behaviour that the type-formers piece pinned — the
located errors for applying a dependent arrow, which this piece replaces with
real rules — the change follows the red-green convention: the RED commit records
the current rejection, the GREEN commit shows exactly which programs start
type-checking.
