# Mutable first-order unifier

This is the first executable inference stage on the Pref stack. It uses shared
`Var`, `Bool`, `Arrow` and `Link` cells. Representative lookup and occurs checking
read the actual cells. Successful variable binding writes a link. Arrow
unification processes the children in order and retains the first child's
mutations if the second child fails.

## Review surfaces

- `testsuite/tests/vox/unifier.ml`: executable operations and their local ghost
  evidence construction.
- `testsuite/tests/vox/unifier_spec.ml`: cell/type vocabulary, finite execution
  evidence, and predicates over saved heap observations.
- `testsuite/tests/vox/unifier_proofs.ml`: total ghost proofs connecting that
  evidence to ownership framing and type equations.
- `testsuite/tests/vox/unifier_demo.ml`: runtime cases with constructed ownership
  and scope witnesses.
- `testsuite/tests/vox/unifier_rejected.ml`: rejected incorrect implementations.
- `testsuite/tests/vox/unifier_vox_limits.ml`: regressions for Vox proof ergonomics.

No SMT quantifiers are used. An existential execution witness is returned in a
direct `@@ ghost` field. Universal facts are explicit total functions; for example the
scope argument has the shape:

```ocaml
(q : node Pref.t) @ immutable ->
{u : unit | not (H.mem h q) || scoped h q}
```

For one allocated node, `scoped h q` says its children are also allocated. The
function describes the saved observation `h`; the live token separately has
`Pref.own t === h`. Shared references convey identity without duplicating
ownership. Recursive operations pass aggregate ownership. A call that returns
also returns the successor token.

## Semantic contract

A valuation is a total function `rho : node Pref.t -> ty`. A model witness is a
function taking any node `x` and proving `equation h rho x`. One equation states
that a variable is unconstrained, a Boolean denotes `TBool`, an arrow denotes the
arrow of its children, or a link denotes its target.

For a successful execution from `h` to `after`, the two exported proof functions
are:

- `success_forward_at`: given a model witness for `after` and a requested node
  `x`, prove its old equation and `rho p === rho q`.
- `success_backward_at`: given a model witness for `h`, the requested operand
  equality and a node `x`, prove its equation in `after`.

The recursive proofs construct intermediate model callbacks directly. Their
recursive calls descend on child execution witnesses, using the closure support
in #139.

They preserve the supplied valuation. Thus the successor models are exactly the
old models satisfying the requested equality. These are total, checked proof
functions, not assumed predicates or axiomatized invariant constructors.

`failure_refutes` takes an old model witness and the operand equality and derives
false for any failed execution. The occurs case uses a finite search witness and
a total mathematical type-size function over `Bigint`; this does not add a
runtime traversal or arithmetic operation to the unifier.

`unified_frame` proves allocation-domain preservation and transports scope for
one requested node. `unified_edits` describes the exact sequence of variable
writes, including writes retained on failure. Physical framing does not promise
unchanged type meaning: roots containing a bound variable change meaning even
when their own cells are untouched.

## Representation and current boundary

The runtime has no substitution table, pair cache, or proof heap. Resolution,
search and execution evidence erase. Result packages use unboxed records with
direct `@@ ghost` fields, enabled by #140, so proof packaging does not introduce
boxed result wrappers.
Bytecode still uses the backend's erased placeholders. The executable operations
are partial; all specification functions and proof functions are total.

The base unifier proves safe access, exact equations and correct rejection.
The additional layer in `unifier-finite-readback.md` constructs finite unfoldings
from allocation and preserves them through unification, proving acyclicity and
finite-model existence for inputs carrying that invariant. Most-general
substitutions and principal STLC still require factorization proofs.

The initial API accepts a scoped graph. It can diverge on a cyclic input;
termination and native resource failures have no recovery contract here.

## Checks

Run `./dev test vox/unifier_demo.ml`, `./dev test vox/unifier_rejected.ml`, and
`./dev test vox/unifier_vox_limits.ml`. The demo covers shared arrows,
variable-variable and variable-structure binding, multi-link resolution,
identity, direct and nested occurs rejection, constructor clash, and a retained
binding before a later clash. The rejection tests omit a write, omit occurs
checking, omit an arrow child, and claim an unjustified failure.

See `unifier-pr-sequence.md` for subsequent stages and `unifier-vox-findings.md`
for language/compiler findings and workarounds.
