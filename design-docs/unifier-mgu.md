# Most-general unification

This layer proves an MGU contract for every successful execution of the existing
mutable unifier from a graph with finite unfolding witnesses. It constructs the
substitution in ghost code; the executable graph operations are unchanged.

## Specification

`ty` now uses `TVar of node Pref.t`. Distinct physical handles are distinct
symbolic variable identities. Canonical `readback` preserves those identities,
follows links, and interprets Boolean and arrow nodes. It has no label function
that could merge independent variables.

A substitution is a total function from handles to finite types. `substitute`
extends it homomorphically to types. Substitution is simultaneous: a variable is
replaced once, even if its image mentions the same variable. The mathematical
substitution does not allocate a runtime table.

For an input heap `h`, operands `p` and `q`, and successful successor `after`,
`with_mgu` constructs:

```
sigma x = readback of x in after       when x is allocated
sigma x = TVar x                      otherwise
```

Its explicit total continuation receives `sigma` and two total proof functions:

- `solution x` proves `equation h sigma x`, `sigma p === sigma q`,
  `sigma x === substitute sigma (sigma x)`, and identity outside `h`.
- `factor rho model x equal` proves
  `rho x === substitute rho (sigma x)`. Here `model` proves every old heap
  equation pointwise, and `equal` proves `rho p === rho q`.

Thus `sigma` solves the original graph equations and requested equality, is
idempotent, and has support contained in the original finite heap. Every other
solution factors through it. The explicit factor substitution is `rho` itself;
its values on the successor's residual variables determine the instantiation.
The equation holds for every handle, including handles outside the heap.

`instance_solution_at` proves the converse closure property: applying any
substitution to `sigma` yields another solution. The earlier `failure_refutes`
proof continues to reject every putative solution of a failed execution, now
using handle-indexed type variables too.

These are the equations for the constraints represented by the input graph,
plus equality of the two operands. This stage does not claim principal typing
for a term language or termination of the runtime decision procedure.

## Proof structure

`readback_factor` descends on a finite unfolding and shows that any model's
value at its root is an instance of its canonical readback. Its variable case
uses the physical handle directly.

`mgu_solution_at` combines canonical successor readback with the existing
forward model theorem. `mgu_factor_at` transports an arbitrary old solution to
the successor with the existing backward theorem, then applies
`readback_factor`. `mgu_support_at` uses allocation-domain preservation to prove
identity outside the original heap. Applying factorization to `sigma` itself
establishes idempotence.

`with_mgu` constructs successor unfolding witnesses using `unified_finite_at`,
then constructs the substitution and all exported proof callbacks. Its input is
the original unfolding invariant and the actual successful execution witness;
no successor model or factorization theorem is assumed.

## Review and checks

- `unifier_spec.ml` changes the variable identity type.
- `unifier_finite_spec.ml` makes readback canonical.
- `unifier_mgu_spec.ml` defines substitution and the normalization predicate.
- `unifier_mgu_proofs.ml` contains the factorization proofs and exported API.
- `unifier_finite_demo.ml` consumes the API after actual unification, instantiates
  its result with types containing their own variable identities, and invokes
  factorization on variables and shared arrow roots.
- `unifier_mgu_rejected.ml` rejects merged identities, over-specialization,
  omission of an arrow child in substitution, and factorization without the
  operand-equality premise.

The existing unifier and finite-unfolding tests continue to cover occurs
failures, constructor clashes, sharing, alias chains and retained mutations.
All specification and proof functions are checked total; their calls in the
runtime fixture erase. No SMT quantifiers, compiler changes or new trusted
primitives are introduced. Principal STLC inference is the next stage.

## Mutable levels and generic templates

`level_mgu_spec.ml` and `level_mgu_proofs.ml` expose the same contract for
`Copy_spec.node`, using `Variable`, `Boolean` and `Function` and the richer
unifier's `node_equation`. The finite forest transport handles level lowering;
the MGU proof then factors canonical descriptor readback as above. Levels and
memo fields remain runtime metadata, without a graph conversion or runtime
substitution table.

`pooled_demo.ml` consumes this API after actual unification at different levels,
then continues through generalization and repeated copying. The callback
instantiates the MGU with self-containing finite types and checks factorization
at variables and a shared arrow root. `level_mgu_rejected.ml` checks the same
four invalid claims on the richer representation. Principal HM still requires
the generalization provenance and term-inference layers.

`Optimized_mgu_proofs.with_mgu` provides the same solution, substitution
factorization, idempotence and outside-heap identity contract for
`Optimized_unifier.unify`. Its proof composes the original binding cases with
exact-model graph rewrites and finite-forest transport. The shared-graph fixture
consumes the factorization with an independently constructed substitution.
