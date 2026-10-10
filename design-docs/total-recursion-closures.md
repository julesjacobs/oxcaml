# Total recursion through closures

## Finding

Vox already admits recursive calls in inline callback arguments, for both
structural recursion and explicit numerical decreases. `Recursive_function`
previously rejected the same calls inside a let-bound or returned function.
That syntactic distinction blocked the unifier's recursive model-transport
proofs, whose universal witnesses are total functions.

The existing descent checkers already traverse ordinary closures:

- `Structural_recursion.check_parameter` carries identifier-indexed descendant
  facts into the body. A captured proper descendant stays smaller; a fresh
  parameter, including one with the same source name, gains no such fact.
- `Vox_vc.expression` checks a function body under its captured symbolic state
  and fresh parameter values, with their pattern/refinement assumptions. The
  termination call hook compares every direct recursive call's measure with
  the enclosing invocation's entry measure. It returns the captured state to
  the surrounding expression, so facts learned inside the closure do not
  become facts at its construction site.

## Change and justification

Traverse ordinary function bodies uniformly in `Recursive_function.check_uses`.
Remove the special case for inline callback arguments. Keep checks for direct,
fully applied recursive calls and absence of self-reference in type predicates.
Keep delayed-body rejection for lazy expressions, quotations, binding operators,
functors and class bodies; their support requires separate reasoning.

The termination argument is well-founded induction on the enclosing invocation.
Inside a closure, every use of the recursive function must still be at a smaller
argument. The induction hypothesis supplies the total recursive function at
those smaller arguments. The existing mode checker checks the rest of the
closure, including any higher-order calls. A closure can be returned or invoked
more than once: each invocation has the same descent obligations. An arbitrary
new callback argument is not smaller merely because the closure is total.

For structural recursion the order comes from checked inductive declarations.
For measures it is the existing numerical order: strictly decreasing signed
machine integers, or nonnegative decreasing `Bigint` values. No new trusted
primitive, SMT axiom, or measure rule is introduced. This is a justification of
the extension relative to the existing datatype, totality and verification
rules, not a mechanized soundness proof of Vox. The negative-datatype
elimination boundary is covered by `negative_totality.ml`.

Checking is conservative at closure construction. For example, a callback that
unconditionally recurses on `n - 1` and is only called later under `n > 0` remains
rejected: the later guard is unavailable inside that callback. Move the guard
inside the callback or construct it under the guard. This also prevents machine
integer wraparound from masquerading as descent.

## Regression coverage

`closure_termination.ml` checks the erased unifier-shaped proof callback,
repeated callback invocation, returned structural and numerical callbacks, and
refined callback parameters. It rejects unchanged structural arguments, fresh
callback arguments without a descent relation, unchanged/arbitrary numerical
measures, escaped recursive identifiers and partial applications. Existing
`scoped_termination.ml` and `numerical.ml` retain the rejected unguarded numerical
examples with descent diagnostics, and retain unsupported delayed-body checks.

The interactive top-level recursion discrepancy is separate: `pat_modes` uses
legacy modes when `force_toplevel` is set, bypassing the provisional recursive
mode variable. Changing that policy affects other mode axes and is outside this
closure change. Module-scoped definitions are the current workaround.

## Downstream use

The mutable unifier can construct an intermediate model callback by invoking its
recursive transport proof on a child execution witness. It no longer needs an
independent edit traversal just to work around delayed-body rejection. The
exact model contract and erased evidence remain unchanged.
