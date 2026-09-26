# Language problems

## Logical definitions of access-preserving observers

Generated definition lemmas retain the already-checked body, so an observer
can return mutable elements at ordinary access. Explicit source predicates
still check calls under an immutable lock. Relational laws that call such
observers therefore need sufficient mode-crossing information. The sparse-array
proof functor uses an abstract `immutable_data` element type, which keeps this
information available under principal typing while the runtime observer remains
polymorphic over mutable elements too.

## Polymorphic constants in total functions

Inside a total function, a polymorphic constant such as `M.empty` is available
only at immutable access. It therefore cannot be instantiated where an
ordinary-access map is required. A nullary total constructor such as
`M.Refined.empty ()` instantiates it at the required access. This is an API
ergonomics limitation.

## Earlier arguments of curried total functions

An argument's relative totality does not survive across a later function
binder. If a total function retains an earlier argument after accepting a
later argument, annotate the earlier argument `@ total` or reorder the
arguments. This follows argument-relative modes but is easy to miss.

## Refinement introduction

Refinement introduction is implicit when the expected type is known. Nested
refinements still need expected types at their construction sites: an
unannotated tuple binding can infer plain component types that a later tuple
annotation cannot strengthen. Function adaptation requires a stable local
function; see [proof ergonomics](design-docs/vox-proof-ergonomics.md).
