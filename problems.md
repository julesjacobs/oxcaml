# Language problems

## Dependent bounds through record fields

For a function of the form

```ocaml
(overlay : 'a overlay) ->
{i : int | i < Iarray.length overlay.base} -> ...
```

the checker can fail to transfer the bound from `overlay.base` to a local
binding of that field. The explicit dependent binder and the runtime parameter
can receive distinct logical identities. Pass `base` as a separate argument to
the dependent function. This is a dependent-type inference limitation.

## Logical definitions of access-preserving observers

`[@def]` rejects a polymorphic observer that returns a mutable element at
ordinary access. The generated definition checks its parameters under the
immutable logical lock, while operations such as `Map.Refined.find` preserve
the element's access. State the proof using the observer's operations directly.
A future logical-definition design should represent access-preserving observers
without weakening their runtime types.

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

## Polymorphic tuple refinements

A refinement over a tuple of polymorphic results can fail during SMT
translation with two apparently identical tuple sorts. Separate
instantiations of the type variable currently produce distinct datatype
identities. Specialize the theorem or avoid the tuple result. This is an
implementation bug.

## Refinement introduction

`refine_ value` requires a known expected refinement type and `value` must be a
plain local variable. Bind compound expressions before introducing their
refinement. This is a deliberate initial limitation.
