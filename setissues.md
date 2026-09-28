# Integer-set demo issues

- A duplicate-free list is not a canonical set representation: two lists can
  contain the same elements in different orders. The demo therefore refines a
  strictly increasing list; strict ordering implies no duplicates and makes
  extensional equality valid.
- `size` uses `Bigint.t`. An `int` size can wrap to zero for a nonempty list, so
  `size s = 0` if and only if `s = empty` is false with 63-bit arithmetic.
- Persistent predicates retained by `[@def]` do not support local-open
  expressions such as `Bigint.(1Z + n)`. The demo uses `Bigint.add` instead.
- Calls whose dependent result mentions an argument require that argument to
  be a plain local variable. Even constants used with generated definition
  lemmas must first be named.
- Dependent results are not inferred from an unconstrained function body. A
  function that returns a refinement mentioning one of its parameters needs
  an explicit dependent result type; otherwise the dependency is reported as
  a scope escape.
- Persistent predicates retained by `[@def]` reject explicit result-mode
  constraints. `union_repr` leaves its result mode inferred and constrains the
  result at the non-retained call site instead.
- The higher-order recursive extensionality proof produces large verification
  conditions. The bytecode test checks the proof and runtime output in normal and
  principal modes without repeating the native backend.

## AVL demo

- A refinement-wrapped inductive argument is not recognized as a structural
  recursion argument. The insertion proof passes validity as a refined `unit`
  witness while keeping the tree as the recursive argument.
- AVL shape depends on insertion order. Its interface therefore uses semantic
  `equal`, defined by the canonical inorder sequence; `===` remains
  representation equality.
- `Set.MakeTotal` models comparator equivalence with opaque classes because a
  comparator may identify logically distinct values. Relating another set
  implementation that uses source equality therefore requires an explicit
  compatibility premise. Exact-key membership and operations that merely
  combine membership predicates need no such premise.
