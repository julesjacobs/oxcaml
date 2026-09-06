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
