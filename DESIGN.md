# OxCaml Refinement Verification

- A refined type is written `{v:int | p}` (or `{v:bool | p}`).
  Predicates are built from `v`, program variables, int/bool literals,
  `+ - *`, comparisons, and `&& || not`. A program variable in a
  refinement means the logical value associated with it, not the
  program value. Predicates are UNTYPED -- the compiler never checks
  them; the logic types them, as in first-order logic. An ill-sorted
  predicate surfaces as a solver error at VC time, which counts as a
  verification failure.
- Refined types are rigid: `{v:int | v>0}` is an ordinary type, distinct
  from `int` and from `{v:int | v>1}`. There is no subtyping anywhere.
  Unification handles them like a type constructor and asserts equal
  predicates -- structurally equal, variables compared by stamp. So
  `{v|v>0}` vs `{v|0<v}` is a type error, as are two separately written
  but alpha-equal dependent signatures. Sharp edges, not bugs.
- Introduction: `refine_ e` wraps `e` at the refined type expected from
  context (an annotation, or the parameter type at an argument
  position). This is the ONLY construct that generates a proof
  obligation. No refined expected type in context is an error.
- `assume_ e` is `refine_ e` with the proof obligation skipped (reported
  as ASSUMED in diagnostics).
- Elimination: the irrefutable pattern `refine_ x`, as in
  `let refine_ x = e`, binds `x` at the skeleton type. Free -- no proof
  obligation. `e`'s type must be refined. Both forms erase at runtime;
  `refine_` and `assume_` are keywords (bare `refine`/`assume` collide with existing identifiers across the compiler).
- Function types may name their parameters, and later refinements may
  mention them: `(x:int) -> (y:int) -> {z:int | z = x * y}`. Dependent
  types arise from annotations only; inferred arrows are never
  dependent. At an application, if the parameter's name occurs in the
  remaining type, the argument must be a VARIABLE (else: "let-bind the
  argument first"); its stamp is substituted.
- Scope: a refinement may mention only parameters of its own type and
  program variables in scope at every point the type flows to; escape
  is an error ("annotate"). In module signatures, refinements may
  mention only parameters of their own type -- never top-level values.
  So .cmi predicates are self-contained.
- The compiler attaches no logical meaning to any program operation or
  constant. Both are defined in user code with `assume_`:

      let mul (x : int) (y : int) : {z:int | z = x * y} = assume_ (x * y)
      let zero : {v:int | v = 0} = assume_ 0

  (the `x * y` in the predicate is the logic's multiplication; the one
  in the body is the program's). Constants are exported refined and
  unpacked at use: facts are module-local, types travel.

## VC generation

Once the program is type inferred, we extract verification conditions
from the typed tree. Every value has a logical NAME: binders get one
keyed by their stamp (never their source name -- shadowing), everything
else is a fresh unknown. Names are declared to the solver by OCaml
type: int as Int, bool as Bool, anything else at a single uninterpreted
sort (equality is all the logic knows about other types). Solver error,
unknown, and timeout all count as verification FAILURE, never success.
Facts about names come from exactly three places:

- Unpacking: `let refine_ x = e` with `e : {v | p}` contributes
  `p[v:=x]`. The same rule applies to any binder of refined type
  (function parameters, pattern variables): matching
  `xs : {v:int|v>0} list` against `x :: _` makes `x > 0` available.
- Path facts: `if c then e1 else e2` checks `e1` under `c = true` and
  `e2` under `c = false`, where `c` is the condition's name.
- Dependent application: applying `f : (x:int) -> {z | p}` to variable
  `a` substitutes: the result type is `{z | p[x:=a]}`, so
  `let refine_ m = mul a b` yields `m = a * b`.

A plain `let x = e` contributes nothing: `x` is a fresh unknown.
Aliasing is expressed with the existing forms,

    let refine_ x = (refine_ y : {v:int | v = y})

whose obligation is the trivial `y = y` and whose unpacking yields
`x = y`.

`refine_ e` at expected type `{v | p}` yields the VC
`facts |- p[v := n]`, where `n` is `e`'s name.

End-to-end example, one VC, provable (`lt` returns a refined bool that
is unpacked before `if`; `zero` is unpacked to bring `z = 0` into
scope; `100` may be passed directly since `div`'s first parameter
occurs in no refinement):

    let zero : {v:int | v = 0} = assume_ 0
    let lt (x : int) (y : int) : {z:bool | z = (x < y)} = assume_ (x < y)
    let div (a : int) (b : {v:int | not (v = 0)}) : int =
      let refine_ b = b in a / b
    let safe x =
      let refine_ z = zero in
      let refine_ c = lt z x in
      if c then div 100 (refine_ x) else 0

The facts `z = 0`, `c = (z < x)`, and the path fact `c = true` prove
`refine_ x`'s obligation `not (x = 0)`.

A `-dump-vc` flag prints every VC (hypotheses, goal, source location);
`-vox-dry-run` skips the solver, so VC generation is testable without
z3 (see testsuite/tests/vox, promoted like other reference tests).
