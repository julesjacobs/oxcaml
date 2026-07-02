# OxCaml Refinement Verification

- A refined type is written compactly as `ty{ p }`, with `_` for the
  bound value variable: `int{ _ >= 0 }`, `int{ x + _ = y }`,
  `unit{ x > y }` (a bare proposition).  The bound variable can be
  named: `(y : int{ y > 3 })` (parenthesized; as a function result,
  `... -> (y : int{ y > x })`), and inside a dependent binder's own
  type the binder name already denotes the refined value:
  `(x : int{x > 3}) -> ...`, `let f (x : int{x > 3}) = ...`.  The
  long form `{v:int | p}` (binder named explicitly) is also accepted.
  Refinements are allowed at EVERY skeleton type: the solver knows
  int as Int and bool as Bool; every other type lives at a single
  uninterpreted sort where equality is all the logic knows.
  Predicates are built from the bound variable, program variables,
  int/bool literals, `+ - *`, comparisons, and `&& || not`.  A
  program variable in a refinement means the logical value associated
  with it, not the program value. Predicates are UNTYPED -- the
  compiler never checks them; the logic types them, as in first-order
  logic. An ill-sorted predicate surfaces as a solver error at VC
  time, which counts as a verification failure.  Types print in the
  compact format, dependent binders included ([(x : int) -> int{ _ =
  x }]), and the printed form reparses to an alpha-equivalent type.
  Restrictions: a binder's name denotes the refined value only in the
  refinement at the TOP of its own annotation (in a nested refinement
  it is an unbound-variable error -- annotate what you mean);
  optional/position parameters cannot carry dependent binders (the
  caller may pass the option itself); [-dsource] output of the
  compact form does not reparse ([_] in the encoded payload is not
  an expression) -- the long form round-trips.
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
    let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
      fun x y -> assume_ (x < y)
    let div (a : int) (b : {v:int | not (v = 0)}) : int =
      let refine_ b = b in a / b
    let safe x =
      let refine_ z = zero in
      let refine_ c = lt z x in
      if c then div 100 (refine_ x) else 0

(The dependent type must be written as an annotation, [(x : int) ->
...]: a refinement written directly in terms of a lambda's parameters
would name them as free program variables, which may not appear in the
function's own type -- the escape checks reject it, with the dependent
arrow as the sanctioned spelling.)

The facts `z = 0`, `c = (z < x)`, and the path fact `c = true` prove
`refine_ x`'s obligation `not (x = 0)`.

A `-dump-vc` flag prints every VC (hypotheses, goal, source location);
`-vox-dry-run` skips the solver, so VC generation is testable without
z3 (see testsuite/tests/vox, promoted like other reference tests).

## Escape enforcement (the activation problem)

Logical names are static stamps, which do not distinguish function
ACTIVATIONS: a refinement mentioning a recursive function's parameter
that reaches a value from a DIFFERENT activation of that function
(e.g. a stored closure) would prove facts that are false at runtime.
Soundness rests on making such types unable to travel, enforced in
three layers, all on FINAL (post-inference) types so refinements that
arrive by unification alone are caught too:

- Signature self-containment: every item exported by an
  implementation, interface, or toplevel phrase -- values, type
  manifests, record fields, constructor and extension-constructor
  arguments, submodules, module types, classes -- must carry only
  program-variable-free refinements. (At the toplevel the session's
  accumulated signature is re-checked each phrase: a later phrase can
  instantiate an earlier item's weak type variable.)
- Scope checks at binders: a local binder's type may only mention
  variables in scope at the binding. In particular a (rec) function
  whose own type mentions its own parameter is rejected -- that is the
  only way a type can cross between activations, since the arguments
  and results of a recursive call are part of the callee's binder
  type, and a heap cell shared by activations has a binder outside the
  parameter's scope.
- A backstop check on binders inside constructs the VC walker does not
  model (module structures in expressions, try handlers, letops):
  they contribute no facts but are still escape-checked.

Channels that need no check: local exceptions and extension
constructors are generative per evaluation, so a handler only ever
matches same-activation payloads; existentials (GADTs, abstract types)
hide the refinement, making it unrecoverable rather than unsound.

The cost is that a function cannot relate its parameters through
FREE program variables; the sanctioned spelling is a dependent arrow
((x:int) -> ... {v | v = x} ...), which is closed and re-instantiated
at every call -- see the next section.

## Dependent arrows as a binding form (mirrors Tpoly)

The activation problem is the same problem the compiler already
solves for universal type quantification: Tpoly binds Tunivar nodes
inside a type, and soundness comes from never letting a bound name
leak -- USE sites open the quantifier with fresh instantiable
variables (Ctype.instance_poly), DEFINITION sites open it with rigid
names that must not escape (instance_poly_fixed + check_univars), and
escape is caught by dedicated machinery. Refinement systems in the
literature (DML, Liquid Types) are built the same way: function types
BIND their parameters, application is substitution, and
well-formedness (fv(type) in scope) is checked at formation -- the
activation problem is definitionally impossible because no type ever
names a live binder.

Dependent arrows implement exactly that:

- REPRESENTATION: the arrow stores its binder [Ident] in its
  [arrow_desc] (like Tpoly stores its univars); refinements in the
  codomain reference it as an ordinary [Pvar]. There is no positional
  (de Bruijn) indexing: opening is stamp substitution, so partial,
  labelled and commuted applications are all correct by construction.
  An arrow whose codomain never mentions the binder drops it, so
  [(x:int) -> t] and [int -> t] are the same type when [x] is unused.

- BINDER IDENTITY: binders are [Scoped] idents, a constructor
  [Ident.same] never equates with the [Local] idents of program
  variables -- so a binder stamp marshalled through a .cmi cannot
  collide with a consuming unit's variables (stamps are only
  process-local; this closes a family of cross-unit false-fact
  exploits found in review).

- ALPHA-EQUIVALENCE: two independently written (hence
  differently-stamped) dependent signatures are compared under a
  binder pairing pushed at each arrow during moregen/eqtype/subtyping
  ([Refinement.with_binder_pair], the analogue of [univar_pairs],
  orientation-insensitive because comparison can swap its sides); so
  an [.mli] can spell the binder with a different name than the
  [.ml].  UNIFICATION instead renames one binder to the other before
  unifying the codomains: unification merges graphs node-wise and a
  pairing could otherwise leave one side's binder combined with the
  other side's predicates.

- SUBSTITUTION REACH: [subst_binder] does not rebuild objects,
  polymorphic variants or package types; a binder occurring under one
  cannot be opened, and such a type is rejected at the point the
  binder would be opened rather than left dangling.

- USE (analogue of instance_poly): the application's arrow peel
  ([collect_apply_args]) substitutes each consumed dependent binder
  by its (syntactically required to be a variable) argument's stamp
  throughout the remaining type -- BEFORE later arguments are
  typechecked, so later parameter types and the result are
  instantiated at the caller's variables. An omitted or commuted-past
  dependent parameter has no name to substitute and is an error.

- DEFINITION (analogue of instance_poly_fixed): checking [fun x
  prev -> body] against a dependent arrow opens the consumed arrow's
  binder at the just-bound parameter's stamp before the rest of the
  type is used ([type_function]); the parameter must be a variable
  pattern. The stamp is a natural skolem -- rigid predicate equality
  means nothing can unify it away -- and inside the body the opened
  refinements are activation-local truths.

- RECURSION is thereby the textbook rule: the rec binder's stored
  type is closed; each occurrence (recursive calls included) opens it
  fresh at the actual arguments, so a closure refined at THIS
  activation's variable simply fails to typecheck as an argument
  refined at the NEXT activation's (see testsuite/tests/vox:
  [countdown] accepted, [unsound] rejected). The declared refinement
  is assumed at recursive calls: standard partial correctness
  (divergence makes facts vacuous).

Remaining in-checker hardening (optional, defense in depth): ride the
existing level/scope escape machinery (update_level's [level < scope]
check, as used for Tconstr/Tpackage paths to local modules and GADT
existentials) by giving each Trefine node a scope derived from its
free Pvars' binding levels, catching escapes at the offending
unification instead of in the post-typing walk.

Later (v2): existential quantification at scope exit
(Knowles-Flanagan) to weaken escaping types instead of erroring, and
liquid-style inference, whose well-formedness constraints are exactly
the scope rule above.

## Wishlist

- [x] Compact syntax for refinements:
    int{ _ >= 0 }
    int{ x + _ = y }
    unit{ x > y }
  (plus named binders ((y : int{ y > 3 })), self-reference in a
  binder's own annotation, and refinements at every skeleton type;
  the named form requires parentheses -- unparenthesized
  [-> y:int{...}] is LR(1)-ambiguous with labeled arrows and labeled
  tuples)
- [x] test/toplevel/error-message output is in that format
- [ ] strong update for @ unique mutation
- [ ] RustHorn style borrows via block indices
- [ ] Algebraic data types with refined constructors
