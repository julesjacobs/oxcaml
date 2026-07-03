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
  predicates -- structurally equal, variables compared by stamp, except
  that dependent-arrow binders compare under a binder pairing, so two
  separately written alpha-equal dependent signatures are the same
  type. `{v|v>0}` vs `{v|0<v}` is a type error. Sharp edges, not bugs.
- Introduction: `refine_ e` wraps `e` at the refined type expected from
  context (an annotation, or the parameter type at an argument
  position). This is the ONLY construct that generates a proof
  obligation. With no refined expected type in context, `refine_ e`
  instead SYNTHESIZES the exact refinement `{v:t | v = e'}`, where `e'`
  is the logic translation of `e` -- definitionally true, so no
  obligation; the fact then flows from the binder as usual
  (`let c = refine_ (0 < x)` gives `c : bool{ _ = (0 < x) }`).
  Expressions the logic cannot express are an error there ("add a
  refined type annotation").
- `assume_ e` is `refine_ e` with the proof obligation replaced by a
  COMPILED RUNTIME CHECK of the predicate (reported as RUNTIME CHECKED
  in diagnostics): the value is tested against the predicate and
  `Failure` is raised when it does not hold. Dependent-arrow binders in
  the predicate have been opened to the enclosing parameters' stamps by
  the time the check is compiled. The check is compiled only when it is
  FAITHFUL to the logic: every value it reads must be int- or
  bool-sorted (other sorts are uninterpreted, where the machine's
  physical equality is stricter than logical equality, so a coherent
  assumption could fail at run time). A predicate reading a
  non-int/bool value, or mentioning a variable not in scope at the
  node, is a compile-time error pointing at `assume_unchecked_`.
- `assume_unchecked_ e` is `refine_ e` with the proof obligation
  skipped outright (reported as ASSUMED in diagnostics). No check is
  compiled; this is the trusted escape hatch for predicates `assume_`
  cannot check.
- Elimination: the irrefutable pattern `refine_ x`, as in
  `let refine_ x = e`, binds `x` at the skeleton type. Free -- no proof
  obligation. `e`'s type must be refined. `refine_` and
  `assume_unchecked_` erase at runtime; `assume_` erases to its runtime
  check. `refine_`, `assume_`, and `assume_unchecked_` are keywords
  (bare `refine`/`assume` collide with existing identifiers across the
  compiler).
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
- Simple algebraic data types: constructors of "simple" variants
  (monomorphic, non-GADT, closed, at least one constructor, tuple
  arguments only -- hence immutable: mutability enters variants only
  through inline-record fields, and a mutated field plus injectivity
  would prove false equalities) may appear in predicates:
  `ilist{ _ = Cons (3, Nil) }`.  The solver models them with its
  datatype theory (free, injective, pairwise-distinct constructors);
  datatypes reach it as `declare-datatypes` blocks (Z3) or single-line
  `inductive`s (Lean) in dependency order.  Mutually recursive
  datatypes are not supported (self-recursion is fine).  Constructor
  argument types may themselves be refined (`W of {v:int | v > 0}`):
  matching then contributes the field's refinement at the binder.
- Simple records (monomorphic, EVERY field immutable) are
  single-constructor datatypes with named selectors (Lean:
  [structure]s, whose projections come built in).  Field projection
  appears in predicates -- `point{ _.px = p.py }` -- binding tighter
  than application, as in expressions; the label resolves at
  elaboration like constructors do.  Construction names the
  constructor term; a functional update `{ p with px = e }` projects
  kept fields out of the base's name (the frame comes for free).  A
  mutable field disqualifies the whole record from precise tracking
  (naming a term and then mutating a field would prove false
  equalities): its fields may not appear in predicates and its reads
  stay fresh unknowns.  Refinements ON a mutable field's type still
  work, as invariants re-proved at every write.  Record literals do
  not appear in predicates (project instead: `_.px = 1 && _.py = 2`);
  structure extensionality is not automatic (grind proves projection
  goals, not bare record equalities from equal projections).  A field
  named [mk] (the structure's constructor), or two fields whose
  sanitized solver names collide, makes the emitted declaration
  invalid: a solver error, i.e. a verification failure (fails
  closed).
- Spec functions: any other applied identifier in a predicate,
  `len _` or `mem 2 _`, denotes a logical function that the user
  defines on the solver side in a `-vox-prelude` file, inserted
  verbatim -- just after the datatype declarations -- into every
  generated solver input that applies a spec function (not into other
  inputs: the prelude may reference datatypes of a different module) (for Lean: `@[grind] def len : Vox_M_ilist -> Int ...`).
  Spec functions live in their own namespace -- program functions have
  no logical meaning, so there is nothing to collide with -- and, like
  the rest of the predicate language, they are untyped.  To make
  preludes writable, solver-side datatype names are STABLE: stamp-free
  and unit-qualified (`Vox_<Unit>_<path>`), the same in the defining
  module and in every client; distinct types that would collide are
  rejected ("rename one of them").
- REFLECTED functions: a module-level `let rec f ... = ...
  [@@vox.reflect]` is a program function that DEFINES the spec function
  of the same name -- the compiler translates its body into an
  equation-style logical definition (Vox_reflect.translate_def) and
  emits it into the solver input between the datatypes and the prelude
  (for Lean an honest `@[grind] def`, so prelude lemmas can be stated
  about it), and a saturated application of `f` in program code
  translates like a primitive: `refine_ (len l)` at `int{ _ = len l }`
  is trivial, and `let refine_ r = refine_ (fib m)` is the induction
  hypothesis at a recursive call.  The reflectable fragment is small
  (sharp edges, not bugs): parameters are plain variables of int, bool
  or simple-variant sort; the body is built from the translatable
  operations, constructors, saturated calls to reflected functions
  (self included), `if` on translatable conditions, and exhaustive
  one-level `match` on a variable; the definition must be CLOSED (only
  its own parameters and match fields), which is why reflected
  bindings are module-level only.  TERMINATION is the solver's to
  check -- an inconsistent definition (`f x = f x + 1`) would prove
  anything, so a rejected definition is a verification failure
  reported against the binding.  Structural recursion needs nothing;
  int-indexed recursion carries `[@@vox.decreases e]` (an int metric
  over the parameters), emitted as `termination_by (e).toNat` with an
  omega `decreasing_by` -- the branch guards are in context for those
  goals, so `fib` with guards `n <= 0` / `n = 1` needs exactly
  `[@@vox.decreases n]`.  Two reflected functions may not share a
  name; a reflected name shadowing a prelude definition is a solver
  error (fails closed).  See testsuite/tests/vox/lean_reflect.ml (a
  spec library with an EMPTY prelude) and lean_fib.ml (reflected fib,
  with the fast-doubling lemmas stated about it in the prelude).
  Caveats: the program/logic correspondence of a reflected call is
  partial-correctness (a diverging call returns no value) and ideal
  arithmetic (overflow, as everywhere); the definition is emitted only
  in its own module (client modules re-reflect nothing -- cross-module
  reflected calls are future work, needing the definition in the
  .cmi).
- The compiler attaches logical meaning to exactly the operations the
  predicate language models (Vox_reflect): variables, int/bool
  constants, `+ - * ~-` (and `succ`/`pred`), `&& || not`, and
  comparisons at int or bool -- recognized by PRIMITIVE, so shadowing
  `(+)` cannot be mistaken for integer addition. So built-in operations
  need no userland wrappers:

      let mul : (x : int) -> (y : int) -> {z:int | z = x * y} =
        fun x y -> refine_ (x * y)
      let zero : {v:int | v = 0} = refine_ 0

  (both obligations are trivial: `x * y = x * y` and `0 = 0`).
  Operations and facts BEYOND the translatable fragment are still
  defined in user code with `assume_` / `assume_unchecked_`. Constants
  are exported refined and unpacked at use: facts are module-local,
  types travel.
- CAVEAT: the logic's ints are unbounded while the machine's wrap, so
  reflecting `+ - *` equates modular with ideal arithmetic; overflow is
  outside the model (`x + 1 > x` is provable and false at `max_int`).

## VC generation

Once the program is type inferred, we extract verification conditions
from the typed tree. Every value has a logical NAME: binders get one
keyed by their stamp (never their source name -- shadowing);
expressions in the translatable fragment are named by their logic
translation; everything else is a fresh unknown. Names are declared to
the solver by OCaml type: int as Int, bool as Bool, anything else at a
single uninterpreted sort (equality is all the logic knows about other
types). Solver error, unknown, and timeout all count as verification
FAILURE, never success. Facts about names come from exactly three
places:

- Unpacking: `let refine_ x = e` with `e : {v | p}` contributes
  `p[v:=x]`. The same rule applies to any binder of refined type
  (function parameters, pattern variables): matching
  `xs : {v:int|v>0} list` against `x :: _` makes `x > 0` available.
- Path facts: `if c then e1 else e2` checks `e1` under `c = true` and
  `e2` under `c = false`, where `c` is the condition's name -- so a
  translatable compound condition contributes itself:
  `if 0 < x then ...` checks the branch under `0 < x` directly.
- Dependent application: applying `f : (x:int) -> {z | p}` to variable
  `a` substitutes: the result type is `{z | p[x:=a]}`, so
  `let refine_ m = mul a b` yields `m = a * b`.
- Match facts (the match refines the thing it matched on): in
  `match s with ...` where `s` is a VARIABLE, a case whose pattern is
  one constructor of a simple variant over variables or wildcards
  checks its guard and body under `s = C x1 ... xn` (wildcards name
  fresh unknowns); a simple-record pattern contributes `xi = s.li` per
  variable sub-pattern (per-field, so partial patterns are fine).
  `let p = x in ...` gets the same facts, so destructuring a record
  binds its fields logically.  Deeper patterns -- nesting, aliases,
  or-patterns, constants -- contribute nothing, which is sound.
  NEGATIVE facts: an arm also learns that every EARLIER arm failed to
  match, usable exactly when that failure is decided by the head
  alone: each guard-free earlier arm of the simple shape contributes
  `not (s is C)` (an internal constructor tester, not surface syntax).
  Guarded arms contribute no negation (the pattern may have matched
  with the guard false), nor do arms with refuting sub-patterns
  (`A 0`: the head may have matched anyway).  Z3 has native testers;
  Lean encodes `s is C` existentially and each theorem with tester
  facts also receives the subject's exhaustiveness disjunction
  ((∃ a, s = A a) ∨ ... ∨ s = C) as a hypothesis, so grind can case on
  the negations -- a default arm below `A _` and `B` proves `s = C`.

Constructors are the one program construct with built-in logical
meaning ("the usual refinements"): the name of `K e1 ... en` is
`K n1 ... nn` over the arguments' names, so `refine_ (K 3)` at
`t{ _ = K 3 }` has the trivial obligation `K 3 = K 3`, and checking
`fun x -> refine_ (K x)` against `(x : int) -> t{ _ = K x }` opens the
binder to the trivial `K x = K x`.  With a measure in the prelude,
recursive functions verify INDUCTIVELY: each recursive call
re-instantiates the dependent signature at the actual arguments, so
its refined result is the induction hypothesis (see append/rev in
testsuite/tests/vox/lean_spec.ml).

A plain `let x = e` contributes nothing: `x` is a fresh unknown.
Aliasing is expressed with the existing forms,

    let refine_ x = (refine_ y : {v:int | v = y})

whose obligation is the trivial `y = y` and whose unpacking yields
`x = y`.

`refine_ e` at expected type `{v | p}` yields the VC
`facts |- p[v := n]`, where `n` is `e`'s name.

End-to-end example, one VC, provable -- the comparison is reflected
directly into the path fact, and `100` may be passed directly since
`div`'s first parameter occurs in no refinement:

    let div (a : int) (b : {v:int | not (v = 0)}) : int =
      let refine_ b = b in a / b
    let safe x =
      if 0 < x then div 100 (refine_ x) else 0

The path fact `0 < x` proves `refine_ x`'s obligation `not (x = 0)`.
The same works through a binding (`let c = refine_ (0 < x) in
if (c :> bool) then ...`: the binder fact `c = (0 < x)` plus the path
fact `c`), and through userland dependent operations for anything
beyond the translatable fragment:

    let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
      fun x y -> refine_ (x < y)

(The dependent type must be written as an annotation, [(x : int) ->
...]: a refinement written directly in terms of a lambda's parameters
would name them as free program variables, which may not appear in the
function's own type -- the escape checks reject it, with the dependent
arrow as the sanctioned spelling.)

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
