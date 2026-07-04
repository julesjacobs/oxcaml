# OxCaml Refinement Verification

- A refined type is written compactly as `ty{ p }`, with `_` for the
  bound value variable: `int{ _ >= 0 }`, `int{ x + _ = y }`,
  `unit{ x > y }` (a bare proposition).  The bound variable can be
  named: `(y : int{ y > 3 })` (parenthesized; as a function result,
  `... -> (y : int{ y > x })`), and inside a dependent binder's own
  type the binder name already denotes the refined value:
  `(x : int{x > 3}) -> ...`, `let f (x : int{x > 3}) = ...`.  The
  long form `{v:int | p}` (binder named explicitly) is also accepted.
  Refining a type whose EXPANSION is already refined -- an
  abbreviation like `type set = tree{ bst _ }` -- CONJOINS the layers
  on the underlying skeleton at elaboration: `set{ p }` IS
  `tree{ bst _ && p }`, the same rigid type, and layers accumulate
  through abbreviation chains (mechanics/flatten.ml).  A skeleton
  that becomes refined only through later instantiation of a type
  variable is not flattened; rigid unification fails closed there.
  Refinements are allowed at EVERY skeleton type: the solver knows
  int as Int and bool as Bool; every other type lives at a single
  uninterpreted sort where equality is all the logic knows.
  Predicates are built from the bound variable, program variables,
  int/bool literals, `+ - * / mod` (division and remainder with
  OCaml's semantics: T-division, truncating toward zero -- the solver
  sees Lean's `Int.tdiv`/`Int.tmod`; the logic totalizes `x / 0` as 0
  where the program raises, sound under partial correctness),
  comparisons, `&& || not`, implication `p -> q` (native and
  right-associative; structurally distinct from its expansion
  `not p || q`, as every respelling is), and quantifiers (below).  A
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
  an expression, and quantifier keywords are not operators) -- the
  long form round-trips for quantifier-free predicates.
- Refined types are rigid: `{v:int | v>0}` is an ordinary type, distinct
  from `int` and from `{v:int | v>1}`. There is no subtyping in the
  TYPE ALGEBRA: unification handles refinements like a type
  constructor and asserts equal predicates -- structurally equal,
  variables compared by stamp, except that dependent-arrow binders
  (and quantifier binders) compare under a binder pairing, so two
  separately written alpha-equal signatures are the same type.  What
  SOFTENS the edges is implicit subsumption (below), which re-proves a
  VARIABLE's refinement at a differently-spelled annotation as an
  ordinary obligation -- so `{v|v>0}` vs `{v|0<v}` is a proof
  obligation at a binder and remains a type error only where the
  refinement sits under a type constructor, where no obligation can
  be minted.  Sharp edges, not bugs.
- Introduction: `refine_ e` wraps `e` at the refined type expected from
  context (an annotation, or -- as an explicit cast -- the parameter
  type at an argument position). Proof obligations arise from exactly
  two places: `refine_` intros -- written explicitly, or inserted
  implicitly at check positions (see the IMPLICIT bullet below) --
  and applications to refined PARAMETERS (see "Parameters as
  preconditions" below). With no refined expected type in context,
  `refine_ e` instead SYNTHESIZES the exact refinement `{v:t | v = e'}`,
  where `e'` is the logic translation of `e` -- definitionally true, so
  no obligation; the fact then flows from the binder as usual
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
- `unreachable_` is an expression of any expected type whose proof
  obligation is `false` under the path facts: a match arm (or branch)
  is accepted exactly when it is PROVED unreachable.  It compiles to
  `assert false` -- dead code by that proof.  A reachable `unreachable_`
  fails with a counterexample.  (See demo/lean_nth.ml: a safe `nth`
  whose `Nil` arm is dead because `0 <= i < len l` contradicts
  `len Nil = 0`.)
- Elimination: the irrefutable pattern `refine_ x`, as in
  `let refine_ x = e`, binds `x` at the skeleton type. Free -- no proof
  obligation. With BINDERS AS FACTS (below) this is what every binder
  does; the explicit pattern remains for emphasis on a genuinely
  refined scrutinee, and an unrefined scrutinee is an error (the
  unpack would be dead code -- a plain `let` binds at the skeleton
  and carries the self fact already).  `refine_` and
  `assume_unchecked_` erase at runtime; `assume_` erases to its runtime
  check. `refine_`, `assume_`, and `assume_unchecked_` are keywords
  (bare `refine`/`assume` collide with existing identifiers across the
  compiler).
- BINDERS AS FACTS: every LOCAL binder whose type carries a top-level
  refinement -- function parameters (via the contract rule), match and
  pattern binders, and `let`-bound names, annotated or inferred --
  binds at the SKELETON; the predicate becomes a fact at the binder's
  stamp (contributed by the verification pass from the pattern, whose
  type keeps the refinement).  An annotation on a binder therefore
  means "prove here, assume after": the refined type never enters the
  local typing context, so a bound name composes with everything its
  carrier does (equality, polymorphic instantiation, deep positions)
  -- the DML discipline, where Sigma-packages are opened at every
  binding.  Exemptions: MODULE-LEVEL bindings keep their refined types
  (types travel across module boundaries; facts are module-local), and
  MUTABLE binders keep theirs (a persistent fact about a mutable name
  would survive assignment).  Deep refinements (`int{p} list`) are
  untouched: only the top-level refinement of the bound name opens.
- IMPLICIT introduction/elimination (inference for the above; see
  testsuite/tests/vox/mechanics/infer.ml).  Contract positions are
  owned by the contract rule (a monomorphic refined arrow domain is
  stripped at the application, so no coercion is ever needed there),
  and local binders are owned by BINDERS AS FACTS (their uses are
  carrier-typed from the start); the implicit rules below cover the
  remaining RIGID positions -- refined expected types at annotations,
  results and constructor payloads, values imported at module-level
  refined types, and the refined domains contracts leave rigid
  (polymorphic ones).  Refined types stay rigid in unification, so
  elaboration can insert the coercion exactly where unification is
  certain to fail.  Where the EXPECTED type is refined: a syntactic
  value form (constant, constructor, tuple, record, array, function)
  is typed at the skeleton and marked as a `refine_` intro; an
  inferred form (variable, application, field, ...) is typed without
  the expected type and reconciled -- equal refinement or flexible
  type: pass-through, no obligation; unrefined rigid type: an intro;
  a VARIABLE or an APPLICATION at a different refinement: re-refined
  (the obligation is discharged with the subject's own refinement --
  a variable's from its binder fact, an application's SELFIFIED at
  the node's name by the verification pass, the arrow spine walked
  and dependent binders substituted exactly as typing did: the inline
  unpack that `let q = f x in q` used to spell, so a recursive call
  whose postcondition speaks of the smaller instance is the bare
  TAIL).  Dually, a VARIABLE of refined type used where a rigid
  unrefined type is expected is implicitly eliminated,
  obligation-free, and an APPLICATION's refined result is erased at a
  rigid unrefined expected type (no obligation arises there, and the
  fact of an unnamed value is unreachable either way -- name it with
  a `let` to keep it).  Other forms at a different refinement must be
  let-bound ("let-bind it first").  MODULE-LEVEL values participate
  by PATH: `Pglobal` is the global counterpart of `Pvar`, stamp-free
  and .cmi-stable like the type paths in `Pconstr`/`Pfield`/`Pis`.
  An import names itself, qualifies as a DEPENDENT argument
  ([insert 2 empty]), matches and destructures directly (imports are
  carrier-typed at use), and its .cmi refinement arrives as a global
  fact, registered at VC emission and pulled into exactly the VCs
  that mention the path.  Two paths to one value are two names --
  both facts true, their equality not assumed (sound, incomplete).
  Globals are unreflectable in runtime checks (assume_ points at
  assume_unchecked_).  Branch constructs propagate the
  expected type, so implicit intros land at the LEAVES, under each
  branch's path facts (and, since local binders are carrier-typed,
  unannotated joins of refined-fact-carrying and plain branches are
  simply well-typed at the carrier, in either order).  Implicit
  insertion always chooses `refine_` (an honest obligation), never
  `assume_`; `refine_` remains required in synthesis position and
  wherever the expected type is not yet resolved when the expression
  is checked.  Synthesis names the full NAMEABLE fragment: the
  reflected operations plus constructor terms, record literals,
  tuples, and immutable field reads (`let k = refine_ (Cons (x, Nil))`
  synthesizes `{v | v = Cons (x, Nil)}`).
- Function types may name their parameters, and later refinements may
  mention them: `(x:int) -> (y:int) -> {z:int | z = x * y}`. Dependent
  types arise from annotations only; inferred arrows are never
  dependent. At an application, if the parameter's name occurs in the
  remaining type, the argument must be an expression the logic can
  NAME before it is typed: an immutable variable (its stamp), a
  literal (itself), or a pure surface expression over the reflected
  int/bool operations, saturated total_ calls, unlabeled tuples, and
  `fst`/`snd`, which names itself (`f (i + 1)`, `f (fib k)`,
  `f (a, b)`); the name is substituted throughout the remaining type
  (else: "let-bind the argument first").  Recognition resolves
  identifiers in the environment -- the same resolution the later
  typing performs -- so a shadowed `(+)` cannot lie; tuples need no
  type gate (the product model is per-arity, polymorphic), and
  `fst`/`snd` are admitted by their resolved value's DECLARED pair
  domain, which any typechecking application's argument then has; the
  POLYMORPHIC comparisons are excluded (their operand sort is unknown
  before typing, and the logic's equality disagrees with the
  program's at floats and functions); mutable variables are rejected
  as everywhere.  The fragment is pure up to Division_by_zero
  (partial correctness).
- Parameters as preconditions: a refinement on an arrow PARAMETER is a
  CONTRACT, not a value type.  Checking `fun x -> body` against
  `(x : int{p}) -> t` binds `x` at the SKELETON int -- so the body uses
  `x` directly, and every type the body writes about `x` speaks of the
  same stamp as the opened annotation -- and `p[v:=x]` is assumed as a
  fact.  Every application `f a` discharges `p` at `a`'s logical name
  as a proof obligation; arguments are passed BARE (compound arguments
  are named by their logic translation when they have one, a fresh
  unknown otherwise).  An argument spelled with an intro form
  (`refine_ e`/`assume_ e`/`assume_unchecked_ e`) instead keeps the
  rigid behavior: an explicit cast typed at the refined parameter
  type, carrying its own obligation.  Rationale: a refined binder
  under rigid equality behaves as an existential package, and
  existential elimination (unpacking) mints a fresh stamp -- any type
  that must be equal across the unpack (e.g. a result predicate
  mentioning the parameter) then fails rigid equality.  Quantifying
  the index at the arrow and constraining it -- the DML arrow -- is
  the semantics that needs no unpack; contracts are its vox spelling.
  Refinements on STORED values (let-annotations, constructor payloads,
  results) remain rigid TYPES -- but the names bound to them do not
  stay packages: BINDERS AS FACTS (above) opens every local binder at
  the skeleton, so a call's refined result is unpacked by the `let`
  that names it (which also remembers WHICH value it opened -- the
  self fact), and weakening it to the enclosing instantiation is an
  implicit re-proof at the annotation.  A parameter refined by a
  PATTERN annotation (`fun (b : {v:int | p}) -> ...`) likewise binds
  at the skeleton, the refined type still flowing into the arrow
  through the constraint, so the two parameter spellings agree and
  callers get the contract convention.  `refine_ x` on a variable
  that IS refined is a checked CAST: the subject keeps its own type,
  only the skeletons must agree, and the expected refinement becomes
  an obligation at `x`'s name, provable from the subject's own binder
  fact.  (Detection of contract use in a module with no
  vox syntax of its own -- bare calls into a contract API, possibly
  behind a type abbreviation -- is flagged by the type checker at the
  point it strips the parameter refinement, where the domain is
  already being expanded at the correct stage; the verification gate
  itself never expands the types of unannotated programs, which would
  stage-fault on quotations.)
- Scope: a refinement may mention only parameters of its own type and
  program variables in scope at every point the type flows to; escape
  is an error ("annotate"). In module signatures, refinements may
  mention parameters of their own type and MODULE-LEVEL values by
  path (`Pglobal` -- stamp-free, so .cmi predicates stay
  self-contained in the sense that matters: no process-local
  stamps).  Local variables never appear.
- Simple algebraic data types: constructors of "simple" variants
  (monomorphic, non-GADT, closed, at least one constructor, tuple
  arguments only -- hence immutable: mutability enters variants only
  through inline-record fields, and a mutated field plus injectivity
  would prove false equalities) may appear in predicates:
  `ilist{ _ = Cons (3, Nil) }`.  The solver models them with its
  datatype theory (free, injective, pairwise-distinct constructors);
  datatypes reach it as single-line Lean `inductive`s in dependency
  order.  Mutually recursive
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
- Native tuples: unlabeled tuples are STRUCTURAL datatypes -- each
  ARITY is modelled by one Sort-polymorphic product structure (VoxT2,
  VoxT3, ...; the shape of Lean's PProd, so a Prop component -- the
  model of bool -- instantiates as readily as a Type one), declared
  once and instantiated by the solver's inference.  Construction
  [(a, b)] and the pair projections [fst _]/[snd _] appear in
  predicates with no instantiation info, keeping predicates untyped;
  [fst]/[snd] are RESERVED in predicates (they never fall through to
  the spec-function namespace).  Tuple expressions built from
  translatable components translate ([refine_ (a, b)] synthesizes its
  exact refinement); [fst]/[snd] translate by PRIMITIVE
  ([%field0_immut]/[%field1_immut]), gated on the argument being an
  unlabeled pair.  Matching a variable against a tuple pattern (or
  destructuring [let (x, y) = p]) contributes [xi = proj_i s] per
  VARIABLE component, exactly like simple records; projections beyond
  pairs have no surface syntax and arise from match facts only
  (printed 1-based, [t.1]).  Component types may be any sort
  (datatypes, other tuples, uninterpreted); labeled and unboxed
  tuples are not modelled and degrade soundly to the uninterpreted
  sort.  The polymorphic EQUALITY translates at tuples of int/bool
  (nested included): structural equality on immutable int/bool
  components is exactly the product datatypes' equality.  The ORDER
  comparisons do not (OCaml's tuple order is lexicographic; the logic
  has no order at product sorts).  CAVEAT: a dependent binder over a
  tuple domain must parenthesize it -- [(p : (int * int)) -> ...] --
  since
  [(p : int * int)] is the LABELED TUPLE type [p:int * int] (the
  LR(1) ambiguity above); the printer emits the parenthesized form.
- Quantifiers: `forall_ x. p` and `exists_ x. p` (keywords, like
  `refine_`: bare `forall`/`exists` collide with existing
  identifiers); binders may be listed (`forall_ i j. p`) and extend
  maximally right, in both the compact and long spellings (one
  [vox_pred] grammar serves both).  A binder
  is a fresh `Scoped` ident -- like a dependent-arrow binder, so a
  .cmi-marshalled stamp can never collide with a client's variables --
  compared under the same binder pairing, so alpha-variants are the
  SAME rigid type.  Binders are UNSORTED like the rest of the
  predicate language: the Lean side emits them unannotated and lets
  elaboration infer (a formula whose atoms involve only binders is
  stuck at polymorphic operators -- ground it with an int literal).
  `assume_` cannot check a quantifier (compile-time error pointing at
  `assume_unchecked_`).  The automation envelope, measured against
  grind (demo/lean_quant.ml): a `forall_` GOAL is reliable (grind
  introduces the binder); an `exists_` FACT is reliable (grind
  skolemizes it); an `exists_` GOAL needs the witness-equation idiom
  (`exists_ y. y = 3 && ...`); instantiating a `forall_` FACT is
  unreliable (E-matching against linear-arithmetic normal forms) --
  heavy quantified reasoning, e.g. sortedness instantiated at
  discovered indices, still belongs in `[%%vox.lean]` prelude lemmas,
  where `@[grind]` annotations control the patterns.  Verification
  stays fails-closed throughout: an obligation grind cannot discharge
  is a compile error, never unsoundness.
- Spec functions: any other applied identifier in a predicate,
  `len _` or `mem 2 _`, denotes a logical function (`not`, `fst`,
  `snd`, `succ`, `pred` and `mod` are RESERVED builtins with their
  program meaning -- `succ x` is `x + 1` in a predicate exactly as in
  a reflected program expression -- and never fall through to the
  spec namespace) that the user
  defines on the solver side -- in a `-vox-prelude` file or an
  embedded `[%%vox.lean]` block (`@[grind] def len : Vox_M_ilist ->
  Int ...`), or implicitly by a `total_` definition (below).  Prelude
  text is inserted verbatim -- just after the datatype declarations --
  into every generated solver input that applies a spec function (not
  into other inputs: a prelude may reference datatypes of a different
  module).
  Spec functions live in their own namespace -- program functions have
  no logical meaning, so there is nothing to collide with -- and, like
  the rest of the predicate language, they are untyped.  To make
  preludes writable, solver-side datatype names are STABLE: stamp-free
  and unit-qualified (`Vox_<Unit>_<path>`), the same in the defining
  module and in every client; distinct types that would collide are
  rejected ("rename one of them").
- TOTAL (reflected) functions: a module-level `let rec total_ f ... =
  ...` (the marker rides the binder pattern as a `vox.total`
  attribute; the `[@@vox.total]` spelling also works) is a program
  function that DEFINES the spec function
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
  error (fails closed).  See testsuite/tests/vox/demo/lean_reflect.ml
  (a spec library with no prelude at all) and demo/lean_fib.ml
  (reflected fib, with the fast-doubling lemmas stated about it in an
  embedded prelude block).
  Reflected functions CROSS MODULES: the marker rides the binder
  pattern into val_attributes and hence the .cmi, and the definition
  rides the unit's spec export (pre-rendered, ahead of the unit's own
  blocks, which may state lemmas about it) -- so a client names
  [A.fib x] like a local call, applies [A.fib] (or [fib], after
  [open]) in predicates, and may reflect its own functions in terms of
  imported ones (imported blocks are emitted before this module's
  definitions).  A unit with an .mli exports neither marker nor
  definition: its total_ functions stay private, and a client's use
  degrades to an unknown (sound).  Two units exporting the same
  definition name collide in the client's solver input (fails
  closed).  See testsuite/tests/vox/demo/lean_reflectclient.ml.
  Caveats: the program/logic correspondence of a reflected call is
  partial-correctness (a diverging call returns no value) and ideal
  arithmetic (overflow, as everywhere).
- Embedded solver blocks: `[%%vox.lean {lean|...|lean}]` puts the
  solver-side text directly in the module, next to the datatypes and
  reflected definitions it is about (blocks are not "preludes":
  reflected definitions precede them, so a block may state lemmas
  about the module's own total_ functions).  Blocks are emitted, in
  source order, into every solver input that needs them -- one whose
  VCs apply a spec function, or a module with reflected definitions;
  a block in a module with neither is never even elaborated (see
  mechanics/lean_embed_err.ml).  A solver error inside a block is
  reported at the block's own location (with the line within the
  block).
  Like `assume_unchecked_` and `-vox-prelude`, an embedded block is
  trusted (an `axiom` proves anything).
- Native immutable arrays: [int iarray] is a built-in theory.
  `Iarray.length a` and `Iarray.get a i` (surface sugar `a.(i)`)
  appear in predicates and reflect in expressions (recognized by
  PRIMITIVE, gated on the [int iarray] type -- the mutable array's
  identical primitives do not reflect); values sort at an opaque
  [VoxIA] with reserved operations and ONE compiler-owned axiom,
  length nonnegativity.  [get] is TOTAL in the logic, like division:
  the safe program get raises out of bounds, so no value flows there
  and the unconstrained fact is vacuous (partial correctness) --
  bounds SAFETY is an opt-in contract, e.g.
  `(i : int{ 0 <= _ && _ < Iarray.length a })`, and such a wrapper is
  then PROVED, not assumed (see demo/lean_binsearch.ml, which assumes
  nothing).  Element sorts beyond int are future work.
- The library layer (testsuite/tests/vox/lib): the userland trusted
  base lives in ONE place -- the heap-theory encodings (sep_lib,
  borrow_lib, pcell_lib), the verified bst, and the bounds-contract
  array reads (ia_lib, which assumes nothing) -- shared by the demos
  through cross-directory [modules] references.  `grep
  assume_unchecked_ testsuite/tests/vox/lib` IS the userland trust
  audit.  Specs travel to the demo clients through the .cmi (specced
  signatures); promotion to a real build unit awaits API stability,
  and the sep/borrow/pcell crowning decision is the Wishlist's.
- Ghost sorts: `type g [@@vox.sort int]` (or `bool`) declares that an
  abstract type's LOGICAL REPRESENTATIVE is its value at a base sort:
  values of `g` are modelled as opaque Ints instead of at the
  uninterpreted sort, so refinements can use them directly as the
  values they stand for (prophecies, refs denoting their contents).
  TRUSTED: the declaring library asserts every fact it issues is true
  of that interpretation -- the attribute is the module's axiom, in
  the same trust class as `assume_unchecked_` and solver blocks.  A
  malformed or aliased `[@@vox.sort]` is an eager error (a typo on a
  ghost type must not silently degrade to VoxU).  Relation to the
  pcell separation tokens: both are library encodings of interior
  mutability (tokens carry the cell's contents as facts; a ghost sort
  lets the value DENOTE them); no doctrine yet picks between them --
  see the Wishlist.  See mechanics/lean_vox_sort.ml.
  KIND SYNTAX: the modeling may equivalently be declared in the
  type's kind -- `type t : value refines int` (or `bool`) -- which is
  the same trusted assertion carried as declaration METADATA in the
  jkind: the kind algebra never combines or solves it (it is a peer
  of the layout, inert for programs that never write it), the
  verifier reads it through the environment (so it survives
  abstraction, and applies to every instance of a parameterized
  head), and it is checked exactly once, at signature inclusion: an
  interface may declare `refines` only if the implementation carries
  the same modeling -- by its own annotation or attribute, or
  structurally (`type t = int` satisfies `refines int` unannotated).
  Sharp edges, v1: the component does not PRINT (a `refines` kind
  displays as its base), and only the base sorts are declarable
  (datatype sorts remain structural).  See mechanics/refines_kind.ml.
- Specced signatures: blocks in an `.mli` are EXPORTED through the
  `.cmi`, together with pre-rendered declarations of the datatypes the
  interface's refinements are about (a client may never mention those
  types itself, yet the spec references them; clients deduplicate the
  declarations by their stable names, and a local type may not shadow
  an imported declaration at the same solver-side name with a
  different shape).  Every client -- including the unit's own
  implementation, which reads its interface like any other import --
  receives the spec in dependency order.  The definition travels with
  the defining module, so a client can never verify against a
  DIFFERENT version of a spec function used in an imported signature,
  and editing a spec changes the `.cmi` (its CRC), forcing clients to
  re-verify.  Two imported units exporting the same spec name is a
  duplicate solver definition: verification fails, blaming the
  colliding unit's block (spec functions are not yet unit-namespaced).
  Blocks in an `.ml` (of a unit with an `.mli`) stay module-local.
- The compiler attaches logical meaning to exactly the operations the
  predicate language models (Vox_reflect): variables, int/bool
  constants, immutable field reads of simple records (the projection
  the predicate language writes as `_.px`; a mutable field
  disqualifies the record, and its reads stay fresh unknowns),
  `+ - * ~-` (and `succ`/`pred`), `&& || not`, and comparisons at int
  or bool -- recognized by PRIMITIVE, so shadowing `(+)` cannot be
  mistaken for integer addition. So built-in operations need no
  userland wrappers:

      let mul : (x : int) -> (y : int) -> {z:int | z = x * y} =
        fun x y -> refine_ (x * y)
      let zero : {v:int | v = 0} = refine_ 0

  (both obligations are trivial: `x * y = x * y` and `0 = 0`).
  Operations and facts BEYOND the translatable fragment are still
  defined in user code with `assume_` / `assume_unchecked_`. Constants
  are exported refined and unpacked at use: facts are module-local,
  types travel.
- CAVEAT: the logic's ints are unbounded while the machine's wrap, so
  reflecting `+ - * / mod` equates modular with ideal arithmetic; overflow is
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
FAILURE, never success. Facts about names come from these places
(each fact enters once: the emitter deduplicates a fact delivered by
two channels, e.g. a binder fact and its selfification equation):

- Binders: a binder of refined type contributes its refinement at the
  binder (binders-as-facts), and a refined PARAMETER contributes its
  contract predicate.
- Selfification: a let-bound translatable expression contributes its
  defining equation (`let s = l + r` carries `s = l + r`).

- Unpacking: `let refine_ x = e` with `e : {v | p}` contributes
  `p[v:=x]` and binds `x` at the SKELETON -- the weakening spelling,
  for a refined value that must flow somewhere unrefined (a plain let
  carries the same fact but keeps the refined type).  Matching
  `xs : {v:int|v>0} list` against `x :: _` makes `x > 0` available.
- Path facts: `if c then e1 else e2` checks `e1` under `c = true` and
  `e2` under `c = false`, where `c` is the condition's name -- so a
  translatable compound condition contributes itself:
  `if 0 < x then ...` checks the branch under `0 < x` directly.
- Dependent application: applying `f : (x:int) -> {z | p}` to variable
  `a` substitutes: the result type is `{z | p[x:=a]}`, so
  `let refine_ m = mul a b` yields `m = a * b`.
- Match facts (the match refines the thing it matched on): the
  scrutinee of `match s with ...` may be ANY expression -- a variable
  matches at its stamp, a module-level value at its path name, and
  anything else at a NAME for the one evaluation being matched (its
  logic translation when it has one, a fresh unknown otherwise).  A
  case whose pattern is one constructor of a simple variant over
  variables or wildcards checks its guard and body under
  `s = C x1 ... xn` (wildcards name fresh unknowns); a simple-record
  pattern contributes `xi = s.li` per variable sub-pattern
  (per-field, so partial patterns are fine).  `let p = e in ...` gets
  the same facts, so destructuring binds components logically --
  `let (p, b) = unpack x` ties `p`/`b` to the projections of the
  call's name, and the result's REFINEMENT holds at that name too
  (recovered from the callee's instantiated result type where
  implicit erasure dropped it: the erasure argument was that an
  unnamed value's fact is unreachable, and the destructuring is
  exactly the naming).  Exception and effect arms receive none of
  this (the scrutinee was interrupted, so there is no value the name
  could denote).  A bare VARIABLE arm aliases the
  scrutinee (`match s with y -> ...` learns `y = s`).  Deeper patterns
  -- nesting, aliases, or-patterns, constants -- contribute nothing,
  which is sound.  `function`-cases are a match on the anonymous
  parameter: a refined domain contributes its contract at that
  parameter, and the arms get the same match facts and negations
  against it (the case patterns type against the skeleton, like the
  other parameter spellings).
  NEGATIVE facts: an arm also learns that every EARLIER arm failed to
  match, usable exactly when that failure is decided by the head
  alone: each guard-free earlier arm of the simple shape contributes
  `not (s is C)` (an internal constructor tester, not surface syntax).
  Guarded arms contribute no negation (the pattern may have matched
  with the guard false), nor do arms with refuting sub-patterns
  (`A 0`: the head may have matched anyway).
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
testsuite/tests/vox/demo/lean_spec.ml).

A plain `let x = e` SELFIFIES: when `e` has a stable logical name
(its reflection, a constructor term, an immutable field read), the
binding contributes `x = name(e)` -- so `let s = l + r` carries
`s = l + r` with no `refine_` in sight, aliasing just works
(`let y = x` gives `y = x`), and an unpack `let refine_ x = e`
additionally remembers WHICH value it opened.  An RHS the logic
cannot name (a call, a mutable read) contributes nothing: `x` is a
fresh unknown.  RECURSIVE bindings contribute no self fact: a cyclic
constructor equation (`let rec ones = 1 :: ones`) is unsatisfiable in
the solver's well-founded datatype theory, which would poison the
hypotheses.

`refine_ e` at expected type `{v | p}` yields the VC
`facts |- p[v := n]`, where `n` is `e`'s name.

End-to-end example, one VC, provable -- the comparison is reflected
directly into the path fact, and `100` may be passed directly since
`div`'s first parameter occurs in no refinement:

    let div (a : int) (b : {v:int | not (v = 0)}) : int = a / b
    let safe x =
      if 0 < x then div 100 x else 0

The path fact `0 < x` proves the contract obligation `not (x = 0)`
at the call.
The same works through a binding (`let c = 0 < x in if c then ...`:
selfification carries `c = (0 < x)`, plus the path fact `c`); a
dependent userland operation is needed only for facts beyond the
translatable fragment, established with `assume_`/`assume_unchecked_`
(the reflected fragment needs no wrappers).  A dependent type must be
written as an annotation, `(x : int) -> ...`: a refinement written
directly in terms of a lambda's parameters would name them as free
program variables, which may not appear in the function's own type --
the escape checks reject it, with the dependent arrow as the
sanctioned spelling.  The direct binding spelling
`let [rec] f (x : t) ... : r = e` is HOISTED into that arrow at
elaboration whenever an annotation carries a vox refinement or named
type (all parameters become dependent binders, so `r` and later
parameters may mention them, and a `let rec` is typed at the full
contract from the start); the hoist is purely syntactic, gated to
vox-typed bindings, and fails open on shapes it does not recognize
(labelled or unannotated parameters, nested lambda chains, missing
result annotation).

A `-dump-vc` flag prints every VC (hypotheses, goal, source location);
`-vox-dry-run` skips the solver, so VC generation is testable without
a solver (see testsuite/tests/vox, promoted like other reference tests).
A failed obligation reports the goal, the hypotheses, and -- when
grind's linear solver leaves one -- a POSSIBLE COUNTEREXAMPLE: its
arithmetic model, rewritten to source names ([a = 0, a#2 = 1],
[fib 4 = 3]).  On linear goals it is a genuine refutation; nonlinear
monomials and theory atoms appear as their own entries, so a model
that is only linear-consistent is visibly so.

## Mutable locals (flow-sensitive SSA versioning)

[let mutable] variables verify flow-sensitively.  Each live mutable
variable has a current logical VERSION (m, m@1, m@2, ...): reads --
including inside reflected expressions, path conditions, and match
scrutinees -- name the version in force at that program point, and
every write mints a fresh one.  Two kinds of facts arise:

- The definitional equation [m@1 = m + 1] of each assignment is a
  Skolem-style definition (each version defined once, from strictly
  earlier names), hence a conservative extension usable in EVERY
  execution; equations are pulled into each VC by relevance.
- The declared refinement instantiated at the new version is a THEOREM
  proved under the assignment's path condition (rigid typing forces
  every write through refine_), so it stays path-scoped: it would be
  unsound in a sibling branch.

Joins and havoc: an [if] whose condition reflects joins with
[(c && m' = m_then) || (not c && m' = m_else)]; matches and
untranslatable conditions havoc (a fresh unconstrained version).
Loops havoc the variables they write at the loop head -- head versions
denote any iteration's entry -- and the continuation of a [while] adds
the negated condition; [for] bodies see reflected bounds
[lo <= i && i <= hi].  Declared refinements survive every havoc (each
write re-proved them): a rigid refinement on a mutable local IS a loop
invariant, e.g.

    let mutable total : {v:int | v >= 0} = refine_ 0 in
    for i = 1 to n do
      let refine_ t = total in
      total <- (refine_ (t + i))
    done

Constructs the walker does not model (application arguments and other
unspecified-evaluation-order positions, try, ...) havoc every variable
their subtree writes; this is COMPLETE, not just conservative, because
closures cannot capture mutable variables, so every mutation is a
syntactic assignment.  The type-level bans stand: mutable stamps still
may not appear in refinements or dependent applications ([let x = m]
pins the current value to an immutable name, with the fact
[x = m@k], and is the sanctioned bridge).

## Loop invariants

Refinements live at the EDGES (signatures, module boundaries); inside
code everything is unpacked to plain values plus the logical
environment.  A loop invariant belongs to the latter: it is a FORMULA
over program variables, not a refinement type -- it never travels and
is never compared -- written as an attribute on the loop:

    (while hi - lo > 1 do ... done)
    [@vox.invariant -1 <= lo && lo < hi && hi <= len a && ...]

Mutable variables may appear (only here): the formula is a template
that the walker instantiates at each boundary point over the
variables' current SSA versions.  The discipline is the classical
quadruple, i.e. exactly how the loop's tail-recursive encoding with a
ghost [unit{...}] parameter would verify: the formula is ASSERTED over
the entry versions (the first call), havoc, ASSUMED over the head
versions (an arbitrary call's parameters), ASSERTED over the body-exit
versions at the back-edge (the recursive call), and after the loop the
head assumption stands with the negated guard.  Mentioned mutables
must be tracked at the loop (a loop inside a closure cannot see the
enclosing function's mutables); written-but-unmentioned variables
havoc as usual.  With invariants, mutable variables are TYPICALLY NOT
REFINED: their per-point facts come from versions, their loop-stable
facts from the invariant, and declared refinements remain merely an
option for continuous per-variable value-properties.

A FOR-loop invariant elaborates in the body's environment, so it may
mention the index.  The quadruple is then index-aware: the entry
assertion instantiates the index at the first value, the back-edge
assertion at the NEXT value (what one iteration establishes is the
next iteration's head state), and the post-loop assumption stands at
the one-past-the-end value when the loop ran -- at the first value
otherwise, where it is just the entry assertion over unchanged
variables.  Bounds are NAMED (their reflection when translatable, a
fresh unknown otherwise): one name per bound serves the head bounds
and the entry/post-loop instances alike, so even an opaque bound
([for i = 0 to f ()]) yields a consistent quadruple.

Single-arm matches -- what unpacks [let refine_ x = e] and
destructuring lets desugar to -- are straight-line code: the walker
threads the arm's out-context (facts and versions) to the
continuation instead of joining, so facts established inside a loop
body reach the back-edge assertion.

Threading stops where control can be INTERRUPTED.  An arm containing
an exception pattern, a try handler, or an effect arm can be reached
with the scrutinee stopped between writes: such arms -- and the
continuation of a match that has one -- receive the pre-scrutinee
state with everything the scrutinee writes havocked, never its
threaded versions or facts.  Children of unmodeled constructs
(application arguments, tuple components, ...) evaluate in
unspecified order, so each child likewise sees the subtree's writes
havocked rather than a sibling's threaded version.

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
  refined at the NEXT activation's (see testsuite/tests/vox/mechanics:
  [countdown] in vc_pi.ml accepted, [unsound] in errors.ml rejected). The declared refinement
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
- [ ] strong update for @ unique mutation (the library encodings so
  far: pcell separation tokens, [@@vox.sort] trusted ghosts)
- [ ] RustHorn style borrows via block indices
- [x] Algebraic data types with refined constructors
- [x] simple records, native tuples
- [x] total_ (reflected) functions, cross-module through the .cmi
- [x] embedded [%%vox.lean] blocks and specced .mli signatures
- [x] quantifiers (forall_/exists_) and implication in predicates
- [ ] a reconciled story for interior mutability (pcell tokens vs
  [@@vox.sort]; two library encodings await a doctrine)
