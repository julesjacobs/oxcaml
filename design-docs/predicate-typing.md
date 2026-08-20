# Vox predicate typing

Refinement predicates become well-typed booleans. Today `int{ 42 }`,
`int{ _ + "x" }` and `string{ String.length _ }` are legal types: the
type-formers piece resolves names and gates the sublanguage syntactically,
but applies no typing judgment ("the typing rules for refinements belong to
a later piece" — types.mli, type-formers.md). This is that piece.

After it, every predicate is checked, at the point the type is formed, to
be a `bool` under an environment where the hole and the dependent-arrow
binders have appropriate types — and the checked result is kept: the
mirror carries each node's type and the identities Typecore selected, so
the later translation piece (and any consumer of an imported predicate)
never re-derives a judgment it cannot reproduce.

This doc was review-looped before implementation; the first draft proposed
a standalone rerunnable typer over the resolved mirror with nothing
stored. The loop falsified that design's premises (see "The road not
taken"), and the design below is the reviewed replacement.

## The judgment

For a refinement type `t{ p }` formed anywhere `transl_type` runs
(expression annotations, declarations, signatures, functor bodies):

    E ; Γ ⊢ p : bool

where `Γ` extends the ambient typing environment `E` with:

- the hole `_` at the *payload* type `t` (every occurrence — `_` means the
  value of the innermost enclosing refinement);
- each dependent-arrow binder in scope, as an ordinary value binding (see
  "Binder types" for what type);
- the predicate's own binders (`let`, `fun`, `match`) as ordinary
  bindings, by ordinary inference.

**The judgment is Typecore's, at mode total, over logical spec entities.**
The predicate is checked against `bool` at an expected mode requiring
`Total` on the totality axis (legacy is Partial, so this is a real
constraint): a call to a partial function inside a predicate is a mode
error, a `fun` inside a predicate is a total closure, and mutable access
fails exactly as the totality piece specifies. The hole and the dependent
binders enter the environment `@ logical` (monadic Logicality): spec
entities, usable in logical positions, unable to leak into physical ones.

**Comparison admission (predicate-scoped).** The totality allowlist
excludes comparisons (polymorphic compare raises on functions, diverges
on cyclic values), which would reject `int{ _ > 0 }`. Inside the
predicate judgment, comparison primitives are admitted as total when
their operand type is immediate — vox2's rule, which the totality
piece's decision log names as the unported follow-up, and which matches
the solver-modeled set. Stated plainly: `string{ _ = "x" }` is rejected
until the modeled set grows, and `/` / `mod` remain partial in
predicates. The admission lives in this judgment, not the global
allowlist. (Chosen over kind-constrained total comparison externals —
the principled long-term unlock, a piece of its own — and over a spec
prelude of wrappers, which would make `Stdlib.(>)` unusable in
predicates.)

**The judgment is Typecore's**, in the ordinary sense: Predicate expressions are typed by the
real expression typer, re-entered through a forward-declared hook
(`Typetexp.type_refinement_predicate`, installed by `Typemod` — the
`type_open`/vox2 precedent), against expected type `bool`, inside a
protected transient frame. Everything that makes expression typing
correct — type-directed constructor and field disambiguation, labelled
application with commuting, principal-mode behavior, level discipline,
ordinary error messages located at the offending subterm — is inherited
rather than reproduced.

Because the typer is Typecore, predicates follow **refinement-flow's
occurrence rules exactly and automatically** — head-only, contextual
stripping at every site refinement-flow strips (identifier occurrences,
application results, field reads, destructuring patterns; the
implementation's list is authoritative), never a deep strip —
`int{p} list` stays distinct from `int list` — alias-expansion rollback
included. `x : int{q}` used in a
predicate as `x + 1` works because the occurrence strips the head, the
same way it does in program code. Refinement obligations recorded by
Typecore *inside* a predicate are discarded with the frame: a refined
constraint in a predicate checks against its payload but records no
obligation (predicates are specs; giving their interior constraints
solver-facing meaning is a later piece's decision — recorded restriction).

### The reentry transaction

The reentry is a transaction over Typecore's ambient state, not just a
`Fun.protect` over a few refs. Its contracts:

- **The hole bridge.** Ordinary `type_expect` raises `Unexpected_hole` on
  `Pexp_hole`. Before reentry, the predicate's holes are rewritten
  (capture-avoidingly, not descending into constraint *types* — a nested
  refinement owns its own holes) to one fresh synthetic value bound at
  the payload type; the mirror build maps that ident back to
  `Rexp_hole`.
- **Modes are checked for real.** There is no mode-isolation carve-out:
  the reentry presents the true environment and the expected mode carries
  Total; a predicate reading ambient values walks the ordinary locks. The
  earlier draft's isolation existed only because mode discipline was
  deferred; it is deleted, not generalized.
- **Type variables.** A complete reentrant `TyVarEnv` frame: the
  enclosing declaration's named variables remain visible (`'a{ _ = 0 }`
  pins `'a = int` — ordinary inference, wanted); new named type
  variables may not be introduced by a predicate. Interior
  `transl_simple_type` calls (constraint types) must not clear the
  enclosing bookkeeping.
- **Rollback.** Unification links are destructive: the frame takes a
  `Btype` snapshot, backtracks it on *any* failure through the end of
  mirror construction, and commits on success (so successful ambient
  constraints stick). Delayed checks and allocation bookkeeping are
  isolated, run for the predicate, and restored. Typecore's cmt saved
  expressions produced under the frame are discarded (predicates are not
  program expressions; the mirror is their record).

### Binder types: two-phase domain formation

A dependent binder scopes over its *own domain* (`x:(int{ x > 0 } * int)`
is a fixture today), so its type cannot be known before the domain
translates. A fresh-placeholder scheme fails here (review round 2): a
predicate typed before the domain completes solves the placeholder to a
bare shape, and reconciliation against the completed domain then hits the
rigid refined/bare mismatch — and an own-domain binder's stored node type
would contain the very predicate that contains it.

So domains form in **two phases**: the domain type translates first, with
every predicate encountered inside it *queued* (its gated parsetree plus
its binder-scope snapshot) rather than typed; when the domain type is
complete, the queued predicates are typed with each binder bound to its
completed declared (payload-headed) type — Typecore's occurrence rules
then strip heads at uses, as everywhere. Predicates outside a binder-carrying
domain type eagerly. While any such domain is being translated, every
predicate nested in it queues, including one that does not itself mention the
binder. Consequences, both wanted:

- `x:(int{ x > 0 } * int)` becomes a type error (`x` is the whole tuple,
  not an int) — current fixtures that accept it flip in the RED/GREEN
  diff;
- `x:(int{ fst x > 0 } * int)` types: `fst x`'s application result strips
  its head per the occurrence rule, and the binder's type was complete
  before the predicate was typed. The order-sensitivity pair
  (early-annotation accepted / late-annotation rejected) from the review
  is pinned as a fixture.

**No stored type on `Rexp_var` or `Rexp_hole`.** Their types are
contextual — the binder's declared type and the innermost payload — and
storing them would create a metadata cycle for own-domain binders (the
domain contains the predicate contains the node whose type is the
domain), which `Btype`'s occur-check traversal must never meet. Every
other node stores `rexp_type`.

### What is stored

`Types.refinement_expression` becomes a **typed mirror**. The parsetree
is the *shape* authority and Typecore's typedtree the *annotation*
authority, joined by an explicit correspondence (the trees are not
isomorphic: source constraints are `exp_extra`s, one `Texp_function`
carries all parameters where the mirror nests unary `Rexp_fun`s,
`Texp_construct` flattens argument tuples, and application arguments are
reordered into function-type order). Application correspondence consumes
same-label, same-anchor matches in source occurrence order; a unique anchor is
the fallback when label representations differ. Colliding locations such as
repeated `Location.none` are never reusable keys, and ambiguous pairings are
rejected. Concretely, the mirror
keeps source shape, order, constants, grouping and locations from the gated
parsetree exactly as today; the correspondence walk supplies, per source node,
the type, the selected constructor/field identity, and binder `Ident.t`s.

Where the typedtree contains what the mirror cannot say, the
*judgment's* representability validation has already rejected it (see
"Two phases"); the build itself is total. Polymorphic `let` inside
predicates is **allowed** (ordinary Typecore generalization; each use
site's mirror node carries its instance).

Stored annotations:

- `rexp_type : type_expr` on every node except `Rexp_var`/`Rexp_hole`
  (see "Binder types"); in the type graph: `Btype`/`Subst`/freshening
  traverse it, `.cmi`s carry it. Before persistence, each stored annotation
  is scanned under the mirror binders in lexical scope: a free value path in a
  nested refinement that denotes a predicate-local binder is promoted to
  `Rexp_var`. Type copying preallocates fresh stamps for all mirror binders
  before mapping stored types, because an outer function node's full arrow type
  can mention parameters from nested `Rexp_fun` nodes. A raw-CMI/functor-copy
  fixture covers both written constraint types and an `Rexp_fun` result
  annotation, and a direct substitution check rejects the old binder stamp.
- Constructor identity as today's substitutable path form, but selected
  by Typecore (post-disambiguation).
- **Field identity as `(parent record type path, label name)`** — `Path.t`
  has no field constructor, so the pair is the concrete substitutable
  key; the parent path rewrites under `Subst.type_path`, and the key
  joins mirror equality and the free-path/dependency scans. This closes
  the type-formers-era functor-parameter-record gap.
- Mirror **equality stays syntactic** over shape + written constraint
  types + the identity keys; derived node types are ignored by
  `Vox_rexp.equal`.

The *syntactic gate* (totality by construction, unsupported-forms rejection)
stays as a pre-pass over the parsetree, so **no syntactically rejected form
reaches Typecore**. The consolidated gate admits simple predicate-`let`
annotations that have a faithful constraint preimage and continues to reject
the other annotation forms (effectful-by-type expressions — a
well-typed call to a partial function, a qualified mutable access — do
reach it and are accepted by this piece; the mode piece decides their
fate later).

### Recursive declarations and signatures

- Inside a recursive type group, the temporary environment gives group
  members `Type_abstract` kinds; a predicate mentioning a *constructor or
  field* of its own group cannot resolve it. That is today's behavior;
  this piece keeps it and turns it into a located error with a fixture
  (structures and signatures both). Lifting it (re-checking predicates
  against the completed group) is deferred and recorded.
- Signature items are typed sequentially: a signature predicate sees
  earlier items only. Imported predicates are never re-elaborated — the
  typed mirror in the `.cmi` is the authority; consumers only
  instantiate/substitute it, which is why the identities stored must be
  paths that `Subst` rewrites.

## The road not taken (recorded, with the falsification)

A standalone rerunnable typer over the resolved untyped mirror, storing
nothing, was drafted and review-looped. It fails on facts of this tree:

- the mirror is not resolved enough to type: constructor identity is
  frozen by first-candidate lookup before any expected type exists,
  and fields are unresolved longidents — reproducing Typecore's
  type-directed disambiguation outside Typecore is a clone, not a
  small judgment;
- translation must handle *imported* predicates, whose source parsetree
  does not exist in the consumer — "re-run the judgment on demand"
  cannot run; types must persist;
- rerun determinism is not structural: recursive-group temporaries,
  principal-mode sensitivity, and signature-local scope all make "same
  code, same verdict" a semantic obligation the design could not meet;
- the claimed performance motivation (vox2's 31% profile line) measures
  syntactic alpha-equivalence, which an untyped mirror pays too — it
  does not measure typed storage.

Persistent node types cost mechanical traversal work in `types.ml`,
`Btype`, `Subst`, equality, and `.cmi` size. That cost is accepted,
bounded, and paid in one piece — and it is what the translation piece
needs anyway. A queued batch types every predicate once for bootstrap, at
least once while stabilizing, and once for authoritative warning replay; the
defensive worst case is quadratic in the number of predicates in one
binder-carrying domain.

## Two phases: a fallible judgment, then a total translation

Everything that can reject lives in one phase; the mirror translation
cannot fail. Concretely:

1. **Judgment (fallible)**: the syntactic gate; hole rewriting; the
   Typecore reentry at `bool @ total` with logical spec entities. Every
   totality, logicality, and type rejection is a located error of this
   phase, and the rollback snapshot's scope is exactly this phase.
2. **Translation (total)**: `mirror_of_typedtree` — the
   parsetree-shape/typedtree-annotation correspondence — cannot fail on
   anything the judgment admits. The formerly rejected typedtree forms
   are **represented** rather than rejected:
   - application argument synthesis gets explicit mirror forms — an
     omitted optional, a defaulted optional, an `%call_pos` argument and
     an `Omitted` required label are distinct argument entries alongside
     ordinary source arguments, so source order and the callee's
     completion are both recorded;
   - `%apply`/`%revapply` keep their source shape with the primitive as
     the applied identity; format-string typing records the rewritten
     application the way the typedtree has it, anchored to the source
     literal;
   - GADT and existential-introducing constructors appear in patterns as
     ordinary constructor patterns; existential types introduced by an
     arm are scoped to that arm's stored types. If existential scoping in
     the persisted mirror proves disproportionate to implement, the
     implementor may keep a narrow judgment-phase rejection for exactly
     that case, recorded as a decision — the principle is represent,
     don't reject.

   The *solver translation* (a later piece) is where per-obligation
   "unsupported feature — will be supported in the future" reports live:
   the mirror is a faithful record; modelability is the backend's
   judgment, made per obligation and fail-closed.

With totality checked up front, "checked predicate" entails "mirror
exists", unconditionally; nothing downstream ever holds a checked
predicate it cannot at least record.

## Deliberately out of scope

- **Obligations inside predicates** (refined interior constraints are
  payload-checked, nothing recorded) — revisit with the translation
  piece.
- **Same-group constructor/field mentions** (located error, above).
- **Any solver-facing semantics** (BV63 vs Int, modelability).

## Compatibility fallout (expected, wanted)

Fixtures whose predicates are ill-typed flip to errors; the RED commit
pins today's acceptance (new fixture file plus the pre-existing fixtures
that change), GREEN lands the judgment and re-promotes, and the
expectation diff is the demonstration. Known flips include the
binder-in-own-domain tuple fixtures and any hole-at-non-bool uses; the
RED promotion enumerates the rest mechanically — the doc deliberately
does not hand-maintain that list. Signature/structure inclusion messages
must not regress. `.cmi` shape changes (typed mirror), so magic-number
hygiene and full-suite reference churn are expected.

## Tests

`testsuite/tests/vox/predicate_typing.ml` (expect):

- Rejections, located: `int{ 42 }`, `int{ _ + "x" }`,
  `string{ String.length _ }`; acceptance: `int{ _ > 0 }`.
- Holes: multiple occurrences, nested refinements (innermost payload),
  under `let`/`match`/`fun`.
- Binders: bare and `~x:` labelled at payload; refined binder head-strip
  (`x:int{q} -> int{ _ > x }`); binder-in-own-domain flips; predicate
  binders shadowing arrow binders.
- Disambiguation: two records sharing a label name, two variants sharing
  a constructor, selected by expected type inside a predicate — the
  fixtures that a first-candidate resolver gets wrong.
- Application: labelled commuting, partial application of a labelled
  function, `Optional`/`Position` callee rejection (all four spellings).
- Polymorphic `let` inside a predicate (`let id = fun x -> x in
  id 0 = 0 && id true` accepts — pins Typecore generalization).
- Occurrence strip inside predicates: application-result head strip,
  nested heads intact (`int{p} list` element projection).
- Refined interior constraint: payload-checked, no obligation recorded
  (probe via -drefinements).
- Ambient-variable constraint: `'a{ _ = 0 }` pins `'a`; principal-mode
  double-take fixtures for the disambiguation cases.
- Mode discipline: a partial call in a predicate rejected; a mutable
  access rejected; comparison acceptance at int / rejection at string;
  `/` and `mod` rejected unguarded; a logical hole flowing to a physical
  argument rejected; a nested `fun` checked total.
- Recursive groups: same-group constructor AND field mention errors
  (structure and signature); non-group mentions fine.
- GADT/existential constructor pattern in a predicate match: located
  rejection.
- Binder order-sensitivity: the early-annotation/late-annotation pair
  from the review (annotation after a constraining use is a clean
  error, not an incidental unification mystery).
- A predicate-local binder occurring in a nested refinement, exported
  and freshened across `.cmi`/functor copy.
- Cross-module: refined type in an `.mli` consumed from another unit;
  `.cmi` round trip of node types and selected identities (a
  disambiguated field keeps its producer-side identity under the
  consumer's different environment).
- Frame hygiene: a failing predicate inside a signature does not corrupt
  subsequent typing (error-then-continue fixture).

## Piece mechanics

Branch `jujacobs/vox/predicate-typing` off the stack tip (`32b38a5527`,
refinement-flow) — the occurrence rules this piece inherits are
refinement-flow's. Red-green commits as above. The mirror change is a
`.cmi`-shape change: stdlib and test-install refreshes are part of the
build discipline.

## Decisions taken during consolidation

- **Lane A (`impl-fable`) is the implementation base.** It keeps the intended
  Typetexp-gate/queue and Typecore-correspondence split, preserves binding
  annotations and signature-local value identities, contains no generic
  dev-loop tooling, and has the smaller semantic diff and broader RED. Lane B
  remains the source for all-failure rollback, reversible mode state,
  contextual variable annotations, annotation closure, and direct `.cmi`
  inspection tests. Neither lane is used unchanged; the review-verified
  defects are repaired in the consolidated GREEN.
- **Queued mirrors are installed at atomic barriers.** A payload-only bootstrap
  types every job before installing any seed; strict whole-batch passes then
  iterate to a fixed point and one authoritative replay emits warnings. A
  non-semantic refinement identity survives frame and ordinary copies, so the
  bootstrap recognizes an instantiated view without sharing its mirror cell.
  Historical batches remain live mutable type graphs and are not sound cycle
  keys, so stabilization uses linear defensive fuel and rolls the entire batch
  back on exhaustion; this conservatively trades a possible pathological
  false rejection for guaranteed termination. Exhaustion and a divergent
  warning replay are located source errors materialized before rollback, not
  internal compiler aborts.
- **Written structure and derived annotations have separate traversals.**
  Occurrence, universal-escape, well-foundedness, and variance checks visit the
  payload plus written constraint types. Copying, substitution,
  generalization, and persistence also visit stored node annotations.
- **Predicate reentry is fully transactional.** Every failure through mirror
  construction freezes its diagnostic before `Btype` rollback; delayed checks,
  allocation state, saved cmt expressions, warning state, and mode-solver
  changes are framed. The type-only boundary enters ghost context. Successful
  ambient type constraints still commit. Mode constraints from the transient
  frame roll back; if a committed weak variable becomes an arrow, its otherwise
  unconstrained modes default conservatively at the phrase boundary. The
  local-argument fixture pins that observable conservative boundary; it does
  not independently isolate the mode-rollback implementation.
- **Frame views copy every non-variable spine.** This includes polymorphic
  variants and first-class packages, object field/poly spines, and unboxed
  tuples as well as constructors, arrows, boxed tuples, and refinements.
  Ambient variables remain shared; only a refinement's predicate update cell
  and non-semantic identity are additionally shared. Every bound `Rexp_var`
  remains contextual, and other stored node types are closed before
  persistence.

Before a queued predicate has produced its typed mirror, an error that prints
its enclosing completed binder can show the internal `{ _ }` placeholder
rather than the written predicate. Retaining unresolved source syntax in
`Types` solely for this bootstrap-only diagnostic would broaden the marshaled
representation or reintroduce a parsetree mirror walker; this cosmetic
limitation is accepted. Successful stored mirrors are unaffected.
- **The shared artifact magic version advances from 583 to 584.** The Types
  representation change affects marshaled CMI/CMT data; the common version
  source intentionally changes all generated artifact magic strings together.
