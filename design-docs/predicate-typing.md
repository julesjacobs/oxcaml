# Vox predicate typing

Refinement predicates become well-typed booleans. Today `int{ 42 }`,
`int{ _ + "x" }` and `int{ String.length _ }` are legal types: the
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

**The judgment is Typecore's.** Predicate expressions are typed by the
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
- **Mode boundary.** Predicates are erased specs: reading a value inside
  one must not constrain the enclosing program's closure locks, totality
  or other modes (a partial call in a predicate must not make the
  enclosing closure partial). The reentry env presents no ambient locks,
  and mode constraints arising inside the predicate are confined to the
  frame. This piece checks types only.
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
then strip heads at uses, as everywhere. Predicates in a codomain (all
binders already completed) type eagerly; a predicate with no dependent
binders in scope types immediately. Consequences, both wanted:

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
reordered into function-type order). Concretely: the mirror keeps source
shape, order, constants, grouping and locations from the gated parsetree
exactly as today; the correspondence walk supplies, per source node, the
type, the selected constructor/field identity, and binder `Ident.t`s.
Typedtree forms with no faithful preimage are rejected with a located
"not supported in a refinement predicate": applications the typechecker
completes or reorders beyond the source (required-label omission —
`Omitted` arguments), `Optional`/`Position` arrows anywhere in an applied
callee's type (all four spellings: `~opt:v`, `?opt:o`, omitted-optional
synthesis, implicit `%call_pos`), `%apply`/`%revapply` and format-string
rewrites, and GADT / existential-introducing constructors in patterns.
Polymorphic `let` inside predicates is **allowed** (ordinary Typecore
generalization; each use site's mirror node carries its instance).

Stored annotations:

- `rexp_type : type_expr` on every node except `Rexp_var`/`Rexp_hole`
  (see "Binder types"); in the type graph: `Btype`/`Subst`/freshening
  traverse it, `.cmi`s carry it. **Traversal becomes binder-context-aware**:
  `Vox_rexp.map`'s type callback receives the current rename map, so a
  predicate-local binder occurring in a nested refinement's stored types
  freshens with its binder — the existing callback closes over only the
  outer map, which this piece fixes (with a `.cmi`/functor-copy fixture
  for exactly that shape).
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

The *syntactic gate* (totality by construction, unsupported-forms
rejection) stays as a pre-pass over the parsetree, unchanged in behavior,
so gate errors keep their current messages and **no syntactically
rejected form reaches Typecore** (effectful-by-type expressions — a
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
needs anyway.

## Deliberately out of scope

- **Mode discipline** (predicates checked total + logical; comparison
  admission tied to the solver-modeled set): its own piece, blocked on
  the comparison-admission and ghost-interaction rulings. This piece
  checks types only; the syntactic totality gate stays. Well-typed must
  not be read as solver-admissible: positive fixtures pin a well-typed
  call to a partial function and a qualified mutable access as accepted
  *by this piece*, so a later mode policy shows up as a diff.
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
  `int{ String.length _ }`; acceptance: `int{ _ > 0 }`.
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
