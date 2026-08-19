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
occurrence rules exactly and automatically**: head-only, contextual
stripping of refined heads at identifier occurrences and application
results (never a deep strip — `int{p} list` stays distinct from
`int list`), alias-expansion rollback included. `x : int{q}` used in a
predicate as `x + 1` works because the occurrence strips the head, the
same way it does in program code. Refinement obligations recorded by
Typecore *inside* a predicate are discarded with the frame: a refined
constraint in a predicate checks against its payload but records no
obligation (predicates are specs; giving their interior constraints
solver-facing meaning is a later piece's decision — recorded restriction).

### The transient frame

The reentry runs under `Fun.protect` saving and restoring the typing
state Typecore mutates: delayed checks, allocation bookkeeping, levels
(the vox2 frame at its typecore.ml:4869-4893 is the hazard map; each item
gets a comment naming why it is saved). Predicate typing may constrain
ambient type variables (the payload's, an enclosing declaration's): that
is ordinary inference and is wanted — `'a{ _ = 0 }` pins `'a = int`.
Type-variable scope is reentrant (predicates may use the enclosing
declaration's variables; they introduce none of their own — no
`'a.`-binders in predicates).

### Binder types

A dependent binder scopes over its *own domain* (`x:(int{ x > 0 } * int)`
is a fixture today), so its type cannot be known before the domain
translates. The binder therefore enters the environment at a **fresh type
variable placeholder**; when its domain finishes translating, the
placeholder is unified with the domain's payload type. Predicates typed
inside the domain constrain the placeholder; the reconciliation surfaces
errors at the unification site. Consequences, both wanted:

- `x:(int{ x > 0 } * int)` becomes a type error (`x` is the whole tuple,
  not an int) — the current fixtures that accept it flip in the RED/GREEN
  diff;
- `x:(int{ fst x > 0 } * int)` types: `fst x`'s application result strips
  its head per the occurrence rule.

### What is stored

`Types.refinement_expression` becomes a **typed mirror**, built from
Typecore's typedtree output rather than from the parsetree:

- every node carries `rexp_type : type_expr` (in the type graph:
  `Btype`/`Subst`/freshening traverse it, `.cmi`s carry it);
- constructors and fields carry the identity **Typecore selected**
  (post-disambiguation), as substitutable paths — this closes the
  type-formers-era gaps: `Rexp_field`'s unresolved longident, and
  constructor identity frozen before the expected type was known;
- mirror **equality stays syntactic**: derived node types are ignored by
  `Vox_rexp.equal` (they are determined by the syntax plus the enclosing
  type; comparing them would be redundant work and a fresh source of
  false inequality) — written `Rexp_constraint` types keep participating
  as today.

Building the mirror from the typedtree replaces the parsetree resolver in
`transl_refinement_predicate`; the *syntactic gate* (totality by
construction, unsupported-forms rejection) stays as a pre-pass over the
parsetree, unchanged in behavior, so gate errors keep their current
messages and nothing effectful ever reaches Typecore.

Where the typedtree contains what the mirror cannot say, the build
rejects with a located "not supported in a refinement predicate":
`Optional`/`Position` arrows anywhere in an applied callee's type
(covering `~opt:v`, `?opt:o`, omitted-optional synthesis, and implicit
`%call_pos` — the mirror will not represent argument synthesis), GADT /
existential-introducing constructors in patterns, and anything else
outside the mirror grammar. Polymorphic `let` inside predicates is
**allowed** (it is ordinary Typecore generalization; each use site's
mirror node carries its instance) — the first draft's monomorphic-`let`
restriction existed only to keep a hand-rolled typer small and dies with
that typer.

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
- Recursive groups: same-group constructor mention errors (structure and
  signature); non-group mentions fine.
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
