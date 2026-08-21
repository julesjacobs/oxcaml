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
constraint). In addition, every application typed during predicate reentry
requires its callee to be Total. This predicate-specific rule reaches calls
through the hole, dependent and predicate-local binders, aliases, and every
consumed stage of a curried call; a `fun` inside a predicate is a total
closure, and mutable access fails exactly as the totality piece specifies.
The hole and dependent binders retain their declared value modes (legacy
Partial when no mode is written), with the Logicality axis forced to
`Logical`: spec entities are usable in logical positions and unable to leak
into physical ones. A function-valued hole or binder is therefore callable
only when its payload or binder context establishes Total.

**Comparison admission (predicate-scoped).** The totality allowlist
excludes comparisons (polymorphic compare raises on functions, diverges
on cyclic values), which would reject `int{ _ > 0 }`. Inside the
predicate judgment, comparison primitives are admitted as total when
their operand type is immediate — the rule named by the totality
piece's decision log as the unported follow-up, and which matches
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
`type_open` precedent), against expected type `bool`, inside a
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
repeated `Location.none` are never reusable keys. When omitted labels and
colliding anchors make the pairing ambiguous, the Typecore judgment rejects
the application before mirror construction. Concretely, the mirror
keeps source shape, order, constants, grouping and locations from the gated
parsetree exactly as today; the correspondence walk supplies, per source node,
the type, the selected constructor/field identity, and binder `Ident.t`s.

Where the typedtree contains what the mirror cannot say, the
*judgment's* representability validation has already rejected it (see
"Two phases"); the build itself is total for parser-produced input.
Inferred polymorphic `let` inside predicates is **allowed** (ordinary Typecore
generalization; each use site's mirror node carries its instance). An explicit
polymorphic binding annotation has no faithful expression-constraint preimage
and is rejected by the syntactic judgment with a located error.

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

The *syntactic gate* stays as a pre-pass over the parsetree, so **no
syntactically rejected form reaches Typecore**. The consolidated gate admits
simple monomorphic predicate-`let` annotations that have a faithful constraint
preimage and rejects explicit polymorphic binding annotations and the other
unrepresentable annotation forms. Effectful-by-type
expressions do reach Typecore: the total/logical judgment rejects a call to a
partial function and a mutable access through a logical value there, with the
ordinary mode diagnostic.

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
- the cited 31% profile line measures
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

For parser-produced input, everything that can reject lives in one phase and
the mirror translation cannot fail. Concretely:

1. **Judgment (fallible)**: the syntactic gate; hole rewriting; the
   Typecore reentry at `bool @ total` with logical spec entities. This phase
   also rejects source patterns whose constructor-wildcard argument would be
   erased or replicated by Typecore, and omitted-label PPX applications whose
   colliding locations make source/typed argument pairing ambiguous. Every
   totality, logicality, type, and representability rejection is a located
   error, and the rollback snapshot's scope is exactly this phase.
2. **Translation (total)**: `mirror_of_typedtree` — the
   parsetree-shape/typedtree-annotation correspondence — cannot fail on
   parser-produced input that the judgment admits. The formerly rejected
   typedtree forms are **represented** rather than rejected:
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
     ordinary constructor patterns; existential types introduced by an arm are
     scoped to that arm's stored types. If existential scoping in the persisted
     mirror proves disproportionate to implement, the implementor may keep a
     narrow judgment-phase rejection for exactly that case, recorded as a
     decision — the principle is represent, don't reject.

   Parser-produced admitted input has no remaining correspondence mismatch.
   A PPX can still erase or duplicate source metadata in ways the parser
   cannot produce; defensive correspondence fallbacks report located errors
   rather than aborting the compiler.

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

- Rejections, located: `int{ 42 }`, `int{ _ + "x" }`, and a Total helper
  returning `int` in `string{ total_length _ }`; acceptance:
  `int{ _ > 0 }`.
- Holes: multiple occurrences, nested refinements (innermost payload),
  under `let`/`match`/`fun`; function-valued holes default Partial, reject
  direct, aliased, and indirect calls, and accept when their payload context
  is explicitly Total. A diverging function pins the soundness consequence.
- Binders: bare and `~x:` labelled at payload; refined binder head-strip
  (`x:int{q} -> int{ _ > x }`); binder-in-own-domain flips; predicate
  binders shadowing arrow binders; default/explicit Partial function binders
  reject calls while an explicitly Total binder accepts direct, `%apply`, and
  `%revapply` spellings.
- Disambiguation: two records sharing a label name, two variants sharing
  a constructor, selected by expected type inside a predicate — the
  fixtures that a first-candidate resolver gets wrong.
- Application: labelled commuting; source-order and callee-order completion
  for partial, optional/defaulted, and `[%call_pos]` arguments, including a
  dedicated omitted-position RED2-to-GREEN2 flip;
  `%apply`/`%revapply`; format expansion; optional/position eta coercions; and
  a genuine ordinary-value layout-polymorphic identifier wrapper. The
  operator-mode fixtures compare
  direct, `%apply`, and `%revapply` calls returning an unused Partial function;
  malformed user externals reusing the primitive names stay on the generic
  application path rather than crashing the specialized reconstruction.
- Inferred polymorphic `let` inside a predicate (`let id = fun x -> x in
  id 0 = 0 && id true` accepts — pins Typecore generalization); an explicit
  polymorphic binding annotation rejects with a located syntactic error.
- Occurrence strip inside predicates: application-result head strip,
  nested heads intact (`int{p} list` element projection).
- Refined interior constraint: payload-checked, no obligation recorded
  (probe via -drefinements).
- Ambient-variable constraint: `'a{ _ = 0 }` pins `'a`; principal-mode
  double-take fixtures for the disambiguation cases.
- Mode discipline: a partial call in a predicate rejected; a mutable
  access rejected; comparison acceptance at int / rejection at string;
  `/` and `mod` rejected unguarded; a logical hole flowing to a physical
  argument rejected; a nested `fun` checked total. Immutable instance-variable
  reads are mirrored, while mutable instance-variable reads are rejected by
  the implicit self capture through the ordinary locks.
- Recursive groups: same-group constructor AND field mention errors
  (structure and signature); non-group mentions fine.
- GADT and existential constructor patterns represented and persisted,
  including value binders whose arm stores no existential type; only an arm
  whose written or derived mirror annotation actually retains its local
  existential gets the narrow located persistence rejection recorded below.
- Constructor wildcard arguments are represented for unary constructors and
  rejected with a located judgment error for nullary or multi-arity
  constructors, where Typecore would erase or replicate the source pattern.
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
  allocation state, saved cmt expressions, and warning state are framed. The
  type-only boundary enters ghost context. `Btype` snapshots already log mode
  solver changes, so failure rolls back both type and mode constraints while a
  successful judgment commits both. The earlier unconditional
  `Mode.with_rollback` confinement and refinement lock cutoff are removed; the
  now-unused rollback helper is deleted as well, leaving the ordinary `Btype`
  transaction as the single rollback mechanism.
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
- **Round 4 uses the ordinary total/logical mode frame plus predicate callee
  checking.** The predicate is expected at `bool @ total`, a normal Total
  closure lock exposes ambient captures, and every application requires its
  callee to be Total. Hole and dependent-binder entries retain their declared
  modes, defaulting to legacy Partial, with Logicality forced to Logical. This
  preserves ordinary capture-based totality outside predicates while closing
  direct, aliased, indirect, and curried calls through Partial spec entities.
- **Comparison admission is occurrence-local.** Each of the six comparison
  primitive instances used by a predicate gets one shared immediate operand
  jkind, Logical argument modes, and a Total value mode. The global primitive
  allowlist is unchanged; strings, `/`, and `mod` remain rejected. Recognition
  trusts the compiler's comparison primitive identities and arity contract.
  Unlike the `%apply` reconstruction, it does not structurally reinterpret a
  callback type, so adding a separate policy for forged noncanonical external
  declarations would not protect any valid program and is deliberately
  omitted.
- **Application completion is persistent syntax.** `Rexp_apply` keeps source
  arguments in source order and a callee-order completion containing source
  indexes, optional wrapping/defaulting, call positions, and omitted
  optional/position/required labels. `%apply` and `%revapply` remain
  source-shaped during predicate typing. For canonical primitive types, fresh
  shared modes reproduce the direct call's callback-domain/value-operand and
  callback-result/operator-result relations, including under `-principal`;
  the shared result mode is not independently forced Total. A user external
  that reuses either primitive name with a noncanonical type stays on the
  generic application path. Synthesized call-position locations are retained
  as metadata but ignored by mirror equality.
- **Typedtree-only rewrites are represented.** Format literals retain their
  literal plus the typed expansion; optional/position function coercions use a
  zero-source application completion and print as the original expression;
  `Texp_apply_layout` is transparent around its source identifier. Immutable
  instance-variable identifiers are represented as value identities too;
  mutable instance variables first capture their implicit self through the
  normal mode locks and therefore fail the Total predicate judgment.
- **Explicit polymorphic binding annotations are outside the predicate
  language.** Inferred polymorphic `let` remains supported. The explicit
  annotation has no faithful expression-constraint mirror node, so the
  syntactic gate rejects it with a located error before Typecore reentry.
- **Existential persistence has one narrow judgment rejection.** Ordinary GADT
  and existential constructor patterns are represented, including unrelated
  or unused value binders. Persistence validation collects the arm-local
  existential identities from typed binders and rejects only if a written or
  derived type actually stored by the provisional mirror retains one; that
  type has no definition outside the typedtree arm and would make CMI writing
  fail. Explicit named existential type binders remain rejected by the
  syntactic gate. Persisting scoped existential definitions would require a
  new mirror scope representation and is disproportionate for this round.
- **Mirror correspondence is total for parser-produced admitted input.** The
  gate, Typecore judgment, and the explicit existential-persistence check own
  ordinary parser-program errors. The judgment includes the narrow checks for
  arity-changing constructor wildcards and ambiguous all-ghost omitted-label
  applications. Residual correspondence mismatches possible only through
  PPX-shaped input are defensive located errors, never compiler-fatal paths.
- **Round 4 advances the artifact magic to 585** (correcting the earlier
  "keep 584" ruling). That ruling reasoned from release lineage — the branch
  is unreleased, so the whole RED/GREEN stack could share one bump — but a
  magic number identifies a marshaled layout, not a release: RED2-era
  compilers wrote `Rexp_apply`'s arguments as a list under `Caml1999I584`,
  and the round-4 record layout under the same magic made the final compiler
  accept and misinterpret such a `.cmi` (reproduced as a signal-11 crash via
  `include module type of` and an inclusion consumer). Same magic must mean
  same layout, so the completion/format grammar takes its own version. The
  bump costs nothing beyond the ordinary stdlib refresh: no released
  artifact carries 584, and stale intermediate-vintage artifacts now fail
  with the ordinary wrong-magic version error instead of crashing.


## Amendment (2026-08-21): dependent application

Owner direction (recorded in `vox/design-decision-rulings-2026-08-21.md`,
"DEPENDENT APPLICATION" and "DEPENDENT-APPLICATION FLAGS", plus a superseding
correction of the same day): dependent-arrow consumption is not a new piece —
it is a deferred gap fixed in the piece that owns it, and the mechanism is
vox2's: **"do what vox2 does. Obviously it has to happen in type checking."**
This amendment is that adaptation spec, with the same-day flag rulings
applied: the parameter-annotation spelling forms a real dependent arrow in
this work (ruling 1), instantiated types may carry evaluated subjects
(ruling 2, wholesale-adoption resolution recorded below), commuted-supply and
the eta path are follow-up with located rejections day one (ruling 3), and
the recursive induction-hypothesis behaviour ships unguarded (ruling 4). It
supersedes the deferral rows in `design-docs/refinement-flow.md:224-227`
("Dependent arrows stay rejected at consumption … is its own piece") and the
corresponding out-of-scope rows here; gap-table rows moved: S5/A2/b§C
(`vox2-gap/refresh-2026-08-21.md`, porting shortlist item 1).

Revised after the first dual design review (both verdicts "no"; findings
folded in throughout — the revision note at the end lists what changed and
answers the reviews' counterexamples).

Citation convention for this section: an unprefixed `file:line` is this tree
(`predicate-typing/dev`, branch `jujacobs/vox/predicate-typing`, commit
`64852382a4`); `vc:` prefixes `vc-generation/dev` at `b0e7815664` (the
consumer stack above this piece — those seams land there at the next
restack); `vox2:` prefixes `/usr/local/home/jujacobs/oxcamls/vox2/main`
(reference only; no code copied).

### Decision 1 (⚑): predicate-typing owns the fix

The gap spans three pieces' territory, so ownership is the first decision:

- The rejections to delete sit in application typing and `Ctype`
  (`typing/typecore.ml:5215,5360`, `typing/ctype.ml:5948`), introduced by
  type-formers (`design-docs/type-formers-final.md:29-31`) and re-recorded as
  deferred by refinement-flow (`design-docs/refinement-flow.md:224-227`).
- The substitution is a typed-mirror operation: a subject is a
  `refinement_expression` carrying its ground instance (`rexp_type`), opened
  into predicates by machinery this piece owns (`Vox_rexp`, the reentry, the
  mirror's `.cmi` layout).
- The consumption is the verification walk's (`vc:typing/vox_verify.ml`,
  `vc:typing/vox_lower.ml`), which exists only above this piece.

**Ruling proposed: this piece (predicate-typing) owns the amendment and the
fix**, because it is the lowest point in the stack where the fix is
implementable and buildable: refinement-flow's tree has no typed mirror
(`grep -c rexp_type refinement-flow/dev/typing/types.mli` → 0), so a subject
cannot carry its ground instance there; and the walk half is specification
for the consumer (the stacking plan's producer/consumer rationale for
predicate-typing < vc-generation, `vox/stacking-plan.md:94`), landing in
vc-generation at the next restack as its own red-green commits. Flagged ⚑
(the batch's only open item) because refinement-flow first recorded the
deferral and owns the application-typing architecture this extends — the
owner may prefer the record there even though that branch cannot build the
fix.

### Target programs

Must verify when the fix (typing here + walk consumption at the vc restack)
is complete; spellings from the existing corpus
(`testsuite/tests/vox/refinement-flow.ml:359`):

```ocaml
(* 1. Caller proves a property of the result from the instantiated codomain *)
external mk : x:int -> int{ _ > x } = "%identity"
let caller : int{ _ > 5 } = mk 5          (* fact: mk 5 > 5; goal: _ > 5 *)

(* 2. Callee-side check obligation — both spellings, per ruling 1 *)
let f : x:int -> int{ _ > x } = fun x -> x + 1   (* goal: x + 1 > x *)
let f' (x : int) : int{ _ > x } = x + 1          (* same arrow, same goal *)

(* 3. Dependent domain: an earlier argument instantiates a later obligation *)
external gt : x:int -> y:int{ y > x } -> int = "%identity"
let ok = gt 1 2                            (* goal: 2 > 1 *)

(* 4. Recursive callee: the instantiated codomain is the induction
      hypothesis at the recursive call (partial correctness; totality
      still rides the axis) *)
let rec bump : n:int -> int{ _ >= n } =
  fun n -> if n <= 0 then 0 else 1 + bump (n - 1)
```

Must be refused (verification refusals — counted defects, unit refused — not
type errors; these programs typecheck):

```ocaml
let bad_callee : x:int -> int{ _ > x } = fun x -> x    (* x > x: Refuted *)
let bad_caller : int{ _ > 6 } = mk 5    (* only mk 5 > 5 is known: Refuted *)
let bad_dep = gt 2 1                                   (* 1 > 2: Refuted *)
```

On this branch alone the demonstrable half is the typing half: the target
programs typecheck (today 1-4 are rejected in the first spelling), `f'`
infers the dependent arrow `x:int -> int{ _ > x }` (today it infers a
scope-escaping type, below), the callee marker and the application metadata
are recorded and observable; the verdicts flip at the vc restack.

### Where the tree stands

Formation is done for the arrow-annotation spelling (gap row S4): the
optional binder lives on `arrow_desc` (`typing/types.mli:425-434`; `Some`
only when a predicate references it; binder names are not part of type
identity). Positional binders (`x:T -> U`) scope over the predicates of both
`T` and `U`; `~x:` labelled binders over `T` only; optional and position
parameters never bind (`typing/typetexp.ml:1295-1323` —
`decide_arrow_arg_name`; "Optional parameters never bind: the argument may
be absent"). `.cmi` import freshens binder stamps and rewrites bound
occurrences through the rename map (`typing/subst.ml:762-781` arrow arm,
`:782-799` `Trefine` arm via `Vox_rexp.map ~rename ~freshen:true`) — note
for later: vox's arrow arm freshens *unconditionally*, which the staged
instantiation below must correct. Partial application preserves binders
(`typing/typecore.ml:4958-4967` — `untyped_omitted_param.arg_binder`;
reconstruction at `:5401-5412`).

Consumption is rejected at three typing gates and, above, two walk gates:

- `typing/typecore.ml:5360` — `collect_apply_args`: a supplied argument for
  a binder-carrying parameter.
- `typing/typecore.ml:5215` — `collect_unknown_apply_args`: the
  not-known-function path meets a binder-carrying arrow.
- `typing/ctype.ml:5948` — `Ctype.filter_arrow` refuses to split a
  binder-carrying arrow (rejects defining a `fun` against a dependent
  annotation; mapped at `typing/typecore.ml:373`, message `:15201-15204`,
  pinned at `testsuite/tests/vox/refinement-flow.ml:364-371`).
- `vc:typing/vox_verify.ml:156-162` — `check_imposable`: an imposed type
  whose predicate has a free `Rexp_var` is a located rejection
  (`Dependent_arrow`, worded at `vc::790-792`).
- `vc:typing/vox_lower.ml:909-917` — the predicate front end's `Rexp_var`
  arm: a binder with no entry in the lowering's binder environment is a
  located `Unsupported`. These two cover the higher-order escape
  (`vc:design-docs/vc-generation.md:204-218`, fixture
  `dependent-arrow-escape`).

The parameter-annotation spelling, probed on `vc-generation/dev` at
`b0e7815664` with `-vox-backend printing` (same-unit and cross-unit), and
independently reproduced by the codex review lane for plain `let`,
`let rec`, and an object method: `let f (x : int) : int{ _ > x } = x + 1`
compiles **today** — the predicate reentry sees `x` as an ordinary ambient
value (this doc, "The judgment"), the mirror records a free
`Rexp_ident (Pident x)`, no binder is formed, and the inferred type prints
as `val f : int -> int{ _ > x }` with `x` escaped from its scope. Above
this piece the callee-side obligation nevertheless verifies (the walk
resolves `x` in the body environment and emits `x + 1 > x`), while at every
caller the codomain fact is silently declined (`Env.find_value` fails on
the dangling ident; fail-open, so conservative). Ruling 1 makes this
spelling form a real dependent arrow in this work — the escape disappears
rather than being sentinel-pinned; mechanism below.

### The vox2 mechanism (reference only)

- **Substitution.** `Vox_dependent.instantiate ~binder ~with_ ty` is
  `Subst.type_expr` over a substitution extended with a refinement-bound
  mapping (`vox2:typing/vox_dependent.ml:100-103`) — non-mutating,
  capture-avoiding opening of the codomain
  (`vox2:typing/vox_dependent.mli:19-24`); a `rename` variant re-spells a
  binder without substituting (`:26-32`). Binder-stamp freshening is
  *conditional*: `freshen_refinement_binders` is false on the identity
  substitution and set only on import-class substitutions
  (`vox2:typing/subst.ml:122,820-828`).
- **Application loop.** Arguments typed in arrow order; after typing an
  argument whose parameter carries a binder, the evaluated subject is
  substituted into every remaining untyped argument's expected types and the
  result type (`vox2:typing/typecore.ml:13345-13374`; per-field substitution
  `:13289-13323`).
- **Subjects.** `evaluated_argument_subject`
  (`vox2:typing/typecore.ml:8174-8231`) lowers the typed actual into a
  refinement expression at typing time: dependent parameters in scope become
  bound references (`Env.dependent_parameter_ids`,
  `vox2:typing/env.mli:733-742`), idents free references, constants stay,
  applications are kept structurally when the callee is total/stable
  (`dependent_argument_call_is_stable`, `:8156-8172` — total mode, or a
  direct integer comparison primitive with int operands), tuples/
  constructors/immutable fields recurse, and any other form becomes an
  opaque fresh ident spelled as a free reference
  (`Rexp_ident (Rfree (Rglobal (Pident id)))`, `:8177-8182`). An omitted
  argument inside a call defeats the structural form (`:8203`).
- **Metadata.** Per-argument records `{ rap_domain; rap_binder;
  rap_supplied; rap_subject }` — the evaluated subject is *stored per
  argument* — plus the instantiated result, attached as a
  `Texp_refinement_application` extra when any involved type is refined or a
  binder exists (`vox2:typing/typedtree.mli:419,460-468`;
  `vox2:typing/typecore.ml:13030-13043`).
- **Omitted binders.** A *supplied* argument's domain predicate mentioning
  an *omitted* parameter's binder becomes a deferred goal, the domain
  logically erased from the record (`vox2:typing/typecore.ml:13376-13410`).
- **Eta path.** The omittable-argument elimination wrapper instantiates the
  codomain at the eta variable with its own metadata
  (`vox2:typing/typecore.ml:13012-13046`).
- **Callee side.** A dependent parameter requires a variable or alias
  pattern (error otherwise); the binder is renamed to the parameter ident in
  the body's expected type; parameter idents are registered as dependent in
  the body environment (`Env.add_dependent_parameters`) so body-scope
  predicates classify mentions as bound, with alias canonicalization
  (`vox2:typing/typecore.ml:12040-12066`) — the registration is also what
  makes vox2 form a dependent arrow from the parameter-annotation spelling.
- **Verifier consumption.** `check_application`
  (`vox2:typing/vox_verify.ml:4558-4677`) hard-errors on refined
  applications missing metadata, *relates* each stored subject to the
  occurrence-local subject (alpha-equality, equality facts, replacement
  lists, `:4594-4645`), and proves each supplied argument's domain
  refinement as a "contract-argument" obligation.

### The adaptation

Same mechanism, vox's representations.

**One retention rule, shared (review CRITICAL-1).** Whether a call inside an
actual is kept structurally is decided by exactly one predicate, used by
both producers so they cannot diverge. The rule is the occurrence lowerer's
*actual* rule, not vox2's weaker one: the callee reads Total
(`occurrence_is_total`, `vc:typing/vox_lower.ml:653-669`) **and** every
argument's type crosses both Totality and Logicality
(`crosses_totality_and_logicality`, `vc:typing/vox_lower.ml:671-685`,
applied at `:781-786`) — the argument half is what stops a `Call` over
non-crossing state from equating two evaluations straddling a write; Total
does not imply stateless (owner ruling "TOTAL ⇏ STATELESS"). The predicate
lands in this piece as
`Vox_dependent.retains_call : is_total_local:(Ident.t -> bool) -> …` (its
inputs — `Mode.Totality.Guts`, `Ctype.crossing_of_ty` — exist below both
producers), with a stated monotonicity invariant covering all three of its
time-varying inputs: retention may only grow with the `is_total_local`
oracle, with mode determination (`check_const_conservative` answers on more
modes as they determine), and with type-variable solving
(`crossing_of_ty` crosses on more types as variables solve). Application
typing calls it with the conservative oracle `fun _ -> false` (typing has
no `Texp_mode` records in hand; fewer retentions only weaken subjects,
never strengthen facts); at the restack the occurrence lowerer's own gate
at `vc::781-786` is replaced by a call to the same predicate with its
existing oracle — consumer change 0 in the walk section. Because typing's
oracle is the bottom of the monotone family and typing runs at or before
the walk on every other input, every typing-time retention is also a
walk-time retention, so a retained call lowers to the same congruent
`Call` on both sides by construction. Only that direction is ever relied
on: the walk never re-runs the rule to reconstruct typing's decisions —
the descent in walk item 4 is driven by the stored subject's shape
(delta-review correction; the rule's non-oracle inputs are exactly what a
replay cannot hold fixed).

**Subjects — vox2's evaluated subjects, adopted wholesale (resolution of
ruling 2's delegated question).** Ruling 2 rejects the alternative this
design first carried (opaque names only, resolved at the walk, with a
located error when a name would outlive the apply): instantiated types may
carry evaluated subjects. That forces structural subjects wherever types
flow — an opaque name embedded in a flowing type is unprintable and
unresolvable, a structural subject is neither — and with the standing "do
what vox2 does" meta-ruling the resolution recorded here is **wholesale**:
subjects are structural lowerings of the typed actual at typing time,
vox2's grammar expressed over vox's mirror, *stored per argument in the
metadata* as vox2 stores `rap_subject`. Per node:

- `Texp_ident (path, lid, …)` → `Rexp_ident (path, lid)`, `rexp_type =
  Some` of the node's `exp_type` (already payload-headed by
  refinement-flow's occurrence strip). `Texp_mutvar` reads are a distinct
  head and are *not* idents here — they fall to the opaque leaf (their walk
  denotation is the per-read opaque constant, `vc:typing/vox_lower.mli:107`);
  a mention of a `ref` cell is an ordinary ident (the cell is the value, not
  its contents).
- `Texp_constant` → `Rexp_constant` of the corresponding
  `Parsetree.constant` (the mirror stores parsetree constants,
  `typing/types.mli:338`; the conversion is the one `Untypeast` performs).
- `Texp_apply` retained by `Vox_dependent.retains_call` (above) →
  `Rexp_apply` with lowered arguments. The completion is the identity
  mapping over the elaborated argument list — synthesized
  eliminated-optional/call-position arguments are retained as the elaborated
  arguments they became — and an actual `Omitted` entry (partial
  application) defeats the structural form: the whole node is an opaque
  leaf, as in vox2 (`vox2:typing/typecore.ml:8203`).
- `Texp_tuple` → `Rexp_tuple`; `Texp_construct` → `Rexp_construct`;
  immutable `Texp_field` → `Rexp_field` — each recursing on children.
- Any other form becomes an **opaque leaf**: a freshly minted
  `Ident.create_local`, spelled as vox2 spells it — a free reference
  `Rexp_ident (Pident v, ·)` with `rexp_type = Some` of the node's
  `exp_type` — naming "the value this subexpression evaluated to". No side
  list of minted idents is kept: the stored subject itself, walked in
  parallel with the actual (below), identifies every leaf and its origin
  subexpression positionally.

What this buys over the rejected variant: instantiated types are meaningful
wherever they flow — `mk (a + 1)` yields the printable, provable codomain
`int{ _ > a + 1 }` (the walk's predicate front end lowers `%addint`
congruently, `vc:typing/vox_lower.ml:602`), and partial applications of
`x:int -> (int{ _ > x } -> int)` carry their evaluated subject instead of
being rejected. Unlike vox2, no verifier step heuristically *relates* a
stored subject to an occurrence-local one (`vox2:typing/vox_verify.ml:
4594-4645`): the shared retention rule makes the stored subject and the
occurrence lowering agree on every retained node by construction, and the
parallel descent (walk section) binds each opaque leaf to the walk's own
term for exactly its origin subexpression — same role as vox2's
relation/replacement, deterministic instead of comparative.

**The substitution operation.** New module `typing/vox_dependent.ml` (the
parity name), entry points:

- `instantiate : binder:Ident.t -> subject:Types.refinement_expression ->
  Types.type_expr -> Types.type_expr` — replace every `Rexp_var binder` in
  every refinement predicate reachable in the type (head, nested, stored
  interior types) with `subject`; the consumed stage's `arrow_desc` slot is
  dropped by the caller.
- `mentions : Ident.t -> Types.type_expr -> bool` — the gate, so unchanged
  types are not rebuilt (analogue `vox2:typing/vox_dependent.ml:51-67`; the
  predicate half exists as `Vox_rexp.mentions_ident`,
  `typing/vox_rexp.mli:91`). It must traverse stored `rexp_type` edges under
  a visited-node guard (stored types can tie metadata cycles,
  `typing/types.mli:315-320`).
- `retains_call` — the shared retention rule above.

Route: through `Subst`, like vox2 — a refinement-bound substitution map on
the substitution record, applied where the `Trefine` arm already rebuilds
predicates (`typing/subst.ml:782-799`). Two corrections found in review:

- *Conditional binder freshening (review finding, blocking).* vox's arrow
  arm freshens binder stamps **unconditionally** (`typing/subst.ml:769-777`),
  which breaks staged instantiation: the fold collects each stage's
  `arg_binder` from the instance *before* substituting, and an unconditional
  freshen during the stage-1 substitution would restamp stage 2's binder,
  leaving the collected ident stale — a nested dependent arrow
  (`w:int -> x:int -> int{ _ > w + x }`) would ship a free `Rexp_var` and
  walk-reject. vox2 already solved this: `freshen_refinement_binders`,
  false on the identity substitution, true on import-class
  (`vox2:typing/subst.ml:122,820-828`). Port the flag: vox's `Subst.t`
  gains it, existing import/module substitution constructors set it (no
  behaviour change anywhere current), and `Vox_dependent.instantiate` runs
  with it unset, so untouched stages keep their stamps. The parity table's
  substitution row records this as a required port, not an already-SAME
  mechanism.
- *Capture invariant on the callback (review audit).* Freshening the
  destination's predicate-local binders is not by itself a specification of
  how a replacement containing `Rexp_var` is renamed: the substitution
  callback must apply the rename map in force at the node to bound
  occurrences *inside the inserted subject* — a subject can mention an
  enclosing dependent binder (predicate-internal instantiation), and if the
  surrounding traversal is renaming that binder (import freshening), the
  inserted copy must follow. Stated as an invariant on the extended
  `Vox_rexp.map` API (`typing/vox_rexp.mli:55-62`), with a discriminating
  test (a subject mentioning an enclosing dependent parameter, imported
  through a `.cmi`).

Sharing: `Subst.type_expr` under `No_action` returns type *variables*
physically unchanged (`typing/subst.ml:632-660` — a `Tvar` is copied only
under `Duplicate_variables`/`Prepare_for_saving` or a jkind change), so
substituting the remaining expectation fields one by one, vox2-style, cannot
sever a variable shared between a later argument's expected type and the
result type. Non-variable structure is copied, which is the point (the
funct's declared arrow must not be mutated).

**Caller side.** `collect_apply_args` keeps its shape; two representational
changes:

- The rejection at `typing/typecore.ml:5360` is deleted; `Known_arg` gains
  `arg_binder : Ident.t option` (as `untyped_omitted_param` has,
  `:4958-4967`), filled from the `arrow_desc` in hand at `:5292`.
- The rejection at `:5215` is deleted; `Unknown_arg` gains the same field
  (vox2's does for the same reason, `vox2:typing/typecore.ml:13301-13308`).

The typing loop is where the mechanism lands. Today the untyped arguments
are typed by an order-preserving `List.mapi` over `type_apply_arg`
(`typing/typecore.ml:11260-11264`; arrow-order typing is already
load-bearing, `vc:design-docs/vc-generation.md:184`). That becomes an
explicit left-to-right fold, vox2's loop shape:

1. Type the argument (`type_apply_arg`, unchanged internally — the
   `Known_arg` pre-strip of both expectation copies stays,
   `typing/typecore.ml:11091-11096`; a refined domain still never enters
   `type_argument`).
2. If the consumed stage carried a binder: build the evaluated subject from
   the typed actual; substitute it, gated by `mentions`, into the
   *remaining* untyped arguments' stored expectation types
   (`Known_arg.ty_arg`/`ty_arg0`, `Unknown_arg.ty_arg_mono`,
   `Eliminated_optional_arg.ty_arg`, `Omitted.ty_arg`) and the running
   result type.
3. Accumulate one metadata entry per stage, storing the subject. The row's
   `dap_domain` is the domain instantiated at the row's *own* subject as
   well (delta-review correction): a binder scopes over its own domain, so
   after `gt 1 2` the second row's domain must record `int{ 2 > 1 }`, not
   `int{ Rexp_var y > 1 }` — closing it in the producer keeps the walk's
   no-free-`Rexp_var` invariant exceptionless, where the previous revision
   would have rejected target 3 at its own metadata check. (The expectation
   copies the argument was *typed* against are untouched — they were
   consumed before the subject existed; only the metadata record is the
   instantiated copy.)

`~x:` (domain-only) binders never occur in later stages or the codomain by
formation, so step 2 is a no-op beyond their own domain and step 3's
own-domain instantiation is the whole of their consumption; the walk needs
no binder environment for them.

An omitted stage has no subject; its binder substitutes nothing. The
reconstructed partial-application arrow keeps the binder exactly as today
(`:5401-5412`); the later application that supplies it instantiates then.
When a *supplied* argument's instantiated domain still mentions an *omitted*
binder (commuted supply: `gt2 ~y:3` for
`gt2 : x:int -> y:int{ y > x } -> int`), typing leaves the mention free and
the walk's free-`Rexp_var` rejection fires on that obligation — a located
refusal where vox2 defers a goal; RULED follow-up (ruling 3), pinned by
fixture.

The apply node's `exp_type` is the payload head of the *instantiated*
codomain — the existing apply-result strip (`typing/typecore.ml:7833`) runs
on the substituted type; a refined head survives only in the metadata, while
nested refinements in the result (a partial application's arrow, a tuple's
component) flow in `exp_type` carrying their evaluated subjects, per
ruling 2. Expression types stay payload-headed at the top; no new `ctype`
unification arms (the D4 divergence is preserved). Two consequences of
embedded subjects, stated: printed types may show evaluated subjects,
including opaque leaf names (vox2's acceptance); and a type carrying a
subject whose free paths later leave scope joins the pre-existing
escaped-path class that signature-side scope validation (gap rows B5/S7)
will police — this fix widens that surface knowingly (ruling 2).

**The metadata record.** One new `exp_extra` constructor (next to
`Texp_refinement_obligation`, `typing/typedtree.mli:445-449`):

```ocaml
| Texp_dependent_application of dependent_application

and dependent_application =
  { dap_args : dependent_argument list;  (* one per stage, arrow order,
                                            aligned with the apply's args *)
    dap_result : Types.type_expr }       (* instantiated codomain after the
                                            last stage this apply consumed *)

and dependent_argument =
  { dap_domain : Types.type_expr;        (* instantiated domain *)
    dap_binder : Ident.t option;         (* the stage's binder, if any *)
    dap_subject : Types.refinement_expression option }
                                         (* the evaluated subject as
                                            substituted — Some exactly when
                                            the stage had a binder and the
                                            argument was supplied *)
```

The stored subject is the review round's convergent correction (both lanes,
independently): the first draft stored only a list of minted idents, whose
singleton case was ambiguous between "the whole actual was opaque" and "a
structural subject with exactly one opaque descendant" — `mk2 (Some (f a))`
would have bound the leaf name to the whole constructed value
(sort-mismatched at best, a false equation between recursive sorts at
worst), and `mk (f a + 1)` would have handed the caller a stronger fact
than was checked. Storing the subject (vox2's `rap_subject` shape) makes
whole-opacity the test `subject ≡ Rexp_ident v` with `v` minted, and gives
every interior leaf a positional origin (the parallel descent, walk
section).

Attached only when at least one consumed stage carried a binder — the
non-dependent case keeps its zero-metadata road (the walker reads the
funct's arrow spine, `vc:design-docs/vc-generation.md:126-161`). Differences
from vox2's `rap` records: no `rap_supplied` (alignment with the apply's
`Arg`/`Omitted` list encodes it), no logical-erasure rewriting (no deferred
goals day one) — same role, divergent payload.

Artifact mechanics (review CRITICAL-class completion): the typed tree is
marshaled into `.cmt`s, so the new constructor changes the `.cmt` layout.
This tree derives *all* artifact magics from the single
`MAGIC_NUMBER__VERSION` (`build-aux/ocaml_version.m4:100`, consumed by the
`DEFINE_MAGIC_NUMBER` list at `:108-127`), so "bump `.cmt` only" is not
expressible without splitting the build's version scheme. The fix takes the
conservative route: bump the common version 585→586 — the `.cmi` moves too,
though its *layout* is unchanged (evaluated subjects are built entirely from
existing `refinement_expression` constructors; only the distribution of
content patterns changes, and a magic identifies a layout) — ordinary
version churn on an unreleased branch, precedent 583→585 in this piece.
Compatibility fixture: a post-change `.cmi` whose exported instantiated type
contains a tuple/call subject and a post-change `.cmt` are both rejected by
a 585-era reader with the ordinary wrong-magic error, never misread.
Mixed-version consequence, recorded (review request): recompiling an
old-style escaping program under the fix strengthens its type monotonically
(the escaped free ident becomes a binder); pre-fix `.cmi`s keep their
meaning until recompiled, and the dangling-stamp exposure of old escaped
artifacts is the pre-existing B5/S7 class, knowingly widened by ruling 2.

**Callee side, half one: functions against declared dependent arrows.**
`Ctype.filter_arrow` returns the binder in `filtered_arrow`
(`typing/ctype.mli:375-380` grows a field; the rejection at
`typing/ctype.ml:5948` is deleted — `Ctype` reports, the caller decides).
Function typing: when the split stage carries a binder, the parameter
pattern must bind the whole argument as a variable or alias (vox2's rule and
error, `vox2:typing/typecore.ml:12040-12045`); the body's expected type gets
`Vox_dependent.instantiate ~binder ~subject:(Rexp_ident (Pident param))`
before the body is typed. The imposed codomain marker then carries a
*closed* predicate mentioning the parameter as a free ident resolvable in
the body environment — the exact shape the walk already lowers end-to-end
(probe-verified via today's parameter-capture behaviour). The function's own
type is untouched — the arrow keeps its binder; only the body's expectations
are opened. Curried dependent arrows repeat the rule per parameter. No
`check_imposable` change for this half: the substituted predicate has no
free `Rexp_var`.

**Callee side, half two: the parameter-annotation spelling forms the arrow
(ruling 1).** `let f (x : int) : int{ _ > x } = …` and
`let g (x : int) (y : int{ _ > x }) = …` produce real dependent arrows.
(Spelling note, from review: a term parameter's *own* name is not in scope
in its own annotation — `(y : int{ y > x })` is "Unbound value y", probed —
so a term parameter's own-value constraint is written with the hole; the
hole/name normalization ruling (`vox/design-decision-rulings-2026-08-21.md`,
⚑ FLAGS item 1) makes `int{ _ > x }` and a named spelling the same type.
The *type* spelling `y:int{ y > x } -> …` binds its own name as always.)
vox2 reaches the formation through body-environment registration
(`Env.add_dependent_parameters`) because its reentry classifies mentions at
predicate-typing time; vox's reentry deliberately classifies ambient values
as free idents (this doc, "The judgment") and the walk's body obligations
depend on that spelling — so vox promotes at *function-type assembly*
instead:

- At each arrow stage the function's inferred type assembles, if the stage's
  parameter pattern binds the whole argument as a variable or alias and the
  parameter's ident (or an alias) has free mentions in refinement
  predicates within the stage's *binding region*, then: promote those free
  mentions to bound mentions of one canonical ident (aliases canonicalize
  to the root as vox2 does, `vox2:typing/typecore.ml:12057-12066`) and set
  the stage's `arrow_desc` binder. The binding region mirrors the formation
  invariant exactly (`typing/types.mli:425-434`): for a positional
  (`Nolabel`) stage, the stage's own domain and the whole suffix type; for
  a `Labelled` stage, the stage's own domain **only** — a labelled
  parameter's codomain/suffix mentions cannot be expressed by any binder
  (`~x:` is domain-only by formation) and stay in the escaped class
  (review correction; the first draft would have minted a
  formation-invariant-violating binder whose printed type does not
  reparse). `Optional`/`Position` stages never promote.
- The promotion is a non-mutating rebuild of the assembled arrow only: the
  body's obligation markers must retain the free-ident form the walk lowers
  today, so shared `ref_pred` cells are never mutated in place.
  `Vox_rexp.promote_locals` rewrites one mirror expression
  (`typing/vox_rexp.mli:100-101`); the promotion needs the type-graph
  traversal around it — a `Vox_dependent.promote_parameter` companion
  covering nested refinements and stored node types, copy-on-write like
  `instantiate` (review audit item).
- A parameter mentioned by a predicate but bound by a decomposing pattern
  (`function`, tuples) is the vox2 error ("a dependent function parameter
  requires a variable or alias pattern") only when a binder would be
  *needed* — the mention exists; otherwise nothing fires.
- Scope of the rule: the function's own annotation surface (parameter and
  return annotations). Body-internal predicates mentioning parameters keep
  the free-ident classification and resolve in the body environment; a type
  built from one that later flows out of the function joins the pre-existing
  escaped-path class for signature-side scope validation (B5/S7). This
  bounds the port to formation, without vox2's environment machinery.
- Recursion note (review; softened per the delta-round probe): a `let rec`
  defined with the parameter spelling assembles and exports the dependent
  arrow when its recursive occurrences leave the annotated codomain
  unconstrained; an occurrence that constrains the codomain pins the shared
  node and the assembly exports the unpromoted type. Either way the
  *recursive occurrences inside its own body* see the pre-promotion type,
  so the induction-hypothesis route of target 4 requires the
  arrow-annotation spelling day one; recorded, with fixtures pinning both
  parameter-spelled recursive shapes.

The inferred type of the probe program flips from the escaping
`val f : int -> int{ _ > x }` to `val f' : x:int -> int{ _ > x }`; caller
instantiation then works identically for both spellings. The same assembly
path covers plain `let`, `let rec`, and object methods (`Pexp_poly` wraps
the same function expression — codex probe); functors are module-level
`Mty_functor` values, not term `Tarrow`s, and are out of this surface.

The eta/omittable-elimination path in `type_argument`
(`typing/typecore.ml:10901-11010`) and the `%ignore` fast path
(`:11175-11190`) are follow-up (ruling 3): optional and position stages
never bind, so the only reachable dependent shape is the final `Nolabel`
stage of an omittable-prefixed function being coerced — the built
`Texp_apply` carries no metadata and the walk's dependent-metadata check
(below) or free-`Rexp_var` rejection fires, same family as the higher-order
escape. vox2 instantiates here (`vox2:typing/typecore.ml:13012-13046`);
recorded parity row, pinned by fixture.

**What the walk and the lowering consume — lands in vc-generation at the
restack.** The instantiated refinement flows through existing paths; the
extensions, numbered:

0. *The retention gate delegates.* The occurrence lowerer's stability gate
   (`vc:typing/vox_lower.ml:781-786`, built from `:653-685`) is replaced by
   a call to `Vox_dependent.retains_call` with its existing
   `is_total_local` oracle — one definition of retention for both
   producers, per the shared-rule section above.
1. *Dependent-metadata check, fail-closed and explicit* (review completion;
   both lanes asked). For every `Texp_apply`, the walk pairs the funct's
   arrow spine with the argument list as today and additionally enforces:
   exactly one `Texp_dependent_application` extra iff any paired stage
   carries a binder with a supplied argument (none otherwise; duplicates
   fatal); `dap_args` length equals the argument-list length, with labels
   and `Arg`/`Omitted` supply state matching pairwise (over-application
   `Unknown_arg` stages and synthesized eliminated-optional/call-position
   entries included in the enumeration — they occupy argument slots and get
   binder-less, subject-less metadata rows); each `dap_binder` agreeing
   with the corresponding spine stage's binder; `dap_subject` present
   exactly where binder-and-supplied; and no free `Rexp_var` in any imposed
   `dap_domain` or in `dap_result`. Violations split into two classes by
   who can produce them (delta-review correction — the first revision made
   everything fatal, which would have crashed the compiler on valid
   source): metadata *absence* where a binder-carrying stage has a supplied
   argument is a **located** dependent-arrow rejection — the eta wrapper
   builds exactly this shape from valid source (a compiler-synthesized
   apply that bypasses the fold) — and a residual free `Rexp_var` that is
   some stage's *omitted* binder is likewise located (commuted supply,
   ruled follow-up); everything else — duplicated extras, length/label/
   supply misalignment, `dap_binder` disagreement, a missing or unexpected
   `dap_subject`, any other free `Rexp_var` (the producer closes every
   supplied binder, own domains included) — is `Misc.fatal_error` with the
   location, since only a typer defect produces it and a walker defect must
   not become a dropped obligation (extends the existing pairing rule,
   `vc:design-docs/vc-generation.md:126-161`; vox2 precedent
   `vox2:typing/vox_verify.ml:4674-4677`). The eta and commuted-supply
   sentinels therefore stop at located rejections — never `fatal_error` on
   valid source, never the legacy fact path.
2. *Per-argument obligations*: when the apply carries metadata, pair
   against `dap_domain` instead of the funct's spine. Obligation semantics
   unchanged: subject = the argument, imposed = the instantiated domain.
3. *Apply-codomain facts* (`apply_codomain` + `add_predicate_fact`
   deposits, `vc:typing/vox_verify.ml:220-236,190-211,273` onward): read
   `dap_result` instead of walking the spine; downstream — hole
   substitution (`vc:typing/vox_lower.mli:166`), once-per-obligation
   deposits, admission recording — unchanged.
4. *Leaf binding by parallel descent* (replaces the first draft's
   singleton-list rule; the convergent review fix — and, per the delta
   round, driven by the stored subject's shape alone, never by replaying
   the retention rule: the rule's non-oracle inputs are time-varying, so a
   dependent stage with a polymorphic domain and an actual `tot y` solved
   later in the unit stores an opaque leaf at typing yet would replay as
   retained at the walk, and a replay-driven descent would fatal on that
   legitimate program). For each stored subject, the walk descends the
   subject and the actual argument expression together, dispatching on the
   subject node: a structural node (`Rexp_apply`/`Rexp_tuple`/
   `Rexp_construct`/`Rexp_field`/`Rexp_constant`) pairs with the
   expression node of the matching head — typing only emits structure
   where the expression had that shape, so a head mismatch is fatal
   (metadata-defect class, item 1) — and descends child-by-child; a
   subject `Rexp_ident path` over a `Texp_ident` of the same path is a
   real ident subject, nothing to bind; a subject `Rexp_ident (Pident v)`
   over any other expression is a minted leaf and binds
   `v ↦ lower_subject(that subexpression)` — the memoized term
   (`vc:typing/vox_lower.ml:45-49`), so the codomain fact, the argument
   goal, and any let equality meet on one term per evaluation. The
   retention rule is never consulted at a leaf. Whole-actual opacity is
   the degenerate descent `subject ≡ Rexp_ident v`. Retained structural
   interiors need no binding: they lower through the existing arms (idents
   via `Env`, constants, congruent `Call`s — depositing their own declared
   facts through `on_resolved` exactly as let-equality right-hand sides
   do), and by the extended monotonicity invariant a call typing retained
   is also retained by the walk's `lower_subject`, so the occurrence
   lowering of the same node is the same term; the walk retaining *more*
   than typing did is harmless — the stored leaf stands for the evaluation
   and binds to whatever `lower_subject` now produces for it, structural
   or opaque. An actual whose `lower_subject` raises falls to the opaque
   tier as ever: facts weaken, goals become unprovable; never a silent
   discharge.
5. *`check_imposable`* (`vc:typing/vox_verify.ml:156-162`): unchanged in
   force — a free `Rexp_var` in an imposed type remains a located rejection
   (higher-order escape, commuted supply, eta path); instantiated types
   contain no free `Rexp_var` by construction, and opaque leaves are free
   idents, not vars.
6. *Unchanged, deliberately*: funnel markers (the callee side produces
   closed predicates); binder facts from `pat_type`; let equalities
   (`vc:typing/vox_verify.ml:681` onward — how a *named* dependent result
   reaches later goals: `let y = mk 5 in …` deposits `y = ir(mk 5)` next to
   the instantiated codomain fact on the same term); value-description
   facts; result-position pushing (metadata bindings ride with the pending
   record).

**Predicates: the reentry and the completion grammar.** Applications inside
predicates are typed by the same `type_application` through the reentry
(`typing/typecore.ml:13214-13225`), so instantiation works there with no
extra code; subjects classify against reentry-typed actuals (a mention of
the hole or a dependent binder in scope is an ident bound by the transient
frame; the mirror build's promotion reclassifies free mentions of *its*
binders, `typing/vox_rexp.mli:100-101` — the capture invariant on the
substitution callback covers the subject side). The instantiated types land
only in stored node types (`rexp_type`), which nothing consumes for facts
today — the predicate front end lowers calls to congruent uninterpreted
`Call`s with no codomain deposit — so predicate-internal dependent
application is admitted at formation and inert at solving, the standing vox
gives predicate-internal calls generally. The completion grammar
(`typing/types.mli:369-395`) is unaffected: it records how application
typing completed the argument list, orthogonal to what the codomain
instantiated to; partial applications inside predicates keep their existing
completion-entry rejections in the lowering. A fixture pins one
predicate-internal dependent call.

**Module boundary.** Beyond the artifact-version mechanics above, nothing
new is persisted. A dependent arrow crosses the `.cmi` as it has since
type-formers; import freshens the binder coherently
(`typing/subst.ml:762-799`, now under the conditional flag with
import-class substitutions keeping it set) and an application of the
imported value instantiates the freshened binder like any local one. An
*instantiated* type that reaches a signature carries its evaluated subject
as ordinary mirror content (constants, paths — persisted exactly as
declared predicates are; opaque leaf idents persist as unbound value paths,
the pre-existing escaped-path class). The codomain fact of an imported
dependent function is an *assumed contract* exactly as its non-dependent
counterpart: `record_admission` fires from the same deposit
(`vc:typing/vox_verify.ml:242-256`), so the verdict stays conditional and
the admission report names the declaration — pinned cross-unit by extending
`vc:testsuite/tests/vox/vc-z3-import.ml`. Signature-side scope validation
(`vox2:typing/vox_dependent.mli:34-39`,
`validate_scopes`/`validate_signature`) remains missing in vox (gap rows
B5/S7); ruling 2 accepts the widened surface until that work.

### vox2 comparison

Per the parity gate: SAME by owner ruling, except the recorded
representation divergences.

| Aspect | Status vs vox2 |
|---|---|
| Instantiation at application typing, capture-avoiding, non-mutating, per remaining-field, arrow order | SAME (`vox2:typing/typecore.ml:13345-13374` ↔ the fold above) |
| Substitution through `Subst` | Same route; **requires porting vox2's conditional `freshen_refinement_binders`** (`vox2:typing/subst.ml:122,820-828`) — vox's arrow arm currently freshens unconditionally (`typing/subst.ml:769-777`), which staged instantiation cannot tolerate |
| Evaluated subjects: structural typing-time grammar, stored per argument, opaque leaves as fresh free idents, embedded in instantiated types | SAME grammar and storage shape (ruling 2 wholesale; `vox2:typing/typecore.ml:8174-8231`, `rap_subject`); retention rule is the *occurrence lowerer's* (Total callee AND argument crossing) — stricter than vox2's callee-only rule, shared between producers by design |
| Subject/occurrence connection at the verifier | Divergent plumbing, same role: vox2 relates/replaces stored vs occurrence subjects (`vox2:typing/vox_verify.ml:4594-4645`); vox's shared retention rule plus parallel-descent leaf binding makes the connection deterministic — nothing is compared because nothing can disagree without a fatal metadata defect |
| Durable per-application metadata, fail-closed | Same role, divergent payload: no `rap_supplied` (arg-list alignment encodes it), no erasure rewriting; explicit absence/uniqueness/alignment algorithm (walk item 1) |
| Callee side: variable/alias-pattern rule; binder connected to the parameter | SAME rule (`vox2:typing/typecore.ml:12040-12066`); vox substitutes to a free ident where vox2 renames — forced by D3/D4 (no `ref_view`, payload-headed `exp_type`, closed markers) |
| Parameter-annotation spelling forms a dependent arrow | SAME capability (ruling 1); vox promotes at function-type assembly (positional: own domain + suffix; labelled: own domain only, per the formation invariant) where vox2 registers dependent parameters in the body environment — forced by vox's reentry classifying ambient values as free idents |
| Binder spelling: bare `x:T` / `~x:T`, binder on `arrow_desc`, hole distinct from binder | DIVERGENT-BY-DESIGN, standing ledger D2-D3 (`vox2-gap/report-codex-c.md:105-106`); no new spelling here (hole/name normalization is ruled separately, type-formers) |
| Expression types payload-headed at the refined head; instantiated heads only in metadata/markers | DIVERGENT-BY-DESIGN, standing ledger D4 (`vox2-gap/report-codex-c.md:107`); nested instantiated refinements flow in `exp_type` as nested refinements always have |
| Deferred goals for supplied domains mentioning omitted binders | FOLLOW-UP (walk-located rejection day one) — RULED (ruling 3), parity row + fixture |
| Eta/omittable-elimination instantiation | FOLLOW-UP (walk-located rejection day one) — RULED (ruling 3), parity row + fixture |
| `[@@vox.spec_only]`, scope/signature validation (`validate_scopes`) | Not this fix (gap rows A10/S8, B5/S7); ruling 2 accepts the interim surface |

No DELIBERATELY-DROPPED rows: the two follow-up rows carry owner rulings;
everything else is SAME or a recorded divergence with its forcing reason.

### Failure modes, stated

- A binder the typing could not close (higher-order escape, commuted supply,
  eta path, unnamed callee parameter) is a **located rejection** — at typing
  where the shape is visible there, in the walk otherwise. Never a silent
  drop: the dependent-metadata check (walk item 1) makes metadata absence a
  located rejection (the eta shape reaches it from valid source) and
  duplication or misalignment fatal, and a free `Rexp_var` cannot lower.
- A descent mismatch between a stored subject and its actual is **fatal**
  (a typer defect, not a user error): the fact must attach to the walk's
  term for the actual evaluation or not at all.
- An actual the lowering cannot represent degrades to the **opaque tier**:
  facts weaken, goals become unprovable; both conservative.
- The assembly-time promotion must rebuild, not mutate: a mutation shared
  into the body's marker types would flip their predicates to bound form and
  break the walk's body-obligation route. Discriminating fixture: an inner
  annotation mentioning the parameter (`let g : int{ _ >= x + 1 } = … in …`)
  verifies inside `f'`.

### Tests

Two tracks, red-green per convention.

**This branch (typing).** `testsuite/tests/vox/refinement-flow.ml` and a
dependent-application block: RED pins today's rejections (the three typing
gates; the parameter-annotation spelling's escaping printed type for `let`,
`let rec`, and an object method — all three probe escaped today), GREEN
flips — the expectation diff is exactly the set of programs admitted and the
types they get. Observability without the walk: printed types (target
program `f'` prints `x:int -> int{ _ > x }`; partial application keeps the
binder; instantiated results print their evaluated subjects, including a
stable-call subject `int{ _ > a + 1 }` and an opaque leaf's minted name),
the `-drefinements` probe's obligation map (the callee marker appears,
closed, mentioning the parameter), `-dtypedtree` for the metadata record.
Application-shape matrix (review completion — each discriminates a distinct
fold behaviour):

- reverse-order supply of a labelled dependent-dependent pair (`h ~y:2 1`
  for `h : x:int -> y:int{ y > x } -> int` — commutation with a supplied
  binder, distinct from the omitted-binder rejection);
- an eliminated optional stage *between* two dependent stages;
- a partial application whose returned arrow's domain and codomain both
  contain the earlier evaluated subject;
- three dependent stages, so each substitution rewrites both a later domain
  and the final result;
- a nested dependent arrow (`w:int -> x:int -> int{ _ > w + x }`) — the
  conditional-freshening discriminator: with unconditional freshening the
  second stage ships a stale binder and a free `Rexp_var`.

Formation fixtures: parameter spelling green for `let`, `let rec`, an
object method, and an alias parameter; located rejection for
`function`/decomposing patterns with a mentioned binder; a labelled
parameter whose predicate mentions it in the codomain stays unpromoted
(escaped class, pinned); the parameter-spelled recursive occurrence pinned
at its pre-promotion type. Rejection fixtures: commuted supply and the eta
shape still building (pinned un-instantiated for the walk to reject above).

**vc restack (verification).** Extending `vc:testsuite/tests/vox/vc-z3.ml`,
`vc-printing.ml`, `vc-z3-import.ml`; each fixture discriminating:

- `dep-caller` / `dep-caller-refuted` — target 1 and its refusal (metadata +
  fact path 3).
- `dep-callee` / `dep-callee-refuted` — target 2, both spellings (binder
  return + parameter substitution + closed marker; assembly promotion).
- `dep-domain` / `dep-domain-refuted` — target 3 (remaining-expectation
  substitution; disabling it leaves a free-`Rexp_var` rejection where a goal
  should be).
- `dep-rec-ih` — target 4, closing the V9 coverage gap named in the refresh;
  ruling 4: no guard, the fixture pins the unguarded behaviour.
- `dep-subject-call` — `let ok : int{ _ > a + 1 } = mk (a + 1)`: a retained
  structural subject lowering congruently on both sides.
- `dep-retention-rule` — the CRITICAL-1 discriminator: a Total callee whose
  argument type does **not** cross Logicality, called twice around a
  mutation; the two evaluations must lower to distinct terms (with the
  first draft's callee-only rule they collapse to one congruent `Call`).
  Also pins the monotonicity invariant directly: the same call site's
  retention under the bottom oracle implies its retention under the
  consumer oracle (codex delta request).
- `dep-opaque-subject` — a whole-opaque actual: `subject ≡ Rexp_ident v`
  binds to the memoized `lower_subject` term; the codomain fact and the
  goal meet on it (disabling the memo or the descent flips it).
- `dep-interior-leaf` — the CRITICAL-2/№1 discriminators, both shapes:
  `mk2 (Some (f a))` (unary constructor around one opaque child — the leaf
  must bind to `f a`, not to the constructed value) and `mk (f a + 1)`
  (retained call around one opaque child — the caller's fact must be about
  `opaque(f a) + 1`, exactly what was checked).
- `dep-partial-embedded` — partial application of
  `x:int -> (int{ _ > x } -> int)`: the returned arrow's printed and
  persisted type carries the evaluated subject; applying it discharges
  against the instantiated domain.
- `dep-let-named` — `let v = a * b in mk v`: ident subject + let equality
  reaching later goals.
- `dep-subject-capture` — a predicate-internal dependent call whose actual
  mentions an enclosing dependent parameter, imported through a `.cmi`: the
  substitution callback's rename-map invariant.
- `dep-labelled` — `~x:` own-domain instantiation (the row's `dap_domain`
  is closed at its own subject; disabling the own-domain half of fold step 3
  flips it); commuted-supply rejection pinned as a located error
  (ruling 3 sentinel).
- `dep-late-solved-subject` — the delta-round discriminator for the
  shape-driven descent: a dependent stage with a polymorphic domain and an
  actual `tot y` whose type and mode solve only later in the unit — typing
  stores an opaque leaf while the walk's own lowering retains; the program
  must verify (a replay-driven descent fatals on it).
- `dep-partial` — `gt 1` then applied: the binder survives reconstruction
  and instantiates at the second application.
- `dep-ho-escape` — the existing `dependent-arrow-escape` fixture stays
  byte-identical.
- `dep-eta-reject` — the omittable-elimination shape as a walk rejection
  (ruling 3 sentinel).
- `dep-metadata-check` — the fail-closed algorithm (walk item 1): a
  hand-mutilated tree is out of reach of a fixture, so this pins the two
  reachable boundary shapes (the eta and commuted-supply sentinels stopping
  at the check or the free-variable rejection, never the legacy fact path).
- `dep-import` — imported dependent arrow: instantiated fact under a
  recorded admission, conditional-verdict line; plus the artifact fixture
  (old 585 reader rejects both new artifacts by magic).
- `dep-predicate-apply` — a dependent call inside a predicate: formation
  accepted, call uninterpreted, byte-pinned query unchanged.

### Decisions taken (amendment)

- **Predicate-typing owns the fix** — Decision 1 above; the batch's only
  open ⚑.
- **Instantiation at application typing** — owner ruling; also forced:
  later arguments' expected types must be instantiated before those
  arguments are typed, and only the typechecker is there.
- **Evaluated subjects adopted wholesale, stored per argument** —
  resolution of ruling 2's delegated question plus the review round's
  convergent correction: embedded subjects force structural subjects
  wherever types flow; the meta-ruling makes wholesale the reading; storing
  the subject (vox2's shape) is what makes leaf binding positional and
  fail-closed.
- **One shared retention rule, the occurrence lowerer's** — review
  CRITICAL-1: Total-callee alone can equate evaluations straddling a write;
  the argument-crossing half is load-bearing, and a single predicate used
  by both producers is the only arrangement that cannot drift.
- **Rows close their own domains; the descent reads shapes** — delta-round
  corrections: the producer instantiates each row's domain at its own
  subject (the free-variable invariant stays exceptionless), and the walk's
  descent dispatches on the stored subject's shape without ever
  re-evaluating the retention rule (its non-oracle inputs are
  time-varying). Round-2 revision note below.
- **Conditional binder freshening ported** — vox2's flag; unconditional
  freshening is incompatible with staged instantiation (nested-arrow
  counterexample), and the parity row now says so instead of claiming SAME.
- **Metadata as a new `exp_extra`, attached only when a binder was
  consumed** — the non-dependent road keeps zero metadata; the explicit
  absence/uniqueness/alignment algorithm distinguishes "no metadata because
  non-dependent" from "misaligned" and makes both defect classes fatal.
- **Callee side substitutes to a free ident; parameter-annotation formation
  promotes at assembly** (vs vox2's rename + environment registration) —
  closed markers the existing walk lowers (probe-verified), formation
  bounded to the function's own annotation surface with the labelled stage
  restricted to its own domain (formation invariant), no new environment
  machinery.
- **`Known_arg` pre-strip, apply-result strip, payload-headed invariant all
  unchanged** — instantiation happens on declared types and metadata copies;
  no new `ctype` arms, no weakening.
- **Commuted supply and eta path: located rejections day one** — RULED
  follow-up (ruling 3), each with a sentinel fixture so admitting them later
  is a visible expectation flip.
- **Artifact version: conservative common bump** (585→586) rather than
  splitting the CMT magic out of `MAGIC_NUMBER__VERSION` — the `.cmi`
  layout-unchanged fact is recorded as the reason the bump is churn, not a
  layout requirement; splitting the build's version scheme is not worth one
  unreleased-branch bump.

### Owner rulings applied (2026-08-21 afternoon)

Recorded in `vox/design-decision-rulings-2026-08-21.md`
("DEPENDENT-APPLICATION FLAGS"), applied throughout this amendment:

1. Parameter-annotation spelling: **ported in this work** (assembly-time
   promotion; the probe-found scope escape disappears).
2. Subject escape: **embedded evaluated subjects adopted**; the
   located-error surface this design first proposed is rejected; the
   delegated wholesale-vs-partial question is resolved **wholesale** (see
   Subjects).
3. Commuted supply and eta path: **follow-up confirmed**; located rejections
   day one, recorded parity rows, sentinel fixtures.
4. Recursive induction hypothesis: **no guard confirmed**; `dep-rec-ih` pins
   it.

### Revision note (design review round 1, 2026-08-21)

Both review lanes returned "design sound: no"; every finding is folded in
above. The substantive changes, with the counterexamples they answer:

- **Stored per-argument subjects** replace the minted-ident list (codex
  CRITICAL-2 ↔ claude #1, convergent): `mk2 (Some (f a))` no longer binds
  the child's name to the constructed value, and `mk (f a + 1)` no longer
  hands the caller a fact about a term stronger than the one checked — the
  parallel descent gives every leaf its positional origin, and whole
  opacity is `subject ≡ Rexp_ident v`, not a singleton-list heuristic.
- **The shared retention rule** replaces the callee-only stability rule
  (codex CRITICAL-1): a Total callee over a non-Logicality-crossing
  argument, called twice around a mutation, no longer collapses to one
  congruent `Call` — typing retains only what the occurrence lowerer
  retains, by shared code with a monotone oracle, and the
  `dep-retention-rule` fixture discriminates it.
- **Conditional binder freshening** (claude #2): the nested-arrow
  counterexample (`gt3 : w:int -> x:int -> int{ _ > w + x }`) no longer
  strands a stale binder — `instantiate` runs with freshening off, vox2's
  flag ported; the parity row is corrected from "SAME" to "requires the
  port".
- **Own-name respelling and limitation** (claude #3): the callee example is
  spelled with the hole; the term-parameter own-name limitation and the
  parameter-spelled-recursion limitation are recorded with fixtures.
- **Labelled promotion restricted to own-domain mentions** (claude #4): the
  formation invariant (`typing/types.mli:425-434`) is cited as the binding
  region's definition; labelled codomain mentions stay escaped.
- **Artifact mechanics completed** (codex #3): the single-source magic
  derivation is cited and the conservative common bump chosen and justified.
- **Fail-closed metadata algorithm made explicit** (codex #4 ↔ claude #6),
  **interior-leaf and matrix fixtures added** (claude #5, codex #5), **the
  mixed-version sentence recorded** (claude #7), and the substitution
  capture invariant and `mentions` stored-type traversal noted from the
  codex audit.

### Revision note (delta review, round 2, 2026-08-21)

Both delta lanes closed every round-1 finding; each found one defect in the
revised walk contract, both folded in above:

- **The descent is shape-driven, never rule-replaying** (claude delta): the
  round-1 text replayed `retains_call` with the typing-time oracle, but the
  rule's *other* inputs are time-varying — a polymorphic domain solved
  after the application (`crossing_of_ty` over a then-unsolved variable) or
  a mode determined later (`check_const_conservative`) makes a walk-time
  replay retain where typing minted a leaf, and the replay-driven descent
  fatals on that legitimate program. Walk item 4 now dispatches on the
  stored subject's shape alone; the monotonicity invariant is extended to
  mode determination and type solving, preserving the one direction facts
  rely on (typing-retained ⇒ walk-retained) while the converse is never
  consulted. Fixture `dep-late-solved-subject`.
- **Own-domain binders are closed in the producer** (codex delta): after
  `gt 1 2` the second row's domain read `int{ Rexp_var y > 1 }`, which the
  round-1 walk's own no-free-`Rexp_var` clause and the unchanged
  `check_imposable` rejected before the promised own-binder binding could
  run — the design's own contract refused target 3 and `dep-labelled`.
  Fold step 3 now instantiates each row's domain at the row's own subject
  (codex's offered alternative), keeping the free-variable invariant
  exceptionless and deleting the special own-binder binding. Reconciling
  it exposed an adjacent round-1 defect in item 1, corrected in the same
  stroke: metadata *absence* where a binder was consumed is a **located**
  rejection (the eta wrapper reaches that shape from valid source), not
  `fatal_error`; only present-but-misaligned metadata is fatal, and a
  residual free `Rexp_var` is located when it is an omitted stage's binder
  (ruled follow-up), fatal otherwise.
- The `let rec` parameter-spelling sentence is softened per the claude
  lane's probe: the dependent arrow exports only when recursive occurrences
  leave the annotated codomain unconstrained; both shapes are pinned.

### ⚑ For confirmation (owner)

1. **Ownership**: this amendment and the fix live on predicate-typing
   (rationale in Decision 1); refinement-flow's deferral row is superseded
   by pointer, and the walk half lands in vc-generation at the restack.
   Confirm the placement.
