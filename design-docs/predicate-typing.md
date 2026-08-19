# Vox predicate typing

Refinement predicates become well-typed booleans. Today `int{ 42 }`,
`int{ _ + "x" }` and `int{ String.length _ }` are legal types: the
type-formers piece resolves names and gates the sublanguage syntactically,
but applies no typing judgment ("the typing rules for refinements belong to
a later piece" — types.mli, type-formers.md). This is that piece.

After it, every predicate is checked, at the point the type is formed, to
be a `bool` under an environment where the hole and the dependent-arrow
binders have their payload types. Everything downstream (the translation to
solver obligations) can then assume well-typed predicates and ask for a
node's type on demand.

## The judgment

For a refinement type `t{ p }` formed anywhere (expression annotations,
declarations, signatures, functor bodies — everywhere `transl_type` runs):

    E ; Γ ⊢ p : bool

where `Γ` extends the ambient typing environment `E` with:

- the hole `_` at the *payload* type `t` (every occurrence, since `_` means
  the value of the innermost enclosing refinement);
- each dependent-arrow binder in scope at its *payload-stripped* domain
  type (see "Binders see payloads" below);
- the predicate's own binders (`let`, `fun`, `match`) as ordinary bindings.

`p` is full OCaml typing over the already-gated sublanguage: application
with labels, tuples, constructors, field access, `if`, single `let`,
`fun`, `match` with the supported patterns, constraints. Type errors are
located at the offending subterm and use the ordinary type-error message
machinery, so `int{ _ + "x" }` reports the `string`/`int` clash exactly
where `+`'s argument is.

### Binders see payloads

In `x:int{q} -> int{ _ > x }`, the binder `x` occurs in a predicate at type
`int`, not `int{q}`. Two reasons, both forced:

- Refined heads are rigid (type-formers): `x : int{q}` would fail to unify
  with `int` at `>` and every predicate mentioning a refined binder would
  be ill-typed.
- It matches the program-side rule (refinement-flow): binders *strip* into
  the environment; the refined type lives on the annotation, the use sites
  see the payload. Predicates are use sites.

The same view applies to free names: a module value whose signature type
carries refined heads is used in a predicate at the stripped type. Deep
heads strip the same way they do in refinement-flow's occurrence rule.

### Where it runs

Inside `transl_type`'s `Ptyp_refine` case (typetexp.ml:1452), after the
payload translates and after (or interleaved with — implementor's choice)
the mirror is built. The dependent-binder plumbing already scopes names
(`refinement_scope`, typetexp.ml:900); it grows a `type_expr` per binder —
the arrow domain's payload — recorded when the arrow's domain is
translated.

## The central decision: a typer over the mirror, not a Typecore reentry

vox2 types predicates by re-entering the full expression typer through a
forward-declared callback (`Typetexp.type_refinement`, installed by
Typemod) inside a transient frame, and stores the result: its mirror
carries `rexp_type` on every node, in `types.ml`, through every traversal
and every `.cmi`. Its plan records the transient frame as the hardest part
(exception safety, levels, env side effects), and its measurements record
the storage cost (alpha-equality over the typed mirror at 31% of checking
time).

This piece takes a different route:

**The predicate typer is a small, standalone, rerunnable judgment over the
resolved mirror** (`typing/vox_rexp_type.ml`, ~one file), using `Ctype`
unification directly. It runs at formation for errors, and the translation
piece reruns it (on demand, per obligation) when it needs node types.
Nothing new is stored: `Types.refinement_expression` is unchanged, `.cmi`s
are unchanged, `Subst`/`Btype`/equality are unchanged.

Why this shape:

- The sublanguage is deliberately tiny; its typing rules fit in one
  readable file. Reusing Typecore buys generality the sublanguage cannot
  express (objects, effects, GADT refutation…) at the price of vox2's
  transient-frame hazards and a permanent Typemod-installed callback knot.
- Rerunnable-over-the-mirror is exactly what translation needs: the mirror
  is resolved (paths, idents), so re-typing needs no scoping context — an
  `Env.t` for path lookups (`find_value`, constructors, labels) and the
  binder types suffice, and those are available wherever obligations are
  discharged, including for predicates imported from other units.
- One judgment, two call sites. The alternative — Typecore at formation,
  a mirror-typer at translation — is two typers that must agree, which is
  a standing divergence defect factory.

Costs, stated honestly:

- Re-typing at translation repeats work. The predicate is small and the
  translation piece may memoize per obligation; vox2's measurements say
  the *storage* was the expensive pole, not the checking.
- The mirror typer reimplements a subset of inference: unification-based
  application/construction/branch typing over the sublanguage. It does NOT
  reimplement generalization: `let` inside predicates is monomorphic
  (recorded restriction, below).
- Determinism obligation: formation-time and translation-time runs must
  produce the same judgment. Same code + resolved input + no
  generalization makes this structural, but it is an invariant to state
  and test (a fixture re-types an imported predicate and compares).

### Typing rules, concretely

- `Rexp_hole` : the payload type (provided by the caller).
- `Rexp_var` : the binder's recorded type (payload-stripped).
- `Rexp_ident p` : `(Env.find_value p).val_type`, instantiated, stripped of
  refined heads at every depth (`Vox_rexp`-style strip; same rule as
  refinement-flow occurrences).
- `Rexp_constant` : as `Typecore.type_constant`.
- `Rexp_apply` : unify the function type against the argument row,
  label-by-label in the sublanguage's already-restricted form (no
  optional-argument shenanigans: labelled and unlabelled only — reject
  optional labels with a located "not supported", extending the existing
  gate).
- `Rexp_tuple`, `Rexp_construct`, `Rexp_field`, `Rexp_ifthenelse`,
  `Rexp_match` (patterns type against the scrutinee; arms unify),
  `Rexp_let` (monomorphic), `Rexp_fun` (fresh domain variable),
  `Rexp_constraint` (unify with the written type — those types are already
  in the type graph).
- The whole predicate unifies with `Predef.type_bool`.
- Levels: the judgment runs inside `Ctype.with_local_level` (or the
  current idiom) so its fresh variables cannot leak into ambient
  generalization; any variable still free at the end is fine — predicates
  may be polymorphic in the payload's variables ('a{ ... } typechecks; the
  solver's stricter rules come later).

### Recorded restrictions (extend the existing "not supported" gate)

- `let` in predicates is monomorphic.
- Optional/`%call_pos` labels in predicate applications are rejected.
- No new sublanguage forms are admitted by this piece.

## Deliberately out of scope

- **Mode discipline** (predicates checked total + logical; comparison
  admission in predicate scope tied to the solver-modeled set): its own
  piece, blocked on two open rulings (comparison-admission mechanism;
  ghost × refinement interaction). This piece checks *types* only. The
  syntactic totality gate stays as is.
- **Storing types** in the mirror or `.cmi` — see the central decision.
- **Any solver-facing semantics** (BV63 vs Int, modelability) — the typer
  types `+` at `int -> int -> int` and does not care what it means.

## Compatibility fallout (expected, wanted)

Existing vox tests contain predicates that only now become errors
(`int{ _ }` — a hole at type int where bool is required — appears in
type-formers fixtures). The piece follows red-green: the RED commit pins
today's acceptance of ill-typed predicates in a new fixture file; GREEN
lands the judgment and re-promotes, and the expectation diff over both new
and pre-existing fixtures is the demonstration. Signature/structure
mismatch messages must not regress (predicates in `.mli`s get checked at
their own formation).

## Tests

`testsuite/tests/vox/predicate_typing.ml` (expect):

- `int{ 42 }`, `int{ _ + "x" }`, `int{ String.length _ }` rejected with
  located messages; `int{ _ > 0 }` accepted.
- Hole typing: multiple occurrences, nested refinements (each hole is the
  innermost payload), hole under `let`/`match`.
- Binders: bare and `~x:` labelled dependent binders usable at payload
  type; refined binder stripped (predicate `x + 1` over `x:int{q}`);
  binder shadowing by predicate-local `let`/`fun`/`match`.
- Free names: qualified values, constructors, fields; a value whose
  signature type is refined is stripped at use.
- Sublanguage typing: `fun`, application with labels, tuples (labelled),
  `match` with the supported patterns, constraints (`(e : t)` unified).
- Monomorphic `let` and optional-label rejections.
- Polymorphic payload: `'a{ _ = _ }`-style acceptance.
- Cross-module: a `.mli`-declared refined type used from another unit;
  the determinism fixture (re-type an imported predicate, same verdict).
- Interaction: rigid unification and printing unchanged (spot pins).

## Piece mechanics

Branch `jujacobs/vox/predicate-typing` off the stack tip (`32b38a5527`,
refinement-flow), because the strip rule references refinement-flow's
occurrence semantics. Red-green commits as above.
