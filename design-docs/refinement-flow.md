# Vox refinement flow

This piece makes refined programs typecheck. Type-formers made `t{p}` exist and
made it rigid; nothing could introduce or consume a refined value. This piece
adds the two directions of flow:

- **Introduction**: a value whose *declared* type is refined enters the program
  — a binder enters the environment, an identifier is used, a function returns,
  a pattern takes a value apart. The value is used at its payload: the
  refinement's head is stripped.
- **Expectation**: an expression is checked against a *declared* refined type —
  a function argument, an annotated binding, a returned value, an assignment.
  The expression is checked against the payload, and the site is recorded as an
  obligation in the typed tree.

No verification conditions are generated and no solver runs. Every obligation
is recorded and accepted. VC generation is a later piece that walks the typed
tree this piece produces; the tree is the interface between the two pieces.

## The invariant

**The typechecker never gives an expression node a refined head.** Every site
that turns a declared type into an expression's type strips the head first,
and every site that checks against a refined head checks against the payload
instead.

Refined types live only where they were declared: value descriptions (the
environment and the `.cmi`), arrow domains and codomains, type declarations,
`pat_type` on patterns, and the obligation markers this piece adds. Nested
refinements — `int{p} list`, an arrow with a refined domain — appear in
`exp_type` freely; only the head is governed.

This is a construction-time discipline, not a property the final tree can
promise. The strips run when a node is built, but unification is global and
time-extended: a node whose type is still an undetermined variable at
construction carries the variable itself, and a *later* unification can solve
that variable against a declared refined type after the strips have run. In
`let f g = (g (), (g : unit -> int{p}))`, the apply node `g ()` is built while
`g`'s codomain is `'b` — nothing to strip — and the annotation on the second
component then solves `'b := int{p}`; the first component keeps the refined
head, and `f` infers `... -> int{p} * (unit -> int{p})`. Swap the components
and the strip fires and it infers `int`. So the honest global statement is:

> A refined head on an expression node is always a type variable that was
> later solved against a declared refinement. The typechecker never places
> one directly, and one never originates from an expression.

The residue this leaves is bounded and directional:

- **Predicates are never invented.** Every refined head traces to a
  refinement somebody wrote; variable solving is the only transport, and a
  variable only meets a refined type by instantiating a declared one.
- **Nothing is discharged silently.** Obligations exist only at marked sites
  and apply nodes; a stale refined head meeting a bare concrete type is the
  type-formers rigid clash — a rejection.
- **Acceptance at the margin is order-dependent, conservatively.** With
  `let y = g () in ...`: annotate `g` before using `y` and the occurrence
  strip fires; use `y` at `int` first and a later refined annotation on `g`
  is a clash. Both orders are sound; one is a rejection the other accepts.
  This imprecision is accepted rather than worked around (see the decision
  at the end).

Head-only is deliberate and load-bearing. Stripping the head of the type of the
value at hand is a projection: forgetting a fact about *this* value. Stripping
under a constructor would change what a type says about *other* values —
type-formers pinned `int{p} list` vs `int list` as a rigid clash, and GADT
refutation over indices needs `int` and `int{p}` to stay distinct. Nested
refinements flow compositionally instead: checking `[a; b]` against
`int{p} list` checks each element against `int{p}`, which is a head.

What the invariant buys:

- **`ctype` gains no new arms.** Unification never meets a one-sided refined
  head placed by an expression, because expressions never produce refined
  heads. The type-formers rigid clash stays as the backstop, so a missed strip
  is a loud type error at the use site — never a silent discharge of a
  predicate. In particular there is no unify-time "weakening" rule anywhere:
  no hook in `unify_exp`, `rue`, `moregen` or `subtype`. Everything happens in
  `Typecore`, before unification.
- **A refinement can never be invented by inference.** A type variable
  acquires a refined type only by unifying with a *declared* type
  (instantiating `val g : int{p} -> unit` against `'a -> 'b` solves
  `'a := int{p}` — that is higher-order contract flow and is wanted). It can
  never acquire one from an expression. So
  `let f c x = if c then x else v` with `v : int{p}` infers
  `bool -> int -> int`; a written contract can propagate into an inferred
  type only through the variable-solving residue above.
- **The tree is observable** — the probe below prints every refined head it
  finds, so the residue is pinned in expected output rather than invisible.

## Introduction: the strip

Rule: when a declared type becomes the type of a value occurrence, take the
payload of its head, after alias expansion, after instantiation.

Sites, concretely:

- **Variable binders**: a *local, immutable* variable pattern enters the
  environment at the payload of its declared type; the pattern's `pat_type`
  keeps the refined type — that is the fact record for VC gen. This is vox2's
  doctrine — names bind at carriers, predicates live on the record — and it
  makes every environment-mediated read of a local binder carrier-typed, not
  just the `Texp_ident` path. Exemptions, each forced:
  - **mutable binders** enter at the declared type — the write site `x <- v`
    checks the right-hand side against the environment type, and stripping the
    entry would silently discharge the contract at every write. Reads are
    covered by the occurrence strip below.
  - **module-level bindings** enter at the declared type — `Typemod` reads the
    exported signature back out of the same environment, and `val v : int{p}`
    must survive to the `.cmi`. Uses are covered by the occurrence strip.
- **`Texp_ident`**: the occurrence's `exp_type` is the payload of the instance
  of `val_type`. All value kinds, including mutable variables. This is what
  makes uses of the *unstripped* entries — module-level values, `.mli`
  imports, mutable reads — carrier-typed; for local immutable binders it is a
  no-op that keeps the invariant if any entry reaches it unstripped.
- **Application results**: the head of the instantiated codomain, at the apply
  node. This is what keeps `g () + 1` and `let x = g ()` (inferring
  `x : int`) working for `g : unit -> int{p}`.
- **Field reads**: the head of the instantiated label type at `Texp_field`.
- **Patterns**: a destructuring pattern (constant, construct, tuple, record)
  checked against a refined-head expected type recurses at the payload, and
  `pat_type` keeps the refined type — that is the fact record for VC gen. A
  variable pattern likewise keeps the refined type on `pat_type` and binds
  into the environment per the binder rule above.

Strip after `instance`, not before: instancing copies the whole `Trefine` so
the payload stays shared with the copied predicate interior; projecting from
the copy keeps them connected.

**Binder strip and occurrence strip together** cover the introduction side:
the binder strip guarantees no environment reader can surface a refined head
for a local immutable name, and the occurrence strip covers the entries that
must stay declared (module-level, imports, mutable). Recursive definitions
need no exemption under head-only stripping: a recursive *function*'s type is
an arrow — not refined-headed, so the binder strip is a no-op on it — and its
domain/codomain contracts are handled at application and by the expectation
funnel; refined-headed recursive values are ruled out by the `let rec`
right-hand-side restriction.

## Expectation: the obligation

Rule: in `type_expect`, before dispatching to `type_expect_`, if the expected
type's head is refined (after alias expansion): check the expression against
the payload expectation instead, and attach an `exp_extra` marker —
`Texp_refinement_obligation of type_expr` — carrying the imposed refined type.
The node's `exp_type` stays payload-headed, per the invariant.

`type_expect_` has exactly one caller, `type_expect` (typecore.ml:6663), and
`type_exp` is `type_expect` at a fresh variable, so this interception point
covers every expression the typechecker visits — annotations, `let`
annotations, a `fun` body against a refined codomain, constructor and record
arguments, list and array elements, `if`/`match` arms, assignment right-hand
sides, optional-argument defaults. None of those get code of their own; they
fall out of the funnel. Hooking here rather than at any unification entry point
is the structural choice: the tree has ~25 direct `unify_exp_types` call sites
that bypass `unify_exp`, so unification is not a funnel, but `type_expect` is.

**Application arguments** are the one exception. At `Known_arg`, the expected
argument types handed to `type_argument` (both instance copies, `ty_arg'` and
`ty_arg0'`) are stripped to their payloads up front, so the funnel never sees a
refined expectation there and no marker is attached. The apply node itself is
the obligation record: the funct's arrow retains its refined domain, and VC gen
reads per-argument obligations from it. Rationale: `type_argument`'s interior
unifies its two expectation copies against the argument and, on the
labelled-reordering path, eta-expands through them; letting a refined head into
that machinery means auditing every internal unification, whereas the arrow is
already the durable record.

An expectation whose head is an undetermined variable is not an obligation site
and is left alone — and by the invariant, no refined head can later flow into
it from an expression, so there is no deferred-strip debt.

## What VC generation will read

The contract with the later piece — everything is in the typed tree, no side
tables:

- **Facts**: patterns keep the refined type in `pat_type` — variable and
  destructuring alike, the fact record for local binders; `Texp_ident` nodes of
  module-level and imported values carry their (declared, refined) value
  description; apply nodes carry the funct whose arrow codomain states the
  result contract.
- **Obligations**: `Texp_refinement_obligation` markers in `exp_extra`; plus
  apply nodes, whose funct arrow domains state the per-argument obligations.

## Aliases

`type nat = int{ _ >= 0 }` must behave exactly like the refinement it names:
both rules fire after head expansion, on both sides. Stripping an occurrence of
type `nat` yields `int` — the alias name is lost from that occurrence's printed
type; accepted. The gate must be cheap because it runs on every identifier
occurrence and every application: a head-descriptor check, then for `Tconstr` a
per-path may-reveal-a-refinement cache over the manifest chain (bypassed when
the environment has local constraints, which can change what a path means).

## Mutability

Mutable binders are exempt from the *binder* strip — the environment keeps the
declared type, which is precisely what makes every write an obligation — and
covered by the *occurrence* strip, which is what keeps reads carrier-typed.
Unlike vox2, no expression node ever carries the refined head: vox2 exempts
mutable names at occurrences too and then needs weakening machinery downstream;
here the two rules split the job instead.

- `let mutable x : int{p} = e`: the initialiser and every `x <- v` check
  against the declared `int{p}` — funnel obligations. Reads strip, like any
  occurrence.
- `ref`, mutable record fields, arrays: the contract sits nested —
  `int{p} ref` — so writes reach a refined expectation by instantiation of
  `:=`/`Array.set`/the label type (funnel), and reads are application or field
  results (strip). Zero special cases.

Reading `x` yields a value that satisfied `p` at every write; whether `p` may
be treated as a stable invariant under aliasing and interleaving is VC gen's
problem, not typechecking's. This piece only fixes where the checks sit.

## Out of scope

- **VC generation and the solver.** Obligations are recorded and all accepted.
- **Dependent arrows stay rejected at consumption**
  (`Unsupported_dependent_arrow`, from type-formers). Substituting the actual
  argument into a codomain predicate — including what to do when the argument
  is not a path — is its own piece.
- **Signature inclusion beyond identity.** `val v : int{p}` in a `.mli`
  matches a definition declared at an alpha-equal `int{p}` (type-formers'
  `moregen` identity arm) and nothing else. The strengthening rule — a bare
  definition satisfying a refined signature by proof — turns inclusion into an
  obligation source inside `Includemod`/`Ctype`, which has no path to the typed
  tree and therefore needs a side channel; that is a later sealing piece. The
  clash message when a bare definition meets a refined signature should say to
  annotate the definition.
- **Coercions.** `(e :> int{p})` stays rejected; coercion never imposes an
  obligation.
- **Class and object types.** A refined method type meets the rigid clash. If
  a strip at `Texp_send` results turns out to be one line, take it; otherwise
  the loud rejection stands, matching type-formers' treatment of class types.
- **`assume`/admitted obligations.** Solver-adjacent; later.

## The probe

Expect tests observe printed types, and a program that typechecks prints the
same whether an interior node secretly carries `int{p}` or `int` — the
deliverable of this piece is a property of the tree, not of the output. So the
piece includes its own instrument: a flag-gated walk of the typed tree that
*prints* every expression node whose `exp_type` has a refined head (alias
expansion included), with its location. The walk also prints every
pattern-bound variable whose *environment entry* (looked up in the enclosing
expression's `exp_env`) has a refined head, tagged distinctly — this is what
makes the binder strip observable: on the fixtures, local immutable binders
never appear, mutable and module-level binders appear and are pinned as
expected, and deleting the binder strip adds lines. An observer, not an assertion: the
legal variable-solving residue means an assertion would fail on valid
programs, and distinguishing a solved variable from a missed strip by graph
shape is fragile. Printing instead makes the expected output the judge — on
the strip fixtures the probe's output is empty and a strip deleted by mutation
adds lines to it; on the residue fixture the output documents exactly which
nodes carry the leftover head, so any later tightening or regression shows as
a diff.

## Failure modes, stated

- A missed introduction strip meeting a concrete type is a rigid clash: loud,
  a wrong *rejection*, never a wrong acceptance. Meeting an undetermined
  variable it propagates silently — indistinguishable after the fact from the
  legal solving residue, which is exactly why the probe prints rather than
  asserts. The binder strip removes this exposure for local immutable names
  (their entries are carrier-typed, whatever reads them); what remains to
  audit is the readers of the exempted entries — module-level, imported and
  mutable values — beyond `Texp_ident`.
- A missed expectation site is also a rigid clash: the program is rejected,
  never accepted with a dropped predicate. Both directions fail conservative.

## Tests

`vox/refinement-flow.ml` expect tests, red-green per convention: RED pins the
current type-formers rejections of every fixture below; GREEN flips them to
the accepting behaviour, so the expectation diff is exactly the set of programs
this piece admits. Each strip/obligation site gets a fixture that fails if that
site alone is disabled:

- occurrence strip + funnel: `let v : int{ _ > 0 } = 5 in v + 1`
- binder strip, via the probe's environment report: the local binder above
  shows no environment line; a module-level `v` and a `let mutable x` show
  pinned lines (declared entries, by exemption); a use of a module-level
  refined value is the occurrence-strip discriminator, since local uses are
  already covered by the binder strip
- no propagation: `let w = v in w` infers `int`;
  `let f c x = if c then x else v` infers `bool -> int -> int`
- apply-result strip: `g : unit -> int{p}`; `g () + 1`; `let x = g ()` infers
  `int`
- argument obligations: `f 5` for `f : int{ _ > 0 } -> int`; labelled,
  optional and commuted variants; a second use of the same contract
- codomain obligations: `let h : unit -> int{p} = fun () -> 5`; per-arm:
  `let k c : int{p} = if c then 1 else 2`
- aliases on both sides: everything above through `type nat = int{ _ >= 0 }`
- nested via expectation: `let l : int{p} list = [5; v]` typechecks;
  `(l : int list)` still clashes
- higher-order: `List.map f l` with `f : int{p} -> int` and
  `l : int{p} list`
- mutability: `let mutable` init, write, read; the `ref` round trip
  `let r : int{p} ref = ref 5 in r := 6; !r + 1`; a mutable record field
- destructuring: `let (a, b) : (int * int){p} = e in a + b`, and the refined
  `pat_type` fact observed via the probe
- export: module-level `let v : int{p} = 5` prints `val v : int{p}`; `.mli`
  identity inclusion; bare definition vs refined signature is an error naming
  the fix
- recursion: `let rec f (x : int{ _ > 0 }) : int = ...` calling itself
- still rejected: dependent-arrow application, `(e :> int{p})`
- the residue, pinned: `let f g = (g (), (g : unit -> int{p}))` infers a
  refined first component and the probe reports its node; the swapped
  `let f' g = ((g : unit -> int{p}), g ())` infers `int` there and the probe
  is silent — the order-dependence is documented, not discovered
- the probe runs over every fixture above, and is empty except where pinned

## Decisions taken

Recorded per AGENTS.md: real forks, the route, and why. vox2 is the evidence
that each mechanism is implementable in this typechecker (its funnel,
`Known_arg` strip, occurrence strip and `type_pat` hook were all located and
read); the routes below deliberately differ from vox2 where stated, and no code
is copied.

- **The variable-solving residue is accepted, not worked around.** A strict
  global no-refined-head invariant is unattainable with undetermined
  variables: the strips are construction-time and unification is not. The two
  ways to force it were both rejected. Forbidding unification from solving a
  variable to a refined head kills higher-order contracts — `List.map g l`
  with `g : int{p} -> unit` needs `'a := int{p}`, and linking to the payload
  instead would drop the contract silently, which is the one failure class
  this design refuses. A post-typing re-strip sweep at generalization
  boundaries can rewrite `exp_type` fields but cannot soundly tell a declared
  refined pattern or environment type from a late-solved one, so it would
  erase exactly what the probe should show. The residue is sound in both
  directions (never invents, never discharges), order-dependent at the
  margin, and pinned by fixture.
- **Obligation nodes keep payload-headed `exp_type`**, with the refined type in
  the marker. vox2 rewrites `exp_type` back to the refined type, and
  consequently needs a covariant weakening rule at its unify wrapper plus
  carve-outs (statement checking, carrier queries) wherever a refined head can
  resurface. Keeping expression types payload-headed makes "no new `ctype`
  arms" and the probe possible. Cost: the toplevel echoes `(5 : int{p})` as
  `- : int`; the contract remains visible in `val` printing and in the marker.
- **Binders strip into the environment, vox2-style; occurrences strip too**
  (amended 2026-08-18, superseding the earlier occurrence-only decision;
  direction set by the project owner: introductions strip, as in vox2). The
  earlier draft kept declared types in the environment to avoid vox2's
  exemptions; the amended rule takes the exemptions (mutable, module-level —
  recursive right-hand sides turn out to need none under head-only stripping)
  in exchange for vox2's guarantee that *no* environment reader can surface a
  refined head for a local immutable name — the earlier draft had to leave
  "audit every environment reader" as an implementation obligation. The
  occurrence strip is retained for the exempted entries, so, unlike vox2,
  mutable reads are still carrier-typed and no weakening machinery is needed.
  The probe's environment report makes the binder strip observable.
- **`Known_arg` pre-strips instead of letting the funnel fire inside
  `type_argument`** — rationale under "Expectation" above; the apply node is
  the obligation record for arguments.
- **Mutable variables are exempt from the binder strip, not the occurrence
  strip.** vox2 exempts `Val_mut` at *both*, so mutable uses carry refined
  heads and downstream weakening machinery absorbs them; here the environment
  keeps the declared type (writes are funnel obligations against it) while
  reads are ordinary projections, so no refined head reaches an expression
  type and the invariant holds.
- **No same-refinement shortcut.** vox2 skips the obligation when the value
  already carries a syntactically identical refinement. Here expression types
  are never refined-headed, so the case cannot arise; the trivial obligation
  (fact `p` proves obligation `p`) is recorded and left to the solver piece.
- **Signature inclusion is out of scope** rather than half-supported: identity
  matching already works via type-formers, and the strengthening rule needs a
  side channel that would compromise "the typed tree is the whole interface".
