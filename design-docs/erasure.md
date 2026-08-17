# Vox erasure

We add an erasure mode axis, a construct that produces values on it, and a
field modality that erases data from representations.

    erased_ e            -- e is deleted from compilation
    @ erased             -- the value may only flow to erased positions
    { x : t @@ erased }  -- an erased record field: occupies no slot

The division of labour is deliberate and is the heart of the design:

- The **mode** `@ erased` is an information-flow property and nothing else.
  It has **no effect on the ABI**: an erased function parameter is passed
  physically like any other, an erased local is an ordinary binding. What the
  mode guarantees is that no retained computation ever reads an erased value,
  so the value's *content* is unobservable and `erased_ e` may compile to a
  placeholder without evaluating `e`.
- The **modality** `@@ erased` on a record field is what changes
  representation: the field occupies no slot in the record. Reading it
  fabricates a placeholder ("null or an appropriate value of the field's
  kind") at mode erased.
- To erase something from an ABI, wrap it: `'a Erased.t`, defined in the
  stdlib as `type 'a t = { erased : 'a @@ erased }`. Its zero-width field
  means the wrapper carries no data.

Additional conveniences are deferred until practice shows they are needed.

## History

The first iteration of this piece made `@ erased` itself ABI-bearing: an
erased parameter was not passed (zero-width, down the void path). That
entangled the mode with representation everywhere modes flow: erasure had to
be *invariant* in argument position, and the invariance had to be asserted
separately at every path that relates arrow modes directionally (`moregen`,
`subtype`, `build_subtype`, the `type_argument` loosening — review found them
one by one, each with a reproduced ABI mismatch). Optional parameters and
externals had to be rejected, generic higher-order functions could not accept
erased-parameter callbacks (no monomorphization, so no single ABI serves
both), and codegen needed a per-unit table of erased identifiers. Moving the
representation effect to a declaration-site modality removes the whole
family: modes flow with ordinary variance, and representation is determined
by types and declarations alone, as it is for everything else.

## Naming

`erased`, not `ghost`. The compiler already uses `ghost` for a different kind
of absence: `loc_ghost` (75 uses), `ghost_loc` (12) and `Location.ghost` (10)
across 32 files in `typing`, `parsing` and `lambda`, and `loc_ghost` is part of
the public parsetree surface. `erased` has no incumbent.

Say in user-facing documentation that this is what Why3 and Dafny call ghost
code, so the term stays findable.

## Lattice

| axis | min | max | legacy | fragment |
|---|---|---|---|---|
| Erasure | `Retained` | `Erased` | `Retained` | comonadic |

`Retained <= Erased`. A real value may be used where an erased one is
expected, since the context promises not to read it. The reverse cannot hold.

Comonadic on the substantive test: a value's mode is bounded above by the meet
of what its uses demand, so using a value anywhere retained forces it
retained.

## The information-flow rule

### Ambient erasure

*What may be used* at a position is governed by the expected mode, through
ordinary submoding. A value may be used only where the expected erasure is at
least its own.

*What gets deleted* is governed by the expression's own erasure. `erased_ e`
sets the expression to Erased, so it is deleted. An ordinary expression is
Retained and is evaluated, whatever position it sits in. Deletion never
happens implicitly; it follows the source, not the context.

An expression is checked at ambient Erased in two places: the body of
`erased_ e`, and the body of a closure that is itself erased. Everything else
is retained. Because `Retained <= Erased`, real values are usable inside
erased contexts, which is what specifications need. Erased values fail
everywhere else:

- `x + y` in retained code requires both retained
- `if`/`match`/`while`/`for`/guards/`assert` require retained scrutinees
- destructuring patterns read; variables, wildcards and aliases bind without
  reading, so `let x = erased_ e in ...` works
- record field access and mutation require a retained record
- the function position of an application requires retained (the call reads
  the closure), independently of any erased *parameters* the function has

The permissive expected mode (`mode_max`) requires Retained on the erasure
axis; erased-tolerant positions are a closed, spelled-out set: type-driven
positions (an `@ erased` arrow argument, an erased return), erased contexts,
and statement position (which discards the value).

### Arguments are not erased silently

An `@ erased` parameter accepts both kinds of argument:

    f (expensive ())            -- evaluated and passed like any argument;
                                -- the callee just cannot read it
    f (erased_ (expensive ()))  -- never evaluated; a placeholder is passed
    let x = erased_ e in f x    -- x is already a placeholder

The argument position does not create an erased context: an ordinary call
must not silently drop its argument's effects. Deleting an evaluation is
something the programmer writes.

### Closures

Capture propagates nothing: a retained closure may capture erased values
(the capture check meets the captured value's erasure down to Retained; body
uses still see the true erasure). The same carve-out applies in `close_over`
for partial application, so applying across an erased parameter does not
erase the result.

A closure's body is checked at the closure's own erasure: `erased_ (fun y ->
g y)` accepts `g @ erased` in its body. A lambda written directly at an
`@ erased` argument position without `erased_` is retained and its body is a
retained context — it is genuinely constructed and evaluated.

### Arrow modes: ordinary variance

With no ABI at stake, erasure follows the same rules as every other comonadic
axis: contravariant in argument position, covariant in return position,
equated by unification. In particular:

- an erased-parameter implementation seals behind a retained-parameter
  signature (it promises to read less), and the reverse is rejected
- the same through `(e :> t)` coercions, on both the `subtype` and
  `build_subtype` paths
- generic higher-order functions accept erased-parameter callbacks
- optional parameters and externals may be erased like anything else; the
  argument is physically passed, and the mode constrains OCaml-side uses only

All four arrow-mode paths are pinned in both directions in
`testsuite/tests/vox/erasure_subsumption.ml`.

### Structures and modules

A module block's fields are legacy (retained), so an erased value cannot be
stored in a structure — a compilation unit, a local module, a `let open
struct ... end`. A module allocation's erasure is capped to Retained so the
structure-item check rejects these uniformly. (`Erased.t` is the way to store
an erased value.)

### Erasure and mode crossing

No type crosses erasure, ever: an erased value's content may be a
placeholder, so treating it as retained is unsound regardless of the type —
the whole property collapses on immediates first. Enforced at the places
crossings are built (`Mod_bounds.min_crossable`, `cross_all_crossable`,
`Crossing.always_constructed_at`, `Axis_lattice.create`, the bool-created
crossings). `mod erased` / `mod retained` are rejected as kind modifiers and
`mod everything` excludes erasure (precedent: staticity).

## How `erased_ e` compiles

`e` is type-checked and then deleted, effects included: `transl_erased` emits
a placeholder of whatever layout the context requests (`dummy_constant` for
values, zeros for unboxed numbers, recursively for unboxed products; vector
layouts have no placeholder and remain a compiler error). The mode system
guarantees no retained code reads the placeholder. There is no other codegen:
no erased calling convention, no per-occurrence layout changes.

`erased_ (print_string "hi")` prints nothing. This is deliberate for now and
is unsound as a specification mechanism; the fix is to require `e @ total`,
deferred to keep this piece independent of totality.

## The `@@ erased` field modality

The one place erasure touches representation. `{ x : t @@ erased; ... }`
declares a field that occupies no slot in the record:

- **Construction** `{ x = e; ... }` checks `e` at expected erasure Erased (so
  both retained and erased values are accepted), evaluates a retained `e` for
  its effects, and stores nothing.
- **Projection** `r.x` has mode `erased` and fabricates a placeholder of the
  field's kind — null / a dummy value; it never reads memory.
- **Patterns** on the field bind placeholders at mode erased.
- **Signature matching** is fail-closed: two sides of a module boundary must
  agree on a field's erasure, since it decides the record's layout.
- **Mutable erased fields are rejected**: writing would be a no-op.
- The modality is recorded in the `.cmi` and printed back.

This is a comonadic *weakening* (`Join_const Erased`), which the modality
machinery (`Meet_const` only) does not express; it is carried as a separate
marker on the label declaration rather than widening the general modality
algebra for one constant use.

## `Stdlib.Erased`

    type 'a t = { erased : 'a @@ erased }

`'a Erased.t` is how a value is erased from an ABI: the wrapper is a record
none of whose fields occupy slots. `make` / combinators are deferred;
construct and project directly.

## Constraints

The `.cmi` records erasure on arrows (as it does every mode) and field
erasure on records (it decides layout).

## Tests

- `testsuite/tests/vox/erasure.ml` — the information-flow discipline
- `testsuite/tests/vox/erasure_subsumption.ml` — arrow-mode variance on all
  four paths, inference, optionals, externals, structures
- `testsuite/tests/vox/erasure_runtime.ml` — runtime semantics: effect
  deletion, placeholders, partial application, erased optionals
- `testsuite/tests/vox/erasure_units.ml` — cross-unit `.cmi` round trip

## Deferred

Requiring `e @ total` in `erased_ e`. Erased array elements. `erased_` at
vector layouts (currently a compiler fatal error rather than a located user
error; reachable only with `-extension simd`). `erased_` in quotations
(rejected). Constructor-argument `@@ erased`. Convenience functions on
`Erased.t`. Interaction with refinement predicates, which is what erasure
exists for.

## Decisions taken during implementation

### The ambient rule is an environment flag checked at the submode funnel

"Checked at ambient Erased" is implemented as a flag on the typing
environment (`Env.enter_erased_context`), consulted at the single point
where every expression's mode meets its expectation (`Typecore.submode`):
inside an erased context the erasure axis is not checked, because the
context is deleted from compilation. This is compositional and gives the
closure body rule for `erased_ (fun ...)` for free.

### The default expectation requires retained

`Erased` is the top of the axis, so an *unconstrained* expected mode would
accept erased values — and "read position" is a semantic notion nobody can
grep for. The polarity is therefore flipped: `mode_max` requires Retained on
the erasure axis, and the erased-tolerant positions are the closed set listed
above. Positions built from fresh mode variables still need an explicit
constraint where they read (destructuring patterns, field access/mutation,
the function position of an application, splice and quotation-overwrite
cells).

### Closure carve-outs

`Env.closure_mode` / `const_closure_mode` (captures) and `close_over`
(partial application) meet the erasure component down to Retained rather than
joining it into the closure's mode. The lock machinery applies uniformly
across captures, so a per-capture modality was not available.

### Erasure is fixed top-down

A lambda's body is an erased context exactly when the lambda is syntactically
under `erased_`. A lambda at an `@ erased` argument position without
`erased_` is retained, genuinely evaluated, and checked with a retained body.

### Mode crossing pins are at construction sites

A review suggestion to pin at the two readers of stored bounds was tried and
reverted: the kind machinery mixes readers, and pinning only some views made
ordinary kind subsumption fail (672 testsuite failures). The
construction-site pins keep every view consistent.

### `@@ erased` implementation choices

- **A flag, not a modality atom.** `ld_erased : bool` on the label
  declaration (and `lbl_erased` on descriptions), parsed out of the modality
  list in `transl_labels` for boxed-record fields only; everywhere else the
  name stays in the list and `Typemode` rejects it ("Unrecognized modality"),
  so constructor arguments, value descriptions and `[@@unboxed]` records fail
  closed. Mutable erased fields are rejected with their own error. The
  auto-derived unboxed (`#`) version of a record does not inherit erasedness:
  it is an independent unboxed product, all of whose fields are manifest in
  its layout.
- **Representation rides the void machinery.** An erased label's `ld_sort`
  is `Base Void` and its element classification is `Void`, so records with
  erased fields become mixed blocks whose erased entries have zero width —
  the same path void-typed fields already take. An all-erased record (which
  the empty-record check now permits when the voidness comes from erasure)
  compiles to the immediate `0`: construction sequences the field
  expressions' effects and yields `0`, and `Typeopt.value_kind` reports it
  as an immediate.
- **Modes.** Reading an erased field joins Erased into the result mode
  (other axes inherited from the record, conservatively). Writing one
  expects nothing of the value (statement-like `Value.max` expectation):
  nothing is stored, so no axis can be violated. The typed-tree field sort
  for an erased field is the sort of the field's *type* (used to evaluate
  the expression for effects); the slot sort is Void.
- **Reads fabricate placeholders.** Projection translates to the record's
  effects followed by `Lambda.placeholder_of_layout`; record patterns bind
  placeholders (`matching.ml`); the toplevel printer prints `<erased>`
  without reading memory.
- **Signature matching is fail-closed in both directions** (`Erasedness`
  label mismatch in `includecore`), and `mcomp` treats differently-erased
  labels as incompatible.

### Call-site inference of parameter erasure

Within one structure, a call site can raise an unannotated parameter's
erasure before the binding's modes are zapped (`let h x = 42` plus
`ignore (h (erased_ 5))` gives `h : 'a @ erased -> int`). Under the first
design this was a silent ABI change and flagged as a gap; with no ABI it is
ordinary, sound mode inference and is pinned as such.
