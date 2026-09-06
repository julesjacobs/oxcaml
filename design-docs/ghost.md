# Vox ghost code

We add a ghostliness mode axis, a construct that produces values on it, and a
field modality that erases data from representations.

    ghost_ e            -- e is deleted from compilation
    @ ghost             -- the value may only flow to ghost positions
    { x : t @@ ghost }  -- a ghost record field: occupies no slot

The division of labour is deliberate and is the heart of the design:

- The **mode** `@ ghost` is an information-flow property and nothing else.
  It has **no effect on the ABI**: a ghost function parameter is passed
  physically like any other, a ghost local is an ordinary binding. What the
  mode guarantees is that no real computation ever reads a ghost value,
  so the value's *content* is unobservable and `ghost_ e` may compile to a
  placeholder without evaluating `e`.
- The **modality** `@@ ghost` on a record field is what changes
  representation: the field occupies no slot in the record. Reading it
  fabricates a placeholder (a dummy value of the field's kind) at mode
  ghost.
- To erase something from an ABI, wrap it: `'a Ghost.t`, defined in the
  stdlib as `type 'a t = { ghost : 'a @@ ghost }`. An all-ghost record has
  **kind void**, so the wrapper is not merely dataless but entirely absent
  from ABIs: a `Ghost.t` parameter occupies no register, and a field of a
  void type occupies no slot — with no modality needed at the use site,
  since the type alone carries it. (Being void, `Ghost.t` cannot inhabit
  value-polymorphic containers such as `'a list`; store it as a record
  field instead.)

Additional conveniences are deferred until practice shows they are needed.

## Lattice

| axis | min | max | legacy | fragment |
|---|---|---|---|---|
| Ghostliness | `Real` | `Ghost` | `Real` | comonadic |

`Real <= Ghost`. A real value may be used where a ghost one is
expected, since the context promises not to read it. The reverse cannot hold.

Comonadic on the substantive test: a value's mode is bounded above by the meet
of what its uses demand, so using a value anywhere real forces it
real.

## The information-flow rule

### Ambient ghostliness

*What may be used* at a position is governed by the expected mode, through
ordinary submoding. A value may be used only where the expected ghostliness is at
least its own.

*What gets deleted* is governed by the expression's own ghostliness. `ghost_ e`
sets the expression to Ghost, so it is deleted. An ordinary expression is
Real and is evaluated, whatever position it sits in. Deletion never
happens implicitly; it follows the source, not the context.

An expression is checked at ambient Ghost inside `ghost_ e` (including
closure bodies) and in static refinement predicates. Runtime predicate replay
by `assume_` uses a real context. Because `Real <= Ghost`, real values are usable inside
ghost contexts, which is what specifications need. Ghost values fail
everywhere else:

- `x + y` in real code requires both real
- `if`/`match`/`while`/`for`/guards/`assert` require real scrutinees
- destructuring patterns read; variables, wildcards and aliases bind without
  reading, so `let x = ghost_ e in ...` works
- record field access and mutation require a real record
- the function position of an application requires real (the call reads
  the closure), independently of any ghost *parameters* the function has

The permissive expected mode (`mode_max`) requires Real on the ghostliness
axis; ghost-tolerant positions are a closed, spelled-out set: type-driven
positions (an `@ ghost` arrow argument, a ghost return), ghost contexts,
and statement position (which discards the value).

### Arguments do not become ghost silently

An `@ ghost` parameter accepts both kinds of argument:

    f (expensive ())            -- evaluated and passed like any argument;
                                -- the callee just cannot read it
    f (ghost_ (expensive ()))  -- never evaluated; a placeholder is passed
    let x = ghost_ e in f x    -- x is already a placeholder

The argument position does not create a ghost context: an ordinary call
must not silently drop its argument's effects. Deleting an evaluation is
something the programmer writes.

### Closures

Capture propagates nothing: a real closure may capture ghost values
(the capture check meets the captured value's ghostliness down to Real; body
uses still see the true ghostliness). The same carve-out applies in `close_over`
for partial application, so applying across a ghost parameter does not
erase the result.

A closure's body is checked at the closure's own ghostliness: `ghost_ (fun y ->
g y)` accepts `g @ ghost` in its body. A lambda written directly at an
`@ ghost` argument position without `ghost_` is real and its body is a
real context — it is genuinely constructed and evaluated.

### Arrow modes: ordinary variance

With no ABI at stake, ghostliness follows the same rules as every other comonadic
axis: contravariant in argument position, covariant in return position,
equated by unification. In particular:

- a ghost-parameter implementation seals behind a real-parameter
  signature (it promises to read less), and the reverse is rejected
- the same through `(e :> t)` coercions, on both the `subtype` and
  `build_subtype` paths
- generic higher-order functions accept ghost-parameter callbacks
- optional parameters without defaults and externals may be ghost; the
  argument is physically passed, and the mode constrains OCaml-side uses only

Default selection inspects the option, so defaulted optional parameters must
be real.

All four arrow-mode paths are pinned in both directions in
`testsuite/tests/vox/ghost_subsumption.ml`.

### Structures and modules

A module block's fields are legacy (real), so a ghost value cannot be
stored in a structure — a compilation unit, a local module, a `let open
struct ... end`. A module allocation's ghostliness is capped to Real so the
structure-item check rejects these uniformly. (`Ghost.t` is the way to store
a ghost value.)

### Ghostliness and mode crossing

No type crosses ghostliness, ever: a ghost value's content may be a
placeholder, so treating it as real is unsound regardless of the type —
the whole property collapses on immediates first. Enforced at the places
crossings are built (`Mod_bounds.min_crossable`, `cross_all_crossable`,
`Crossing.always_constructed_at`, `Axis_lattice.create`, the bool-created
crossings). `mod ghost` / `mod real` are rejected as kind modifiers and
`mod everything` excludes ghostliness (precedent: staticity).

## How `ghost_ e` compiles

`e` is checked at total under the total closure lock, then deleted: `transl_ghost` emits
a placeholder of whatever layout the context requests (`dummy_constant` for
values, zeros for unboxed numbers, recursively for unboxed products; vector
layouts have no placeholder and are rejected with a located error). The mode system
guarantees no real code reads the placeholder. There is no other codegen:
no ghost calling convention, no per-occurrence layout changes.

`ghost_` requires a total result and rejects partial calls, mutable-state
operations, and exceptions using the current totality checker. In particular,
`ghost_ (print_string "hi")` is rejected. Ordinary arguments passed to ghost
parameters are still evaluated normally.

## The `@@ ghost` field modality

The one place ghostliness touches representation. `{ x : t @@ ghost; ... }`
declares a field that occupies no slot in the record:

- **Construction** `{ x = e; ... }` checks `e` at expected ghostliness Ghost (so
  both real and ghost values are accepted), requires a total value, evaluates a real `e` for
  its effects, and stores nothing.
- **Projection** `r.x` has mode `ghost` and fabricates a placeholder of the
  field's kind — a recognizable dummy for values, zeros for unboxed
  numbers; it never reads memory.
- **Patterns** on the field bind placeholders at mode ghost.
- **Signature matching** is fail-closed: two sides of a module boundary must
  agree on a field's ghostliness, since it decides the record's layout.
- **Mutable ghost fields are rejected**: writing would be a no-op.
- The modality is recorded in the `.cmi` and printed back.

This is a comonadic *weakening* (`Join_const Ghost`), which the modality
machinery (`Meet_const` only) does not express; it is carried as a separate
marker on the label declaration rather than widening the general modality
algebra for one constant use.

## `Stdlib.Ghost`

    type 'a t = { ghost : 'a @@ ghost }

`'a Ghost.t` is how a value is ghost from an ABI: the wrapper is a record
none of whose fields occupy slots. `make` / combinators are deferred;
construct and project directly.

## Constraints

The `.cmi` records ghostliness on arrows (as it does every mode) and field
ghostliness on records (it decides layout).

## Tests

- `testsuite/tests/vox/ghost.ml` — the information-flow discipline
- `testsuite/tests/vox/ghost_subsumption.ml` — arrow-mode variance on all
  four paths, inference, optionals, externals, structures
- `testsuite/tests/vox/ghost_fields.ml` — the `@@ ghost` field modality
- `testsuite/tests/vox/ghost_runtime.ml`,
  `ghost_fields_runtime.ml` — runtime semantics: effect deletion,
  placeholders, partial application, ghost optionals, slot elision, the
  void-kinded wrapper
- `testsuite/tests/vox/ghost_units.ml` — cross-unit `.cmi` round trip
- `testsuite/tests/vox/ghost_refinements.ml` — total erasure and static/runtime predicate separation
- `testsuite/tests/vox/ghost_erasure.ml` — generated Lambda for erased recursive proofs
- `testsuite/tests/vox/functional_queue.ml` and `regex.ml` — erased proofs in
  queue operations and a verified DFA client

## Deferred

Ghost array elements. `ghost_` at
vector layouts and in quotations (both report located unsupported-feature
errors). Constructor-argument `@@ ghost`. Convenience functions on
`Ghost.t`.

## Decisions taken during implementation

### The ambient rule is an environment flag checked at the submode funnel

"Checked at ambient Ghost" is implemented as a flag on the typing
environment (`Env.enter_ghost_context`), consulted at the single point
where every expression's mode meets its expectation (`Typecore.submode`):
inside a ghost context the ghostliness axis is not checked, because the
context is deleted from compilation. This is compositional and gives the
closure body rule for `ghost_ (fun ...)` for free.

### The default expectation requires real

`Ghost` is the top of the axis, so an *unconstrained* expected mode would
accept ghost values — and "read position" is a semantic notion nobody can
grep for. The polarity is therefore flipped: `mode_max` requires Real on
the ghostliness axis, and the ghost-tolerant positions are the closed set listed
above. Positions built from fresh mode variables still need an explicit
constraint where they read (destructuring patterns, field access/mutation,
the function position of an application, splice and quotation-overwrite
cells).

### Closure carve-outs

`Env.closure_mode` / `const_closure_mode` (captures) and `close_over`
(partial application) meet the ghostliness component down to Real rather than
joining it into the closure's mode. The lock machinery applies uniformly
across captures, so a per-capture modality was not available.

### Ghostliness is fixed top-down

A lambda's body is a ghost context exactly when the lambda is syntactically
under `ghost_`. A lambda at an `@ ghost` argument position without
`ghost_` is real, genuinely evaluated, and checked with a real body.

### Mode crossing pins are at construction sites

A review suggestion to pin at the two readers of stored bounds was tried and
reverted: the kind machinery mixes readers, and pinning only some views made
ordinary kind subsumption fail (672 testsuite failures). The
construction-site pins keep every view consistent.

### `@@ ghost` implementation choices

- **A flag, not a modality atom.** `ld_ghost : bool` on the label
  declaration (and `lbl_ghost` on descriptions), parsed out of the modality
  list in `transl_labels` for boxed-record fields only; everywhere else the
  name stays in the list and `Typemode` rejects it ("Unrecognized modality"),
  so constructor arguments, value descriptions and `[@@unboxed]` records fail
  closed. Mutable ghost fields are rejected with their own error. The
  auto-derived unboxed (`#`) version of a record does not inherit ghostliness:
  it is an independent unboxed product, all of whose fields are manifest in
  its layout.
- **Representation rides the void machinery.** A ghost label's `ld_sort`
  is `Base Void` and its element classification is `Void`, so records with
  ghost fields become mixed blocks whose ghost entries have zero width —
  the same path void-typed fields already take. An all-ghost record (which
  the empty-record check now permits when the voidness comes from
  ghostliness) has kind void: construction sequences the field expressions'
  effects and yields the empty unboxed product, and the value never exists.
  The first version of this design made all-ghost records the immediate `0`
  (kind value), which kept them usable in `'a list` but still spent a
  register per parameter and a slot per field; full erasure won.
- **Modes.** Reading a ghost field produces Ghost on the ghostliness axis
  and the minimum on the other axes. Writing one requires a total value. Its logical contents remain available to total
  ghost computations even though no runtime slot is stored. The typed-tree field sort
  for a ghost field is the sort of the field's *type* (used to evaluate
  the expression for effects); the slot sort is Void.
- **Reads fabricate placeholders.** Projection translates to the record's
  effects followed by `Lambda.placeholder_of_layout`; record patterns bind
  placeholders (`matching.ml`); the toplevel printer prints `<ghost>`
  without reading memory.
- **Signature matching is fail-closed in both directions** (`Ghostliness`
  label mismatch in `includecore`), and `mcomp` treats differently-ghost
  labels as incompatible.

### Call-site inference of parameter ghostliness

Within one structure, a call site can raise an unannotated parameter's
ghostliness before the binding's modes are zapped (`let h x = 42` plus
`ignore (h (ghost_ 5))` gives `h : 'a @ ghost -> int`). Under the first
design this was a silent ABI change and flagged as a gap; with no ABI it is
ordinary, sound mode inference and is pinned as such.

## Integration with the current refinement checker

Refinement predicates are ghost contexts. Runtime replay by `assume_` uses
a real context: it cannot read a ghost operand or ghost data in its predicate.
Proof calls can be erased while their result refinements remain available:
`let refine_ proof = ghost_ (lemma x) in ...`.

A ghost field stores a total logical value. Construction may evaluate an
ordinary expression for effects, but the resulting value must be total.
This makes the field's total ghost read valid even when its enclosing record
has kind `void` and carries no bounds from the field's type.

Runtime physical identity establishes logical equality only when the type
is known to preserve its data through erasure. Ghost fields and abstract or
polymorphic types prevent that check; `assume_` raises `Invalid_argument`
instead. Static logical equality remains available for these types.
