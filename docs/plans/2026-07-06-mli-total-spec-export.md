# Exporting `total_` spec functions through an `.mli`

*Design + implementation notes, 2026-07-06.*

## The question

vox lets you define a *spec function* in an implementation with the
`total_` marker:

```ocaml
let rec total_ len l =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + len t
```

Other functions' refinement types then *mention* it:

```ocaml
val append : (a : ilist) -> (b : ilist) -> ilist{ len _ = len a + len b }
```

So `len` has to be part of the `.mli` *somehow* — otherwise a client
cannot even write `len` in the type of a function it calls. The design
question is: **can (and should) a `total_` spec function appear in an
interface, and what is the proper story?** And, separately: is there a
distinction between exporting the *name* of `len` (clients can state and
receive facts about it, but it stays opaque — abstraction preserved) and
exporting its *defining equations* (clients' proofs may unfold it — more
provable, less abstract)?

## What already existed (probed empirically)

| `.mli` spelling for `len` | `.mli` / impl compile | client behaviour |
|---|---|---|
| *no `.mli` at all* | ok | name **and** equations travel via the auto-generated `.cmi`; full proofs work (this is the `reflect_lib` demo) |
| `val len : ilist -> int` (plain) | ok | client **rejected**: *"a qualified identifier in a predicate must denote a total_ function"* — the marker never reached the client |
| `val total_ len : ilist -> int` | **syntax error** | not parseable |
| `val len : ... [@@vox.total]` | ok | name travels (client recognises `Lib.len`) but **Lean fails** *"Function expected at"* — no declaration for `len` was exported |
| `[%%vox.lean] @[grind, expose] public def len ...` block | ok | name **and** equations travel; clients unfold (this is the `lean_sig` demo) |
| `[%%vox.lean] public opaque len : ...` block | ok | **name-only**: contract-chaining proofs pass, but a proof that must unfold `len` fails |

The root cause of the gap: a unit's spec export is produced from
whichever file writes the `.cmi`. With **no** `.mli`, that is the
implementation, and `Vox_verify.cmi_export_of_structure` passes the
reflected `total_` definitions (`~defs:!spec_defs`) — name and equations
both travel. With an `.mli`, the `.cmi` comes from the interface and
`cmi_export_of_signature` passed `~defs:[]`: the interface has no bodies
to reflect, so `total_` functions stayed private to the implementation
and clients' calls degraded to unknowns (sound, but useless).

The `[%%vox.lean]` block route already lets an author export a spec
function through an `.mli` with *full control* over abstraction (`opaque
len` = name-only; `@[grind, expose] def len := ...` = equations). The
only thing missing was an **ML-ergonomic** way to say "export this
`total_` function's name" without hand-writing Lean and keeping it in
sync with the OCaml body.

## The design

Two orthogonal choices, mapped onto two spellings that compose with the
existing machinery rather than duplicating it:

### Name-only (new): `val total_ len : ilist -> int`

`total_` is now accepted in a value description, mirroring `let rec
total_`; the parser attaches the same `vox.total` attribute to the value
description that the let-binding attaches to its pattern. So the marker
rides the `.cmi` and clients recognise `Lib.len` as a reflected function
(named by an ordinary qualified value path).

At interface-export time (`cmi_export_of_signature`), each `total_`-marked
`val` emits a **name-only** declaration into the unit's sig module:

```lean
public opaque len : Vox_Mli_totalspec_ilist -> Int
```

Clients import the sig module, so they may mention `len` in refinements
and receive facts about it — an exported contract that mentions `len`, or
an interface `axiom` discharged by the implementation's seal — but they
**cannot unfold** it. The implementation keeps its `let rec total_ len`
body and discharges *its own* VCs with the real equations; it reads the
opaque stub only as any client would. This is the abstraction-preserving
default: over an interface that hides a type, a client must not compute a
measure on that type's constructors.

Soundness is the standard "opaque = existential" argument, identical to
`[@@vox.sort opaque]` for types: the concrete `len` the implementation
defines is one witness for the interface's opaque `len`, and every fact a
client receives is either a contract the implementation proved for that
witness or a sealed obligation — so a client can never prove something
false about `len`, only fewer true things than the implementation can.

### Equations exposed (already existed): interface `[%%vox.lean]` block

To let clients unfold `len`, the author writes the equations in the
interface as a block:

```lean
@[grind, expose] public def len : Vox_Mli_exposed_ilist -> Int
  | .Nil => 0
  | .Cons _ t => 1 + len t
```

The equations become part of the interface (the author commits to them,
and the implementation is checked against them). This is deliberately
*not* auto-derived from the `.ml` body: the `.mli` is compiled before —
and independently of — the `.ml`, so the equations an interface exposes
must live in the interface, exactly as `lean_sig.mli` already does.

### Composition

If a `val total_ f` and an interface block that *defines* `f` are both
present, the block is authoritative and the opaque stub is suppressed
(`block_declares` scans the block text for `def`/`opaque`/`abbrev f`), so
Lean never sees `f` declared twice.

## Implementation

- `parsing/parser.mly`: `value_description` accepts an optional `total_`
  after the `poly_flag`, lowering to the `vox.total` attribute.
- `typing/vox_verify.ml`:
  - `block_declares` — does an interface block already declare a name.
  - `total_spec_decls` — for each `total_`-marked `val` not already
    declared by a block, render `public opaque <name> : <sorts>` from the
    value's type (peeling the `Tpoly`/`Trefine` wrappers a signature puts
    on a value type; int→`Int`, bool→`Prop`, a datatype→`Vox_<path>`,
    which `register_datatypes_in_blocks` declares from the same token).
  - `cmi_export_of_signature` appends these decls to the interface's
    blocks and builds a sig module when any exist.

## Tests (`testsuite/tests/vox/demo`)

- `mli_totalspec.{mli,ml}` + `mli_totalspec_client.ml` — name-only
  export; client chains `append`'s contract and states its own
  `len`-mentioning refinement, both discharged with no unfolding.
- `mli_totalspec_abstract_fail.ml` — the same lib; a client that
  constructs a literal list and asserts its length (which *would* need
  unfolding) fails at the solver, demonstrating the abstraction.
- `mli_exposed.{mli,ml}` + `mli_exposed_client.ml` — the equations-exposed
  contrast: the same literal-list assertion succeeds because the
  interface exported `len`'s equations.
