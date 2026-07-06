# Extensible reflection primitives: `[@@vox.reflect "LeanSymbol"]`

Date: 2026-07-06

## Problem

vox reflects a fixed, compiler-baked set of OCaml operations into its
logic. The gap: operations that `total_` cannot express -- externals /
builtins (bit ops `land`/`lor`/`lsr`, `String`/`Bytes` ops, array
primitives) and abstract library functions -- have no way to enter the
reflection table from inside a vox development. Adding each one means
editing the compiler. We want a development author to *declare* "this
OCaml value denotes this Lean symbol", the value-level analogue of the
existing `type t [@@vox.sort lean "Name"]`.

## Phase 1 -- the current reality

The reflector (`typing/vox_reflect.ml`) turns a typed expression into a
`Refinement.pred`. What it understands, and how:

### The primitive table (`prim_pred`)
Keyed on the OCaml **primitive name** (`Val_prim prim.prim_name`),
never the source name -- so shadowing `(+)` cannot masquerade as
addition:

| primitive(s) | logic |
| --- | --- |
| `%addint %subint %mulint %divint %modint` | `Pbinop Add/Sub/Mul/Div/Mod` |
| `%negint %succint %predint` | `-a`, `a+1`, `a-1` |
| `%sequand %sequor %boolnot` | `Pand/Por/Pnot` |
| `%equal %notequal` (type-gate `eq_ok`: int/bool/tuples) | `Eq/Neq` |
| `%lessthan %lessequal %greaterthan %greaterequal` (gate `cmp_ok`: int/bool) | `Lt/Le/Gt/Ge` |
| `%field0_immut %field1_immut` (gate `proj_ok`: unlabeled pair) | `Pproj` |
| `%array_length %array_safe_get` (gate `ia_ok`: `int iarray`) | `Pfun(ia_len/ia_get, ..)` |

Beyond primitives: unlabeled tuples -> `Ptuple`, simple-record field
reads -> `Pfield`, simple-variant constructors -> `Pconstr` (in the rhs
/ nameable fragments), and `total_` functions -> `Pfun(source_name,
args)` via `reflected_call_info`.

### `total_` -- the extensible route that exists today
`let rec total_ f ... = ...` translates the *body* into an
equation-style `spec_def` and emits it as a Lean `@[grind] def`
(`lean_spec_def`). A saturated call then reflects to `Pfun(name,
args)`. Recognition rides two channels: a local stamp table
(`reflected`), and the `[@@vox.total]`/`total_` marker on
`val_attributes`, which is marshalled into the `.cmi` -- so *imported*
reflected functions are recognized too. This is the gold standard:
**proved** (Lean checks the definition and its termination); nothing is
assumed.

### Unknown-primitive behavior
`prim_pred` returns `None` for any unrecognized primitive. The
consequence depends on the caller:
- in a refinement / VC (`translate`): the subterm degrades to a
  **fresh opaque unknown** (`name_of_expr`) -- silent, sound, but
  nothing can be proved about it. So `land x y` in a refinement today
  is an opaque atom.
- in a `total_` body (`translate_rhs`): `None` is a hard **error** at
  the binding -- you cannot even mention `land` in a reflected body.

### The back end already generalizes
`Pfun (f, args)` emits `(f arg0 arg1 ...)` **verbatim** into Lean
(`vox_verify.ml` `lean_of_pred`, `Pfun` case) -- a dotted stdlib name
or a block-defined name survives intact; an undefined or ill-sorted
application is a solver error, i.e. a verification failure. **The
emission layer needs no change.** The only missing piece is the front
door: a way to make an OCaml value's application produce
`Pfun(lean_symbol, args)` without a translated body.

### The type-level precedent
`type t [@@vox.sort lean "Name"]` maps a type to a user-named Lean
sort. It is an **assumed** correspondence (author's word, TCB),
validated only lightly (`validate_lean_sort_name`: non-empty, no
reserved `Vox_`/`v_` prefix). `[@@vox.reflect]` is its exact
value-level analogue.

## Phase 2 -- design

Add `external land : int -> int -> int = "%andint" [@@vox.reflect
"Vox.land"]` (also works on a plain `val f : t -> int [@@vox.reflect
"MyLean.f"]`). The attribute binds the value's solver-side name to the
given Lean symbol at its sorts. A call `land x y` then translates to
`Pfun("Vox.land", [x; y])` in *every* reflection context -- refinement
predicates, `total_` bodies, `[@@vox.lemma]` bodies -- and **no**
definition is emitted.

### Smallest delta
Everything funnels through `reflected_call_info`. Teaching it to
recognize `[@@vox.reflect]` (returning the Lean symbol as the `Pfun`
name) is the entire front-end change; `translate`'s reflected branch
already precedes its `Val_prim` branch, so externals are handled. The
surface twin (`translate_surface`, used for dependent arguments) is
reordered to try `reflected_call_info` before `prim_pred`, keeping the
two fragments in step. No `.cmi` format change: the attribute rides
`val_attributes` natively, exactly as `[@@vox.total]` does, so a
library declares the binding once and clients translate calls to it.

### The Lean symbol must exist
It resolves against: Lean stdlib, a `[%%vox.lean]` block (in this unit
or an imported `.mli`), or an imported spec module. A missing symbol is
a solver error at VC time -- **fail closed**. Sort mismatches (wrong
arity, wrong operand sort) likewise surface as Lean elaboration errors,
because predicates are untyped and the compiler neither resolves nor
sorts a `Pfun`.

### What is checked vs. assumed (the trust story)
| mechanism | trust | who checks |
| --- | --- | --- |
| `total_` | **proved** | Lean checks the translated definition + termination |
| runtime `assume_` | **checked** | compiled runtime check calls the reflected fn |
| `[@@vox.reflect]` | **assumed** | nobody -- the OCaml value <-> Lean symbol correspondence is the author's word |
| `[@@vox.sort lean]` | **assumed** | (type-level precedent, same trust) |

`[@@vox.reflect]` is TCB, exactly like a `[%%vox.lean]` `axiom` in a
`.ml` block or a `[@@vox.sort lean]`. What the compiler *can* check it
does: arity (arrow count) governs saturation, the payload is a
well-formed non-empty Lean name, the reserved `Vox_`/`v_` prefixes are
refused, and a value may not be both `total_` and `[@@vox.reflect]`.
What it *cannot* check is the semantic correspondence itself -- that
`land` (the OCaml runtime primitive) actually equals `Vox.land` (the
Lean symbol). As with vox's existing "ideal arithmetic" stance
(unbounded ints, overflow out of model), the correspondence holds only
in the modeled semantics.

Because it is TCB, a `[@@vox.reflect]` declaration belongs in the
**interface** (`.mli`), the reviewable surface, and rides the `.cmi` to
clients along with its trust. It is permitted in a `.ml` too (local
externals), reviewed like any `.ml` axiom.

### Registration + cross-unit
Zero new plumbing: `val_attributes` -> `.cmi` -> client
`reflected_call_info`. If the Lean symbol is block-defined, the client
already imports the declaring unit's `VoxSig_*` module (existing
`imported_specs` path), so the symbol resolves there too.

## Demo

`land`/`lor` as reflected externals over a `[%%vox.lean]` block model
(Lean 4.31 has no `Int.land`), verifying real masking algebra
(idempotence of a mask, mask-commutation) that is **impossible today**:
an external hits `prim_pred` -> `None` -> opaque atom, and cannot be
`total_`'d (no body). The block supplies `opaque bland` + the algebraic
laws (`@[grind] axiom` idem/zero/comm/assoc); `grind` discharges the
masking VCs. TCB here = block axioms + the reflect correspondence, both
documented.

## Soundness tests
- unknown Lean symbol -> solver error (fail closed), not a false pass;
- malformed / empty payload, reserved prefix, `total_`+`reflect` on one
  value -> rejected at the declaration;
- a *false* claim about a reflected symbol still fails (the laws only
  prove what they entail).

## Implementation notes (what the build revealed)

Two things surfaced while wiring this up that shaped the final design.

1. **The predicate language spells names literally.** An unqualified
   applied lowercase identifier in a refinement (`{ _ = band x m }`)
   becomes `Pfun("band", ..)` from the *source* name, whereas the code
   side reflects `band` to its Lean symbol `bland`. Left alone the two
   disagree (`bland ... = band ...`, and `band` is unknown to Lean). So
   `Typetexp.elab_vox_pred` now resolves an applied identifier through
   the environment: a `[@@vox.reflect]` value contributes its Lean
   symbol, everything else keeps its literal name (prelude/`total_`
   spec functions are unchanged, since their `Pfun` name already equals
   the source name). Net effect: the author writes the **OCaml name**
   in code and in refinements, and both reflect to the same symbol; the
   Lean name is an implementation detail. Handled for qualified
   (`M.band`) names too.

2. **`total_` bodies and same-file blocks.** In the solver input,
   reflected `def`s are emitted *before* the module's own `[%%vox.lean]`
   blocks (so a block may state lemmas about a `total_` def). A `total_`
   body that calls a reflect symbol defined in the *same file's* block
   therefore sees the symbol as not-yet-declared. Imported blocks and
   the `-vox-prelude`, by contrast, are emitted *before* the defs, so a
   `total_` body **can** call a reflect symbol whose model rides an
   imported `.mli` (demonstrated in the cross-unit test, where a
   client's `total_ dmin` calls the imported reflected `imin`). This is
   not a soundness gap (a not-yet-declared symbol fails closed); it is
   an ordering constraint on same-file composition, and the natural
   place for a reflect model is the interface anyway.

## Files
- `typing/vox_reflect.ml`: `reflect_attr_name`, `validate_reflect_attr`,
  `reflected_call_info` (prefers the reflect name), surface twin reorder.
- `typing/vox_verify.ml`: `validate_signature_sorts` (`Sig_value`),
  `walk_items` (`Tstr_primitive` + `Tstr_value` validation, and the
  `total_`+`reflect` rejection).
- `typing/typetexp.ml`: `reflect_name_of` + predicate-language
  resolution of reflect names (unqualified and qualified).
- Tests: `testsuite/tests/vox/demo/lean_reflect_prim.ml` (single-file
  bit-op demo), `.../demo/reflectbits.{mli,ml}` +
  `.../demo/lean_reflectprim_client.ml` (cross-unit, `total_`
  composition), `.../mechanics/lean_reflect_prim_fail.ml` (soundness).
