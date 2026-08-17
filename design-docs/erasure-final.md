# Vox erasure — final report

State of the piece after the information-flow redesign, on branch
`jujacobs/vox/erasure-red` (tip `721b475c7b`). The design doc proper is
`erasure.md` next to this file; its "Decisions taken during implementation"
section records each choice and the alternatives. The report for the first,
ABI-bearing iteration is in the history of this file; the doc's History
section explains why that design was replaced.

## The design in one paragraph

`@ erased` is an information-flow mode with **no ABI effect**: an erased
value may only flow to erased positions, so its content is unobservable, and
`erased_ e` deletes the evaluation of `e`, compiling to a placeholder that is
never read. Representation-affecting erasure lives in exactly one place: the
`@@ erased` record-field modality, whose field occupies no slot; reading such
a field fabricates a placeholder ("null") at mode erased. `Stdlib.Erased`
(`type 'a t = { erased : 'a @@ erased }`) is the wrapper this enables: the
immediate `0` at run time, whatever it wraps.

## What landed

**The axis** (unchanged from the first iteration): `Erasure`
(`Retained < Erased`, comonadic, legacy `Retained`), never crossable, pinned
at every crossing construction site.

**`erased_ e`** (`Pexp_erased` / `Texp_erased` exp_extra): typed at ambient
Erased (an environment flag consulted at the `Typecore.submode` funnel),
translated to `Lambda.placeholder_of_layout` at whatever layout the context
requests, never evaluated. Grammar matches `local_`/`exclave_`
(`erased_ seq_expr`), so record fields and arguments need parentheses.

**Information-flow discipline**: retained-by-default expected modes with a
closed list of erased-tolerant positions; destructuring reads, field access,
function position require retained; closure capture and `close_over`
carve-outs; the closure body rule; modules never store erased bindings; no
mode crossing on the axis.

**Ordinary arrow variance**: with no ABI at stake, erasure is contravariant
in argument position and covariant in return position on all four
arrow-mode paths (unify, `type_argument` loosening, moregen, subtype /
build_subtype). Erased-parameter functions seal behind and coerce to
retained-parameter arrows, generic HOFs take erased-parameter callbacks,
optionals and externals may be erased. All pinned in both directions in
`erasure_subsumption.ml`.

**`@@ erased` fields**: `ld_erased` flag on label declarations (not a
modality atom); representation rides the void machinery (Void element in a
mixed block, zero width natively; bytecode keeps a placeholder word, as it
does for void-typed fields). All-erased records are legal and compile to the
immediate `0`. Construction evaluates retained field expressions for effects
only (statement-like expected mode: nothing is stored, nothing is required);
projection and patterns fabricate placeholders at mode erased; the toplevel
printer shows `<erased>` without reading memory. Signature matching is
fail-closed in both directions. Mutable fields, `[@@unboxed]` records,
constructor arguments and value descriptions reject the modality.

**`Stdlib.Erased`**: the type above, nothing else; construct and project
directly. Deferred conveniences until practice demands them.

## Tests

- `erasure.ml` — the information-flow discipline
- `erasure_subsumption.ml` — arrow variance on all four paths, inference,
  optionals, externals, module storage
- `erasure_fields.ml` — the field modality: modes, matching, rejections,
  all-erased records, fail-closed signatures
- `erasure_runtime.ml`, `erasure_fields_runtime.ml` — effect deletion,
  placeholder passing, slot elision (`Obj.size`), the immediate-0 wrapper
  (`Obj.is_int`), functional update, `Stdlib.Erased` (native reference;
  bytecode keeps a word per erased field and has its own reference)
- `erasure_units.ml` — cross-unit `.cmi` round trip

## Known gaps

- `erased_` (or an erased occurrence) at a SIMD vector layout is a compiler
  fatal error rather than a located user error (needs `-extension simd`).
- `erased_` in quotations is rejected.
- Erased fields' types still contribute conservatively to their record's
  kind bounds (fewer crossings than the erased semantics would justify).
- The `jujacobs/vox/erasure-abi` branch (emitted-code ABI pins) is
  superseded by this redesign: `@ erased` no longer has an ABI to pin.
