# Vox ghost code — final report

State of the piece after the information-flow redesign and the rename to
ghost vocabulary, on branch `jujacobs/vox/ghost`. The design doc proper is
`ghostliness.md` next to this file; its "Decisions taken during implementation"
section records each choice and the alternatives. The report for the first,
ABI-bearing iteration is in the history of this file; the doc's History
section explains why that design was replaced.

## The design in one paragraph

`@ ghost` is an information-flow mode with **no ABI effect**: a ghost
value may only flow to ghost positions, so its content is unobservable, and
`ghost_ e` deletes the evaluation of `e`, compiling to a placeholder that is
never read. Representation-affecting ghostliness lives in exactly one place: the
`@@ ghost` record-field modality, whose field occupies no slot; reading such
a field fabricates a placeholder ("null") at mode ghost. `Stdlib.Ghost`
(`type 'a t = { ghost : 'a @@ ghost }`) is the wrapper this enables: kind
void, so absent from ABIs entirely — no register, no slot — whatever it
wraps.

## What landed

**The axis** (unchanged from the first iteration): `Ghostliness`
(`Real < Ghost`, comonadic, legacy `Real`), never crossable, pinned
at every crossing construction site.

**`ghost_ e`** (`Pexp_ghost` / `Texp_ghost` exp_extra): typed at ambient
Ghost (an environment flag consulted at the `Typecore.submode` funnel),
translated to `Lambda.placeholder_of_layout` at whatever layout the context
requests, never evaluated. Grammar matches `local_`/`exclave_`
(`ghost_ seq_expr`), so record fields and arguments need parentheses.

**Information-flow discipline**: real-by-default expected modes with a
closed list of ghost-tolerant positions; destructuring reads, field access,
function position require real; closure capture and `close_over`
carve-outs; the closure body rule; modules never store ghost bindings; no
mode crossing on the axis.

**Ordinary arrow variance**: with no ABI at stake, ghostliness is contravariant
in argument position and covariant in return position on all four
arrow-mode paths (unify, `type_argument` loosening, moregen, subtype /
build_subtype). Ghost-parameter functions seal behind and coerce to
real-parameter arrows, generic HOFs take ghost-parameter callbacks,
optionals and externals may be ghost. All pinned in both directions in
`ghost_subsumption.ml`.

**`@@ ghost` fields**: `ld_ghost` flag on label declarations (not a
modality atom); representation rides the void machinery (Void element in a
mixed block, zero width natively; bytecode keeps a placeholder word, as it
does for void-typed fields). All-ghost records are legal and have kind
void: no value exists at run time. Construction evaluates real field expressions for effects
only (statement-like expected mode: nothing is stored, nothing is required);
projection and patterns fabricate placeholders at mode ghost; the toplevel
printer shows `<ghost>` without reading memory. Signature matching is
fail-closed in both directions. Mutable fields, `[@@unboxed]` records,
constructor arguments and value descriptions reject the modality.

**`Stdlib.Ghost`**: the type above, nothing else; construct and project
directly. Deferred conveniences until practice demands them.

## Tests

- `ghost.ml` — the information-flow discipline
- `ghost_subsumption.ml` — arrow variance on all four paths, inference,
  optionals, externals, module storage
- `ghost_fields.ml` — the field modality: modes, matching, rejections,
  all-ghost records, fail-closed signatures
- `ghost_runtime.ml`, `ghost_fields_runtime.ml` — effect deletion,
  placeholder passing, slot elision (`Obj.size`), the void-kinded wrapper,
  functional update, `Stdlib.Ghost` (native reference;
  bytecode keeps a word per ghost field and has its own reference)
- `ghost_units.ml` — cross-unit `.cmi` round trip

## Known gaps

- `ghost_` (or a ghost occurrence) at a SIMD vector layout is a compiler
  fatal error rather than a located user error (needs `-extension simd`).
- `ghost_` in quotations is rejected.
- Ghost fields' types still contribute conservatively to their record's
  kind bounds (fewer crossings than the ghost semantics would justify).
- The `jujacobs/vox/erasure-abi` branch (emitted-code ABI pins, named for
  the feature's first vocabulary) is
  superseded by this redesign: `@ ghost` no longer has an ABI to pin.
