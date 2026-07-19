# Root cause: "type at cursor" is empty on refinement-predicate sub-expressions

## Summary

The hypothesis is **confirmed**. The IDE's "type at cursor" readout is driven
entirely by the `types` array of the `/check` response, which the server derives
from the compiler's textual `.annot` output. The `.annot` stream only contains
entries for ordinary program `Typedtree.expression` / pattern nodes. The
sub-expressions of a refinement predicate (`_`, `>`, `0`, `_ > 0` in
`int{ _ > 0 }`) live in a *separate* data structure (`refinement_expression`,
attached to the `Trefine` type constructor in `typing/types.ml`) that is never
part of the saved Typedtree the `.annot` writer walks. So no `.annot` entry has a
source span covering those columns, and the client shows "No inferred expression
type at the cursor."

The type information the IDE wants *does* exist in the compiler and *is*
location-tagged (every `refinement_expression` node carries both `rexp_type` and
`rexp_loc`) — it is simply never emitted on any channel the IDE consumes.

Recommendation: **Option B** — emit a per-predicate subterm type map on the
existing VC-JSON dump channel and merge it into the client's `expressionTypes`.

---

## 1. Evidence: the `.annot` stream does not cover predicate spans

Test file (live compiler
`/usr/local/home/jujacobs/oxcamls/vox2/worktrees/scratch-h3/_install/bin/ocamlc.opt`,
`TMPDIR=/usr/local/home/jujacobs/tmp`):

```
let f (x : int{ _ > 0 }) = x       (line 1)
let g = 5                          (line 2)
```

Column map for line 1 (0-based): `x`(binder)=7, `int`=11-13, `{`=14, `_`=16,
`>`=18, `0`=20, `}`=22, `x`(use)=27.
So the predicate `_ > 0` occupies **cols 16-20**; the written type annotation
`int{ _ > 0 }` occupies **cols 11-22**.

`ocamlc.opt -c -annot input.ml` produced `input.annot` whose *only* line-1
entries (span shown as `char1-bol1 .. char2-bol2`) are:

| span (cols) | source text | recorded type |
|-------------|-------------|---------------|
| 4-5   | `f` (binder)   | `int{ _ > 0 } -> int` |
| 7-8   | `x` (binder)   | `int{ _ > 0 }` |
| 27-28 | `x` (body use) | `int` |

There is **no entry whose span covers cols 11-22 or 16-20**. Placing the cursor
on `_` (16), `>` (18), `0` (20), or anywhere in `int{ _ > 0 }` therefore matches
no range in `expressionTypes`, and `renderCursorType()`
(`voxide/app.js:247`) falls through to the "No inferred expression type" string.
Note the whole refinement type only ever appears as the *type payload* of the
`x` binder entry (span 7-8) — its text is not a span the cursor can land on.

This is exactly the client contract: `renderCursorType()` filters
`expressionTypes` by `contains(range, cursor)` and picks the smallest span
(`voxide/app.js:247-254`). `expressionTypes` is `response.types`
(`voxide/app.js:636`, `:1445`), and `types` is built by
`parse_annot()` from the `.annot` file
(`voxide/compiler.py:325-326`, `:1026-1029`). `parse_annot`
(`voxide/compiler.py:193-237`) emits one `{start,end,type}` per
`"..." L C .. "..." L C` + `type( ... )` block — i.e. exactly the cmt2annot
node stream, nothing more.

## 2. Where the predicate subterm types live in the compiler

- **The data structure.** A refinement type is `Trefine of refinement_desc`
  (`typing/types.ml:190`). `refinement_desc.ref_pred` is a
  `refinement_expression` (`typing/types.ml:252-256`). Every
  `refinement_expression` node carries **`rexp_type : type_expr`** and
  **`rexp_loc : Location.t`** (`typing/types.ml:196-200`). The desc mirrors a
  subset of `Typedtree.expression_desc` (`Rexp_ident`, `Rexp_constant`,
  `Rexp_apply`, `Rexp_ifthenelse`, …; `typing/types.ml:202-220`). So the
  per-subterm type **and** its source location are both retained.

- **How it is produced.** `Typecore.type_refinement`
  (`typing/typecore.ml:14112`) type-checks the predicate as an ordinary
  expression via `type_expect env ... predicate (mk_expected Predef.type_bool)`
  (`:14180-14183`), yielding a full `Typedtree.expression`
  (`typed_predicate`), then immediately lowers it to a `refinement_expression`
  with `lower_refinement_expression`
  (`typing/typecore.ml:13972`, called at `:14184-14185`). The lowered form is
  stored in the type; **the intermediate `typed_predicate` Typedtree node is
  discarded** and never attached to any structure item.

- **Why `.annot` misses it.** `.annot` is generated *after* typing by
  `cmt2annot.ml` walking the saved Typedtree with `Tast_iterator`: it records a
  node per `Ti_expr` / `Ti_pat` (`typing/cmt2annot.ml:117`, `:121`) and the
  `.cmt` is populated from `Cmt_format` saved types
  (`typing/typecore.ml:392`, `:6732-6733`). The iterator descends structure →
  expressions → patterns; it does **not** descend into `type_expr` internals,
  and the refinement predicate is not a Typedtree expression node reachable
  from any structure item (it was discarded during lowering). Hence no
  predicate subterm is ever handed to `Stypes.record`.

- **What the type checker already knows is faithful.** These `rexp_type`s are
  the very types used to drive VC generation and the Lean sort check
  (`typing/vox_lean.ml` reads `expression.rexp_type` / `.rexp_loc` throughout,
  e.g. `:321`, `:407`, `:558`), so they are the checked types, not a
  re-inference.

## 3. Fix options

### Option A — emit predicate subterms into the `.annot` / cmt stream
Make the compiler record predicate subterm types on the same channel `.annot`
uses. Two variants, both invasive:

- **A1 (retain the Typedtree predicate):** keep `typed_predicate` attached to
  the saved Typedtree so `cmt2annot` walks it. There is no natural structural
  slot for an expression that lives inside a *type*; this distorts the `.cmt`
  and risks confusing every other `.cmt`/`.annot`/merlin-style consumer with
  expression nodes that have no runtime existence. High risk, touches the
  upstream-shared cmt format.
- **A2 (teach cmt2annot to descend into `Trefine`):** walk `ref_pred` from
  within the type and emit `Ti_expr`-like records. But `refinement_expression`
  is not a `Typedtree.expression`, so `Stypes`/`cmt2annot` would need a new
  code path over the parallel AST, and the predicate must first be serialized
  into the `.cmt` (it currently is not). Medium-high effort, still touches the
  legacy `.annot`/cmt machinery that the rest of the toolchain shares.

Fidelity: good (types are the checked ones). Risk: high — perturbs a
format shared with upstream OCaml tooling for an IDE-only need. Size: large.

### Option B — per-predicate subterm type map on the VC-JSON dump (recommended)
The IDE already consumes the compiler's `-vox-dump-vc-json` output for the proof
pane (`voxide/compiler.py:574`, `:975`; live binary confirms the flag,
schema_version 2, with a top-level `verification_conditions` array and
per-goal `source_span` / `display`). Add a sibling top-level array, e.g.
`refinement_expression_types`, produced by a recursive walk over each
refinement type's `ref_pred`, emitting `{ file, start, end, type }` for every
`refinement_expression` node using `rexp_loc` and a rendered `rexp_type`.

- **What changes:**
  - Compiler: a ~30-40 line recursive walker over `refinement_expression_desc`
    (8 constructors) at the point the VC JSON is assembled, reusing the existing
    location→JSON helper and the existing refinement/type printer already used
    for `goal.display` and the source-like predicate printer (task #141).
    Emission must be keyed off *refinement types encountered*, not off VCs — a
    predicate on an assumed parameter (`int{_>0}` as an argument) may produce no
    obligation yet still has subterms to type. (This is the one design nuance:
    gather the map during translation of refinement types, independent of VC
    discharge.)
  - Server: `compiler.py` already parses the VC JSON; read the new array and
    normalize its coordinates to 0-based UTF-16 columns exactly like
    `parse_annot` does (reuse `_utf16_col`).
  - Client: merge these ranges into `expressionTypes` (or check them as a
    fallback in `renderCursorType`). `contains` / `spanSize` already handle the
    smallest-span selection, so nested predicate subterms just work.

- **Soundness / fidelity:** types come straight from the checked
  `refinement_expression`, identical to what VC generation used — no
  re-inference, no drift. No change to `.cmt`/`.annot`, so no upstream-tooling
  blast radius. The dump is already IDE-only and off by default.
- **Size:** moderate; localized to the VC-JSON assembly + a few lines each in
  `compiler.py` and `app.js`. Reuses the channel, the printer, and the client
  span machinery that already exist.

### Option C — client-side synthesis
Infeasible. The browser has no type information for predicate subterms; the
whole point is that no channel currently carries it. Rejected.

## 4. Recommendation

**Option B.** The exact data needed (`rexp_loc` + `rexp_type` per subterm) is
already present and location-tagged in `ref_pred`; the delivery channel
(`-vox-dump-vc-json`) is already built and already consumed by the IDE; the type
printer and the client's smallest-span selection already exist. It gives
faithful, checked types with a small, self-contained change and zero risk to the
shared `.cmt`/`.annot` format. Option A buys nothing extra in fidelity while
perturbing upstream-shared machinery; Option C is impossible.

Key design note for the implementer: emit the subterm map per *refinement type
encountered during translation*, not per VC, so predicates that generate no
obligation (e.g. refined parameters used as assumptions) still get cursor types.

---

## 5. Mode at cursor: showing `type @ mode` (scope addendum)

The user wants the cursor readout to show both the type **and** the vox mode of
the expression, e.g. `7 : int{ _ > 0 } @ total`. This section covers where a
per-expression mode lives, whether any stream exposes it, and the cheapest
faithful way to fold it into the `/check` `types` entries.

### Background: where per-expression modes live

- **The axes.** The vox mode system's axes are `Mode.Value` axes:
  **Totality, Logicality, Portability, Contention**
  (`typing/mode.ml:555-641` in the axis signatures; concrete modules at
  `:5982-6069`). A value's full mode is a `Mode.Value.t` ranging over all axes;
  the vox-relevant readout is the projection onto these four.
- **Critical asymmetry vs. types.** There is **no** per-expression mode field on
  `Typedtree.expression`. The record is
  `{ exp_desc; exp_loc; exp_extra; exp_type; exp_env }`
  (`typing/typedtree.mli:389-394`) — an `exp_type` but no `exp_mode`/`value_mode`.
  A mode is a *checking judgment*: an `expected_mode` (upper bound) is pushed down
  (`typing/typecore.ml:444-472`) and the value's actual mode is
  synthesized/submoded bottom-up; the result is generally **not persisted**.
  Precise enumeration of what *is* stored, from `expression_desc`
  (`typing/typedtree.mli:389-760`):
  - **`Texp_ident { mode : Mode.Value.l; ... }`** (`typedtree.mli:232`) — the
    **only** constructor that stores a resolved *full value mode* of the
    expression itself. This is the one node from which a vox mode
    (totality/logicality/portability/contention) can be read directly.
  - **Allocating constructors carry an `alloc_mode : Mode.Alloc.r`** —
    `Texp_tuple`, `Texp_construct`, `Texp_variant`, `Texp_record`,
    `Texp_array`, `Texp_atomic_loc` (grep of the desc). `Mode.Alloc` is the
    *allocation's comonadic mode* (locality/linearity/portability/yielding); it
    is **not** the full `Mode.Value` and does **not** carry totality or
    logicality, so it cannot answer `@ total`.
  - **`Texp_function`** carries `ret_mode : Mode.Alloc.l modes` and
    `alloc_mode : alloc_mode` — the closure's return/allocation modes, not the
    value mode of the function expression as a whole.
  - Everything else (`Texp_apply`, `Texp_ifthenelse`, `Texp_constant`,
    `Texp_field`, `Texp_sequence`, …) stores **no mode**.

  So a resolved vox value mode is attached to expression nodes **only for
  identifier leaves**; it is otherwise on bindings/arrows-as-allocation or not
  materialized at all. The mode of an arbitrary sub-expression is therefore
  *not* recoverable from the saved Typedtree / `.cmt` the way `exp_type` is.
- **Consequence for streams.** `.annot`/`Stypes` records only `exp_type`
  (`typing/stypes.ml:141-142`, `cmt2annot.ml:117`) — it has no slot for a mode
  and cannot carry one without a format change to an upstream-shared file. The
  VC-JSON dump currently emits no mode field. **No existing artifact exposes an
  expression's mode at a source span.**

### (i) Can we get the mode of the expression at a cursor span?

For the **same `Ti_expr` nodes the `.annot` walker visits**: only for
*identifier* nodes (`Texp_ident.mode`). Every other visited expression node has
no resolved value mode to read off, and the allocation modes some carry are the
wrong lattice (no totality/logicality). So there is no artifact that yields a
faithful vox mode for a general expression span.

Surfacing it faithfully requires instrumenting
the type checker to snapshot each expression's mode at the point it is checked
(where the mode is in hand), keyed by `exp_loc` — a new recording stream
analogous to "Stypes but for `Mode.Value`". Two real complications:

1. **Which mode.** The *actual* synthesized value mode (what the expression
   *is*) vs. the `expected_mode` (what context requires). For `7 : int @ total`
   the actual value mode is intended. `Texp_ident` already stores its actual
   mode; composite expressions must have it captured during `type_exp`.
2. **Unsolved mode variables.** Value modes contain inference variables during
   checking; to print a concrete `@ total` you must read a bound or zap
   (`Value.zap_to_ceil`/`zap_to_floor`, per-axis `proj`;
   `typing/mode_intf.mli:192-194`, `:660-686`). Zapping mid-check forces a
   variable and can perturb inference, so the snapshot must record bounds and
   resolve lazily, or be read **after the unit is fully typed**
   (post-generalization). This is the main design hazard.

Feasible, but materially larger than type-at-cursor: it adds a typing-path side
table + axis projection + careful variable handling, not just a re-walk of
already-persisted data.

### (ii) Can we get it for refinement-predicate subterms too?

Same shape as the type problem, one notch harder. `refinement_expression`
carries `rexp_type` and `rexp_loc` but **no mode field** (`types.ml:196-200`).
The *coarse* frame is, however, known for free from `type_refinement`: the
predicate is checked at `predicate_mode = Total`
(`typing/typecore.ml:14174-14179`) and the hole `_` (`ref_view`) is bound at a
**logical** `self_mode` (`:14146-14171`). So a faithful coarse readout is
available with no new capture: the predicate context is `total`, and the `_`
reference is `logical`. Per-*subterm* actual modes are not retained — the
`typed_predicate` Typedtree is discarded at lowering (the same mechanism that
loses the subterm types, §2). To get faithful per-subterm modes you'd either
(a) capture them during the predicate's `type_expect` before lowering, or
(b) add a mode field to `refinement_expression` populated at lowering.

### (iii) Cheapest faithful way to surface `type @ mode` into the `types` entries

The `types` entries already carry `{span, type}`. Extend them to
`{span, type, mode}` rather than inventing a second stream, and deliver on the
**same VC-JSON channel as Option B** (never `.annot` — upstream-shared, no mode
slot). Staged plan, cheapest faithful first:

- **Phase 1 — types in refinement (Option B).** Small; unblocks the primary
  complaint (no type on predicate subterms).
- **Phase 2 — modes.** Add a per-`loc` mode snapshot in the type checker,
  resolved *after* the unit is typed, projected onto
  `{totality, logicality, portability, contention}` and rendered with the
  existing `Mode.Value` axis printers (`mode_intf.mli` `print` / `print_axis`).
  Emit a `mode` string alongside each entry in the VC JSON; the server folds it
  into the `types` entry; the client renders `type + " @ " + mode` in
  `renderCursorType` (`app.js:247-254`) — a one-line client change.
- **Predicate subterms.** Seed their mode from the known coarse frame
  (predicate = `total`, `_` = `logical`) if per-subterm capture is deferred;
  upgrade to true per-subterm modes when the Phase-2 recorder also runs over the
  predicate's `type_expect`.

### Recommendation (mode axis)

Extend the same VC-JSON channel to `{span, type, mode}`; do **not** touch
`.annot`. Ship types-in-refinement (Option B) **first** — it is small, faithful,
and resolves the reported bug. Treat mode-at-cursor as a **Phase 2 follow-on**
with its own design + soundness review, because it touches the mode-inference
hot path and hinges on resolving mode variables without perturbing inference
(snapshot bounds / read post-typing). The client change to concatenate
` @ mode` is trivial; the compiler-side mode recorder is the real work and the
real risk.

