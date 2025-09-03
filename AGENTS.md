Agent Notes: ikinds build + Ikind shim

Overview
- Enabled building of `typing/ikinds` sources in the dune build and exposed a thin shim `Ikind.sub_jkind_l` that currently delegates to `Jkind.sub_jkind_l`.
- Redirected `ctype.ml` to use the shim so we can instrument and experiment without invasive changes elsewhere.

What changed
- Dune: copy and compile `typing/ikinds/*.ml{i}` and include their modules in the `ocamlcommon` library.
  - Added modules: `typing/ikinds/global_counters.ml`, `typing/ikinds/lattice_intf.ml`, `typing/ikinds/product_lattice.ml`, `typing/ikinds/lattice_polynomial.ml`, `typing/ikinds/ldd.ml`, `typing/ikinds/ldd_jkind_solver.ml`, `typing/ikinds/axis_lattice.ml`, `typing/ikinds/ikind.ml`.
- Ikind shim: `typing/ikinds/ikind.ml` defines:
  - `let sub_jkind_l ~type_equal ~context sub super =
       print_endline "sub_jkind_l"; Jkind.sub_jkind_l ~type_equal ~context sub super`
- Call sites updated in `ctype` to go through the shim:
  - `typing/ctype.ml:7361` and `typing/ctype.ml:7380` now call `Ikind.sub_jkind_l` instead of `Jkind.sub_jkind_l`.
- Support: added `typing/ikinds/global_counters.ml` to satisfy references from ikinds helpers.

How to see the message
- Use the in-tree compiler built by dune:
  - `_build/default/main_native.exe -version`
  - `_build/default/main_native.exe -c experiments/jkinds/B.mli`
  - `_build/default/main_native.exe -c experiments/jkinds/B.ml`
- You should see the line `sub_jkind_l` printed during compilation (from the shim).

Why `_install/bin/ocamlc` won’t show it
- `_install/bin/ocamlc` reflects the last installation; unless you reinstall after these changes, it won’t include the shim or print. Prefer `_build/default/main_native.exe` for quick local testing.

Goal and Migration Plan
- Ultimate goal: replace the existing jkind system with a new ikind system.
- Current scope: start by replacing only the `sub_jkind_l` check.
  - We have hooked all `sub_jkind_l` call sites to `Ikind.sub_jkind_l`.
  - For now, the shim prints a marker and delegates to the original `Jkind.sub_jkind_l`.
- Next steps for `sub_jkind_l`:
  - Convert both argument jkinds into an internal ikind representation and print them for inspection.
  - Implement the ikind-based sub-check; run it alongside the jkind check and assert results match.
  - Once stable, switch call sites to use the ikind result directly.
- Longer term: incrementally port remaining jkind functionality to ikind, step by step, until jkind can be fully retired.

Kinds and Types Reference
- Types.jkind_l: The jkind of an actual type (left side of sub-jkind checks).
  - Definition: `type jkind_l = (allowed * disallowed) Types.jkind` (see `typing/types.mli:402`).
  - Purpose: Describes the runtime representation (layout + axes/mod-bounds) inferred for a concrete type.
  - Fields (via record `Types.jkind`): `jkind` (layout and axes), `annotation`, `history`, `has_warned`, `ran_out_of_fuel_during_normalize`, `quality`.
  - Quality: `Best` means we won’t later lower it; `Not_best` may be refined.
  - Usage:
    - From environment: `Env.find_type p env` → use `decl.type_jkind`.
    - From a type expression: `Ctype.type_jkind env ty` (or the “pure” variants) returns a `jkind_l`.
    - Operations: `Jkind.sub_jkind_l`, `Jkind.round_up`, `Jkind.extract_layout`, etc.

- Types.type_expr: The core type expression used throughout typing.
  - Abstract type; inspect with `Types.get_desc : type_expr -> type_desc`.
  - Key constructors in `type_desc` (see `typing/types.mli`):
    - `Tvar`/`Tunivar`: type variables (carry a `jkind_lr`).
    - `Tconstr (Path.t, type_expr list, abbrev_memo ref)`: named types with arguments.
    - `Tarrow`, `Ttuple`, `Tunboxed_tuple`, `Tobject`, `Tvariant`, `Tpoly`, `Tpackage`, `Tof_kind`.
  - Typical sources:
    - `Env.find_type p env` returns a `type_declaration`, whose `type_manifest : type_expr option` is the body when present (abbreviation), or `None` for truly abstract types.
    - `context.lookup_type p` (added to `Jkind.jkind_context`) yields `(jkind_l, type_expr option)` for quick lookups.
  - Common helpers: `Types.get_level`, `Types.get_scope`, `Types.get_id`; traversal via `Btype.iter_type_expr` and friends.

Layout And Axes
- Types.layout_and_axes: Core payload of a jkind — layout plus axis bounds and with-bounds.
  - Definition: `type ('layout, 'd) layout_and_axes = { layout : 'layout; mod_bounds : Jkind_mod_bounds.t; with_bounds : 'd with_bounds }` (`typing/types.mli`).
  - In jkinds: `'layout` is `Jkind_types.Sort.t Jkind_types.Layout.t` (a Sort or Product of Sorts; or Any on the right), and `'d` is a pair indicating left/right allowance.
  - Fields:
    - layout: Concrete memory layout description (e.g., immediate, boxed, product of component layouts).
    - mod_bounds: Bounds for modality axes (linearity, uniqueness, externality, nullability, separability, etc.). See `Types.Jkind_mod_bounds` accessors.
    - with_bounds: Either `No_with_bounds` or `With_bounds of with_bounds_types`, tracking existential type equations used to refine layout/axes during checks.
  - Left/Right allowance: `'d = 'l * 'r` is managed by the allowance machinery; left jkinds allow joins, right jkinds allow meets. Some operations require disallowing one side to enforce invariants.
  - Common ops (see `typing/jkind.ml` Layout_and_axes): mapping over layouts, equality when both sides have `No_with_bounds`, and helpers to allow/disallow sides.
