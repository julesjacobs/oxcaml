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

Cleanup / next steps
- Replace the `print_endline` with a gated log (e.g., environment variable) or remove once the experiment is done.
- If desired, move additional jkind experimentation behind the `Ikind` shim to keep `ctype` stable.

