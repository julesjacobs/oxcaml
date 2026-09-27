#!/usr/bin/env bash
# Builds the Vox playground as a directory of static files.
#
#   verification/playground/build.sh [--out DIR] [--library-prefix PREFIX]
#
# Needs a configured checkout (see AGENTS.md) and the oxcaml-5.4.0+oxcaml
# opam switch with js_of_ocaml 6.3.2, plus node and npm. Steps:
#
# 1. `make boot-compiler runtime-stdlib`: the compiler front end and the
#    verifier are built in Dune's boot context by the switch's compiler, so
#    the switch's js_of_ocaml can compile them; the standard library
#    interfaces come from the runtime_stdlib context, built by that front end.
# 2. Link vox_playground.ml with those libraries into a bytecode executable
#    and compile it to JavaScript.
# 3. Install z3-solver 4.16.0 from npm (the Z3 version the native Vox uses).
# 4. Copy the page, the interfaces and Z3 into DIR (default
#    _build/playground/site).
#
# The verified library's interfaces are included when PREFIX/lib/ocaml/vox
# exists (verification/library/build.sh PREFIX installs them there).
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=$root/_build/playground/site
library_prefix=
while [[ $# -gt 0 ]]; do
  case $1 in
    --out) out=$2; shift 2 ;;
    --library-prefix) library_prefix=$2; shift 2 ;;
    *) echo "unknown option $1" >&2; exit 2 ;;
  esac
done
work=$root/_build/playground/obj

eval "$(opam env --switch=oxcaml-5.4.0+oxcaml --set-switch)"
cd "$root"

echo "== compiler libraries (boot context) and standard library"
make -s boot-compiler runtime-stdlib > "$root/_build/playground-make.log" 2>&1 \
  || { tail -40 "$root/_build/playground-make.log"; exit 1; }
dune build --root=. --workspace=duneconf/boot.ws \
  ocamlcommon.cma ocamlfrontend.cma \
  middle_end/flambda2/numbers/floats/flambda2_floats.cma \
  verification/vox_smt.cma verification/vox_vc.cma

echo "== bytecode"
boot=$root/_build/default
mkdir -p "$work"
cp "$root/verification/runtime/vox_verify.enabled.ml" "$work/vox_verify.ml"
cp "$root/verification/runtime/vox_verify.mli" "$work/vox_verify.mli"
cp "$root/verification/vox_smt_solver.mli" "$work/vox_smt_solver.mli"
cp "$here/vox_smt_solver.ml" "$here/vox_playground.ml" "$work/"
includes=(-I "$boot/.ocamlcommon.objs/byte" -I "$boot/.ocamlfrontend.objs/byte"
          -I "$boot/middle_end/flambda2/numbers/floats/.flambda2_floats.objs/byte"
          -I "$boot/verification/.vox_smt.objs/byte"
          -I "$boot/verification/.vox_vc.objs/byte")
(
  cd "$work"
  ocamlfind ocamlc -g -package js_of_ocaml,unix -w +a-4-40-41-42-44-45-70 \
    "${includes[@]}" -c vox_smt_solver.mli vox_smt_solver.ml \
    vox_verify.mli vox_verify.ml vox_playground.ml
  ocamlfind ocamlc -g -package js_of_ocaml,unix -linkpkg -noautolink -no-check-prims -o vox_playground.bc \
    "$boot/ocamlcommon.cma" \
    "$boot/middle_end/flambda2/numbers/floats/flambda2_floats.cma" \
    "$boot/ocamlfrontend.cma" \
    "$boot/verification/vox_smt.cma" "$boot/verification/vox_vc.cma" \
    vox_smt_solver.cmo vox_verify.cmo vox_playground.cmo
)

echo "== JavaScript"
js_of_ocaml --opt=3 --no-sourcemap \
  "$here/runtime.js" \
  -o "$work/vox.js" "$work/vox_playground.bc" 2>&1 \
  | grep -v "^Warning: your program contains effect handlers" || true

echo "== Z3"
(cd "$here" && npm ci --ignore-scripts --no-audit --no-fund --silent)
z3=$here/node_modules/z3-solver/build

echo "== site in $out"
rm -rf "$out"
mkdir -p "$out/lib"
cp "$work/vox.js" "$z3/z3-built.js" "$z3/z3-built.wasm" "$out/"
cp "$here"/web/* "$out/"
cp "$root/verification/catalogue/style.css" "$out/catalogue.css"
# The interfaces, as one bundle with an index: the standard library, and the
# verified library when it is installed.
directories=("ocaml=$root/_build/runtime_stdlib_install/lib/ocaml_runtime_stdlib")
if [[ -n $library_prefix && -d $library_prefix/lib/ocaml/vox ]]; then
  directories+=("vox=$library_prefix/lib/ocaml/vox")
fi
python3 - "$out/lib" "$(git -C "$root" rev-parse --short=10 HEAD)" "${directories[@]}" <<'PY'
import json, os, sys
out, revision, *directories = sys.argv[1:]
files, offset = [], 0
with open(os.path.join(out, 'bundle.bin'), 'wb') as bundle:
    for entry in directories:
        prefix, directory = entry.split('=', 1)
        for name in sorted(os.listdir(directory)):
            if not name.endswith('.cmi'):
                continue
            data = open(os.path.join(directory, name), 'rb').read()
            bundle.write(data)
            files.append([f'lib/{prefix}/{name}', offset, len(data)])
            offset += len(data)
json.dump({'revision': revision, 'files': files}, open(os.path.join(out, 'index.json'), 'w'))
print(f'{len(files)} interfaces, {offset} bytes')
PY
du -sh "$out"
