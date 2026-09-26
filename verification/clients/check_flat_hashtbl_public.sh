#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:-"$root/_install"}
prefix=$(cd "$prefix" && pwd)
output="$root/_build/flat-hashtbl-public"
mkdir -p "$output/public"
for module in pref ghost_pref vox_verified_flat_hashtbl; do
  cp "$prefix/lib/ocaml/vox/$module.cmi" "$output/public/"
done
cp "$root/verification/clients/flat_hashtbl_public.ml" "$output/"
cd "$output"
flags=(-nostdlib -I "$prefix/lib/ocaml" -I public -extension refinement_types)
for compiler in ocamlc ocamlopt; do
  "$prefix/bin/$compiler" "${flags[@]}" -dlambda -c flat_hashtbl_public.ml \
    2> "$compiler.lambda"
  if [[ $compiler == ocamlc ]]; then ext=cmo; archive=cma; else ext=cmx; archive=cmxa; fi
  "$prefix/bin/$compiler" "${flags[@]}" "$prefix/lib/ocaml/vox/vox_borrow.$archive" \
    "flat_hashtbl_public.$ext" -o "$compiler.exe"
  ./"$compiler.exe"
done
"$prefix/bin/ocamlopt" "${flags[@]}" -dcmm -c flat_hashtbl_public.ml 2> ocamlopt.cmm
python3 "$root/verification/clients/flat_hashtbl_rejections.py" "$prefix" "$output"
python3 "$root/verification/clients/flat_hashtbl_erasure.py" "$output"
printf 'Public-only bytecode/native clients and rejection checks passed.\n'
