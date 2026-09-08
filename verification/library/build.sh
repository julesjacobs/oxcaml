#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:?Usage: build.sh COMPILER_PREFIX}
prefix=$(cd "$prefix" && pwd)
output="$root/_build/vox-library"
destination="$prefix/lib/ocaml/vox"
modules=(vox_sequence vox_int_sequence vox_iarray borrow borrow_iarray)
mkdir -p "$output"
for module in "${modules[@]}"; do
  cp "$root/verification/library/$module.mli" "$root/verification/library/$module.ml" "$output/"
done
cd "$output"
flags=(-nostdlib -I "$prefix/lib/ocaml" -I . -extension refinement_types -principal)
for module in "${modules[@]}"; do
  "$prefix/bin/ocamlc" "${flags[@]}" -c "$module.mli"
  "$prefix/bin/ocamlc" "${flags[@]}" -c "$module.ml"
  "$prefix/bin/ocamlopt" "${flags[@]}" -c "$module.ml"
done
"$prefix/bin/ocamlc" "${flags[@]}" -a -o vox_borrow.cma "${modules[@]/%/.cmo}"
"$prefix/bin/ocamlopt" "${flags[@]}" -a -o vox_borrow.cmxa "${modules[@]/%/.cmx}"
mkdir -p "$destination"
for module in "${modules[@]}"; do
  cp "$module".{mli,cmi,cmx} "$destination/"
done
cp vox_borrow.{cma,cmxa,a} "$destination/"
cp "$root/verification/library/META" "$destination/"
printf 'Verified borrow library installed in %s\n' "$destination"
