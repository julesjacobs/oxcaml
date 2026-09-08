#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:?Usage: build.sh COMPILER_PREFIX}
prefix=$(cd "$prefix" && pwd)
output="$root/_build/vox-library"
destination="$prefix/lib/ocaml/vox"
mkdir -p "$output"
for module in vox_sequence vox_int_sequence borrow; do
  cp "$root/verification/library/$module.mli" "$root/verification/library/$module.ml" "$output/"
done
cd "$output"
flags=(-nostdlib -I "$prefix/lib/ocaml" -I . -extension refinement_types -principal)
for module in vox_sequence vox_int_sequence borrow; do
  "$prefix/bin/ocamlc" "${flags[@]}" -c "$module.mli"
  "$prefix/bin/ocamlc" "${flags[@]}" -c "$module.ml"
  "$prefix/bin/ocamlopt" "${flags[@]}" -c "$module.ml"
done
"$prefix/bin/ocamlc" "${flags[@]}" -a -o vox_borrow.cma vox_sequence.cmo vox_int_sequence.cmo borrow.cmo
"$prefix/bin/ocamlopt" "${flags[@]}" -a -o vox_borrow.cmxa vox_sequence.cmx vox_int_sequence.cmx borrow.cmx
mkdir -p "$destination"
cp vox_sequence.{mli,cmi,cmx} vox_int_sequence.{mli,cmi,cmx} borrow.{mli,cmi,cmx} vox_borrow.{cma,cmxa,a} "$destination/"
cp "$root/verification/library/META" "$destination/"
printf 'Verified borrow library installed in %s\n' "$destination"
