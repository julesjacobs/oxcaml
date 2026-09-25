#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
prefix="$root/_install"
output="$root/_build/vox-http-demo"
if [[ ! -x "$prefix/bin/ocamlc" ]]; then
  echo 'Configure with a worktree-local _install prefix and run make install first.' >&2
  exit 1
fi
mkdir -p "$output"
cp "$root/verification/library/vox_sequence."{ml,mli} "$output/"
cp "$root/verification/library/vox_http_spec."{ml,mli} "$output/"
cp "$root/verification/library/vox_http."{ml,mli} "$output/"
cp "$root/verification/demos/http_stream.ml" "$output/"
cd "$output"
"$prefix/bin/ocamlc" -nostdlib -I "$prefix/lib/ocaml" \
  -extension refinement_types vox_sequence.mli vox_sequence.ml \
  vox_http_spec.mli vox_http_spec.ml vox_http.mli vox_http.ml http_stream.ml -o http_stream.byte
exec ./http_stream.byte
