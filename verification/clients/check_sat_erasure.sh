#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=$(cd "${1:?Usage: check_sat_erasure.sh COMPILER_PREFIX}" && pwd)
built="$root/_build/vox-library"
audit=$(mktemp -d "${TMPDIR:-/tmp}/vox-sat-erasure.XXXXXX")
trap 'rm -rf "$audit"' EXIT
for module in vox_sat vox_cdcl vox_cdcl_total; do
  cp "$built/$module.ml" "$built/$module.cmi" "$audit/"
done
cd "$audit"
flags=(-nostdlib -I "$prefix/lib/ocaml" -I . -I "$built"
       -extension refinement_types -dlambda)
for module in vox_sat vox_cdcl vox_cdcl_total; do
  "$prefix/bin/ocamlc" "${flags[@]}" -c "$module.ml" 2>"$module.byte.lambda"
  "$prefix/bin/ocamlopt" "${flags[@]}" -c "$module.ml" 2>"$module.native.lambda"
done
if grep -E 'empty_unsatisfiable|semantic_unsat_at|exhaustive_result|rejects_extensions|derivation_valid' ./*.lambda; then
  echo 'SAT proof code survived in a public entrypoint.' >&2
  exit 1
fi
for backend in byte native; do
  for module in vox_sat vox_cdcl vox_cdcl_total; do
    expected=1
    if [[ "$module" == vox_cdcl_total ]]; then expected=2; fi
    actual=$(grep -c '(apply ' "$module.$backend.lambda")
    if [[ "$actual" != "$expected" ]]; then
      echo "Unexpected runtime calls in $module ($backend)." >&2
      exit 1
    fi
  done
done
printf 'Public SAT proof bridges erase in bytecode and native Lambda.\n'
