#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=$(cd "${1:?Usage: check_sat_erasure.sh COMPILER_PREFIX}" && pwd)
built="$root/_build/vox-library"
if [[ -n "${2:-}" ]]; then
  mkdir -p "$2"
  audit=$(cd "$2" && pwd)
else
  audit=$(mktemp -d "${TMPDIR:-/tmp}/vox-sat-erasure.XXXXXX")
  trap 'rm -rf "$audit"' EXIT
fi
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
if grep -E 'empty_unsatisfiable|semantic_unsat_at|exhaustive_result|rejects_extensions|derivation_valid|classify_input' ./*.lambda; then
  echo 'SAT proof code survived in a public entrypoint.' >&2
  exit 1
fi
for backend in byte native; do
  for module in vox_sat vox_cdcl vox_cdcl_total; do
    expected=1
    if [[ "$module" == vox_cdcl_total ]]; then expected=6; fi
    actual=$(grep -c '(apply ' "$module.$backend.lambda")
    if [[ "$actual" != "$expected" ]]; then
      echo "Unexpected runtime calls in $module ($backend)." >&2
      exit 1
    fi
  done
done
printf 'Public SAT proof bridges erase in bytecode and native Lambda.\n'
nm "$built/vox_cdcl_total_proof.o" > cdcl.symbols
if grep -E 'clause_rank|resolve_rank|earlier_clause_rank|variable_source_member|prefix_|no_current_false|resolve_preserves_current|clause_universe|literal_universe|count_absent|progress_measure|progress_learning|progress_decision|trail_levels|decision_level_bound' \
  cdcl.symbols; then
  echo 'CDCL analysis rank proof survived in native code.' >&2
  exit 1
fi
printf 'CDCL analysis rank proofs erase in native code.\n'
