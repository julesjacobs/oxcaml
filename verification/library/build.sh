#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:?Usage: build.sh COMPILER_PREFIX}
prefix=$(cd "$prefix" && pwd)
output="$root/_build/vox-library"
destination="$prefix/lib/ocaml/vox"
modules=(vox_sequence vox_http_spec vox_http vox_int_sequence vox_iarray
         vox_sat_spec vox_sat_proof vox_sat
         vox_cdcl_proof vox_cdcl vox_cdcl_total_proof vox_cdcl_total
         vox_credits vox_ordered_sequence vox_merge_proofs vox_sort_cost
         vox_merge_sort borrow borrow_iarray
         pref vox_pref_semantics ghost_pref vox_big_credits vox_ackermann
         vox_union_find_potential vox_union_find_levels vox_union_find_path_cost
         vox_union_find_model vox_union_find_forest vox_union_find_rank
         vox_union_find_mass vox_union_find_link vox_union_find_worker
         vox_union_find_amortized vox_union_find_bank vox_union_find_spec
         vox_union_find vox_union_find_complexity
         vox_union_find_simple vox_union_find_online vox_connectivity
         vox_union_find_online_cost
         raw_memory verified_atomic unique_cell one_shot
         channel_buffer unique_lock reference_lock
         vox_control vox_table_model vox_table_model_proofs vox_table_bits
         vox_table_probe vox_table_wrap vox_table_mask vox_table_map
         vox_table_invariant vox_table_initial vox_table_update_proofs
         vox_table_insert_proofs vox_table_migration_proofs
         vox_table_read_proofs vox_table_search_spec vox_table_stop_proof
         vox_table_storage vox_table_search vox_table_mutation
         vox_table_coverage vox_table_occupancy vox_table_vacancy_progress
         vox_table_vacancy vox_table_insert vox_table_migrate vox_table_resize
         vox_table_implementation vox_table_bindings vox_table_bindings_bridge
         vox_verified_flat_hashtbl)
mkdir -p "$output"
for module in "${modules[@]}"; do
  cp "$root/verification/library/$module.ml" "$output/"
  if [[ -f "$root/verification/library/$module.mli" ]]; then
    cp "$root/verification/library/$module.mli" "$output/"
  else
    rm -f "$output/$module.mli"
  fi
done
cd "$output"
flags=(-nostdlib -I "$prefix/lib/ocaml" -I . -extension refinement_types)
for module in "${modules[@]}"; do
  module_flags=("${flags[@]}" -principal)
  # OxCaml -principal rejects even int option at an immutable_data parameter.
  # These modules still undergo all refinement and termination checks.
  case "$module" in
    vox_http | vox_cdcl_total | vox_cdcl_total_proof | vox_table_* | \
    vox_verified_flat_hashtbl)
      module_flags=("${flags[@]}") ;;
  esac
  if [[ -f "$module.mli" ]]; then
    "$prefix/bin/ocamlc" "${module_flags[@]}" -c "$module.mli"
  fi
  "$prefix/bin/ocamlc" "${module_flags[@]}" -c "$module.ml"
  "$prefix/bin/ocamlopt" "${module_flags[@]}" -c "$module.ml"
done
"$prefix/bin/ocamlc" "${flags[@]}" -a -o vox_borrow.cma "${modules[@]/%/.cmo}"
"$prefix/bin/ocamlopt" "${flags[@]}" -a -o vox_borrow.cmxa "${modules[@]/%/.cmx}"
mkdir -p "$destination"
for module in "${modules[@]}"; do
  case "$module" in
    vox_sat_proof | vox_cdcl_proof | vox_cdcl_total_proof) continue ;;
  esac
  cp "$module".{cmi,cmx} "$destination/"
  if [[ -f "$module.mli" ]]; then
    cp "$module.mli" "$destination/"
  fi
done
cp vox_borrow.{cma,cmxa,a} "$destination/"
cp "$root/verification/library/META" "$destination/"
printf 'Verified Vox library installed in %s\n' "$destination"
