#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:?Usage: build.sh COMPILER_PREFIX}
prefix=$(cd "$prefix" && pwd)
output="$root/_build/vox-library"
destination="$prefix/lib/ocaml/vox"
modules=(vox_sequence vox_int_sequence vox_iarray vox_string_view
         vox_credits vox_ordered_sequence vox_merge_proofs vox_sort_cost
         vox_merge_sort vox_lz4_model borrow borrow_iarray
         pref ghost_pref raw_memory
         vox_lz4_spec_storage vox_lz4_spec_decode vox_lz4_spec_bytes vox_lz4_spec_match
         vox_lz4_spec_plan vox_lz4_spec_token vox_lz4_spec_wire
         vox_lz4_spec_hashes vox_lz4_spec_scan vox_lz4_spec
         vox_lz4_buffer vox_lz4_packed
         vox_lz4_encode_buffer vox_lz4_packed_encode vox_lz4_snapshot
         vox_lz4_string_copy
         vox_lz4_roundtrip vox_lz4_general_match vox_lz4_string_match
         vox_lz4_general_plan
         vox_lz4_general_encode vox_lz4_general_wire
         vox_lz4_general_cost vox_lz4_general_bridge
         vox_lz4_general_sized vox_lz4_string_encode
         vox_lz4_general_roundtrip
         vox_lz4_fast_plan_model vox_lz4_mutable_scan vox_lz4_string_scan
         vox_lz4_fast_plan_roundtrip
         vox_lz4_string_decode vox_lz4_string_codec
         vox_lz4_string_roundtrip
         vox_lz4_forward_model vox_lz4_streaming
         vox_lz4_streaming_codec vox_lz4_streaming_roundtrip
         vox_lz4_fast_hints_reference
         vox_lz4_checked_api vox_lz4
         verified_atomic unique_cell one_shot
         vox_control vox_table_model vox_table_model_proofs vox_table_bits
         vox_table_probe vox_table_wrap vox_table_mask vox_table_map
         vox_table_invariant vox_table_initial vox_table_update_proofs
         vox_table_insert_proofs vox_table_migration_proofs
         vox_table_read_proofs vox_table_search_spec vox_table_stop_proof
         vox_table_storage vox_table_search vox_table_mutation
         vox_table_coverage vox_table_occupancy vox_table_vacancy_progress
         vox_table_vacancy vox_table_insert vox_table_migrate vox_table_resize
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
    vox_table_* | vox_verified_flat_hashtbl) module_flags=("${flags[@]}") ;;
  esac
  if [[ -f "$module.mli" ]]; then
    "$prefix/bin/ocamlc" "${module_flags[@]}" -c "$module.mli"
  fi
  "$prefix/bin/ocamlc" "${module_flags[@]}" -c "$module.ml"
  native_flags=()
  case "$module" in
    vox_lz4* | vox_string_view | raw_memory | borrow_iarray) native_flags=(-O3) ;;
  esac
  "$prefix/bin/ocamlopt" "${module_flags[@]}" "${native_flags[@]}" -c "$module.ml"
done
"$prefix/bin/ocamlc" "${flags[@]}" -a -o vox_borrow.cma "${modules[@]/%/.cmo}"
"$prefix/bin/ocamlopt" "${flags[@]}" -a -o vox_borrow.cmxa "${modules[@]/%/.cmx}"
mkdir -p "$destination"
for module in "${modules[@]}"; do
  cp "$module".{cmi,cmx} "$destination/"
  if [[ -f "$module.mli" ]]; then
    cp "$module.mli" "$destination/"
  fi
done
cp vox_borrow.{cma,cmxa,a} "$destination/"
cp "$root/verification/library/META" "$destination/"
printf 'Verified borrow library installed in %s\n' "$destination"
