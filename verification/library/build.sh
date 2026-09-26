#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:?Usage: build.sh COMPILER_PREFIX}
prefix=$(cd "$prefix" && pwd)
output="$root/_build/vox-library"
destination="$prefix/lib/ocaml/vox"
modules=(vox_sequence vox_http_spec vox_http vox_int_sequence vox_iarray vox_string_view
         vox_sat_spec vox_sat_proof vox_sat
         vox_cdcl_proof vox_cdcl vox_cdcl_total_proof vox_cdcl_total
         vox_credits vox_ordered_sequence vox_merge_proofs vox_sort_cost
         vox_merge_sort vox_lz4_model borrow borrow_iarray
         pref vox_pref_semantics ghost_pref vox_big_credits vox_ackermann
         vox_union_find_potential vox_union_find_levels vox_union_find_path_cost
         vox_union_find_model vox_union_find_forest vox_union_find_rank
         vox_union_find_mass vox_union_find_link vox_union_find_worker
         vox_union_find_amortized vox_union_find_bank vox_union_find_spec
         vox_union_find_events vox_union_find vox_union_find_complexity
         vox_union_find_simple vox_union_find_online vox_connectivity
         vox_union_find_online_cost
         raw_memory
         vox_lz4_spec_storage vox_lz4_spec_parse vox_lz4_spec_decode
         vox_lz4_spec_decode_bytes vox_lz4_spec_bytes vox_lz4_heap_bytes
         vox_lz4_spec_match
         vox_lz4_spec_plan vox_lz4_spec_token vox_lz4_spec_wire
         vox_lz4_spec_hashes vox_lz4_spec_scan vox_lz4_spec
         vox_lz4_buffer vox_lz4_packed
         vox_lz4_encode_buffer vox_lz4_packed_encode vox_lz4_snapshot
         vox_lz4_string_copy
         vox_lz4_roundtrip vox_lz4_general_match vox_lz4_string_match
         vox_lz4_general_plan
         vox_lz4_general_encode vox_lz4_general_wire
         vox_lz4_decode_bytes_proof vox_lz4_decode_bytes_roundtrip
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
# Units already verified with identical inputs are not verified again.
export VOX_VERIFY_CACHE=${VOX_VERIFY_CACHE-$root/_build/vox-verify-cache}
# Copy only changed sources, so that make rebuilds only what they affect.
for module in "${modules[@]}"; do
  for source in "$module.ml" "$module.mli"; do
    if [[ -f "$root/verification/library/$source" ]]; then
      cmp -s "$root/verification/library/$source" "$output/$source" ||
        cp "$root/verification/library/$source" "$output/$source"
    else
      rm -f "$output/$source"
    fi
  done
done
cd "$output"
flags="-nostdlib -I $prefix/lib/ocaml -I . -extension refinement_types"
sources=()
for module in "${modules[@]}"; do
  sources+=("$module.ml")
  [[ -f "$module.mli" ]] && sources+=("$module.mli")
done
dependency_lines=$("$prefix/bin/ocamldep" -modules "${sources[@]}")
# Each unit is verified by ocamlc and then compiled natively without
# verifying it again. A unit's dependents wait for its native compilation, so
# no compilation reads a .cmi that ocamlopt is rewriting. Every object also
# depends on the compilers, so installing a new compiler rebuilds the library.
compilers="$(cd "$prefix/bin" && pwd -P)/ocamlc.opt $(cd "$prefix/bin" && pwd -P)/ocamlopt.opt"
{
  printf 'all: vox_borrow.cma vox_borrow.cmxa\n'
  printf 'vox_borrow.cma: %s\n' "${modules[*]/%/.cmo}"
  printf '\t%s/bin/ocamlc %s -a -o $@ $^\n' "$prefix" "$flags"
  printf 'vox_borrow.cmxa: %s\n' "${modules[*]/%/.cmx}"
  printf '\t%s/bin/ocamlopt %s -a -o $@ $^\n' "$prefix" "$flags"
  for module in "${modules[@]}"; do
    module_flags="$flags -principal"
    # OxCaml -principal rejects even int option at an immutable_data
    # parameter. These modules still undergo all refinement and termination
    # checks.
    case "$module" in
      vox_http | vox_cdcl_total | vox_cdcl_total_proof | vox_table_* | \
      vox_verified_flat_hashtbl)
        module_flags=$flags ;;
    esac
    dependencies=" $compilers"
    for dependency in $(printf '%s\n' "$dependency_lines" |
        grep -E "^$module\.mli?:" | cut -d: -f2); do
      dependency=$(printf '%s' "${dependency:0:1}" | tr '[:upper:]' '[:lower:]')${dependency:1}
      if [[ " ${modules[*]} " == *" $dependency "* ]]; then
        dependencies="$dependencies $dependency.cmx"
      fi
    done
    interface=
    if [[ -f "$module.mli" ]]; then
      interface=$module.cmi
      printf '%s.cmi: %s.mli%s\n' "$module" "$module" "$dependencies"
      printf '\t%s/bin/ocamlc %s -c %s.mli\n' "$prefix" "$module_flags" "$module"
    fi
    printf '%s.cmo: %s.ml %s%s\n' "$module" "$module" "$interface" "$dependencies"
    printf '\t%s/bin/ocamlc %s -c %s.ml\n' "$prefix" "$module_flags" "$module"
    native_flags=
    case "$module" in
      vox_lz4* | vox_string_view | raw_memory | borrow_iarray)
        native_flags=" -O3" ;;
    esac
    printf '%s.cmx: %s.cmo%s\n' "$module" "$module" "$dependencies"
    printf '\t%s/bin/ocamlopt %s%s -smt-assume-verified -c %s.ml\n' \
      "$prefix" "$module_flags" "$native_flags" "$module"
  done
} > build.mk
make -s -f build.mk -j "${VOX_BUILD_JOBS:-$(getconf _NPROCESSORS_ONLN)}"
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
