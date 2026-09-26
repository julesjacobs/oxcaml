#!/usr/bin/env bash
set -euo pipefail
export OCAMLRUNPARAM="${OCAMLRUNPARAM:-b},d=8"
root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=${1:-"$root/_install"}
work="$root/_build/concurrency-boundary"
mkdir -p "$work/library" "$work/public" "$work/logs"
modules=(pref ghost_pref raw_memory verified_atomic unique_cell one_shot channel_buffer unique_lock reference_lock)
for backend in ocamlc ocamlopt; do
  flags=(-nostdlib -I "$prefix/lib/ocaml" -extension refinement_types -alert -do_not_spawn_domains)
  cd "$work/library"
  for module in "${modules[@]}"; do
    cp "$root/verification/library/$module.mli" "$root/verification/library/$module.ml" .
    "$prefix/bin/$backend" "${flags[@]}" -principal -c "$module.mli"
    codegen=(); [[ "$backend" == ocamlopt ]] && codegen=(-S -dcmm)
    "$prefix/bin/$backend" "${flags[@]}" -principal "${codegen[@]}" -dlambda -c "$module.ml" 2> "$work/logs/$backend-$module.lambda"
    cp "$module.cmi" "$work/public/"
  done
  if rg 'caml_pref_(own|heap_|split|join)|caml_vox_atomic_key|caml_unique_cell_location' \
      "$work/logs/$backend-one_shot.lambda" "$work/logs/$backend-channel_buffer.lambda" \
      "$work/logs/$backend-unique_lock.lambda" "$work/logs/$backend-reference_lock.lambda"; then
    echo 'unexpected runtime ghost primitive' >&2; exit 1
  fi
  cd "$work/public"
  clients=(one_shot_demo one_shot_public_client channel_buffer_demo atomic_lock reference_lock_parallel unique_lock_demo unique_lock_parallel unique_lock_buffer_client)
  objects=()
  ext=cmo; [[ "$backend" == ocamlopt ]] && ext=cmx
  for module in "${modules[@]}"; do objects+=("$work/library/$module.$ext"); done
  for client in "${clients[@]}"; do
    cp "$root/testsuite/tests/vox/$client.ml" .
    "$prefix/bin/$backend" "${flags[@]}" -c "$client.ml" > "$work/logs/$backend-$client.compile" 2>&1
    extra=(); [[ "$client" == unique_lock_parallel ]] && extra+=("unique_lock_demo.$ext")
    "$prefix/bin/$backend" "${flags[@]}" -o "$client.exe" "${objects[@]}" "${extra[@]}" "$client.$ext"
    if [[ "$backend" == ocamlc ]]; then
      "$prefix/bin/ocamlrun" "$client.exe"
    else
      "./$client.exe"
    fi
  done
  for source in "$root/verification/concurrency/reject-"*.ml; do
    name=$(basename "$source")
    cp "$source" .
    if "$prefix/bin/$backend" "${flags[@]}" -c "$name" > "$work/logs/$backend-$name.log" 2>&1; then
      echo "unexpected acceptance: $name" >&2; exit 1
    fi
    case "$name" in
      *hidden*) expected='Unbound module' ;;
      *reuse*) expected='already been used as unique' ;;
      *) expected='Refinement could not be proved' ;;
    esac
    rg -q "$expected" "$work/logs/$backend-$name.log"
  done
 done
printf 'Public-only clients, rejection cases and Lambda/Cmm erasure checks passed. Logs: %s\n' "$work/logs"
