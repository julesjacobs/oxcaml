#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=$(cd "${1:?Usage: check_sat_public.sh COMPILER_PREFIX}" && pwd)
if [[ -n "${2:-}" ]]; then
  mkdir -p "$2"
  public_dir=$(cd "$2" && pwd)
else
  public_dir=$(mktemp -d "${TMPDIR:-/tmp}/vox-sat-public.XXXXXX")
  trap 'rm -rf "$public_dir"' EXIT
fi
for module in vox_sat_spec vox_sat vox_cdcl vox_cdcl_total; do
  cp "$prefix/lib/ocaml/vox/$module.cmi" "$public_dir/"
done
cp "$root/verification/clients/sat_public.ml" "$public_dir/"
cd "$public_dir"
flags=(-nostdlib -I "$prefix/lib/ocaml" -I . -extension refinement_types)
"$prefix/bin/ocamlc" "${flags[@]}" -c sat_public.ml
"$prefix/bin/ocamlc" "${flags[@]}" \
  "$prefix/lib/ocaml/vox/vox_borrow.cma" sat_public.cmo -o sat_public.byte
"$prefix/bin/ocamlrun" sat_public.byte
"$prefix/bin/ocamlopt" "${flags[@]}" -w -58 -opaque -c sat_public.ml
"$prefix/bin/ocamlopt" "${flags[@]}" -w -58 \
  "$prefix/lib/ocaml/vox/vox_borrow.cmxa" sat_public.cmx -o sat_public.native
./sat_public.native
printf 'Public-only SAT client passed (bytecode and native).\n'
