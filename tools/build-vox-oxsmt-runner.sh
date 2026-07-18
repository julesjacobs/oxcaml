#!/usr/bin/env bash

set -euo pipefail

die() {
  printf 'build-vox-oxsmt-runner: %s\n' "$*" >&2
  exit 1
}

repo_root=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
oxsmt_root=${OXSMT_ROOT:-/usr/local/home/jujacobs/oxsmt/main}
ocamlopt=${OCAMLOPT:-/home/jujacobs/.opam/5.4.0/bin/ocamlopt.opt}
source_file="$repo_root/tools/vox_oxsmt_runner.ml"
build_dir="$repo_root/_build/vox_oxsmt_runner"
output="$repo_root/_build/vox_oxsmt_runner.exe"
oxsmt_build="$oxsmt_root/_build/default"

case "$ocamlopt" in
  */*)
    test -x "$ocamlopt" \
      || die "OCaml compiler is not executable: $ocamlopt"
    ;;
  *) command -v "$ocamlopt" >/dev/null 2>&1 \
    || die "OCaml compiler is not on PATH: $ocamlopt" ;;
esac

test -f "$source_file" || die "missing runner source: $source_file"
test -d "$oxsmt_build" \
  || die "missing oxsmt build artifacts: $oxsmt_build"

include_dirs=(
  "$oxsmt_build/smt/core/.oxsmt_core.objs/byte"
  "$oxsmt_build/smt/solver/.oxsmt_solver.objs/byte"
  "$oxsmt_build/smt/preprocess/.oxsmt_preprocess.objs/byte"
  "$oxsmt_build/smt/theories/euf/.oxsmt_euf.objs/byte"
  "$oxsmt_build/smt/theories/lia/.oxsmt_lia.objs/byte"
  "$oxsmt_build/smt/theories/dt/.oxsmt_dt.objs/byte"
  "$oxsmt_build/smt/theories/arr/.oxsmt_arr.objs/byte"
  "$oxsmt_build/smt/combine/.oxsmt_combine.objs/byte"
  "$oxsmt_build/smt/ematch/.oxsmt_ematch.objs/byte"
  "$oxsmt_build/smt/bitblast/.oxsmt_bitblast.objs/byte"
  "$oxsmt_build/smt/interface/.oxsmt_interface.objs/byte"
  "$oxsmt_build/smt/lexical/.oxsmt_lexical.objs/byte"
  "$oxsmt_build/smt/smtlib/.oxsmt_smtlib.objs/byte"
  "$oxsmt_build/smt/smtlib/parser/.oxsmt_smtlib_parser.objs/byte"
)

archives=(
  "$oxsmt_build/smt/core/oxsmt_core.cmxa"
  "$oxsmt_build/smt/solver/oxsmt_solver.cmxa"
  "$oxsmt_build/smt/preprocess/oxsmt_preprocess.cmxa"
  "$oxsmt_build/smt/theories/euf/oxsmt_euf.cmxa"
  "$oxsmt_build/smt/theories/lia/oxsmt_lia.cmxa"
  "$oxsmt_build/smt/theories/dt/oxsmt_dt.cmxa"
  "$oxsmt_build/smt/theories/arr/oxsmt_arr.cmxa"
  "$oxsmt_build/smt/combine/oxsmt_combine.cmxa"
  "$oxsmt_build/smt/ematch/oxsmt_ematch.cmxa"
  "$oxsmt_build/smt/bitblast/oxsmt_bitblast.cmxa"
  "$oxsmt_build/smt/interface/oxsmt_interface.cmxa"
  "$oxsmt_build/smt/lexical/oxsmt_lexical.cmxa"
  "$oxsmt_build/smt/smtlib/oxsmt_smtlib.cmxa"
  "$oxsmt_build/smt/smtlib/parser/oxsmt_smtlib_parser.cmxa"
)

for directory in "${include_dirs[@]}"; do
  test -d "$directory" || die "missing oxsmt include directory: $directory"
done

for archive in "${archives[@]}"; do
  test -f "$archive" || die "missing oxsmt archive: $archive"
  test -f "${archive%.cmxa}.a" \
    || die "missing oxsmt native archive: ${archive%.cmxa}.a"
done

mkdir -p "$build_dir/tmp"
export TMPDIR="$build_dir/tmp"

include_args=()
for directory in "${include_dirs[@]}"; do
  include_args+=( -I "$directory" )
done

object="$build_dir/vox_oxsmt_runner.cmx"
"$ocamlopt" "${include_args[@]}" -c -o "$object" "$source_file"
"$ocamlopt" -o "$output.tmp" "${archives[@]}" "$object"
mv -f "$output.tmp" "$output"
printf '%s\n' "$output"
