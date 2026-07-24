#!/usr/bin/env bash

set -euo pipefail

usage() {
  echo "usage: $0 verify SOURCE_PREFIX REPOSITORY MANIFEST" >&2
  echo "       $0 publish SOURCE_PREFIX REPOSITORY DESTINATION" >&2
  exit 2
}

fail() {
  echo "vox compiler release check: $*" >&2
  exit 1
}

rename_noreplace() {
  python3 - "$1" "$2" <<'PY'
import ctypes
import errno
import os
import sys

at_fdcwd = -100
rename_noreplace = 1
libc = ctypes.CDLL(None, use_errno=True)
renameat2 = libc.renameat2
renameat2.argtypes = [
    ctypes.c_int,
    ctypes.c_char_p,
    ctypes.c_int,
    ctypes.c_char_p,
    ctypes.c_uint,
]
renameat2.restype = ctypes.c_int
result = renameat2(
    at_fdcwd,
    os.fsencode(sys.argv[1]),
    at_fdcwd,
    os.fsencode(sys.argv[2]),
    rename_noreplace,
)
if result != 0:
    error = ctypes.get_errno()
    if error == errno.EEXIST:
        sys.exit(3)
    raise OSError(error, os.strerror(error), sys.argv[2])
PY
}

[[ $# -eq 4 ]] || usage

mode=$1
source_prefix=$2
repository=$3
target=$4

[[ $mode == verify || $mode == publish ]] || usage
[[ -d $source_prefix/bin ]] || fail "missing bin directory in $source_prefix"
git -C "$repository" rev-parse --is-inside-work-tree > /dev/null 2>&1 \
  || fail "$repository is not a git worktree"
source_prefix=$(cd "$source_prefix" && pwd -P)
repository=$(cd "$repository" && pwd -P)
if [[ $target != /* ]]; then
  target="$PWD/$target"
fi
: "${TMPDIR:?set TMPDIR to a writable scratch directory}"
[[ -d $TMPDIR && -w $TMPDIR ]] || fail "TMPDIR is not writable: $TMPDIR"
: "${VOX_SMT_SOLVER:?set VOX_SMT_SOLVER to the configured solver command}"

commit=$(git -C "$repository" rev-parse HEAD)
status=$(git -C "$repository" status --porcelain --untracked-files=all)
if [[ -n $status ]]; then
  dirty=true
else
  dirty=false
fi
if [[ $mode == publish && $dirty == true ]]; then
  fail "publish requires a clean source worktree"
fi
tracked_diff_hash=$(
  git -C "$repository" diff --binary HEAD | sha256sum | awk '{print $1}'
)
status_hash=$(printf '%s' "$status" | sha256sum | awk '{print $1}')
[[ -f $repository/Makefile.config ]] || fail "missing build configuration"
build_config_hash=$(
  sha256sum "$repository/Makefile.config" | awk '{print $1}'
)

ignored_input_paths_for() {
  local output=$1
  local input
  while IFS= read -r -d '' input; do
    case $input in
      _build/*|_install/*|_runtest/*|_compare/*|_opam/*)
        continue
        ;;
    esac
    [[ $input != *$'\n'* ]] || fail "ignored input path contains a newline"
    printf '%s\n' "$input"
  done < <(
    git -C "$repository" ls-files -z --others --ignored --exclude-standard
  ) | LC_ALL=C sort -u > "$output"
  [[ -s $output ]] || fail "source tree has no ignored build inputs"
}

ignored_input_entries_for() {
  local paths=$1
  local output=$2
  local input path hash link_target
  {
    while IFS= read -r input; do
      [[ -n $input && $input != /* && $input != ../* \
         && $input != */../* ]] \
        || fail "invalid ignored input path: $input"
      path="$repository/$input"
      if [[ -f $path && ! -L $path ]]; then
        hash=$(sha256sum "$path" | awk '{print $1}')
        echo "file_sha256.$input=$hash"
      elif [[ -L $path ]]; then
        link_target=$(readlink "$path")
        echo "symlink_target.$input=$link_target"
      elif [[ ! -e $path ]]; then
        echo "absent.$input=true"
      else
        fail "unsupported ignored input: $input"
      fi
    done < "$paths"
  } | LC_ALL=C sort > "$output"
}

assert_repository_unchanged() {
  local current_build_config_hash current_commit current_status
  local current_tracked_diff_hash
  current_commit=$(git -C "$repository" rev-parse HEAD)
  current_status=$(
    git -C "$repository" status --porcelain --untracked-files=all
  )
  current_tracked_diff_hash=$(
    git -C "$repository" diff --binary HEAD | sha256sum | awk '{print $1}'
  )
  current_build_config_hash=$(
    sha256sum "$repository/Makefile.config" | awk '{print $1}'
  )
  [[ $current_commit == "$commit" \
     && $current_status == "$status" \
     && $current_tracked_diff_hash == "$tracked_diff_hash" \
     && $current_build_config_hash == "$build_config_hash" ]] \
    || fail "source repository changed while constructing the release"
  ignored_input_paths_for "$scratch/ignored-input-paths.current"
  cmp -s "$scratch/ignored-input-paths.initial" \
    "$scratch/ignored-input-paths.current" \
    || fail "ignored input set changed while constructing the release"
  ignored_input_entries_for "$scratch/ignored-input-paths.current" \
    "$scratch/ignored-inputs.current"
  cmp -s "$scratch/ignored-inputs.initial" \
    "$scratch/ignored-inputs.current" \
    || fail "ignored build inputs changed while constructing the release"
}

scratch=$(mktemp -d "$TMPDIR/vox-compiler-release.XXXXXX")
staging=
manifest_staging=
ignored_input_paths_for "$scratch/ignored-input-paths.initial"
ignored_input_entries_for "$scratch/ignored-input-paths.initial" \
  "$scratch/ignored-inputs.initial"
ignored_input_hash=$(
  sha256sum "$scratch/ignored-inputs.initial" | awk '{print $1}'
)
cleanup() {
  if [[ -n $manifest_staging \
        && ( -e $manifest_staging || -L $manifest_staging ) ]]; then
    rm -f -- "$manifest_staging"
  fi
  rm -rf "$scratch"
  if [[ -n $staging && -d $staging ]]; then
    chmod -R u+w "$staging" 2>/dev/null || true
    rm -rf "$staging"
  fi
}
trap cleanup EXIT

artifact_entries_for() {
  local prefix=$1
  local output=$2
  {
    shopt -s dotglob globstar nullglob
    for published_file in "$prefix"/**; do
      relative=${published_file#"$prefix"/}
      if [[ $relative == vox-compiler-release.manifest ]]; then
        continue
      fi
      if [[ -f $published_file && ! -L $published_file ]]; then
        hash=$(sha256sum "$published_file" | awk '{print $1}')
        echo "file_sha256.$relative=$hash"
      elif [[ -L $published_file ]]; then
        link_target=$(readlink "$published_file")
        echo "symlink_target.$relative=$link_target"
      fi
    done
  } | LC_ALL=C sort > "$output"
}

if [[ $mode == publish ]]; then
  [[ ! -e $target && ! -L $target ]] \
    || fail "destination already exists: $target"
  target_parent=$(dirname "$target")
  target_base=$(basename "$target")
  mkdir -p "$target_parent"
  staging=$(mktemp -d "$target_parent/.${target_base}.staging.XXXXXX")
  cp -a "$source_prefix"/. "$staging"/
  checked_prefix=$staging
  manifest="$staging/vox-compiler-release.manifest"
else
  checked_prefix="$scratch/checked-prefix"
  mkdir "$checked_prefix"
  cp -a "$source_prefix"/. "$checked_prefix"/
  manifest=$target
fi

# From here on, validate and probe only the private snapshot.  In publish mode
# it is on the destination filesystem so that the final rename is atomic.
artifact_entries_for "$checked_prefix" "$scratch/snapshot-entries"
make -s -C "$repository" _install
canonical_prefix="$repository/_install"
[[ -d $canonical_prefix/bin ]] || fail "current build did not produce _install"
artifact_entries_for "$canonical_prefix" "$scratch/canonical-entries"
if ! cmp -s "$scratch/canonical-entries" "$scratch/snapshot-entries"; then
  fail "release snapshot does not match the current build's complete install"
fi

ocamlc="$checked_prefix/bin/ocamlc.opt"
ocamlopt="$checked_prefix/bin/ocamlopt.opt"
[[ -x $ocamlc ]] || fail "missing executable ocamlc.opt"
[[ -x $ocamlopt ]] || fail "missing executable ocamlopt.opt"

symbols_for() {
  nm -a --defined-only "$1" \
    | awk '$3 ~ /^camlVox_/ {print $3}' \
    | LC_ALL=C sort -u
}

symbols_for "$ocamlc" > "$scratch/ocamlc.symbols"
symbols_for "$ocamlopt" > "$scratch/ocamlopt.symbols"
[[ -s $scratch/ocamlc.symbols ]] || fail "ocamlc.opt has no Vox symbols"
if ! cmp -s "$scratch/ocamlc.symbols" "$scratch/ocamlopt.symbols"; then
  fail "ocamlc.opt and ocamlopt.opt have different Vox symbol fingerprints"
fi

build_ocamlc="$repository/_build/main/main_native.exe"
build_ocamlopt="$repository/_build/main/oxcaml_main_native.exe"
[[ -f $build_ocamlc ]] || fail "missing current ocamlc build artifact"
[[ -f $build_ocamlopt ]] || fail "missing current ocamlopt build artifact"
cmp -s "$build_ocamlc" "$ocamlc" \
  || fail "ocamlc.opt does not match the current build artifact"
cmp -s "$build_ocamlopt" "$ocamlopt" \
  || fail "ocamlopt.opt does not match the current build artifact"

solver=$VOX_SMT_SOLVER

run_probes() {
  local compiler=$1
  local name=$2
  local directory="$scratch/$name"
  mkdir -p "$directory"
  printf '%s\n' \
    'let requires_one (value : int{ _ = 1 }) = value' \
    'let _ = requires_one 1' \
    > "$directory/positive.ml"
  printf '%s\n' \
    'let requires_one (value : int{ _ = 1 }) = value' \
    'let _ = requires_one 2' \
    > "$directory/negative.ml"
  if ! (
    cd "$directory"
    env -u CAMLLIB -u CAML_LD_LIBRARY_PATH \
      OCAMLLIB="$checked_prefix/lib/ocaml" \
      "$compiler" \
      -extension-universe alpha \
      -vox-backend z3 \
      -vox-smt-solver "$solver" \
      -c positive.ml
  ) > "$scratch/$name-positive.output" 2>&1; then
    sed -n '1,160p' "$scratch/$name-positive.output" >&2
    fail "$name positive refinement probe failed"
  fi

  set +e
  (
    cd "$directory"
    env -u CAMLLIB -u CAML_LD_LIBRARY_PATH \
      OCAMLLIB="$checked_prefix/lib/ocaml" \
      "$compiler" \
      -extension-universe alpha \
      -vox-backend z3 \
      -vox-smt-solver "$solver" \
      -c negative.ml
  ) > "$scratch/$name-negative.output" 2>&1
  local status=$?
  set -e
  if [[ $status -ne 2 ]]; then
    sed -n '1,160p' "$scratch/$name-negative.output" >&2
    fail "$name negative probe exited $status instead of 2"
  fi
  if ! grep -Fq "Refinement verification failed (disproved)" \
      "$scratch/$name-negative.output"; then
    sed -n '1,160p' "$scratch/$name-negative.output" >&2
    fail "$name negative probe did not report the expected failed precondition"
  fi
}

run_probes "$ocamlc" ocamlc
run_probes "$ocamlopt" ocamlopt
assert_repository_unchanged

manifest_tmp="$scratch/manifest"
symbol_hash=$(sha256sum "$scratch/ocamlc.symbols" | awk '{print $1}')
artifact_entries="$scratch/artifact-entries"
artifact_entries_for "$checked_prefix" "$artifact_entries"
if ! cmp -s "$scratch/canonical-entries" "$artifact_entries"; then
  fail "release snapshot changed while running compiler probes"
fi
artifact_hash=$(sha256sum "$artifact_entries" | awk '{print $1}')

{
  echo "artifact_aggregate_scheme=sha256-of-sorted-artifact-entry-lines"
  echo "artifact_aggregate_sha256=$artifact_hash"
  echo "build_freshness=private-snapshot-matches-current-rebuild"
  echo "canonical_install_content_match=true"
  echo "ignored_input_aggregate_scheme=sha256-of-sorted-entry-lines"
  echo "ignored_input_aggregate_sha256=$ignored_input_hash"
  echo "ignored_input_excluded_roots=_build,_install,_runtest,_compare,_opam"
  echo "format=vox-compiler-release-v1"
  echo "manifest_self=excluded"
  echo "source_commit=$commit"
  echo "source_dirty=$dirty"
  echo "source_status_sha256=$status_hash"
  echo "source_tracked_diff_sha256=$tracked_diff_hash"
  echo "source_nonignored_untracked_contents=not-hashed"
  echo "build_config_sha256=$build_config_hash"
  echo "vox_symbol_sha256=$symbol_hash"
  echo "vox_symbol_scope=defined-symbol-names-matching-^camlVox_"
  echo "vox_symbol_limit=names-only-not-code-bytes"
  cat "$artifact_entries"
} | LC_ALL=C sort > "$manifest_tmp"

mkdir -p "$(dirname "$manifest")"
manifest_staging=$(mktemp "${manifest}.tmp.XXXXXX")
cp "$manifest_tmp" "$manifest_staging"
mv -T "$manifest_staging" "$manifest"
manifest_staging=

if [[ $mode == publish ]]; then
  chmod -R a-w "$staging"
  chmod 0555 "$staging"
  chmod 0444 "$manifest"
  artifact_entries_for "$checked_prefix" "$scratch/prepublish-entries"
  if ! cmp -s "$scratch/canonical-entries" "$scratch/prepublish-entries"; then
    fail "release snapshot changed before publication"
  fi
  cmp -s "$manifest_tmp" "$manifest" \
    || fail "release manifest changed before publication"
  assert_repository_unchanged
  set +e
  rename_noreplace "$staging" "$target"
  rename_status=$?
  set -e
  if [[ $rename_status -eq 3 ]]; then
    fail "destination appeared while publishing: $target"
  elif [[ $rename_status -ne 0 ]]; then
    fail "atomic publication rename failed with status $rename_status"
  elif [[ -e $staging || -L $staging || ! -d $target ]]; then
    fail "atomic publication rename did not move the release"
  fi
  staging=
  manifest="$target/vox-compiler-release.manifest"
fi

echo "vox compiler release check: passed"
echo "manifest: $manifest"
