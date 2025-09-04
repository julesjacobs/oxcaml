#!/usr/bin/env bash
set -euo pipefail

# Run ikind/jkind prints over all experiment files.
# Location: experiments/run_all.sh (top-level experiments directory)

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
ROOT_DIR=$(cd -- "${SCRIPT_DIR}/.." && pwd -P)
EXPDIR="${SCRIPT_DIR}"
OCAMLC_BIN="${ROOT_DIR}/_build/default/main_native.exe"

if [[ ! -x "${OCAMLC_BIN}" ]]; then
  echo "error: ${OCAMLC_BIN} not found or not executable." >&2
  echo "hint: run: RUNTIME_DIR=runtime dune build _build/default/main_native.exe" >&2
  exit 1
fi

echo "Using compiler: ${OCAMLC_BIN}"
echo "Experiment dir: ${EXPDIR}"

exit_code=0

run_one() {
  local f="$1"
  echo "--- ${f}"
  echo "+++ source: ${f}"
  echo ""
  if command -v bat >/dev/null 2>&1; then
    bat --style=plain --pager=never "${f}" || true
  else
    nl -ba "${f}" || true
  fi
  echo ""
  RUNTIME_DIR=runtime "${OCAMLC_BIN}" -I "${EXPDIR}" -c "${f}" || exit_code=$?
}

shopt -s nullglob

# Compile all .mli first to produce .cmi files
files_mli=("${EXPDIR}"/*.mli)
if (( ${#files_mli[@]} > 0 )); then
  for f in "${files_mli[@]}"; do run_one "${f}"; done
fi

# Then compile all .ml files (reusing .cmi where present)
files_ml=("${EXPDIR}"/*.ml)
if (( ${#files_ml[@]} > 0 )); then
  for f in "${files_ml[@]}"; do run_one "${f}"; done
fi

exit ${exit_code}
