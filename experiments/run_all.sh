#!/usr/bin/env bash
set -euo pipefail

# Run ikind/jkind prints over all experiment files.
# Default behavior: only show mismatches and a count of matches.
# Options:
#   --show-all       Show all checks and full file listings
#   --show-source    Always print source listings
#   --show-matches   Print matching checks as well (ik/jk=T/T)

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
ROOT_DIR=$(cd -- "${SCRIPT_DIR}/.." && pwd -P)
EXPDIR="${SCRIPT_DIR}"
OCAMLC_BIN="${ROOT_DIR}/_build/default/main_native.exe"

if [[ ! -x "${OCAMLC_BIN}" ]]; then
  echo "error: ${OCAMLC_BIN} not found or not executable." >&2
  echo "hint: run: RUNTIME_DIR=runtime dune build _build/default/main_native.exe" >&2
  exit 1
fi

MISMATCH_ONLY=1
SHOW_SOURCE=0
SHOW_MATCHES=0
SHOW_ERRORS=0

for arg in "$@"; do
  case "$arg" in
    --show-all)
      MISMATCH_ONLY=0; SHOW_SOURCE=1; SHOW_MATCHES=1 ;;
    --show-source)
      SHOW_SOURCE=1 ;;
    --show-matches)
      SHOW_MATCHES=1 ;;
    --show-errors)
      SHOW_ERRORS=1 ;;
    *)
      echo "Unknown option: $arg" >&2; exit 2 ;;
  esac
done

echo "Using compiler: ${OCAMLC_BIN}"
echo "Experiment dir: ${EXPDIR}"

exit_code=0
total_matches=0
total_mismatches=0

print_header_and_source() {
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
}

run_one() {
  local f="$1"
  # Compile and capture output
  local out
  if ! out=$(RUNTIME_DIR=runtime "${OCAMLC_BIN}" -I "${EXPDIR}" -c "${f}" 2>&1); then
    exit_code=$?
    # Even on compile errors, honor trimming and printing mode
    :
  fi

  # Optionally trim compiler error blocks (suppress huge error output)
  local trimmed
  if (( SHOW_ERRORS == 0 )); then
    trimmed=$(awk 'BEGIN{stop=0} /^File |^Error:/{stop=1} !stop{print}' <<< "$out")
  else
    trimmed="$out"
  fi

  # Count matches and mismatches
  local file_matches file_total_mismatches
  file_matches=$(printf "%s" "$trimmed" | rg "ik/jk=T/T" -c || true)
  total_matches=$(( total_matches + file_matches ))
  file_total_mismatches=$(printf "%s" "$trimmed" | rg "ik/jk=" | rg -v "ik/jk=T/T" -c || true)
  total_mismatches=$(( total_mismatches + file_total_mismatches ))

  if (( MISMATCH_ONLY == 1 )); then
    # Only print mismatches (and optional source if mismatches present)
    if (( file_total_mismatches > 0 )); then
      if (( SHOW_SOURCE == 1 )); then
        print_header_and_source "${f}"
      else
        echo "--- ${f}"
      fi
      # Print all lines except exact matches, to keep relevant context
      printf "%s\n" "$trimmed" | rg -v "ik/jk=T/T" || true
      if (( SHOW_ERRORS == 0 && exit_code != 0 )); then
        echo "(compile error output suppressed; pass --show-errors to see full error)"
      fi
    fi
  else
    # Show all
    print_header_and_source "${f}"
    if (( SHOW_MATCHES == 1 )); then
      printf "%s\n" "$trimmed"
    else
      printf "%s\n" "$trimmed" | rg -v "ik/jk=T/T" || true
    fi
  fi
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

echo ""
echo "Matches: ${total_matches}"
echo "Mismatches: ${total_mismatches}"

exit ${exit_code}
