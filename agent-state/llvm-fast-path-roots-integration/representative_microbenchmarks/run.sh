#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

export OCAMLOPT="${OCAMLOPT:-$ROOT/_install/bin/ocamlopt.opt}"
export OCAMLLIB="${OCAMLLIB:-$ROOT/_install/lib/ocaml}"
export LLVM_PATH="${LLVM_PATH:-}"
export PAIRS="${PAIRS:-3}"
export CASES="${CASES:-}"

if [[ -z "$LLVM_PATH" ]]; then
  echo "Set LLVM_PATH to the branch-local clang wrapper." >&2
  exit 2
fi

python3 "$SCRIPT_DIR/run.py"
