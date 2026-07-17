#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

if command -v pdflatex >/dev/null 2>&1; then
  pdflatex -interaction=nonstopmode -halt-on-error -jobname=paper main.tex
  pdflatex -interaction=nonstopmode -halt-on-error -jobname=paper main.tex
elif command -v tectonic >/dev/null 2>&1; then
  tectonic --outdir . main.tex
  mv -f main.pdf paper.pdf
else
  echo "Neither pdflatex nor tectonic is available." >&2
  exit 1
fi

test -s paper.pdf
