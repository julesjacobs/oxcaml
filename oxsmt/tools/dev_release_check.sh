#!/usr/bin/env bash
# Dev-vs-release output-equality gate (board #69; codex AP4 / budget-reviewer H1).
#
# The `release` profile turns OFF OCaml assertions AND the EUF debug self-check oracle
# (corpus_classify forces the latter). That must change only PERFORMANCE, never an
# observable result. This builds corpus_classify both ways and asserts BYTE-IDENTICAL
# "<verdict> <effort>" on every sample file — so the soundness-neutrality (and the
# effort-determinism) of the release config is a REPEATABLE in-repo check, not the one-off
# spot-check from review. Nonzero exit on any divergence.
#
# Sample: $DEV_RELEASE_DIRS (default tests/cases — always in-repo, and the round trip is
# cheap). Point it at a corpus subset (e.g. ../corpora/QF_UF) for a wider sweep.
set -uo pipefail

DUNE="${DUNE:-dune}"
DIRS="${DEV_RELEASE_DIRS:-tests/cases}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "dev-release-check: building dev + release classifiers…"
$DUNE build tests/corpus/corpus_classify.exe
cp _build/default/tests/corpus/corpus_classify.exe "$TMP/cc_dev"
$DUNE build --profile release tests/corpus/corpus_classify.exe
cp _build/default/tests/corpus/corpus_classify.exe "$TMP/cc_rel"
# Restore the dev build for the rest of the dev loop.
$DUNE build tests/corpus/corpus_classify.exe

dev_stamp=$("$TMP/cc_dev" --stamp)
rel_stamp=$("$TMP/cc_rel" --stamp)
echo "  dev stamp:     $dev_stamp"
echo "  release stamp: $rel_stamp"
# Guard against a vacuous check: the two profiles MUST be distinguishable (assertions on vs
# off), else the release profile did not apply and equality would be trivially true.
dev_a=$(printf '%s' "$dev_stamp" | sed -n 's/.*assertions=\([a-z]*\).*/\1/p')
rel_a=$(printf '%s' "$rel_stamp" | sed -n 's/.*assertions=\([a-z]*\).*/\1/p')
if [ "$dev_a" != "on" ] || [ "$rel_a" != "off" ]; then
  echo "dev-release-check: FAIL — profiles not distinct (dev assertions=$dev_a, release=$rel_a);" >&2
  echo "  the release -noassert profile did not apply, so the check would be vacuous." >&2
  exit 1
fi

n=0
diffs=0
while IFS= read -r -d '' f; do
  n=$((n + 1))
  d=$("$TMP/cc_dev" "$f" 2>/dev/null)
  r=$("$TMP/cc_rel" "$f" 2>/dev/null)
  if [ "$d" != "$r" ]; then
    diffs=$((diffs + 1))
    echo "  DIFF $f : dev=[$d] release=[$r]" >&2
  fi
done < <(for dir in $DIRS; do find "$dir" -name '*.smt2' -print0 2>/dev/null; done)

echo "dev-release-check: $n files, $diffs divergence(s)"
if [ "$diffs" -ne 0 ]; then
  echo "dev-release-check: FAIL — release config changed an observable result (must be perf-only)" >&2
  exit 1
fi
echo "dev-release-check: OK — dev and release produce byte-identical verdict+effort on $n files"
