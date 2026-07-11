#!/usr/bin/env bash
# Parallel, label-checked full-corpus sweep (board #124).
#
# The solver is single-threaded and deterministic, so the corpus is embarrassingly
# parallel: fan out over `xargs -P` with one corpus_classify PROCESS per file under a hard
# `timeout` (SIGKILL). Per-process isolation means no shared state across files — this is
# what makes a tight timeout safe and deterministic (no shared intern table / SIGALRM
# mid-intern). The run's payload is ZERO soundness mismatches — a definite verdict
# contradicting a file's :status is CRITICAL and makes this exit nonzero; unknown-vs-label
# is the expected v1 completeness gap.
#
# Output is JSON in the committed baseline schema (oxsmt-corpus-baseline/v1) written to
# ../logs — NEVER to tests/corpus/baseline_summary.json (that committed snapshot is a
# deliberate manual copy+commit; see tests/README.md). Digest to stdout; per-file detail
# to $CORPUS_RAW.
#
# Env knobs (Makefile sets them): CLASSIFY (the corpus_classify exe), CORPUS_TIMEOUT (s),
# CORPUS_JOBS, CORPUS_MAX_BYTES, CORPUS_JSON, CORPUS_RAW. Positional args: the logic dirs.
set -uo pipefail

CLASSIFY="${CLASSIFY:?CLASSIFY must point at the corpus_classify exe}"
TIMEOUT="${CORPUS_TIMEOUT:-2}"
JOBS="${CORPUS_JOBS:-48}"
MAXBYTES="${CORPUS_MAX_BYTES:-20971520}"
JSON="${CORPUS_JSON:-../logs/corpus-run.json}"
RAW="${CORPUS_RAW:-../logs/corpus-run.raw}"
TRUNK="$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
mkdir -p "$(dirname "$JSON")" "$(dirname "$RAW")"

# Per-file: emit "<logic> <outcome> <file>". Outcome is a corpus_classify token
# (solved-sat|solved-unsat|unknown|unknown-incremental|parse-fail|mismatch) or, added
# here, skip-too-big / timeout / error.
classify_one() {
  local file="$1" logic="$2" tok rc sz
  sz=$(stat -c%s "$file" 2>/dev/null || echo 0)
  if [ "$sz" -gt "$MAXBYTES" ]; then
    echo "$logic skip-too-big $file"
    return
  fi
  tok=$(timeout -k 1 "${TIMEOUT}s" "$CLASSIFY" "$file" 2>/dev/null)
  rc=$?
  if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
    tok=timeout
  elif [ "$rc" -ne 0 ] || [ -z "$tok" ]; then
    tok=error
  fi
  echo "$logic $tok $file"
}
export -f classify_one
export CLASSIFY TIMEOUT MAXBYTES

: >"$RAW"
start=$(date +%s.%N)
declare -A avail
for d in "$@"; do
  [ -d "$d" ] || {
    echo "corpus-run: no such dir $d (skipping)" >&2
    continue
  }
  logic=$(basename "$d")
  avail[$logic]=$(find "$d" -name '*.smt2' | wc -l)
  find "$d" -name '*.smt2' -print0 \
    | xargs -0 -P "$JOBS" -I{} bash -c 'classify_one "$1" "$2"' _ {} "$logic" >>"$RAW"
done
end=$(date +%s.%N)
wall=$(awk "BEGIN{printf \"%.1f\", $end-$start}")
nfiles=$(wc -l <"$RAW")
fps=$(awk "BEGIN{ if ($wall>0) printf \"%.1f\", $nfiles/$wall; else print 0 }")

OUTCOMES="solved-sat solved-unsat unknown unknown-incremental parse-fail timeout skip-too-big error"
logics=$(awk '{print $1}' "$RAW" | sort -u)
c() { awk -v l="$1" -v o="$2" '$1==l && $2==o' "$RAW" | wc -l; }
total_mismatch=$(awk '$2=="mismatch"' "$RAW" | wc -l)

# JSON in the committed baseline schema (oxsmt-corpus-baseline/v1) + run metadata.
{
  echo "{"
  echo "  \"schema\": \"oxsmt-corpus-baseline/v1\","
  echo "  \"trunk\": \"$TRUNK\","
  echo "  \"timeout_s\": $TIMEOUT, \"workers\": $JOBS, \"wall_s\": $wall, \"files_per_s\": $fps,"
  echo "  \"logics\": {"
  first=1
  for l in $logics; do
    [ $first -eq 1 ] || echo ","
    first=0
    tot=0
    fields=""
    for o in $OUTCOMES; do
      n=$(c "$l" "$o")
      tot=$((tot + n))
      fields="$fields \"$o\": $n,"
    done
    mm=$(c "$l" mismatch)
    tot=$((tot + mm))
    printf "    %s: { \"total_available\": %s, \"scanned\": %d, \"outcomes\": {%s }, \"mismatches\": %d }" \
      "\"$l\"" "${avail[$l]:-$tot}" "$tot" "${fields%,}" "$mm"
  done
  echo ""
  echo "  },"
  echo "  \"mismatch_count\": $total_mismatch"
  echo "}"
} >"$JSON"

# Digest to stdout.
echo "corpus-run @ $TRUNK | timeout ${TIMEOUT}s | $JOBS workers | ${wall}s | ${fps} files/s"
printf "%-10s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n" \
  logic scanned solv-sat solv-uns unknown incr parsefail timeout toobig MISMATCH
emit_row() {
  local l="$1"
  printf "%-10s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n" \
    "$l" "$(awk -v l="$l" '$1==l' "$RAW" | wc -l)" \
    "$(c "$l" solved-sat)" "$(c "$l" solved-unsat)" "$(c "$l" unknown)" \
    "$(c "$l" unknown-incremental)" "$(c "$l" parse-fail)" "$(c "$l" timeout)" \
    "$(c "$l" skip-too-big)" "$(c "$l" mismatch)"
}
for l in $logics; do emit_row "$l"; done
printf "%-10s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n" \
  TOTAL "$nfiles" \
  "$(awk '$2=="solved-sat"' "$RAW" | wc -l)" "$(awk '$2=="solved-unsat"' "$RAW" | wc -l)" \
  "$(awk '$2=="unknown"' "$RAW" | wc -l)" "$(awk '$2=="unknown-incremental"' "$RAW" | wc -l)" \
  "$(awk '$2=="parse-fail"' "$RAW" | wc -l)" "$(awk '$2=="timeout"' "$RAW" | wc -l)" \
  "$(awk '$2=="skip-too-big"' "$RAW" | wc -l)" "$total_mismatch"
echo "json: $JSON"
echo "raw:  $RAW"

if [ "$total_mismatch" -gt 0 ]; then
  echo ""
  echo "CRITICAL: $total_mismatch SOUNDNESS MISMATCH(ES) — verdict contradicts label:"
  awk '$2=="mismatch"{print "  "$3}' "$RAW" | head -20
  exit 1
fi
echo "soundness: 0 mismatches"
