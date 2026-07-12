#!/usr/bin/env bash
# Solver-regression-suite oracle (board #162). Runs the cvc5 and z3 regression benchmarks
# through the SHIPPED solve path as an EDGE-CASE soundness oracle — NOT a completeness
# headline. It is deliberately SEPARATE from `make corpus-run` / the CORPUS_DIRS sweep: this
# never enters the headline denominator.
#
# WHY IT IS SOUND BY CONSTRUCTION. The verdict for a kept file comes from the exact same
# binary the headline uses — tests/corpus/corpus_classify.exe (the shipped Session path;
# guarded against the CLI by tests/corpus/driver_equiv_test). corpus_classify itself
# fail-closes: it degrades an incremental file to `unknown-incremental` and an
# unsupported/unparsable file to `parse-fail`, and it NEVER returns a definite sat/unsat it
# cannot stand behind. So this runner's own static filter (regress_scan.awk) only shapes the
# skip CENSUS and avoids wasting time on hopeless files — a mis-scan can mislabel a skip
# bucket but can never manufacture a false MISMATCH.
#
# FAIL-CLOSED FILTER (v1: non-incremental + supported logic only). A file is KEPT only when
# it is single-check, in a logic we ship (QF_UF / QF_LIA / QF_UFLIA), quantifier-free, and
# carries a definite sat/unsat expectation. Everything else is SKIPPED with a reason — the
# skip census is a deliverable (our feature-gap map). Expected status is read out-of-band:
# cvc5 from inline `; EXPECT: sat|unsat`, z3 from the paired `<name>.expected.out`.
#
# OUTCOME of a kept file vs its expected verdict:
#   agree-sat / agree-unsat   we and they agree (completeness win)
#   our-unknown               we said unknown (acceptable — a completeness gap, not a bug)
#   timeout                   we hit the per-file wall cap (acceptable — like unknown)
#   MISMATCH                  we said sat and they say unsat (or vice versa), OR our verdict
#                             contradicts the file's own (set-info :status). This is a
#                             potential SOUNDNESS bug or an upstream label quirk: the runner
#                             LISTS every one and EXITS NONZERO. (Triage is manual — a
#                             mismatch is never auto-filed.)
#
# Env knobs (Makefile sets them): CLASSIFY (the corpus_classify exe), REGRESS_TIMEOUT (s,
# wall, per file), REGRESS_JOBS, REGRESS_MAX_BYTES, REGRESS_MAX_EFFORT (optional counted
# cap), REGRESS_RAW, REGRESS_REPORT. Positional args: the suite roots to enumerate; the
# suite (cvc5|z3) is detected from each path.
set -uo pipefail

CLASSIFY="${CLASSIFY:?CLASSIFY must point at the corpus_classify exe}"
TIMEOUT="${REGRESS_TIMEOUT:-1}"
JOBS="${REGRESS_JOBS:-48}"
MAXBYTES="${REGRESS_MAX_BYTES:-20971520}"
MAXEFFORT="${REGRESS_MAX_EFFORT:-}"
RAW="${REGRESS_RAW:-../logs/regress-run.raw}"
REPORT="${REGRESS_REPORT:-../logs/regress-harness-report.md}"
SCAN="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/regress_scan.awk"

mkdir -p "$(dirname "$RAW")" "$(dirname "$REPORT")"

# Soft no-op when no suite is present (so `make test` stays green on a checkout without the
# corpora). A root that does not exist is skipped with a note, not an error.
present=0
for d in "$@"; do [ -d "$d" ] && present=1; done
if [ "$present" -eq 0 ]; then
  echo "regress-test: no regression suite dirs present (${*:-none}); skipping."
  exit 0
fi

# Per-file classification. Emits one TAB row to stdout:
#   <suite> <result> <expected> <ourtoken> <file>
# result is one of: agree-sat agree-unsat our-unknown timeout mismatch error
#   skip:incremental skip:no-logic skip:unsupported-logic skip:quantifier-logic
#   skip:quantifiers skip:expected-unknown skip:no-expected skip:unsupported-feature
#   skip:too-big
supported() { case "$1" in QF_UF | QF_LIA | QF_UFLIA) return 0 ;; *) return 1 ;; esac; }

classify_one() {
  local file="$1" suite="$2"
  local sz logic nchk pp q ci ne ev si exp z3_multi=0 result tok="-"

  sz=$(stat -c%s "$file" 2>/dev/null || echo 0)
  if [ "$sz" -gt "$MAXBYTES" ]; then
    printf '%s\tskip:too-big\t-\t-\t%s\n' "$suite" "$file"
    return
  fi

  # Static scan (single awk pass): logic, checks, push/pop, quantifiers, cvc5 EXPECT/CLI,
  # set-info status.
  IFS=$'\t' read -r logic nchk pp q ci ne ev si < <(awk -f "$SCAN" "$file" 2>/dev/null)
  logic="${logic:--}"; nchk="${nchk:-0}"; pp="${pp:-0}"; q="${q:-0}"
  ci="${ci:-0}"; ne="${ne:-0}"; ev="${ev:--}"; si="${si:--}"

  # Expected verdict, out-of-band per suite.
  case "$suite" in
    cvc5)
      exp="$ev"
      [ "$exp" = "-" ] && exp="$si"
      ;;
    z3)
      local outf raw norm
      outf="${file%.smt2}.expected.out"
      if [ -f "$outf" ]; then
        raw=$(cat "$outf" 2>/dev/null)
        norm=$(printf '%s' "$raw" | tr -d '[:space:]')
        case "$norm" in
          sat) exp=sat ;;
          unsat) exp=unsat ;;
          unknown) exp=unknown ;;
          *)
            # Multiple bare verdict lines => an incremental transcript; anything else
            # (models, goals, get-value output, echoes) => not a clean oracle.
            if [ "$(printf '%s\n' "$raw" | grep -cE '^(sat|unsat|unknown)$')" -gt 1 ]; then
              z3_multi=1
            fi
            exp=none
            ;;
        esac
      else
        exp="$si"
      fi
      ;;
    *) exp="$si" ;;
  esac
  [ -z "$exp" ] && exp="-"
  [ "$exp" = "-" ] && exp=none

  # Incremental? (any structural or out-of-band signal.)
  local incr=0
  [ "$nchk" -gt 1 ] && incr=1
  [ "$pp" = 1 ] && incr=1
  [ "$ci" = 1 ] && incr=1
  [ "$ne" -gt 1 ] && incr=1
  [ "$z3_multi" = 1 ] && incr=1

  # Fail-closed skip decision (first match wins).
  if [ "$incr" = 1 ]; then
    printf '%s\tskip:incremental\t%s\t-\t%s\n' "$suite" "$exp" "$file"; return
  fi
  if [ "$logic" = "-" ]; then
    printf '%s\tskip:no-logic\t%s\t-\t%s\n' "$suite" "$exp" "$file"; return
  fi
  if ! supported "$logic"; then
    case "$logic" in
      QF_*) printf '%s\tskip:unsupported-logic\t%s\t-\t%s\n' "$suite" "$exp" "$file" ;;
      *) printf '%s\tskip:quantifier-logic\t%s\t-\t%s\n' "$suite" "$exp" "$file" ;;
    esac
    return
  fi
  if [ "$q" = 1 ]; then
    printf '%s\tskip:quantifiers\t%s\t-\t%s\n' "$suite" "$exp" "$file"; return
  fi
  if [ "$exp" = unknown ]; then
    printf '%s\tskip:expected-unknown\t%s\t-\t%s\n' "$suite" "$exp" "$file"; return
  fi
  if [ "$exp" = none ]; then
    printf '%s\tskip:no-expected\t%s\t-\t%s\n' "$suite" "$exp" "$file"; return
  fi

  # KEEP: solve through the shipped path. SIGKILL at the wall cap (the solver loop ignores
  # SIGTERM; see the oxsmt-cli-timeout memo). A timeout only ever downgrades to unknown, so
  # the MISMATCH gate is machine-independent even though the agreement count is not.
  local out rc
  out=$(timeout -s KILL "$TIMEOUT" "$CLASSIFY" ${MAXEFFORT:+--max-effort "$MAXEFFORT"} "$file" 2>/dev/null)
  rc=$?
  if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
    result=timeout; tok=timeout
  elif [ "$rc" -ne 0 ] || [ -z "$out" ]; then
    result=error; tok=error
  else
    tok=$(printf '%s' "$out" | awk '{print $1}')
    case "$tok" in
      solved-sat) [ "$exp" = sat ] && result=agree-sat || result=mismatch ;;
      solved-unsat) [ "$exp" = unsat ] && result=agree-unsat || result=mismatch ;;
      mismatch) result=mismatch ;;
      unknown) result=our-unknown ;;
      parse-fail) result=skip:unsupported-feature ;;
      unknown-incremental) result=skip:incremental ;;
      *) result=error ;;
    esac
  fi
  printf '%s\t%s\t%s\t%s\t%s\n' "$suite" "$result" "$exp" "$tok" "$file"
}
export -f classify_one supported
export CLASSIFY TIMEOUT MAXBYTES MAXEFFORT SCAN

: >"$RAW"
start=$(date +%s.%N)
for d in "$@"; do
  [ -d "$d" ] || {
    echo "regress-run: no such dir $d (skipping)" >&2
    continue
  }
  case "$d" in
    *cvc5*) suite=cvc5 ;;
    *z3*) suite=z3 ;;
    *) suite=unknown ;;
  esac
  # Sorted enumeration for determinism (xargs runs in parallel, but the tallies and the
  # sorted mismatch list below are order-independent).
  find "$d" -name '*.smt2' -print0 \
    | sort -z \
    | xargs -0 -P "$JOBS" -I{} bash -c 'classify_one "$1" "$2"' _ {} "$suite" >>"$RAW"
done
end=$(date +%s.%N)
wall=$(awk "BEGIN{printf \"%.1f\", $end-$start}")
nfiles=$(wc -l <"$RAW")

# ---- Aggregation -----------------------------------------------------------------------
c() { awk -F'\t' -v r="$1" '$2==r' "$RAW" | wc -l; }
cs() { awk -F'\t' -v s="$1" -v r="$2" '$1==s && $2==r' "$RAW" | wc -l; }

kept=$(( $(c agree-sat) + $(c agree-unsat) + $(c our-unknown) + $(c timeout) + $(c mismatch) + $(c error) ))
agree=$(( $(c agree-sat) + $(c agree-unsat) ))
mismatch=$(c mismatch)

SKIP_REASONS="incremental no-logic unsupported-logic quantifier-logic quantifiers expected-unknown no-expected unsupported-feature too-big"

# ---- Report + digest -------------------------------------------------------------------
{
  echo "# Regression-suite oracle — first-run report (board #162)"
  echo
  echo "Generated by \`tests/regress/regress_run.sh\` at $(date -u +%Y-%m-%dT%H:%M:%SZ)."
  echo "Solve path: \`$CLASSIFY\` (the shipped Session path; same binary as the headline"
  echo "sweep). Per-file wall cap ${TIMEOUT}s (SIGKILL); ${JOBS} workers.${MAXEFFORT:+ counted cap --max-effort $MAXEFFORT.}"
  echo
  echo "## Totals"
  echo
  echo "| metric | count |"
  echo "|---|---:|"
  echo "| total .smt2 enumerated | $nfiles |"
  echo "| kept (solved through the shipped path) | $kept |"
  echo "| agreements (agree-sat + agree-unsat) | $agree |"
  echo "| &nbsp;&nbsp;agree-sat | $(c agree-sat) |"
  echo "| &nbsp;&nbsp;agree-unsat | $(c agree-unsat) |"
  echo "| our-unknown (completeness gap, acceptable) | $(c our-unknown) |"
  echo "| timeout (acceptable) | $(c timeout) |"
  echo "| error | $(c error) |"
  echo "| **MISMATCH (soundness / label quirk)** | **$mismatch** |"
  echo
  echo "## Skip census (the feature-gap map)"
  echo
  echo "| reason | total | cvc5 | z3 |"
  echo "|---|---:|---:|---:|"
  for r in $SKIP_REASONS; do
    printf '| %s | %s | %s | %s |\n' "$r" "$(c "skip:$r")" "$(cs cvc5 "skip:$r")" "$(cs z3 "skip:$r")"
  done
  echo
  echo "## Per-suite kept outcomes"
  echo
  echo "| suite | agree-sat | agree-unsat | our-unknown | timeout | error | MISMATCH |"
  echo "|---|---:|---:|---:|---:|---:|---:|"
  for s in cvc5 z3; do
    printf '| %s | %s | %s | %s | %s | %s | %s |\n' "$s" \
      "$(cs "$s" agree-sat)" "$(cs "$s" agree-unsat)" "$(cs "$s" our-unknown)" \
      "$(cs "$s" timeout)" "$(cs "$s" error)" "$(cs "$s" mismatch)"
  done
  echo
  echo "Runtime: ${wall}s."
  echo
  if [ "$mismatch" -gt 0 ]; then
    echo "## MISMATCHES (manual triage — potential soundness bug OR upstream label quirk)"
    echo
    echo "| our verdict | expected | file |"
    echo "|---|---|---|"
    awk -F'\t' '$2=="mismatch"{print "| "$4" | "$3" | "$5" |"}' "$RAW" | sort
    echo
  else
    echo "## MISMATCHES"
    echo
    echo "None. No kept file's definite verdict contradicts its expected status."
    echo
  fi
} >"$REPORT"

# Digest to stdout (mirrors corpus_run.sh's shape).
echo "regress-run | ${TIMEOUT}s/file | $JOBS workers | ${wall}s"
printf "%-30s %7s\n" "metric" "count"
printf "%-30s %7s\n" "total .smt2" "$nfiles"
printf "%-30s %7s\n" "kept" "$kept"
printf "%-30s %7s\n" "  agree-sat" "$(c agree-sat)"
printf "%-30s %7s\n" "  agree-unsat" "$(c agree-unsat)"
printf "%-30s %7s\n" "  our-unknown" "$(c our-unknown)"
printf "%-30s %7s\n" "  timeout" "$(c timeout)"
printf "%-30s %7s\n" "  error" "$(c error)"
printf "%-30s %7s\n" "  MISMATCH" "$mismatch"
echo "skip census:"
for r in $SKIP_REASONS; do printf "  %-22s %7s\n" "$r" "$(c "skip:$r")"; done
echo "report: $REPORT"
echo "raw:    $RAW"

if [ "$mismatch" -gt 0 ]; then
  echo ""
  echo "CRITICAL: $mismatch MISMATCH(ES) — verdict contradicts expected status (triage):"
  awk -F'\t' '$2=="mismatch"{print "  our="$4" exp="$3" "$5}' "$RAW" | sort | head -40
  exit 1
fi
echo "soundness: 0 mismatches"
