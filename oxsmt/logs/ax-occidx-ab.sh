#!/bin/bash
# Wall A/B for W5 Lever A: OFF vs OXSMT_AX_OCCIDX=1. Net solves at the headline wall.
# Usage: ax-occidx-ab.sh <dir> [wall] [max]
CLI=/usr/local/home/jujacobs/oxsmt/worktrees/adapter-incr/_build/default/tests/solver/oxsmt_cli.exe
DIR="$1"; WALL="${2:-2}"; MAX="${3:-100000}"
verdict() { grep -oE '\(verdict [a-z]*\)' | head -1 | sed 's/(verdict //;s/)//'; }
now() { date +%s%3N; }
off_s=0; on_s=0; n=0; disagree=0; off_ms=0; on_ms=0
for f in $(ls "$DIR"/*.smt2 2>/dev/null | head -n "$MAX"); do
  n=$((n+1))
  t0=$(now); ov=$(timeout -s KILL "$WALL" "$CLI" "$f" 2>/dev/null | verdict); t1=$(now); off_ms=$((off_ms+t1-t0))
  t0=$(now); av=$(OXSMT_AX_OCCIDX=1 timeout -s KILL "$WALL" "$CLI" "$f" 2>/dev/null | verdict); t1=$(now); on_ms=$((on_ms+t1-t0))
  [ -z "$ov" ] && ov=to; [ -z "$av" ] && av=to
  case "$ov" in sat|unsat) off_s=$((off_s+1));; esac
  case "$av" in sat|unsat) on_s=$((on_s+1));; esac
  if { [ "$ov" = sat ] && [ "$av" = unsat ]; } || { [ "$ov" = unsat ] && [ "$av" = sat ]; }; then
    disagree=$((disagree+1)); echo "DISAGREE $(basename "$f") off=$ov on=$av"; fi
done
echo "$DIR  files=$n wall=${WALL}s  solved OFF=$off_s ON=$on_s  delta=$((on_s-off_s))  disagree=$disagree  ms OFF=$off_ms ON=$on_ms"
