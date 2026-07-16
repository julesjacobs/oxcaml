#!/bin/bash
# Fix #5 (OXSMT_FOREST_BALANCE) per-family ON-vs-OFF. NOT byte-identical: counters differ by
# design. GATE: 0 both-solved verdict FLIPS per family (reroot direction is verdict-neutral).
# Also reports counter-diff count (informational: proves the lever is actually active).
CLI=/usr/local/home/jujacobs/oxsmt/worktrees/theory-perf/_build/default/tests/solver/oxsmt_cli.exe
OUT=/usr/local/home/jujacobs/oxsmt/tmp-scratch/tp-forest-ab-result.txt
BUD="${1:-30000}"
N="${2:-40}"
verdict() { grep -oE '\(verdict [a-z]*\)' | head -1; }
sig() { grep -oE '\(verdict [a-z]*\)|\(conflicts [0-9]*\)|\(decisions [0-9]*\)|\(propagations [0-9]*\)' | tr '\n' ' '; }
: > "$OUT"
for L in QF_UF QF_AX QF_DT; do
  DIR=/usr/local/home/jujacobs/oxsmt/corpora/$L
  n=0; both=0; flip=0; cdiff=0; to=0
  for f in $(find "$DIR" -name '*.smt2' 2>/dev/null | sort | head -n "$N"); do
    n=$((n+1))
    soff=$(timeout -s KILL 25 "$CLI" --max-effort "$BUD" "$f" 2>/dev/null | sig)
    son=$(OXSMT_FOREST_BALANCE=1 timeout -s KILL 25 "$CLI" --max-effort "$BUD" "$f" 2>/dev/null | sig)
    voff=$(echo "$soff" | grep -oE '\(verdict [a-z]*\)')
    von=$(echo "$son" | grep -oE '\(verdict [a-z]*\)')
    if [ -z "$soff" ] || [ -z "$son" ]; then to=$((to+1)); continue; fi
    if { [ "$voff" = "(verdict sat)" ] || [ "$voff" = "(verdict unsat)" ]; } && \
       { [ "$von" = "(verdict sat)" ] || [ "$von" = "(verdict unsat)" ]; }; then
      both=$((both+1))
      if [ "$voff" != "$von" ]; then
        flip=$((flip+1)); { echo "FLIP $f off=$voff on=$von"; } >> "$OUT"
      fi
    fi
    [ "$soff" != "$son" ] && cdiff=$((cdiff+1))
  done
  echo "$L files=$n both_solved=$both verdict_flips=$flip counter_diffs=$cdiff timeouts=$to" >> "$OUT"
done
echo "==========" >> "$OUT"
echo "DONE" >> "$OUT"
