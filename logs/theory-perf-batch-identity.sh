#!/bin/bash
# Broad multi-logic counted-identity: theory-perf BRANCH binary vs BASE (d18a9934c0).
# Default flags => exercises #1,#2,#3-OFF,#4,#5-OFF (all byte-identical on the default path).
BR=/usr/local/home/jujacobs/oxsmt/worktrees/theory-perf/_build/default/tests/solver/oxsmt_cli.exe
BASE=/usr/local/home/jujacobs/oxsmt/worktrees/fe2l4-base/_build/default/tests/solver/oxsmt_cli.exe
OUT=/usr/local/home/jujacobs/oxsmt/tmp-scratch/tp-batch-identity-result.txt
BUD=30000
N="${1:-15}"
sig() { grep -oE '\(verdict [a-z]*\)|\(conflicts [0-9]*\)|\(decisions [0-9]*\)|\(propagations [0-9]*\)' | tr '\n' ' '; }
: > "$OUT"
tot=0; div=0; to=0
run_dir() {
  local DIR="$1" MAX="$2"; local n=0 d=0 t=0
  for f in $(find "$DIR" -name '*.smt2' 2>/dev/null | sort | head -n "$MAX"); do
    n=$((n+1)); tot=$((tot+1))
    rb=$(timeout -s KILL 25 "$BR"   --max-effort "$BUD" "$f" 2>/dev/null | sig)
    rt=$(timeout -s KILL 25 "$BASE" --max-effort "$BUD" "$f" 2>/dev/null | sig)
    if [ -z "$rb" ] || [ -z "$rt" ]; then t=$((t+1)); to=$((to+1)); continue; fi
    if [ "$rb" != "$rt" ]; then
      d=$((d+1)); div=$((div+1))
      { echo "DIVERGE $f"; echo "  BR  : $rb"; echo "  BASE: $rt"; } >> "$OUT"
    fi
  done
  echo "$(basename "$DIR") checked=$n diverge=$d timeouts=$t" >> "$OUT"
}
for L in QF_BV QF_UF QF_LIA QF_AX QF_DT QF_UFLIA; do
  run_dir /usr/local/home/jujacobs/oxsmt/corpora/$L "$N"
done
run_dir /usr/local/home/jujacobs/oxsmt/worktrees/theory-perf/tests/cases 40
echo "==========" >> "$OUT"
echo "TOTAL checked=$tot diverge=$div timeouts/empty=$to" >> "$OUT"
echo "DONE" >> "$OUT"
