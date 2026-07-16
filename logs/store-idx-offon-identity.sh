#!/bin/bash
# Fix #3 OFF-vs-ON counted-identity, bounded per-family sample (established bar).
# Args: CLI (default branch binary), MAX per family, EFFORT.
CLI="${1:-/usr/local/home/jujacobs/oxsmt/worktrees/theory-perf/_build/default/tests/solver/oxsmt_cli.exe}"
MAX="${2:-40}"
BUD="${3:-30000}"
OUT=/usr/local/home/jujacobs/oxsmt/tmp-scratch/store-idx-idresult2.txt
sig() { grep -oE '\(verdict [a-z]*\)|\(conflicts [0-9]*\)|\(decisions [0-9]*\)|\(propagations [0-9]*\)' | tr '\n' ' '; }
: > "$OUT"
tot=0; div=0; to=0
for DIR in /usr/local/home/jujacobs/oxsmt/corpora/QF_AX/swap \
           /usr/local/home/jujacobs/oxsmt/corpora/QF_AX/storecomm \
           /usr/local/home/jujacobs/oxsmt/corpora/QF_AX/storeinv \
           /usr/local/home/jujacobs/oxsmt/corpora/QF_AX/cvc; do
  n=0; d=0; t=0
  for f in $(ls "$DIR"/*.smt2 2>/dev/null | head -n "$MAX"); do
    n=$((n+1)); tot=$((tot+1))
    o=$(timeout -s KILL 25 "$CLI" --max-effort "$BUD" "$f" 2>/dev/null | sig)
    a=$(OXSMT_AX_OCCIDX=1 timeout -s KILL 25 "$CLI" --max-effort "$BUD" "$f" 2>/dev/null | sig)
    if [ -z "$o" ] || [ -z "$a" ]; then t=$((t+1)); to=$((to+1)); continue; fi
    if [ "$o" != "$a" ]; then
      d=$((d+1)); div=$((div+1))
      { echo "DIVERGE $(basename "$f")"; echo "  OFF: $o"; echo "  ON : $a"; } >> "$OUT"
    fi
  done
  echo "$(basename "$DIR") files=$n diverge=$d timeouts=$t" >> "$OUT"
done
echo "==========" >> "$OUT"
echo "TOTAL files=$tot identity_diverge=$div timeouts/empty=$to" >> "$OUT"
echo "DONE" >> "$OUT"
