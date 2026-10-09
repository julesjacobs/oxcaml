#!/bin/bash
# Counted-identity check for W5 Lever A (OXSMT_AX_OCCIDX): OFF vs ON must produce identical
# verdict + counters on every file. Usage: ax-occidx-identity.sh <dir> [wall] [max]
CLI=/usr/local/home/jujacobs/oxsmt/worktrees/adapter-incr/_build/default/tests/solver/oxsmt_cli.exe
DIR="$1"; WALL="${2:-20}"; MAX="${3:-40}"
sig() { grep -oE '\(verdict [a-z]*\)|\(conflicts [0-9]*\)|\(decisions [0-9]*\)|\(propagations [0-9]*\)' | tr '\n' ' '; }
n=0; diff=0; to=0
for f in $(ls "$DIR"/*.smt2 2>/dev/null | head -n "$MAX"); do
  n=$((n+1))
  o=$(timeout -s KILL "$WALL" "$CLI" "$f" 2>/dev/null | sig)
  a=$(OXSMT_AX_OCCIDX=1 timeout -s KILL "$WALL" "$CLI" "$f" 2>/dev/null | sig)
  if [ -z "$o" ] || [ -z "$a" ]; then to=$((to+1)); continue; fi
  if [ "$o" != "$a" ]; then
    diff=$((diff+1))
    echo "DIVERGE $(basename "$f")"
    echo "  OFF: $o"
    echo "  ON : $a"
  fi
done
echo "files=$n identity_diverge=$diff timeouts/empty=$to"
