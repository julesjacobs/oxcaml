#!/bin/bash
# Wall A/B @2s for W5 Lever B (OXSMT_DT_INCR): solved-count + total wall, OFF vs ON.
# Verdict disagreements are counted (must be 0 — a cache, not an inference change).
# Usage: dt-incr-ab.sh <dir> [wall] [max]
CLI=/usr/local/home/jujacobs/oxsmt/worktrees/dt-incr/_build/default/tests/solver/oxsmt_cli.exe
DIR="$1"; WALL="${2:-2}"; MAX="${3:-200}"
verdict() { grep -oE '\(verdict [a-z]*\)' | head -1 | grep -oE '(sat|unsat|unknown)' | head -1; }
run() { # $1=env, $2=file -> prints "verdict_or_TO wall_ms" (verdict is a single word)
  local st et v
  st=$(date +%s%N)
  v=$(env $1 timeout -s KILL "$WALL" "$CLI" "$2" 2>/dev/null | verdict)
  et=$(date +%s%N)
  echo "${v:-TO} $(( (et - st) / 1000000 ))"
}
n=0; solved_off=0; solved_on=0; woff=0; won=0; disagree=0
for f in $(ls "$DIR"/*.smt2 2>/dev/null | head -n "$MAX"); do
  n=$((n+1))
  read voff toff < <(run "" "$f")
  read von  ton  < <(run "OXSMT_DT_INCR=1" "$f")
  woff=$((woff+toff)); won=$((won+ton))
  [ "$voff" != "TO" ] && [ "$voff" != "unknown" ] && solved_off=$((solved_off+1))
  [ "$von"  != "TO" ] && [ "$von"  != "unknown" ] && solved_on=$((solved_on+1))
  if [ "$voff" != "TO" ] && [ "$von" != "TO" ] && [ "$voff" != "$von" ]; then
    disagree=$((disagree+1)); echo "DISAGREE $(basename $f): OFF=$voff ON=$von"
  fi
done
echo "files=$n solved_OFF=$solved_off solved_ON=$solved_on delta=$((solved_on-solved_off)) wall_OFF_ms=$woff wall_ON_ms=$won disagree=$disagree"
