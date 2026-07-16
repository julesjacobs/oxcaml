#!/bin/bash
# LGC arm head-to-head A/B (robust, 2-pass, per-family). Three arms off ONE release binary
# via explicit env: OFF (conflict-count schedule), FIXED (LGC init 5000), SIZEREL (LGC init
# max(1000,#clauses/3)). Metric = solved-count at a fixed wall (product headline); "solved"
# = sat|unsat within WALL. ROBUST = solved in BOTH passes. 0-flip gate: any two arms that
# both return a non-TO verdict must agree. Box load is the caller's to note.
#
# Usage: bash lgc-arm-ab.sh WALL CAP STRIDE FAMILY_DIR [FAMILY_DIR ...]
#   WALL   = per-solve wall seconds (e.g. 2)
#   CAP    = max sampled files per family dir (0 = no cap)
#   STRIDE = sample every STRIDE-th file (1 = all)
set -u
shopt -s globstar nullglob

CLI=/usr/local/home/jujacobs/oxsmt/worktrees/lgc-flip/_build/default/tests/solver/oxsmt_cli.exe
# SHARED fleet wall lock (NOT a worktree-local dir): serialize against every other 2s wall
# sweep on this box so solved-count-at-wall is not depressed by concurrent load. Release
# with `rm -rf` (an owner file makes the dir non-empty, so bare rmdir would wedge it).
LOCK=/usr/local/home/jujacobs/oxsmt/logs/.wall-ab-lock

WALL="$1"; CAP="$2"; STRIDE="$3"; shift 3
FAMS=("$@")

# WAIT-LOOP for the shared lock (fleet convention: never force-remove another lane's lock).
waited=0
while ! mkdir "$LOCK" 2>/dev/null; do
  if (( waited == 0 )); then echo "waiting for wall lock, held by: $(cat "$LOCK"/owner 2>/dev/null)"; fi
  sleep 15; waited=$((waited+15))
  if (( waited > 5400 )); then echo "wall lock still held after 90m — aborting." >&2; exit 3; fi
done
echo "lgcflip-builder lgc-arm-ab $(date)" > "$LOCK/owner"
echo "acquired wall lock after ${waited}s"
trap 'rm -rf "$LOCK" 2>/dev/null' EXIT

verdict() { grep -oE '(sat|unsat|unknown)' | head -1; }

# one solve: env-string file -> "sat|unsat|unknown|TO"
solve() {
  local v
  v=$(env $1 timeout -s KILL "$WALL" "$CLI" "$2" 2>/dev/null | verdict)
  echo "${v:-TO}"
}
is_solved() { [ "$1" = "sat" ] || [ "$1" = "unsat" ]; }

declare -A P1O P1F P1S P2O P2F P2S
G_off=0; G_fix=0; G_siz=0; G_disagree=0; G_files=0

printf "LGC arm A/B: WALL=%ss CAP=%s STRIDE=%s  arms=OFF/FIXED/SIZEREL  %s\n" \
  "$WALL" "$CAP" "$STRIDE" "$(date '+%F %T')"
printf "loadavg at start: %s\n" "$(cat /proc/loadavg)"

for FAM in "${FAMS[@]}"; do
  files=("$FAM"/**/*.smt2)
  # deterministic EVENLY-SPREAD sample of ~CAP files: per-family stride = ceil(n/CAP), so a
  # big subdir is sampled across its whole range (not just its alphabetical head). STRIDE
  # arg is a floor multiplier (usually 1). CAP=0 => take all.
  n=${#files[@]}
  if (( CAP > 0 && n > CAP )); then st=$(( (n + CAP - 1) / CAP )); else st=1; fi
  st=$(( st * STRIDE )); (( st < 1 )) && st=1
  sample=(); i=0
  for f in "${files[@]}"; do
    if (( i % st == 0 )); then sample+=("$f"); fi
    i=$((i+1))
  done
  fam_off=0; fam_fix=0; fam_siz=0; fam_dis=0; fam_lose=0
  for pass in 1 2; do
    for f in "${sample[@]}"; do
      vo=$(solve "OXSMT_LGC_FIXED=0" "$f")
      vf=$(solve "OXSMT_LGC_FIXED=1 OXSMT_LGC_SIZEREL=0" "$f")
      vs=$(solve "OXSMT_LGC_FIXED=1 OXSMT_LGC_SIZEREL=1" "$f")
      if [ "$pass" = 1 ]; then P1O["$f"]=$vo; P1F["$f"]=$vf; P1S["$f"]=$vs
      else P2O["$f"]=$vo; P2F["$f"]=$vf; P2S["$f"]=$vs; fi
      # 0-flip gate (per solve; disagreement between any two non-TO verdicts)
      for a in "$vo" "$vf" "$vs"; do
        for b in "$vo" "$vf" "$vs"; do
          if is_solved "$a" && is_solved "$b" && [ "$a" != "$b" ]; then
            echo "DISAGREE $(basename "$f"): OFF=$vo FIXED=$vf SIZEREL=$vs"
            fam_dis=$((fam_dis+1))
          fi
        done
      done
    done
  done
  # robust tallies over the sample
  for f in "${sample[@]}"; do
    G_files=$((G_files+1))
    ro=0; rf=0; rs=0
    is_solved "${P1O[$f]}" && is_solved "${P2O[$f]}" && ro=1
    is_solved "${P1F[$f]}" && is_solved "${P2F[$f]}" && rf=1
    is_solved "${P1S[$f]}" && is_solved "${P2S[$f]}" && rs=1
    fam_off=$((fam_off+ro)); fam_fix=$((fam_fix+rf)); fam_siz=$((fam_siz+rs))
    # sizerel regression vs fixed: fixed robust-solves, sizerel does not
    if (( rf == 1 && rs == 0 )); then fam_lose=$((fam_lose+1)); fi
  done
  G_off=$((G_off+fam_off)); G_fix=$((G_fix+fam_fix)); G_siz=$((G_siz+fam_siz)); G_disagree=$((G_disagree+fam_dis/2))
  printf "FAMILY %-55s n=%-5d robust_solved OFF=%-5d FIXED=%-5d SIZEREL=%-5d | fixedΔ=%+d sizerelΔ=%+d | sizerel_loses_vs_fixed=%d disagree=%d\n" \
    "$(basename "$FAM")" "${#sample[@]}" "$fam_off" "$fam_fix" "$fam_siz" \
    "$((fam_fix-fam_off))" "$((fam_siz-fam_off))" "$fam_lose" "$((fam_dis/2))"
done

echo "-----"
printf "TOTAL files=%d  robust_solved OFF=%d FIXED=%d SIZEREL=%d | fixedΔ=%+d sizerelΔ=%+d | disagree=%d\n" \
  "$G_files" "$G_off" "$G_fix" "$G_siz" "$((G_fix-G_off))" "$((G_siz-G_off))" "$G_disagree"
printf "loadavg at end: %s   %s\n" "$(cat /proc/loadavg)" "$(date '+%F %T')"
