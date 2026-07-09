#!/bin/sh
# check_wave2.sh -- wave-2 integrator acceptance harness for the vox stdlib.
#
# Verifies the wave-2 via-abstract FACE module (Vset) and its smoke client
# against the wave-1 backend artifacts in vox_stdlib/_artifacts/ (blueprint
# 2026-07-06 §7), with a FAIL-LOUD missing-dependency check, plus the §6.1
# face-checks that wave 1 could not exercise (§6.1a abstraction totality,
# §6.1b no structural set/map spec, §6.1c calls-the-backend / ships-the-bridge
# / .mli-hides-the-repr). One PASS/FAIL/WARN per artifact; nonzero on any FAIL.
#
# Usage:  sh vox_stdlib/check_wave2.sh [MODULE_DIR]
#   MODULE_DIR defaults to the vox_stdlib/ next to this script.
#   VSET_DEPS overrides the dependency list (default: Vset_bst).

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/.." && pwd)
STD="${1:-$HERE}"
ART="$STD/_artifacts"

PINNED=/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean
LEAN="${VOX_LEAN:-$(command -v lean 2>/dev/null || echo "$PINNED")}"
OC="$ROOT/_install/bin/ocamlc.opt"
export TMPDIR="${TMPDIR:-/usr/local/home/jujacobs/tmp}"
mkdir -p "$TMPDIR" 2>/dev/null || true

M=Vset
# Post-eliminator DAG: Vset depends on Vset_bst (backend) AND Vlist (its
# `elements` op enumerates into a Vlist). Both cmi+VoxSig must be staged.
DEPS="${VSET_DEPS:-Vset_bst Vlist}"
BACKEND=Vset_bst   # the exposed-ADT backend Vset's face sits over
NOTE_FIELDS='site:|milestone/gap:|what I tried:|error:|workaround used:|removed by:|severity:'

fail=0; npass=0; nfail=0; nwarn=0
pass(){ printf '  PASS  %s\n' "$1"; npass=$((npass+1)); }
bad(){  printf '  FAIL  %s\n' "$1"; nfail=$((nfail+1)); fail=1; }
warn(){ printf '  WARN  %s\n' "$1"; nwarn=$((nwarn+1)); }
lc(){ printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }
find_ci(){ if [ -f "$1/$2" ]; then printf '%s' "$1/$2"; return 0; fi
           _l=$(lc "$2"); [ -f "$1/$_l" ] && { printf '%s' "$1/$_l"; return 0; }; return 1; }

if [ ! -x "$OC" ]; then echo "FATAL: compiler not found: $OC"; exit 2; fi
if [ ! -x "$LEAN" ]; then echo "FATAL: lean not found: $LEAN"; exit 2; fi
echo "harness: OC=$OC"; echo "harness: LEAN=$LEAN"
echo "harness: MODULE_DIR=$STD"; echo "harness: DEPS=$DEPS"; echo

ml=$(find_ci "$STD" "$M.ml") || true
mli=$(find_ci "$STD" "$M.mli") || true
note=$(find_ci "$STD/notes" "$M.md") || true
smoke=$(find_ci "$STD/clients" "smoke_$M.ml") || true

if [ -z "$ml" ]; then bad "$M.ml missing"; fi
[ -n "$mli" ] || bad "$M.mli missing"
[ -z "$ml" ] && { echo; echo "wave-2: $npass PASS, $nfail FAIL, $nwarn WARN"; echo "RESULT: FAIL"; exit 1; }

W=$(mktemp -d)
# ---- FAIL-LOUD dependency staging: every declared dep's cmi + VoxSig olean ----
for dep in $DEPS; do
  dcmi=$(find_ci "$ART" "$dep.cmi")
  dole="$ART/VoxSig_$dep.olean"
  if [ -z "$dcmi" ]; then bad "dependency $dep.cmi MISSING from _artifacts/ (fail-loud)"; else cp "$dcmi" "$W/"; fi
  if [ ! -f "$dole" ]; then bad "dependency VoxSig_$dep.olean MISSING from _artifacts/ (fail-loud)"; else cp "$dole" "$W/"; fi
done
[ "$fail" -ne 0 ] && { echo; echo "wave-2: $npass PASS, $nfail FAIL, $nwarn WARN"; echo "RESULT: FAIL -- deps missing; stage _artifacts/ first."; rm -rf "$W"; exit 1; }

mlb=$(basename "$ml"); [ -n "$mli" ] && { mlib=$(basename "$mli"); cp "$mli" "$W/"; }
cp "$ml" "$W/"

# ---- verify .mli then .ml against the staged backend artifacts ----
if [ -n "$mli" ]; then
  if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$mlib" ) >"$W/mli.log" 2>&1; then pass "$M.mli"
  else bad "$M.mli (verify)"; sed 's/^/        | /' "$W/mli.log" | head -12; fi
fi
ml_ok=0
if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$mlb" ) >"$W/ml.log" 2>&1; then pass "$M.ml"; ml_ok=1
else bad "$M.ml (verify)"; sed 's/^/        | /' "$W/ml.log" | head -16; fi

# ---- smoke client ----
if [ -n "$smoke" ]; then
  if [ "$ml_ok" -eq 1 ]; then
    smb=$(basename "$smoke"); cp "$smoke" "$W/"
    if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$smb" ) >"$W/smoke.log" 2>&1; then pass "smoke_$M.ml"
    else bad "smoke_$M.ml (verify)"; sed 's/^/        | /' "$W/smoke.log" | head -12; fi
  else bad "smoke_$M.ml (skipped: $M.ml did not verify)"; fi
else bad "clients/smoke_$M.ml missing"; fi

# ============ §6.1 face-checks (the wave-2-specific reviewer gates) ============

# §6.1a abstraction-fn totality: the via abstraction must recurse into every
# data-bearing field (a subtree/element bound to _ is the degenerate trap).
via=$(grep -oE '\[@vox\.via[[:space:]]*\([[:space:]]*[a-zA-Z0-9_]+' "$ml" 2>/dev/null | sed -E 's/.*\([[:space:]]*//' | head -1)
if [ -n "$via" ]; then
  # capture ONLY the abstraction def's pattern arms (def line, then lines
  # starting with `|`, stop at the first non-arm line) -- do NOT run past the
  # Lean block into the .ml's OCaml `add` arms (whose refinements contain `_`).
  body=$(awk -v d="$via" '
    $0 ~ ("def[ ]+" d "([ :]|$)") {f=1; next}
    f && /^[[:space:]]*\|/ {print; next}
    f {exit}' "$ml")
  dropped=$(printf '%s\n' "$body" | grep -E '(,|[[:space:]])_([[:space:]]|,|=>)')
  if [ -n "$dropped" ]; then
    warn "$M §6.1a: via abstraction '$via' binds field(s) to _ -- a set face MUST recurse into BOTH subtrees (left-spine-only is the degenerate trap):"
    printf '%s\n' "$dropped" | sed 's/^/        > /'
  else pass "$M §6.1a: abstraction '$via' recurses all constructor fields"; fi
else warn "$M §6.1a: no [@vox.via] found in $M.ml (expected a via face)"; fi

# §6.1b no STRUCTURAL set/map spec: the ISet list model's structural (=) is not
# set equality, so ops must be membership-based (addspec/isempty), never
# `{ _ = vs_ins x s }`. A structural set spec over the tree repr is the M-1 trap.
# restrict to `val` declaration lines so an illustrative `{ _ = vs_ins x s }`
# in a COMMENT does not false-fire.
if [ -n "$mli" ] && grep -qE '^[[:space:]]*val .*\{[[:space:]]*_[[:space:]]*=[[:space:]]*vs_(ins|add|union)' "$mli" 2>/dev/null; then
  bad "$M §6.1b: structural set spec ({ _ = vs_ins/... }) in a val -- the ISet list model's (=) is not set equality; use membership-based addspec (M-1 trap)"
else pass "$M §6.1b: specs are membership-based (no structural set spec)"; fi

# §6.1c-i face CALLS the backend ops (qualified, or bare under `open BACKEND`).
callq=$(grep -cE "$BACKEND\.(member|insert|add|mem)" "$ml" 2>/dev/null); callq=${callq:-0}
openb=$(grep -cE "^[[:space:]]*open[[:space:]]+$BACKEND\b" "$ml" 2>/dev/null); openb=${openb:-0}
callbare=$(grep -cE '\b(member|insert)[[:space:]]+[a-z]' "$ml" 2>/dev/null); callbare=${callbare:-0}
if [ "$callq" -gt 0 ] || { [ "$openb" -gt 0 ] && [ "$callbare" -gt 0 ]; }; then
  pass "$M §6.1c: face calls the backend ops ($BACKEND.member/insert)"
else
  bad "$M §6.1c: face does NOT call $BACKEND's ops -- a face that hand-builds constructors and never calls the backend is not a face"
fi

# §6.1c-ii face SHIPS the bridge theorem (equates its model to the backend's).
if grep -qE 'theorem' "$ml" 2>/dev/null && grep -qE '\bbmem\b' "$ml" 2>/dev/null; then
  pass "$M §6.1c: ships a bridge theorem referencing the backend's bmem"
else
  bad "$M §6.1c: no bridge theorem referencing $BACKEND's membership (bmem) -- the abstraction is unbridged to the backend"
fi

# §6.1c-iii the .mli does NOT expose the backend repr (abstraction boundary).
if [ -n "$mli" ] && grep -qE "type[[:space:]]+t[[:space:]]*=[[:space:]]*$BACKEND\." "$mli" 2>/dev/null; then
  bad "$M §6.1c: .mli exposes 'type t = $BACKEND....' -- the face must keep t abstract (type t : value refines (...))"
else pass "$M §6.1c: .mli keeps t abstract (does not expose the backend repr)"; fi

# §6.7-liveness (sharpened Amendment A): an exposed NON-recursive def lets grind
# unfold laws about it -> silently DEAD (smoke passes via unfolding). WARN;
# quantified (∀/∃) spec defs are the intended spec vocabulary and are excluded.
if [ -n "$mli" ]; then
  for d in $(grep -oE '@\[grind, expose\][[:space:]]*public[[:space:]]+def[[:space:]]+[a-zA-Z0-9_]+' "$mli" 2>/dev/null | awk '{print $NF}'); do
    dbody=$(awk -v d="$d" '
      $0 ~ ("def[ ]+" d "([ :(]|$)") {f=1; print; next}
      f && (/^@\[/ || /^public / || /^grind_pattern / || /\|lean\}\]/ || NF==0) {exit}
      f {print}' "$mli")
    printf '%s\n' "$dbody" | grep -q '∀\|∃' && continue
    rec=$(printf '%s\n' "$dbody" | tail -n +2 | grep -c "$d"); rec=${rec:-0}
    [ "$rec" -eq 0 ] && warn "$M §6.7: exposed NON-recursive def '$d' -- grind can unfold it, so laws about it may be DEAD; de-expose (public w/o expose) + ship reduction laws unless verified live by the removal test (sharpened Amendment A)."
  done
fi

# ---- notes §5 format ----
if [ -n "$note" ]; then
  nblk=$(grep -c '^### ' "$note" 2>/dev/null); nblk=${nblk:-0}
  if [ "$nblk" -eq 0 ]; then warn "$M: notes/$M.md present but has no '### ' blocks (§5)"
  else
    miss=""; OLDIFS=$IFS; IFS='|'
    for f in $NOTE_FIELDS; do IFS=$OLDIFS; c=$(grep -cF "$f" "$note" 2>/dev/null); c=${c:-0}; [ "$c" -lt "$nblk" ] && miss="$miss [$f]"; IFS='|'; done
    IFS=$OLDIFS
    if [ -n "$miss" ]; then bad "$M: notes/$M.md ($nblk block(s)) missing field(s):$miss (§5)"
    else pass "$M: notes/$M.md ($nblk block(s), §5 fields present)"; fi
  fi
else bad "notes/$M.md missing"; fi

rm -rf "$W"
echo
echo "==================================================="
echo "wave-2: $npass PASS, $nfail FAIL, $nwarn WARN"
if [ "$fail" -ne 0 ]; then echo "RESULT: FAIL -- wave 2 not acceptable."
else echo "RESULT: PASS -- Vset verifies over the backend; face-checks clean."; fi
exit "$fail"
