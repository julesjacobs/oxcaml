#!/bin/sh
# check_wave1.sh -- wave-1 integrator acceptance harness for the vox stdlib.
#
# Compiles + solver-VERIFIES every wave-1 module, its .mli obligations, and
# its smoke client, each in a private temp dir (blueprint 2026-07-06 §7), and
# runs the mechanical slice of the §6 reviewer checklist. Prints one
# PASS/FAIL/WARN line per artifact and exits nonzero on ANY failure.
#
# Usage:  sh vox_stdlib/check_wave1.sh [MODULE_DIR]
#   MODULE_DIR defaults to the vox_stdlib/ next to this script. Override it to
#   self-test the harness against a fixture without touching builder files.
#
# Wave-1 modules (blueprint §1): all independent, no cross-unit model deps.
# Ownership: builders write <M>.{ml,mli}, notes/<M>.md, clients/smoke_<M>.ml;
# the integrator owns ONLY this harness.

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/.." && pwd)
STD="${1:-$HERE}"

PINNED=/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean
LEAN="${VOX_LEAN:-$(command -v lean 2>/dev/null || echo "$PINNED")}"
OC="$ROOT/_install/bin/ocamlc.opt"
export TMPDIR="${TMPDIR:-/usr/local/home/jujacobs/tmp}"
mkdir -p "$TMPDIR" 2>/dev/null || true

# Wave-1 set; override with WAVE1_MODULES to re-check a subset. Vset is the
# via-face and lives in check_wave2. Post-eliminator DAG: Vmap now depends on
# Vlist (its `keys` op enumerates into a Vlist), so it is no longer dep-free —
# see mod_deps below. The other six are independent.
MODULES="${WAVE1_MODULES:-Vlist Voption Vresult Vint Viarray Vset_bst Vmap}"

# Per-module dependency list (space-separated); empty for the independents.
# The harness copies each dep's cmi + VoxSig olean into the build dir first
# (fail-loud), same recipe as check_wave2 / §7.
mod_deps() {
  case "$1" in
    Vmap) echo "Vlist" ;;   # keys : t -> Vlist.t enumerates into a Vlist
    *)    echo "" ;;
  esac
}

# Mandatory §5 note fields (a well-formed note block carries each once).
# Pipe-delimited so multi-word field names survive IFS splitting.
NOTE_FIELDS='site:|milestone/gap:|what I tried:|error:|workaround used:|removed by:|severity:'

fail=0
npass=0; nfail=0; nwarn=0
pass(){ printf '  PASS  %s\n' "$1"; npass=$((npass+1)); }
bad(){  printf '  FAIL  %s\n' "$1"; nfail=$((nfail+1)); fail=1; }
warn(){ printf '  WARN  %s\n' "$1"; nwarn=$((nwarn+1)); }

# Resolve a file case-insensitively: canonical (capitalized) name first, then
# the all-lowercase variant. Blueprint §4 mandates capitalized module files,
# but vox unit/sort/VoxSig names derive from the OCaml MODULE name (always
# capitalized), NOT the raw filename -- probe-confirmed: a lowercase
# voption.ml emits VoxSig_Voption.olean and `open Voption` resolves against
# voption.cmi. So casing is COSMETIC for correctness: tolerate it here, flag
# §4 deviations as WARN, never FAIL purely on casing.
lc(){ printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }
find_ci(){ # $1=dir  $2=canonical-basename ; echoes the found path or nothing
  if [ -f "$1/$2" ]; then printf '%s' "$1/$2"; return 0; fi
  _lcb=$(lc "$2"); if [ -f "$1/$_lcb" ]; then printf '%s' "$1/$_lcb"; return 0; fi
  return 1
}

if [ ! -x "$OC" ]; then echo "FATAL: compiler not found/executable: $OC"; exit 2; fi
if [ ! -x "$LEAN" ]; then echo "FATAL: lean not found/executable: $LEAN"; exit 2; fi
echo "harness: OC=$OC"
echo "harness: LEAN=$LEAN"
echo "harness: MODULE_DIR=$STD"
echo

for M in $MODULES; do
  echo "== $M =="
  # Resolve each file case-insensitively (§4 canonical capitalized, or the
  # lowercase variant some builders shipped). Flag deviations, don't fail.
  ml=$(find_ci "$STD" "$M.ml") || true
  mli=$(find_ci "$STD" "$M.mli") || true
  note=$(find_ci "$STD/notes" "$M.md") || true
  smoke=$(find_ci "$STD/clients" "smoke_$M.ml") || true

  if [ -z "$ml" ]; then bad "$M.ml missing"; echo; continue; fi
  [ -n "$mli" ] || bad "$M.mli missing"
  # casing-deviation advisory (never fatal)
  [ "$ml" = "$STD/$M.ml" ]   || warn "$M: source filename '$(basename "$ml")' is not the §4 canonical '$M.ml' (cosmetic; vox names derive from the module name)"

  W=$(mktemp -d)
  # copy this module's deps (cmi + VoxSig olean) into the build dir, fail-loud
  for dep in $(mod_deps "$M"); do
    dcmi="$STD/_artifacts/$dep.cmi"; [ -f "$dcmi" ] || dcmi="$STD/_artifacts/$(lc "$dep").cmi"
    if [ -f "$dcmi" ]; then cp "$dcmi" "$W/"; else bad "$M: dep $dep.cmi MISSING from _artifacts/"; fi
    if [ -f "$STD/_artifacts/VoxSig_$dep.olean" ]; then cp "$STD/_artifacts/VoxSig_$dep.olean" "$W/"; else bad "$M: dep VoxSig_$dep.olean MISSING from _artifacts/"; fi
  done
  mlb=""; mlib=""; smokeb=""
  [ -n "$mli" ] && { mlib=$(basename "$mli"); cp "$mli" "$W/"; }
  mlb=$(basename "$ml"); cp "$ml" "$W/"

  # ---- verify .mli (declares obligations) then .ml (seal discharges them) ----
  if [ -n "$mli" ]; then
    if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$mlib" ) >"$W/mli.log" 2>&1; then
      pass "$M.mli"
    else
      bad "$M.mli (verify)"; sed 's/^/        | /' "$W/mli.log" | head -10
    fi
  fi
  ml_ok=0
  if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$mlb" ) >"$W/ml.log" 2>&1; then
    pass "$M.ml"; ml_ok=1
  else
    bad "$M.ml (verify)"; sed 's/^/        | /' "$W/ml.log" | head -14
  fi

  # ---- smoke client (forces each shipped law to fire; §6.7 dead-law check) ----
  if [ -n "$smoke" ]; then
    if [ "$ml_ok" -eq 1 ]; then
      smokeb=$(basename "$smoke"); cp "$smoke" "$W/"
      if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$smokeb" ) >"$W/smoke.log" 2>&1; then
        pass "smoke_$M.ml"
      else
        bad "smoke_$M.ml (verify)"; sed 's/^/        | /' "$W/smoke.log" | head -10
      fi
    else
      bad "smoke_$M.ml (skipped: $M.ml did not verify)"
    fi
  else
    bad "clients/smoke_$M.ml missing"
  fi

  # ---- §6.1a: via abstraction-fn totality (via-abstract modules only) ----
  via=$(grep -oE '\[@vox\.via[[:space:]]*\([[:space:]]*[a-zA-Z0-9_]+' "$ml" 2>/dev/null \
        | sed -E 's/.*\([[:space:]]*//' | head -1)
  if [ -n "$via" ]; then
    # extract the `def <via>` block: from its def line to the next blank line.
    body=$(awk -v d="$via" '
      $0 ~ ("def[ ]+" d "([ :]|$)") {f=1}
      f {print}
      f && NF==0 {exit}' "$ml")
    dropped=$(printf '%s\n' "$body" | grep -E '^[[:space:]]*\|.*(,|[[:space:]])_([[:space:]]|,|=>)' )
    if [ -n "$dropped" ]; then
      warn "$M §6.1a: via abstraction '$via' binds field(s) to _ -- confirm no element/subtree data is dropped (degenerate-abstraction trap):"
      printf '%s\n' "$dropped" | sed 's/^/        > /'
    else
      pass "$M §6.1a: abstraction '$via' binds no constructor field to _"
    fi
  fi

  # ---- §6.1b: no STRUCTURAL set/map spec over a non-cons-list repr ----
  # Heuristic: a via-abstract module whose ops use `{ _ = <fn> ... }` set/map
  # specs AND whose repr is a tree is the M-1 trap. Wave-1 modules that are
  # via (Vlist=plain list, Vmap=cons list) are the sanctioned structural
  # cases, so this is expected N/A in wave 1; it fires as a WARN only if a via
  # module's repr type declares a tree/Node constructor.
  # Restrict to actual constructor declarations (a `| Ctor of` or `= Ctor of`
  # line), case-SENSITIVE, so prose like "branching"/"leaf" in a comment does
  # not false-fire (that bit Vmap's cons-list in the first run).
  if [ -n "$via" ] && grep -qE '\{[[:space:]]*_[[:space:]]*=' "$mli" 2>/dev/null; then
    if grep -qE '(\||=)[[:space:]]*(Node|Branch|Tree)[[:space:]]+of' "$ml" 2>/dev/null; then
      warn "$M §6.1b: structural spec ({ _ = ... }) with a tree-shaped repr -- confirm the spec is faithful (membership/find-based unless a genuine cons-list prepend)"
    else
      pass "$M §6.1b: structural spec over a list-shaped repr (sanctioned)"
    fi
  fi

  # ---- §6.1c: a module may only `open` a DECLARED dependency ----
  # Post-eliminator DAG, some modules legitimately open a dep (Vmap opens Vlist
  # for keys). FAIL only on an UNDECLARED open (a hidden cross-unit edge).
  deps=" $(mod_deps "$M") "
  for opened in $(grep -oE '^[[:space:]]*open[[:space:]]+(Vlist|Voption|Vresult|Vint|Viarray|Vset_bst|Vmap)\b' "$ml" 2>/dev/null | awk '{print $2}'); do
    case "$deps" in
      *" $opened "*) pass "$M §6.1c: opens declared dep $opened" ;;
      *) bad "$M §6.1c: opens UNDECLARED module $opened -- add it to mod_deps or remove the edge (report to integrator)" ;;
    esac
  done

  # ---- §6.7-liveness (sharpened Amendment A): exposed NON-RECURSIVE def ----
  # An `@[grind, expose] public def` that does NOT recurse over its argument
  # lets grind discharge laws about it BY UNFOLDING -> the law is silently
  # DEAD (smoke passes via unfolding; only the removal test catches it -- this
  # is what the Phase-C review found in Voption/Vresult/Vlist). WARN so the
  # reviewer removal-tests / de-exposes. Recursive exposed defs are safe;
  # exposed quantified (∀/∃) SPEC defs (vs_addspec/vs_isempty) are the intended
  # spec vocabulary and are excluded to limit noise.
  if [ -n "$mli" ]; then
    for d in $(grep -oE '@\[grind, expose\][[:space:]]*public[[:space:]]+def[[:space:]]+[a-zA-Z0-9_]+' "$mli" 2>/dev/null | awk '{print $NF}'); do
      dbody=$(awk -v d="$d" '
        $0 ~ ("def[ ]+" d "([ :(]|$)") {f=1; print; next}
        f && (/^@\[/ || /^public / || /^grind_pattern / || /\|lean\}\]/ || NF==0) {exit}
        f {print}' "$mli")
      printf '%s\n' "$dbody" | grep -q '∀\|∃' && continue          # spec def, skip
      rec=$(printf '%s\n' "$dbody" | tail -n +2 | grep -c "$d")
      if [ "${rec:-0}" -eq 0 ]; then
        warn "$M §6.7: exposed NON-recursive def '$d' -- grind can unfold it, so laws about it may be DEAD (smoke passes via unfolding). De-expose (public w/o expose) + ship reduction laws, unless verified live by the removal test (sharpened Amendment A)."
      fi
    done
  fi

  # ---- notes file existence + §5 format ----
  if [ -f "$note" ]; then
    # grep -c always prints a count (0 on no match); do NOT add `|| echo 0`
    # (that double-prints and corrupts the integer test).
    nblk=$(grep -c '^### ' "$note" 2>/dev/null); nblk=${nblk:-0}
    if [ "$nblk" -eq 0 ]; then
      warn "$M: notes/$M.md present but has no '### ' note blocks (a clean site still gets a note; §5)"
    else
      miss=""
      OLDIFS=$IFS; IFS='|'
      for f in $NOTE_FIELDS; do
        IFS=$OLDIFS
        c=$(grep -cF "$f" "$note" 2>/dev/null); c=${c:-0}
        [ "$c" -lt "$nblk" ] && miss="$miss [$f]"
        IFS='|'
      done
      IFS=$OLDIFS
      if [ -n "$miss" ]; then
        bad "$M: notes/$M.md ($nblk block(s)) missing/short field(s):$miss (§5 format)"
      else
        pass "$M: notes/$M.md ($nblk block(s), §5 fields present)"
      fi
    fi
  else
    bad "notes/$M.md missing"
  fi

  rm -rf "$W"
  echo
done

echo "==================================================="
echo "wave-1: $npass PASS, $nfail FAIL, $nwarn WARN"
if [ "$fail" -ne 0 ]; then
  echo "RESULT: FAIL -- wave not acceptable (WARNs are advisory; FAILs block)."
else
  echo "RESULT: PASS -- all wave-1 artifacts verify. Review WARNs before committing."
fi
exit "$fail"
