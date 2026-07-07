#!/bin/sh
# check_poly.sh -- v1.5 polymorphic-wave integrator acceptance harness.
#
# Same recipe as check_wave1.sh (compile + solver-VERIFY each module's .mli
# obligations, its .ml seal, and its smoke client in a private temp dir; run
# the mechanical slice of the reviewer checklist), specialised for the
# element-polymorphic modules Vplist / Vpmap / Vpset. Prints one
# PASS/FAIL/WARN line per artifact and exits nonzero on ANY failure.
#
# Usage:  sh vox_stdlib/check_poly.sh [MODULE_DIR]
#   MODULE_DIR defaults to the vox_stdlib/ next to this script.
#
# The module set + per-module deps are READ FROM MODULES.manifest (wave 3
# rows, uncommented), so this harness tracks whatever the wave lands: the
# integrator uncomments a manifest row as each module is accepted, exactly as
# the wave-1 harness's MODULES list gates. Override with POLY_MODULES.
#
# POLY-SPECIFIC CHECK (beyond wave 1): every poly module's smoke MUST exercise
# its laws at BOTH `int <M>.t` AND `string <M>.t`. That dual instantiation is
# the poly study's S_param-resolution proof -- a law proved at the abstract
# element sort must fire at each concrete element type. A smoke that only
# instantiates one element type is a FAIL (it does not prove genericity).
#
# Ownership unchanged: builders write <M>.{ml,mli}, notes/<M>.md,
# clients/smoke_<M>.ml (Vplist also owns any Vlist adoption edits); the
# integrator owns ONLY this harness.

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/.." && pwd)
STD="${1:-$HERE}"
MANIFEST="$STD/MODULES.manifest"

PINNED=/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean
LEAN="${VOX_LEAN:-$(command -v lean 2>/dev/null || echo "$PINNED")}"
OC="$ROOT/_install/bin/ocamlc.opt"
export TMPDIR="${TMPDIR:-/usr/local/home/jujacobs/tmp}"
mkdir -p "$TMPDIR" 2>/dev/null || true

# --- manifest reader: wave-3 module names, and a module's declared deps. ---
# Row format: `Module : dep1 dep2 : wave   # comment`. Commented rows (^#) are
# not-yet-landed and are skipped, which is the accept-gate.
manifest_wave3() {
  awk -F: '
    /^[[:space:]]*#/ { next }
    NF >= 3 {
      m=$1; w=$3
      gsub(/[[:space:]]/,"",m)
      sub(/#.*/,"",w); gsub(/[[:space:]]/,"",w)
      if (m != "" && w == "3") print m
    }' "$MANIFEST" 2>/dev/null
}
manifest_deps() { # $1 = module ; echoes space-separated deps (may be empty)
  awk -F: -v want="$1" '
    /^[[:space:]]*#/ { next }
    NF >= 3 {
      m=$1; d=$2
      gsub(/[[:space:]]/,"",m)
      if (m == want) { gsub(/^[[:space:]]+|[[:space:]]+$/,"",d); print d; exit }
    }' "$MANIFEST" 2>/dev/null
}

MODULES="${POLY_MODULES:-$(manifest_wave3)}"
# All modules whose cmi/olean might be staged as deps (v1 + poly), for §6.1c.
KNOWN='Vlist Voption Vresult Vint Viarray Vset_bst Vmap Vset Vplist Vpmap Vpset'

NOTE_FIELDS='site:|milestone/gap:|what I tried:|error:|workaround used:|removed by:|severity:'

fail=0
npass=0; nfail=0; nwarn=0
pass(){ printf '  PASS  %s\n' "$1"; npass=$((npass+1)); }
bad(){  printf '  FAIL  %s\n' "$1"; nfail=$((nfail+1)); fail=1; }
warn(){ printf '  WARN  %s\n' "$1"; nwarn=$((nwarn+1)); }

lc(){ printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }
find_ci(){ # $1=dir  $2=canonical-basename
  if [ -f "$1/$2" ]; then printf '%s' "$1/$2"; return 0; fi
  _lcb=$(lc "$2"); if [ -f "$1/$_lcb" ]; then printf '%s' "$1/$_lcb"; return 0; fi
  return 1
}

if [ ! -x "$OC" ]; then echo "FATAL: compiler not found/executable: $OC"; exit 2; fi
if [ ! -x "$LEAN" ]; then echo "FATAL: lean not found/executable: $LEAN"; exit 2; fi
if [ -z "${MODULES// /}" ]; then
  echo "FATAL: no wave-3 modules in $MANIFEST (uncomment a row as each lands, or set POLY_MODULES)"; exit 2
fi
echo "harness: OC=$OC"
echo "harness: LEAN=$LEAN"
echo "harness: MODULE_DIR=$STD"
echo "harness: wave-3 modules = $MODULES"
echo

for M in $MODULES; do
  echo "== $M =="
  ml=$(find_ci "$STD" "$M.ml") || true
  mli=$(find_ci "$STD" "$M.mli") || true
  note=$(find_ci "$STD/notes" "$M.md") || true
  smoke=$(find_ci "$STD/clients" "smoke_$M.ml") || true

  if [ -z "$ml" ]; then bad "$M.ml missing"; echo; continue; fi
  [ -n "$mli" ] || bad "$M.mli missing"
  [ "$ml" = "$STD/$M.ml" ] || warn "$M: source filename '$(basename "$ml")' is not the canonical '$M.ml' (cosmetic; vox names derive from the module name)"

  W=$(mktemp -d)
  DEPS=$(manifest_deps "$M")
  for dep in $DEPS; do
    dcmi="$STD/_artifacts/$dep.cmi"; [ -f "$dcmi" ] || dcmi="$STD/_artifacts/$(lc "$dep").cmi"
    if [ -f "$dcmi" ]; then cp "$dcmi" "$W/"; else bad "$M: dep $dep.cmi MISSING from _artifacts/"; fi
    if [ -f "$STD/_artifacts/VoxSig_$dep.olean" ]; then cp "$STD/_artifacts/VoxSig_$dep.olean" "$W/"; else bad "$M: dep VoxSig_$dep.olean MISSING from _artifacts/"; fi
  done
  mlb=""; mlib=""; smokeb=""
  [ -n "$mli" ] && { mlib=$(basename "$mli"); cp "$mli" "$W/"; }
  mlb=$(basename "$ml"); cp "$ml" "$W/"

  # ---- verify .mli then .ml ----
  if [ -n "$mli" ]; then
    if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$mlib" ) >"$W/mli.log" 2>&1; then
      pass "$M.mli"
    else
      bad "$M.mli (verify)"; sed 's/^/        | /' "$W/mli.log" | head -12
    fi
  fi
  ml_ok=0
  if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$mlb" ) >"$W/ml.log" 2>&1; then
    pass "$M.ml"; ml_ok=1
  else
    bad "$M.ml (verify)"; sed 's/^/        | /' "$W/ml.log" | head -16
  fi

  # ---- smoke client ----
  if [ -n "$smoke" ]; then
    if [ "$ml_ok" -eq 1 ]; then
      smokeb=$(basename "$smoke"); cp "$smoke" "$W/"
      if ( cd "$W" && "$OC" -vox-solver-path "$LEAN" -c "$smokeb" ) >"$W/smoke.log" 2>&1; then
        pass "smoke_$M.ml"
      else
        bad "smoke_$M.ml (verify)"; sed 's/^/        | /' "$W/smoke.log" | head -12
      fi
    else
      bad "smoke_$M.ml (skipped: $M.ml did not verify)"
    fi
  else
    bad "clients/smoke_$M.ml missing"
  fi

  # ---- POLY: parameterized via, not VoxU (the ONLY sound genericity route) ----
  # Accept ANY type variable ('a, 'v, 'k, ...) -- Vpmap is 'v-valued, Vplist/
  # Vpset are 'a. The point is the via target is PARAMETERIZED by a tyvar, not
  # the shared VoxU (study F-B4).
  if [ -n "$mli" ]; then
    if grep -qE "type[[:space:]]+'[a-z_]+[[:space:]]+t[[:space:]]*:[[:space:]]*value[[:space:]]+refines[[:space:]]*\([[:space:]]*'[a-z_]+[[:space:]]" "$mli" 2>/dev/null; then
      pass "$M poly: 'x t refines a PARAMETERIZED via ('x <sort>), not VoxU"
    else
      warn "$M poly: could not confirm 'x t : value refines ('x <sort>) -- verify the element/value sort is parameterized (study F-B4: the VoxU 'opaque element' cheat is ill-typed)"
    fi
  fi

  # ---- POLY: smoke MUST exercise BOTH int and string instantiations ----
  # The abstract type may be written qualified (`int M.t`) or bare (`int t`,
  # when the smoke `open`s the module) -- accept either. For Vpmap the value
  # slot is what varies (keys are always int); `int t` / `string t` is exactly
  # the value instantiation (a bare key `int` is not followed by `t`).
  if [ -n "$smoke" ]; then
    has_int=$(grep -cE "\bint[[:space:]]+($M\.)?t\b" "$smoke" 2>/dev/null);   has_int=${has_int:-0}
    has_str=$(grep -cE "\bstring[[:space:]]+($M\.)?t\b" "$smoke" 2>/dev/null); has_str=${has_str:-0}
    if [ "$has_int" -gt 0 ] && [ "$has_str" -gt 0 ]; then
      pass "$M poly: smoke instantiates BOTH int ($has_int) and string ($has_str) $M.t (S_param-resolution proof)"
    else
      bad "$M poly: smoke must exercise laws at BOTH int AND string $M.t (found int=$has_int string=$has_str) -- one instantiation does NOT prove genericity (study F-B1)"
    fi
  fi

  # ---- §6.1c: a module may only `open` a DECLARED dependency ----
  deps=" $DEPS "
  opened_re=$(printf '%s' "$KNOWN" | tr ' ' '|')
  for opened in $(grep -oE "^[[:space:]]*open[[:space:]]+($opened_re)\b" "$ml" 2>/dev/null | awk '{print $2}'); do
    case "$deps" in
      *" $opened "*) pass "$M §6.1c: opens declared dep $opened" ;;
      *) bad "$M §6.1c: opens UNDECLARED module $opened -- add it to the MODULES.manifest dep list or remove the edge" ;;
    esac
  done

  # ---- §6.7-liveness: exposed NON-recursive def (Amendment A) ----
  if [ -n "$mli" ]; then
    for d in $(grep -oE '@\[grind, expose\][[:space:]]*public[[:space:]]+def[[:space:]]+[a-zA-Z0-9_]+' "$mli" 2>/dev/null | awk '{print $NF}'); do
      dbody=$(awk -v d="$d" '
        $0 ~ ("def[ ]+" d "([ :({]|$)") {f=1; print; next}
        f && (/^@\[/ || /^public / || /^grind_pattern / || /\|lean\}\]/ || NF==0) {exit}
        f {print}' "$mli")
      printf '%s\n' "$dbody" | grep -q '∀\|∃' && continue
      rec=$(printf '%s\n' "$dbody" | tail -n +2 | grep -c "$d")
      if [ "${rec:-0}" -eq 0 ]; then
        warn "$M §6.7: exposed NON-recursive def '$d' -- grind can unfold it, so laws about it may be DEAD. De-expose (public w/o expose) + ship reduction laws, unless verified live by the removal test (Amendment A)."
      fi
    done
  fi

  # ---- notes file existence + §5 format ----
  # A note "block" is a pain-site, ANCHORED on its `site:` field -- NOT on a
  # heading. This is format-agnostic: builders may use '## ' or '### ' headers,
  # and prose/summary sections (which have headings but no `site:`) don't count.
  # Every well-formed block has each of the 7 §5 fields once, so each field
  # must appear at least as many times as `site:` does.
  if [ -f "$note" ]; then
    nblk=$(grep -cF 'site:' "$note" 2>/dev/null); nblk=${nblk:-0}
    if [ "$nblk" -eq 0 ]; then
      warn "$M: notes/$M.md present but has no 'site:'-anchored note blocks (a clean site still gets a note; §5)"
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
echo "poly wave: $npass PASS, $nfail FAIL, $nwarn WARN"
if [ "$fail" -ne 0 ]; then
  echo "RESULT: FAIL -- wave not acceptable (WARNs are advisory; FAILs block)."
else
  echo "RESULT: PASS -- all wave-3 artifacts verify. Review WARNs before committing."
fi
exit "$fail"
