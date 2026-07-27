#!/bin/sh
# Attribute verification latency to the phase that spends it.
#
# The question this answers is "how much of the wall clock is the refinement
# machinery, and how much is ordinary OCaml compilation that would have been
# paid anyway".  Subtracting one flag configuration from another gives the cost
# of the layer between them, so every row below is a full compile of the same
# unit under the same compiler, differing only in flags and environment.
#
# TWO THINGS MAKE THIS DIFFERENT FROM A NAIVE FLAG SWEEP.
#
# First, the ordinary-OCaml baseline is a hand-written refinement-stripped TWIN
# of the module, not the module itself under a weaker flag.  No vox flag turns
# refinements off; -vox-type-only still parses, elaborates and mode-checks
# them.  Without a twin the ordinary-typechecking row cannot be measured at all,
# and the refinement cost silently disappears into it.  The twin lives in
# twin/, the verbatim module in original/, and --validate diffs their Lambda so
# the twin cannot quietly have dropped real computation.
#
# Second, the phases between "obligation built" and "solver answered" have no
# flag that separates them.  -vox-dump-vc builds obligations and pretty-prints
# them but never runs the SMT translation, the cache-key derivation or the
# lookup, so a decomposition built on it lumps all three together.
#
# Rather than teach the compiler to stop mid-phase -- which cannot be done
# safely, see the report -- --scaling separates them from the outside, by
# measuring modules that differ only in how many obligations they carry and
# fitting a line.  The intercept is the part of the phase that is paid once per
# compiler invocation; the slope is the part paid per obligation.  That answers
# the question the missing flag was wanted for, using stock flags only.
#
# Cache handling: every row runs against a private VOX_SOLVER_CACHE_DIR created
# under TMPDIR.  The shared default cache sits at its size cap with eviction
# active, so any timing taken against it is unstable between sessions.
#
# Usage:
#   vox_attribute_time.sh --ocamlc PATH --stdlib PATH [options]
#     --original DIR     verbatim refined sources      (default ./original)
#     --twin DIR         refinement-stripped twin      (default ./twin)
#     --unit NAMES       modules timed, sans extension (default bst), in the
#                        order given; more than one turns the twin rows off,
#                        since only bst has a twin
#     --deps "A.ml B.ml" units compiled untimed first  (default set_intf.ml)
#     --repeats N        runs per row, median reported (default 7)
#     --backend NAME     lean | z3 | oxsmt | cross     (default z3)
#     --validate         diff the twin's Lambda against the original's, no timing
#     --scaling          fit cost against obligation count over --unit's modules,
#                        separating per-invocation from per-obligation cost
#     --vox-rows-only    skip the twin rows; time the refined sources alone
set -e

OCAMLC=""; STDLIB=""; BACKEND=z3; REPEATS=7
ORIGINAL=./original; TWIN=./twin; UNIT=bst; DEPS="set_intf.ml"
VALIDATE=no; VOX_ROWS_ONLY=no; SCALING=no
SOLVER="${VOX_SMT_SOLVER:-z3 -in}"

while [ $# -gt 0 ]; do
  case "$1" in
    --ocamlc)        OCAMLC=$2; shift 2 ;;
    --stdlib)        STDLIB=$2; shift 2 ;;
    --original)      ORIGINAL=$2; shift 2 ;;
    --twin)          TWIN=$2; shift 2 ;;
    --unit)          UNIT=$2; shift 2 ;;
    --deps)          DEPS=$2; shift 2 ;;
    --repeats)       REPEATS=$2; shift 2 ;;
    --backend)       BACKEND=$2; shift 2 ;;
    --validate)      VALIDATE=yes; shift ;;
    --scaling)       SCALING=yes; shift ;;
    --vox-rows-only) VOX_ROWS_ONLY=yes; shift ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done
[ -n "$OCAMLC" ] && [ -n "$STDLIB" ] || { echo "need --ocamlc and --stdlib" >&2; exit 2; }

TMPDIR=${TMPDIR:-$HOME/tmp}
export TMPDIR
[ -d "$TMPDIR" ] || { echo "TMPDIR $TMPDIR does not exist" >&2; exit 2; }
case "$TMPDIR" in
  /tmp|/tmp/*|/var/tmp|/var/tmp/*)
    echo "refusing to scratch under $TMPDIR; set TMPDIR to a user filesystem" >&2
    exit 2 ;;
esac
FIRST_UNIT=$(echo "$UNIT" | awk '{print $1}')
# Only bst has a hand-written twin, so a multi-unit run is a vox-rows-only run.
if [ "$(echo "$UNIT" | wc -w)" -gt 1 ]; then VOX_ROWS_ONLY=yes; fi
[ "$SCALING" = yes ] && VOX_ROWS_ONLY=yes

SCRATCH=$(mktemp -d "$TMPDIR/vox-latency.XXXXXX")
trap 'rm -rf "$SCRATCH"' EXIT

OCAMLC=$(cd "$(dirname "$OCAMLC")" && pwd)/$(basename "$OCAMLC")
STDLIB=$(cd "$STDLIB" && pwd)
ORIGINAL=$(cd "$ORIGINAL" && pwd)
[ "$VOX_ROWS_ONLY" = yes ] || TWIN=$(cd "$TWIN" && pwd)

# ---------------------------------------------------------------- workspaces
# Each variant gets its own directory holding a copy of its sources, so an
# artefact from one can never be picked up by the other.
stage() {  # stage SOURCE_DIR NAME -> echoes the staged directory
  _dir=$SCRATCH/$2
  mkdir -p "$_dir"
  cp "$1"/* "$_dir"/ 2>/dev/null || true
  echo "$_dir"
}

compile() {  # compile DIR FLAGS FILE
  _dir=$1; _flags=$2; _file=$3
  # shellcheck disable=SC2086
  ( cd "$_dir" && "$OCAMLC" -nostdlib -I "$STDLIB" -I . $_flags -c "$_file" )
}

# The dependencies and the interface are built once, untimed, and left in
# place.  Every timed row then compiles exactly the same unit against exactly
# the same .cmi set, which is what makes the rows subtractable.
prelude() {  # prelude DIR [FLAGS]
  _dir=$1
  _pflags=${2:--vox-backend $BACKEND}
  for _dep in $DEPS; do
    [ -f "$_dir/$_dep" ] || continue
    VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
      compile "$_dir" "$_pflags" "$_dep" >/dev/null 2>&1
  done
  for _unit in $UNIT; do
    [ -f "$_dir/$_unit.mli" ] || continue
    VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
      compile "$_dir" "$_pflags" "$_unit.mli" >/dev/null 2>&1
  done
}

# --------------------------------------------------------------- validation
# The twin is only a usable baseline if it is the same program with the proofs
# removed.  Compare the Lambda of both: the refined module's extra bindings are
# the six [@vox.def] companion lemmas, which the twin declares by hand, so the
# two Lambdas should agree on every ordinary definition.
validate() {
  mkdir -m 700 -p "$SCRATCH/prelude-cache"; PRELUDE_CACHE=$SCRATCH/prelude-cache
  _o=$(stage "$ORIGINAL" validate-original)
  _t=$(stage "$TWIN" validate-twin)
  prelude "$_o"; prelude "$_t"
  # The refined unit must be dumped WITH verification on: its .cmi was built
  # by the verifying prelude, and the compiler refuses to check a verified
  # interface from a compilation that does not verify.  Lambda is emitted after
  # verification either way, so this does not change what is dumped.
  VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
    compile "$_o" "-vox-backend $BACKEND -dno-unique-ids -dlambda" "$FIRST_UNIT.ml" \
      2>"$SCRATCH/original.lambda" >"$SCRATCH/original.out" || true
  compile "$_t" "-dno-unique-ids -dlambda" "$FIRST_UNIT.ml" \
    2>"$SCRATCH/twin.lambda" >"$SCRATCH/twin.out" || true
  # -dlambda writes to stderr; a genuine diagnostic would appear there too,
  # so refuse to compare a dump that does not start with the module's [let].
  for _side in original twin; do
    if ! head -1 "$SCRATCH/$_side.lambda" | grep -q '^(let'; then
      echo "$_side did not produce a Lambda dump:"
      sed -n '1,8p' "$SCRATCH/$_side.lambda"; exit 1
    fi
  done
  echo "original Lambda: $(wc -l <"$SCRATCH/original.lambda") lines"
  echo "twin Lambda:     $(wc -l <"$SCRATCH/twin.lambda") lines"
  echo
  # A raw line diff is unreadable here: dropping the [L] locality markers
  # rewraps the pretty-printer's output, so almost every line moves.  Compare
  # token streams instead, with the two things the stripping is ALLOWED to
  # change removed -- the [L] markers themselves and the {nlocal = n} counts,
  # both of which are mode annotations and neither of which is computation.
  # Anything else the diff reports is a real divergence between the twin and
  # the module it claims to stand in for.
  python3 - "$SCRATCH/original.lambda" "$SCRATCH/twin.lambda" <<'VALIDATE_PY'
import difflib, re, sys

def tokens(path):
    text = open(path).read()
    text = re.sub(r"\{nlocal = \d+\}", "", text).replace("[L]", "")
    return re.findall(r"[A-Za-z_][A-Za-z0-9_.']*|\d+|%[a-z_]+|\S", text)

left, right = tokens(sys.argv[1]), tokens(sys.argv[2])
print("tokens, modulo mode annotations: original %d, twin %d"
      % (len(left), len(right)))
matcher = difflib.SequenceMatcher(a=left, b=right, autojunk=False)
regions = [op for op in matcher.get_opcodes() if op[0] != "equal"]
if not regions:
    print("IDENTICAL modulo mode annotations.")
    sys.exit(0)
print("%d differing regions:" % len(regions))
for tag, i1, i2, j1, j2 in regions:
    print("  %s" % tag)
    print("    original ...%s >>>%s<<< %s..."
          % (" ".join(left[max(0, i1 - 6):i1]), " ".join(left[i1:i2]),
             " ".join(left[i2:i2 + 6])))
    print("    twin     ...%s >>>%s<<< %s..."
          % (" ".join(right[max(0, j1 - 6):j1]), " ".join(right[j1:j2]),
             " ".join(right[j2:j2 + 6])))
VALIDATE_PY
  echo
  echo "object code:"
  printf '  %-12s %s bytes\n' original "$(wc -c <"$_o/$FIRST_UNIT.cmo")"
  printf '  %-12s %s bytes\n' twin     "$(wc -c <"$_t/$FIRST_UNIT.cmo")"
}

# ------------------------------------------------------------------- timing
# Median of REPEATS.  The unit's own artefacts are removed before each run so
# every run is a full compile; the .cmi files from the prelude stay, and the
# solver cache is whatever the caller set up.
timed() {  # timed DIR FLAGS [PER-REPEAT SETUP]  (environment set by caller)
  _dir=$1; _flags=$2; _setup=$3
  : >"$SCRATCH/times"
  _i=0
  while [ "$_i" -lt "$REPEATS" ]; do
    for _unit in $UNIT; do
      rm -f "$_dir/$_unit.cmo" "$_dir/$_unit.cmt" "$_dir/$_unit.cmti" \
        2>/dev/null || true
    done
    [ -z "$_setup" ] || eval "$_setup"
    _start=$(date +%s.%N)
    for _unit in $UNIT; do
      compile "$_dir" "$_flags" "$_unit.ml" >/dev/null 2>&1 || true
    done
    _end=$(date +%s.%N)
    awk -v s="$_start" -v e="$_end" 'BEGIN{printf "%.6f\n", e-s}' >>"$SCRATCH/times"
    _i=$((_i + 1))
  done
  sort -n "$SCRATCH/times" \
    | awk '{a[NR]=$1} END{printf "%.4f %.4f %.4f", a[int((NR+1)/2)], a[1], a[NR]}'
}

# Confirm a row actually did the work it claims, rather than failing early.
# A row whose compile exits non-zero for a reason other than the flag's own
# design is not a measurement of anything.
check_row() {  # check_row DIR FLAGS LABEL
  _dir=$1; _flags=$2; _label=$3
  if VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
       compile "$_dir" "$_flags" "$FIRST_UNIT.ml" >"$SCRATCH/check.out" 2>&1; then
    echo "ok"
  else
    case "$_flags" in
      *dump-vc*) echo "ok (dump exits non-zero by design)" ;;
      *) echo "FAILED: $_label"; sed -n '1,6p' "$SCRATCH/check.out" ;;
    esac
  fi
}

row() {  # row LABEL VALUE_TRIPLE
  printf '  %-34s %8s   [%s .. %s]\n' "$1" "$2" "$3" "$4"
}

sub() { awk -v a="$1" -v b="$2" 'BEGIN{printf "%.4f", a-b}'; }
pct() { awk -v a="$1" -v b="$2" 'BEGIN{printf "%5.1f%%", (b==0?0:100*a/b)}'; }

# Count the obligations a module carries, by asking for the VC dump as JSON.
obligation_count() {  # obligation_count DIR UNIT
  # Remove the previous dump first.  A failed compile leaves the old file in
  # place, and the count is then silently the previous module's.
  rm -f "$SCRATCH/vc.json"
  VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
    compile "$1" "-vox-backend $BACKEND -vox-dump-vc-json $SCRATCH/vc.json" \
      "$2.ml" >/dev/null 2>&1 || true
  python3 -c 'import json,sys; print(len(json.load(open(sys.argv[1]))["verification_conditions"]))' \
    "$SCRATCH/vc.json" 2>/dev/null || echo 0
}

# Separate per-invocation cost from per-obligation cost without stopping the
# compiler mid-phase.  Each module is compiled alone under three
# configurations; the differences are fitted against the module's obligation
# count by least squares.  An intercept is work the compiler does once however
# many obligations it faces; a slope is work it does per obligation.
scaling() {
  mkdir -m 700 -p "$SCRATCH/cold-cache" "$SCRATCH/warm-cache" \
    "$SCRATCH/prelude-cache"
  COLD_CACHE=$SCRATCH/cold-cache; WARM_CACHE=$SCRATCH/warm-cache
  PRELUDE_CACHE=$SCRATCH/prelude-cache
  _vd=$(stage "$ORIGINAL" scaling-verified);  prelude "$_vd"
  _nd=$(stage "$ORIGINAL" scaling-noverify);  prelude "$_nd" "-vox-no-verify"

  # Pull the compiler and solver binaries into the page cache before timing
  # anything.  Deriving a cache key reads both of them in full -- 64 MB between
  # them -- and the prelude does not, because interfaces and set_intf.ml carry
  # no obligations and so never ask for a key.  Without this the FIRST module
  # measured pays the disk read and reads ~0.09s slower than it should, in both
  # its warm and its cold row, which looks exactly like a module whose solving
  # is free.
  _first=$(echo "$UNIT" | awk '{print $1}')
  VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
    compile "$_vd" "-vox-backend $BACKEND" "$_first.ml" >/dev/null 2>&1 || true

  echo "module      obligs   no-verify      warm       cold   warm-nv   cold-warm"
  : >"$SCRATCH/fit"
  _saved=$UNIT
  for _m in $_saved; do
    UNIT=$_m
    _n=$(obligation_count "$_vd" "$_m")
    _nv=$(VOX_SOLVER_CACHE=0 timed "$_nd" "-vox-no-verify" | awk '{print $1}')
    VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
      timed "$_vd" "-vox-backend $BACKEND" >/dev/null
    _w=$(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
           timed "$_vd" "-vox-backend $BACKEND" | awk '{print $1}')
    _c=$(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$COLD_CACHE \
           timed "$_vd" "-vox-backend $BACKEND" \
             'rm -rf "$COLD_CACHE"; mkdir -m 700 -p "$COLD_CACHE"' \
           | awk '{print $1}')
    printf '%-10s %6s  %9s %9s %9s  %8s  %10s\n' "$_m" "$_n" "$_nv" "$_w" "$_c" \
      "$(sub "$_w" "$_nv")" "$(sub "$_c" "$_w")"
    printf '%s %s %s\n' "$_n" "$(sub "$_w" "$_nv")" "$(sub "$_c" "$_w")" \
      >>"$SCRATCH/fit"
  done
  UNIT=$_saved
  echo
  python3 - "$SCRATCH/fit" <<'FIT_PY'
import sys

rows = [tuple(float(f) for f in line.split())
        for line in open(sys.argv[1]) if line.strip()]

def fit(xs, ys):
    n = len(xs)
    mx, my = sum(xs) / n, sum(ys) / n
    sxx = sum((x - mx) ** 2 for x in xs)
    sxy = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    slope = sxy / sxx
    intercept = my - slope * mx
    ss_tot = sum((y - my) ** 2 for y in ys)
    ss_res = sum((y - (intercept + slope * x)) ** 2 for x, y in zip(xs, ys))
    r2 = 1 - ss_res / ss_tot if ss_tot else float("nan")
    return intercept, slope, r2

xs = [r[0] for r in rows]
for label, index, note in [
        ("translation + key + lookup  (warm - no-verify)", 1,
         "intercept = paid once per compiler invocation"),
        ("solving + 2nd emission      (cold - warm)", 2,
         "intercept should be near zero: solving is per-obligation"),
]:
    intercept, slope, r2 = fit(xs, [r[index] for r in rows])
    print("%s" % label)
    print("    fixed per invocation  %8.4f s      <- %s" % (intercept, note))
    print("    per obligation        %8.5f s" % slope)
    print("    r-squared             %8.4f" % r2)
    print()
FIT_PY
}

if [ "$VALIDATE" = yes ]; then validate; exit 0; fi
if [ "$SCALING" = yes ]; then
  echo "scaling fit over: $UNIT"
  echo "compiler:  $OCAMLC"
  echo "backend:   $BACKEND, median of $REPEATS runs, private solver cache"
  echo "load:      $(uptime | sed 's/.*load average: //')  ($(nproc) cpus)"
  echo
  scaling
  exit 0
fi

# ------------------------------------------------------------------ measure
# The cache refuses any directory that is not private, and refuses it
# SILENTLY -- a 0755 directory reads as a permanent miss, which makes a cold
# run and a warm run indistinguishable and makes solving look free.  Create
# them 0700 and check.
COLD_CACHE=$SCRATCH/cold-cache
WARM_CACHE=$SCRATCH/warm-cache
PRELUDE_CACHE=$SCRATCH/prelude-cache
mkdir -m 700 -p "$COLD_CACHE" "$WARM_CACHE" "$PRELUDE_CACHE"

OD=$(stage "$ORIGINAL" original); prelude "$OD"
# -vox-no-verify cannot be applied to one unit of an otherwise verified build:
# the compiler refuses to check an implementation against an interface that was
# verified when the implementation will not be.  So the no-verify row gets its
# own workspace whose dependencies and interface were also built that way.  The
# two workspaces hold the same sources and differ only in whether the .cmi
# records that its obligations were discharged.
ND=$(stage "$ORIGINAL" original-noverify); prelude "$ND" "-vox-no-verify"
if [ "$VOX_ROWS_ONLY" = no ]; then TD=$(stage "$TWIN" twin); prelude "$TD"; fi

echo "units:     $(for _u in $UNIT; do printf '%s.ml ' "$_u"; done)"
echo "original:  $ORIGINAL"
[ "$VOX_ROWS_ONLY" = no ] && echo "twin:      $TWIN"
echo "compiler:  $OCAMLC"
echo "backend:   $BACKEND, median of $REPEATS runs, private solver cache"
# This box is shared.  A heavy neighbour inflates every row, and not evenly, so
# record the load with the numbers rather than leaving the reader to guess.
echo "load:      $(uptime | sed 's/.*load average: //')  ($(nproc) cpus)"
echo

echo "row sanity"
printf '  %-34s %s\n' "original verify" "$(check_row "$OD" "-vox-backend $BACKEND" orig-verify)"
printf '  %-34s %s\n' "original -vox-no-verify" "$(check_row "$ND" "-vox-no-verify" orig-no-verify)"
printf '  %-34s %s\n' "original -vox-type-only" "$(check_row "$OD" "-vox-type-only" orig-type-only)"
if [ "$VOX_ROWS_ONLY" = no ]; then
  printf '  %-34s %s\n' "twin plain compile" "$(check_row "$TD" "" twin-compile)"
fi
echo

# Every row pays for starting the compiler, loading the stdlib .cmi files and
# writing an artefact, whether or not it typechecks anything.  On this compiler
# that floor is tens of milliseconds -- comparable to the whole of ordinary
# typechecking for a module this size -- so leaving it inside the first row
# would report compiler startup as the cost of OCaml typechecking.  Measure it
# with the same harness, on an empty unit, and give it its own row.
# One empty unit per timed unit: a corpus row runs the compiler once per
# module, so it pays the floor once per module too.  Measuring a single empty
# compile and subtracting it once would leave the other invocations' startup
# inside the typechecking row.
FLOORDIR=$SCRATCH/floor; mkdir -p "$FLOORDIR"
_saved_unit=$UNIT; UNIT=""; _n=0
for _u in $_saved_unit; do
  _n=$((_n + 1)); : >"$FLOORDIR/empty$_n.ml"; UNIT="$UNIT empty$_n"
done
set -- $(timed "$FLOORDIR" "");                                  FLOOR=$1
UNIT=$_saved_unit
echo "configuration                       median      [min .. max]"
row "      compiler startup floor (x$_n)" "$FLOOR" "$2" "$3"

if [ "$VOX_ROWS_ONLY" = no ]; then
  set -- $(VOX_SOLVER_CACHE=0 timed "$TD" "-vox-type-only");     TW_TYPE=$1; row "twin  -vox-type-only" "$1" "$2" "$3"
  set -- $(VOX_SOLVER_CACHE=0 timed "$TD" "");                   TW_FULL=$1; row "twin  plain compile"  "$1" "$2" "$3"
fi
set -- $(VOX_SOLVER_CACHE=0 timed "$OD" "-vox-type-only");       OR_TYPE=$1; row "orig  -vox-type-only" "$1" "$2" "$3"
set -- $(VOX_SOLVER_CACHE=0 timed "$ND" "-vox-no-verify");       OR_NOVF=$1; row "orig  -vox-no-verify" "$1" "$2" "$3"
set -- $(VOX_SOLVER_CACHE=0 timed "$OD" "-vox-dump-vc");         OR_DUMP=$1; row "orig  -vox-dump-vc"   "$1" "$2" "$3"

# Warm: fill the private cache once, untimed, then measure against it hot.
VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
  timed "$OD" "-vox-backend $BACKEND" >/dev/null

set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
           timed "$OD" "-vox-backend $BACKEND");                 OR_WARM=$1; row "orig  verify, warm cache" "$1" "$2" "$3"

# Cold: an empty private directory, not VOX_SOLVER_CACHE=0.  Disabling the
# cache takes a different code path and the two are not comparable.
# Emptying it once is not enough: the first repeat would populate it and every
# later repeat would be a warm run, which drags the median down to the warm
# figure and makes solving look free.  Empty it before EVERY repeat.
set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$COLD_CACHE \
           timed "$OD" "-vox-backend $BACKEND" \
             'rm -rf "$COLD_CACHE"; mkdir -m 700 -p "$COLD_CACHE"'); \
                                                                 OR_COLD=$1; row "orig  verify, cold cache" "$1" "$2" "$3"

echo
echo "attributed to                       seconds    share of cold total"
attribute() { printf '  %-34s %8s   %s\n' "$1" "$2" "$(pct "$2" "$OR_COLD")"; }

if [ "$VOX_ROWS_ONLY" = no ]; then
  attribute "compiler startup, $_n invocation(s)" "$FLOOR"
  attribute "ordinary OCaml typecheck"        "$(sub "$TW_TYPE" "$FLOOR")"
  attribute "refinement elaboration + modes"  "$(sub "$OR_TYPE" "$TW_TYPE")"
  attribute "ordinary artefact staging"       "$(sub "$TW_FULL" "$TW_TYPE")"
  attribute "refined conformance + cmi/cmo"   \
    "$(sub "$(sub "$OR_NOVF" "$OR_TYPE")" "$(sub "$TW_FULL" "$TW_TYPE")")"
else
  attribute "compiler startup, $_n invocation(s)" "$FLOOR"
  attribute "typecheck + refinements"         "$(sub "$OR_TYPE" "$FLOOR")"
  attribute "conformance + cmi/cmo"           "$(sub "$OR_NOVF" "$OR_TYPE")"
fi

attribute "obligation construction"           "$(sub "$OR_DUMP" "$OR_NOVF")"
attribute "translation + key + lookup"        "$(sub "$OR_WARM" "$OR_DUMP")"
attribute "solving, cold"                     "$(sub "$OR_COLD" "$OR_WARM")"
echo
echo "  -vox-dump-vc builds obligations without translating, keying or looking"
echo "  any of them up, so it separates obligation construction from the three"
echo "  phases after it -- but nothing separates those three from each other."
echo "  Run --scaling to split them into a per-invocation and a per-obligation"
echo "  part, which is what the missing flag was wanted for."
echo "  Note the dump also formats every obligation for printing, so the"
echo "  construction row above is an upper bound on construction alone."
echo
echo "  a perfect cache saves the solving row and nothing above it:"
printf '  %-34s %8s   %s\n' "cold total" "$OR_COLD" "$(pct "$OR_COLD" "$OR_COLD")"
printf '  %-34s %8s   %s\n' "warm total" "$OR_WARM" "$(pct "$OR_WARM" "$OR_COLD")"
