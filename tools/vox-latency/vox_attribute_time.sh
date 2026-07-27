#!/bin/sh
# Attribute verification latency to the phase that spends it.
#
# The question this answers is "how much of the wall clock is the refinement
# machinery, and how much is ordinary OCaml compilation that would have been
# paid anyway".  Subtracting one flag configuration from another gives the cost
# of the layer between them, so every row below is a full compile of the same
# unit under the same compiler, differing only in flags and environment.
#
# THREE THINGS MAKE THIS DIFFERENT FROM A NAIVE FLAG SWEEP.
#
# First, the ordinary-OCaml baseline is a hand-written refinement-stripped TWIN
# of the module, not the module itself under a weaker flag.  No vox flag turns
# refinements off; -vox-type-only still parses, elaborates and mode-checks
# them.  Without a twin the ordinary-typechecking row cannot be measured at all,
# and the refinement cost silently disappears into it.  The twin lives in
# twin/, the verbatim module in original/, and --validate compares their Lambda
# and FAILS if they differ, so the twin cannot quietly have dropped real
# computation or changed how the program allocates.
#
# Second, the phases between "obligation built" and "solver answered" have no
# flag that separates them.  -vox-dump-vc builds obligations and pretty-prints
# them but never runs the SMT translation, the cache-key derivation or the
# lookup, so a decomposition built on it lumps all three together.  Worse, and
# this is why no row of the table below is derived from it: -vox-dump-vc EXITS
# BEFORE EMITTING BYTECODE.  It writes no .cmo, and every other configuration
# here does, so dump minus no-verify is (obligation construction + formatting)
# MINUS (bytecode emission) -- two quantities of the same size on a module like
# bst.ml, which lands the row at the noise floor with an arbitrary sign and
# makes it look as though obligations were free to build.  The dump row is
# still measured and printed, as a diagnostic with that boundary stated.
#
# Rather than teach the compiler to stop mid-phase -- which cannot be done
# safely, see the report -- --scaling separates them from the outside, by
# measuring modules that differ only in how many obligations they carry and
# fitting a line.  The intercept is the part of the phase that is paid once per
# compiler invocation; the slope is the part paid per obligation.  That answers
# the question the missing flag was wanted for, using stock flags only.
#
# Third, EVERY DECOMPOSITION THIS TOOL PRINTS CARRIES A ROW WHOSE VALUE IS
# KNOWN BEFORE THE MEASUREMENT IS TAKEN, and prints it whether or not it is
# flattering.  A decomposition with no such row cannot tell a correct
# attribution from a plausible one.
#
#   default   a generated ONE-OBLIGATION control module is put through the same
#             rows.  Any row that is genuinely per-obligation must be near zero
#             for it.  Whatever the control still pays is a per-invocation
#             charge that the same row on the real module is also paying, and
#             is not what that row is named after.  This is the check that
#             caught a retracted "solving is 44%" reading of the cold-minus-warm
#             row: on the control that row is ~0.10 s, and a one-obligation
#             module does not spend 0.10 s solving.
#             The default rows telescope to the cold total by construction, so
#             the cold row is ALSO re-measured after every other row; those two
#             independent measurements of the same quantity are the table's
#             residual.
#   --scaling the (cold - warm) fit's intercept must be near zero, because
#             solving is per-obligation.  Printed with the fit residuals.
#   --validate the twin must be Lambda-identical to the original modulo mode
#             annotations.  The expected region count is zero and any region at
#             all is an error exit.
#
# Cache handling: every row runs against a private VOX_SOLVER_CACHE_DIR created
# under TMPDIR.  The shared default cache sits at its size cap with eviction
# active, so any timing taken against it is unstable between sessions.  The
# cache also refuses a directory that is not mode 0700, and refuses it in
# SILENCE, which makes a cold run and a warm run identical and solving look
# free; every directory here is created 0700.
#
# WHICH COMPILER THE ABSOLUTE NUMBERS DESCRIBE.  Two of the largest rows this
# tool reports -- the cache-key digest of the compiler and solver binaries, and
# the solver-teardown sleep -- are properties of the base compiler and are
# removed on the cache lane, which replaced the binary digest with a filesystem
# stamp and the flat 100 ms sleep with a 200 us backoff.  On a compiler
# carrying those two changes both fixed rows below are absent and the rest of
# the table is unchanged.  Read every absolute figure as "on the compiler
# passed to --ocamlc", which the header of each run records.
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
#     --validate         compare the twin's Lambda against the original's and
#                        exit non-zero on any difference; no timing
#     --scaling          fit cost against obligation count over --unit's modules,
#                        separating per-invocation from per-obligation cost
#     --vox-rows-only    skip the twin rows; time the refined sources alone
#
# Exit status: 0 measured, 2 bad usage, 1 twin validation failed, 3 a compile
# that should have succeeded did not, so no table is printed.
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
  # A VAR=value prefix on a FUNCTION call persists in the caller's shell under
  # /bin/sh, still exported, so every such call here is wrapped in a subshell.
  # Left unwrapped these would leak the prelude's cache directory into every
  # later row that does not set one explicitly.
  for _dep in $DEPS; do
    [ -f "$_dir/$_dep" ] || continue
    ( VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
        compile "$_dir" "$_pflags" "$_dep" >/dev/null 2>&1 )
  done
  for _unit in $UNIT; do
    [ -f "$_dir/$_unit.mli" ] || continue
    ( VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
        compile "$_dir" "$_pflags" "$_unit.mli" >/dev/null 2>&1 )
  done
}

# ------------------------------------------------------- failure propagation
# A compile that failed is not a timing row.  Every timed and every sanity
# compile classifies its exit status, and a status that is not accounted for
# stops the whole run rather than contributing a number.
#
# Most of this file's measurement calls happen inside $( ), where an exit
# cannot end the run, so a failure is recorded in a file and the caller checks
# for it.  Exit codes through a pipe to awk are worse still and are never
# relied on here.
record_fatal() {  # record_fatal MESSAGE DIAGFILE
  [ -f "$SCRATCH/fatal" ] && return 0
  { echo "MEASUREMENT ABORTED: $1"
    echo "A compile that failed is not a timing row, so no table is printed."
    echo "First lines of the compiler's diagnostic:"
    sed -n '1,10p' "$2" 2>/dev/null
  } >"$SCRATCH/fatal"
  return 0
}

abort_if_failed() {
  if [ -f "$SCRATCH/fatal" ]; then
    echo >&2
    cat "$SCRATCH/fatal" >&2
    exit 3
  fi
}

capitalise() { # capitalise NAME -> Name
  printf '%s%s' \
    "$(printf '%s' "$1" | cut -c1 | tr '[:lower:]' '[:upper:]')" \
    "$(printf '%s' "$1" | cut -c2-)"
}

# -vox-type-only writes no .cmi.  In a multi-unit row a later unit that depends
# on an earlier one therefore cannot find its interface, and fails for a reason
# that is the flag's own design rather than a defect in the module.  Recognise
# that case by the diagnostic naming another unit of this same row, NOT by the
# flag alone: treating every type-only failure as expected is exactly how a
# module that does not compile becomes a timing row.
expected_no_cmi() {  # expected_no_cmi DIAGFILE
  for _xu in $UNIT; do
    if grep -q -e "Unbound module $(capitalise "$_xu")" -e "$_xu\.cmi" "$1"
    then return 0
    fi
  done
  return 1
}

# Is a non-zero exit accounted for by the flags it was produced under?
accounted_for() {  # accounted_for RC FLAGS DIAGFILE
  [ "$1" = 0 ] && return 0
  # -vox-dump-vc reports that it dumped rather than discharged, and exits
  # non-zero to say so.  -vox-dump-vc-json does not and is deliberately not
  # matched here.
  case " $2 " in *" -vox-dump-vc "*) return 0 ;; esac
  case " $2 " in
    *" -vox-type-only "*) expected_no_cmi "$3" && return 0 ;;
  esac
  return 1
}

# --------------------------------------------------------------- validation
# The twin is only a usable baseline if it is the same program with the proofs
# removed -- the same definitions, the same calls, and the same allocation.
# Compare the Lambda of both, modulo mode annotations, and require that they
# agree exactly.  The refined module's extra bindings are the six [@vox.def]
# companion lemmas, which the twin declares by hand.
validate() {
  mkdir -m 700 -p "$SCRATCH/prelude-cache"; PRELUDE_CACHE=$SCRATCH/prelude-cache
  _o=$(stage "$ORIGINAL" validate-original)
  _t=$(stage "$TWIN" validate-twin)
  prelude "$_o"; prelude "$_t"
  # The refined unit must be dumped WITH verification on: its .cmi was built
  # by the verifying prelude, and the compiler refuses to check a verified
  # interface from a compilation that does not verify.  Lambda is emitted after
  # verification either way, so this does not change what is dumped.
  ( VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
      compile "$_o" "-vox-backend $BACKEND -dno-unique-ids -dlambda" "$FIRST_UNIT.ml" \
        2>"$SCRATCH/original.lambda" >"$SCRATCH/original.out" ) || true
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
  echo "known in advance: the expected number of differing regions is ZERO."
  echo "The twin keeps every mode annotation that has a code consequence, so"
  echo "there is no allowance to argue about and no region is acceptable."
  echo
  # A raw line diff is unreadable here: dropping the [L] locality markers
  # rewraps the pretty-printer's output, so almost every line moves.  Compare
  # token streams instead, with the two things the stripping is ALLOWED to
  # change removed -- the [L] markers themselves and the {nlocal = n} counts,
  # both of which are mode annotations and neither of which is computation.
  # Anything else the diff reports is a real divergence between the twin and
  # the module it claims to stand in for, and is an error exit.
  _rc=0
  python3 - "$SCRATCH/original.lambda" "$SCRATCH/twin.lambda" <<'VALIDATE_PY' || _rc=$?
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
print("%d differing regions, expected 0:" % len(regions))
for tag, i1, i2, j1, j2 in regions:
    print("  %s" % tag)
    print("    original ...%s >>>%s<<< %s..."
          % (" ".join(left[max(0, i1 - 6):i1]), " ".join(left[i1:i2]),
             " ".join(left[i2:i2 + 6])))
    print("    twin     ...%s >>>%s<<< %s..."
          % (" ".join(right[max(0, j1 - 6):j1]), " ".join(right[j1:j2]),
             " ".join(right[j2:j2 + 6])))
sys.exit(1)
VALIDATE_PY
  echo
  echo "object code:"
  _osz=$(wc -c <"$_o/$FIRST_UNIT.cmo")
  _tsz=$(wc -c <"$_t/$FIRST_UNIT.cmo")
  printf '  %-12s %s bytes\n' original "$_osz"
  printf '  %-12s %s bytes\n' twin     "$_tsz"
  if [ "$_osz" != "$_tsz" ]; then
    echo "  DIFFER: the twin does not stand in for the original."
    _rc=1
  fi
  if [ "$_rc" != 0 ]; then
    echo
    echo "TWIN VALIDATION FAILED.  The ordinary-OCaml row measured against this"
    echo "twin would not be measuring the same program."
    return 1
  fi
  echo
  echo "TWIN VALIDATED: same program, refinement predicates removed."
  return 0
}

# ------------------------------------------------------------------- timing
# Median of REPEATS.  The unit's own artefacts are removed before each run so
# every run is a full compile; the .cmi files from the prelude stay, and the
# solver cache is whatever the caller set up.
#
# Every compile's exit status is classified.  An unaccounted-for failure
# records a fatal and stops producing numbers; the caller must call
# abort_if_failed.
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
      _rc=0
      compile "$_dir" "$_flags" "$_unit.ml" >"$SCRATCH/timed.err" 2>&1 || _rc=$?
      if ! accounted_for "$_rc" "$_flags" "$SCRATCH/timed.err"; then
        record_fatal "$_unit.ml exited $_rc under [$_flags] in $_dir" \
          "$SCRATCH/timed.err"
        return 3
      fi
    done
    _end=$(date +%s.%N)
    awk -v s="$_start" -v e="$_end" 'BEGIN{printf "%.6f\n", e-s}' >>"$SCRATCH/times"
    _i=$((_i + 1))
  done
  sort -n "$SCRATCH/times" \
    | awk '{a[NR]=$1} END{printf "%.4f %.4f %.4f", a[int((NR+1)/2)], a[1], a[NR]}'
}

# Confirm a row actually does the work it claims, rather than failing early --
# for EVERY unit of the row, not just the first, since a corpus row is only as
# good as its last module.
check_row() {  # check_row DIR FLAGS LABEL
  _dir=$1; _flags=$2; _label=$3; _verdict=ok; _note=""
  for _cu in $UNIT; do
    _crc=0
    ( VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
        compile "$_dir" "$_flags" "$_cu.ml" >"$SCRATCH/check.out" 2>&1 ) || _crc=$?
    if [ "$_crc" != 0 ]; then
      if accounted_for "$_crc" "$_flags" "$SCRATCH/check.out"; then
        case " $_flags " in
          *" -vox-dump-vc "*) _note="dump exits non-zero by design" ;;
          *) _note="$_cu: no .cmi under -vox-type-only" ;;
        esac
      else
        record_fatal "row sanity: $_label, $_cu.ml exited $_crc under [$_flags]" \
          "$SCRATCH/check.out"
        _verdict="FAILED: $_label ($_cu.ml)"
      fi
    fi
  done
  if [ -n "$_note" ] && [ "$_verdict" = ok ]; then _verdict="ok ($_note)"; fi
  echo "$_verdict"
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
  _orc=0
  ( VOX_SOLVER_CACHE_DIR=$PRELUDE_CACHE VOX_SMT_SOLVER="$SOLVER" \
      compile "$1" "-vox-backend $BACKEND -vox-dump-vc-json $SCRATCH/vc.json" \
        "$2.ml" >"$SCRATCH/vc.err" 2>&1 ) || _orc=$?
  if [ "$_orc" != 0 ]; then
    record_fatal "obligation count: $2.ml exited $_orc under -vox-dump-vc-json" \
      "$SCRATCH/vc.err"
    echo 0; return 0
  fi
  if ! python3 -c 'import json,sys; print(len(json.load(open(sys.argv[1]))["verification_conditions"]))' \
         "$SCRATCH/vc.json" 2>"$SCRATCH/vc.err"; then
    record_fatal "obligation count: $2.ml produced no readable VC dump" \
      "$SCRATCH/vc.err"
    echo 0
  fi
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
  ( VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
      compile "$_vd" "-vox-backend $BACKEND" "$_first.ml" >/dev/null 2>&1 ) || true

  # Every module is sanity-checked under both configurations it will be timed
  # under, before any of them is timed.  A module that does not compile must
  # not reach the fit: it contributes a data point whose cost is the cost of
  # failing early, and the fit will happily report a negative per-obligation
  # cost with an r-squared of 1.
  echo "row sanity, per module"
  _saved=$UNIT
  for _m in $_saved; do
    UNIT=$_m
    printf '  %-14s verify %-34s no-verify %s\n' "$_m" \
      "$(check_row "$_vd" "-vox-backend $BACKEND" "scaling-verify-$_m")" \
      "$(check_row "$_nd" "-vox-no-verify" "scaling-noverify-$_m")"
  done
  UNIT=$_saved
  abort_if_failed
  echo

  echo "module      obligs   no-verify      warm       cold   warm-nv   cold-warm"
  : >"$SCRATCH/fit"
  _saved=$UNIT
  for _m in $_saved; do
    UNIT=$_m
    _n=$(obligation_count "$_vd" "$_m"); abort_if_failed
    _nv=$(VOX_SOLVER_CACHE=0 timed "$_nd" "-vox-no-verify" | awk '{print $1}')
    abort_if_failed
    ( VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
        timed "$_vd" "-vox-backend $BACKEND" >/dev/null )
    abort_if_failed
    _w=$(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
           timed "$_vd" "-vox-backend $BACKEND" | awk '{print $1}')
    abort_if_failed
    _c=$(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$COLD_CACHE \
           timed "$_vd" "-vox-backend $BACKEND" \
             'rm -rf "$COLD_CACHE"; mkdir -m 700 -p "$COLD_CACHE"' \
           | awk '{print $1}')
    abort_if_failed
    printf '%-10s %6s  %9s %9s %9s  %8s  %10s\n' "$_m" "$_n" "$_nv" "$_w" "$_c" \
      "$(sub "$_w" "$_nv")" "$(sub "$_c" "$_w")"
    printf '%s %s %s %s\n' "$_n" "$(sub "$_w" "$_nv")" "$(sub "$_c" "$_w")" "$_m" \
      >>"$SCRATCH/fit"
  done
  UNIT=$_saved
  echo
  python3 - "$SCRATCH/fit" <<'FIT_PY'
import sys

rows = []
for line in open(sys.argv[1]):
    if not line.strip():
        continue
    n, a, b, name = line.split()
    rows.append((float(n), float(a), float(b), name))

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
    rmse = (ss_res / n) ** 0.5
    return intercept, slope, r2, rmse

xs = [r[0] for r in rows]
names = [r[3] for r in rows]
solving = None
for label, index, note in [
        ("obligation construction + translation + key + lookup"
         "  (warm - no-verify)", 1,
         "intercept = paid once per compiler invocation"),
        ("solving + 2nd emission      (cold - warm)", 2,
         "intercept should be near zero: solving is per-obligation"),
]:
    ys = [r[index] for r in rows]
    intercept, slope, r2, rmse = fit(xs, ys)
    if index == 2:
        solving = (intercept, slope)
    print("%s" % label)
    print("    fixed per invocation  %8.4f s      <- %s" % (intercept, note))
    print("    per obligation        %8.5f s" % slope)
    print("    r-squared             %8.4f      residual RMSE %.5f s"
          % (r2, rmse))
    print("    per-module residual:  " + "  ".join(
        "%s %+.4f" % (nm, y - (intercept + slope * x))
        for nm, x, y in zip(names, xs, ys)))
    print()

print("known in advance")
print("  Solving is per-obligation work, so the (cold - warm) fit's intercept")
print("  is known before the measurement: it must be near zero.  It is not.")
intercept, slope = solving
if slope > 0:
    print("    measured intercept        %8.4f s" % intercept)
    print("    per-obligation cost       %8.5f s" % slope)
    print("    intercept expressed in obligations   %6.0f"
          % (intercept / slope))
    print("  So the (cold - warm) row is not solving.  It carries a")
    print("  per-invocation charge worth roughly %.0f obligations of solving,"
          % (intercept / slope))
    print("  and reading the whole row as solver time overstates it by that")
    print("  much on any module smaller than that.")
else:
    print("    measured intercept        %8.4f s" % intercept)
    print("    per-obligation cost       %8.5f s  <- NON-POSITIVE, the fit is"
          % slope)
    print("      not usable; check the row-sanity block above.")
print()
print("  The first fit's dependent variable is warm minus -vox-no-verify, and")
print("  -vox-no-verify builds no obligations at all, so OBLIGATION")
print("  CONSTRUCTION IS INSIDE THAT ROW.  Do not add the default mode's")
print("  separate construction row to it: that counts construction twice.")
print("  The second fit's dependent variable includes the second SMT emission")
print("  a cold obligation pays, so its slope is an UPPER BOUND on solving")
print("  proper, not a measurement of it.")
FIT_PY
}

# --------------------------------------------------------- known in advance
# A one-obligation control module, generated here rather than taken from the
# fixture directory so that it is the same control whatever --original points
# at.  One obligation is one widening of a bound that holds by transitivity:
# no arithmetic, so nothing depends on overflow reasoning, and the obligation
# is genuinely discharged rather than trivially rejected.
#
# ONE CONTROL PER TIMED UNIT, for the same reason the startup floor uses one
# empty unit per timed unit: the charges this control is here to expose are
# paid once per COMPILER INVOCATION, and a corpus row runs the compiler once
# per module.  A single control against a five-module row would measure one
# fifth of the fixed charge that row is paying and leave the other four fifths
# sitting inside the marginal rows, which is the very confusion between
# per-invocation and per-obligation cost this control exists to prevent.
CONTROL_UNITS=""
CONTROL_OBLIGATIONS=0
write_control() {  # write_control DIR COUNT
  mkdir -p "$1"
  CONTROL_UNITS=""; CONTROL_OBLIGATIONS=0
  _ci=0
  while [ "$_ci" -lt "$2" ]; do
    _ci=$((_ci + 1))
    # The bound varies with the index so that no two control units carry the
    # SAME obligation.  Identical obligations produce identical SMT text and
    # therefore the same cache key, so unit 2 onwards would HIT the entry unit
    # 1 had just written, never spawn the solver, and never pay the
    # per-invocation teardown charge -- which would report a five-invocation
    # row as costing one invocation's worth.
    cat >"$1/control$_ci.ml" <<CONTROL_ML
(* One-obligation control for vox_attribute_time.sh.  Any row of the
   attribution that is genuinely per-obligation must be near zero here. *)
let widen$_ci (n : int{ _ > $_ci }) : int{ _ > 0 } = n
CONTROL_ML
    CONTROL_UNITS="$CONTROL_UNITS control$_ci"
    CONTROL_OBLIGATIONS=$((CONTROL_OBLIGATIONS + 1))
  done
}

if [ "$VALIDATE" = yes ]; then validate; exit $?; fi
if [ "$SCALING" = yes ]; then
  echo "scaling fit over: $UNIT"
  echo "compiler:  $OCAMLC"
  echo "backend:   $BACKEND, median of $REPEATS runs, private solver cache"
  echo "load:      $(uptime | sed 's/.*load average: //')  ($(nproc) cpus)"
  echo
  scaling
  abort_if_failed
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
CTL_COLD_CACHE=$SCRATCH/ctl-cold-cache
CTL_WARM_CACHE=$SCRATCH/ctl-warm-cache
mkdir -m 700 -p "$COLD_CACHE" "$WARM_CACHE" "$PRELUDE_CACHE" \
  "$CTL_COLD_CACHE" "$CTL_WARM_CACHE"

OD=$(stage "$ORIGINAL" original); prelude "$OD"
# -vox-no-verify cannot be applied to one unit of an otherwise verified build:
# the compiler refuses to check an implementation against an interface that was
# verified when the implementation will not be.  So the no-verify row gets its
# own workspace whose dependencies and interface were also built that way.  The
# two workspaces hold the same sources and differ only in whether the .cmi
# records that its obligations were discharged.
ND=$(stage "$ORIGINAL" original-noverify); prelude "$ND" "-vox-no-verify"
if [ "$VOX_ROWS_ONLY" = no ]; then TD=$(stage "$TWIN" twin); prelude "$TD"; fi
NUNITS=$(echo "$UNIT" | wc -w)
CD=$SCRATCH/control; write_control "$CD" "$NUNITS"

_unitlist=$(for _u in $UNIT; do printf '%s.ml ' "$_u"; done)
echo "units:     $(echo "$_unitlist" | sed 's/ *$//')"
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
printf '  %-34s %s\n' "original -vox-dump-vc" "$(check_row "$OD" "-vox-dump-vc" orig-dump-vc)"
if [ "$VOX_ROWS_ONLY" = no ]; then
  printf '  %-34s %s\n' "twin plain compile" "$(check_row "$TD" "" twin-compile)"
  printf '  %-34s %s\n' "twin -vox-type-only" "$(check_row "$TD" "-vox-type-only" twin-type-only)"
fi
_saved_unit=$UNIT; UNIT=$CONTROL_UNITS
printf '  %-34s %s\n' "control verify" "$(check_row "$CD" "-vox-backend $BACKEND" control-verify)"
printf '  %-34s %s\n' "control -vox-no-verify" "$(check_row "$CD" "-vox-no-verify" control-no-verify)"
UNIT=$_saved_unit
abort_if_failed
echo

# How much proof work the timed row actually carries.  Without this the reader
# cannot tell what fraction of the row the one-obligation control stands for,
# and the obligation count is the x-axis of every per-obligation claim made
# from these rows.
OBLIGATIONS=0
for _u in $UNIT; do
  _oc=$(obligation_count "$OD" "$_u"); abort_if_failed
  OBLIGATIONS=$((OBLIGATIONS + _oc))
done
echo "obligations: $OBLIGATIONS across $NUNITS unit(s)"
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
abort_if_failed
echo "configuration                       median      [min .. max]"
row "      compiler startup floor (x$_n)" "$FLOOR" "$2" "$3"

if [ "$VOX_ROWS_ONLY" = no ]; then
  set -- $(VOX_SOLVER_CACHE=0 timed "$TD" "-vox-type-only");     TW_TYPE=$1; row "twin  -vox-type-only" "$1" "$2" "$3"
  abort_if_failed
  set -- $(VOX_SOLVER_CACHE=0 timed "$TD" "");                   TW_FULL=$1; row "twin  plain compile"  "$1" "$2" "$3"
  abort_if_failed
fi
set -- $(VOX_SOLVER_CACHE=0 timed "$OD" "-vox-type-only");       OR_TYPE=$1; row "orig  -vox-type-only" "$1" "$2" "$3"
abort_if_failed
set -- $(VOX_SOLVER_CACHE=0 timed "$ND" "-vox-no-verify");       OR_NOVF=$1; row "orig  -vox-no-verify" "$1" "$2" "$3"
abort_if_failed
set -- $(VOX_SOLVER_CACHE=0 timed "$OD" "-vox-dump-vc");         OR_DUMP=$1; row "orig  -vox-dump-vc"   "$1" "$2" "$3"
abort_if_failed

# Warm: fill the private cache once, untimed, then measure against it hot.
( VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
    timed "$OD" "-vox-backend $BACKEND" >/dev/null )
abort_if_failed

set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$WARM_CACHE \
           timed "$OD" "-vox-backend $BACKEND");                 OR_WARM=$1; row "orig  verify, warm cache" "$1" "$2" "$3"
abort_if_failed

# Cold: an empty private directory, not VOX_SOLVER_CACHE=0.  Disabling the
# cache takes a different code path and the two are not comparable.
# Emptying it once is not enough: the first repeat would populate it and every
# later repeat would be a warm run, which drags the median down to the warm
# figure and makes solving look free.  Empty it before EVERY repeat.
set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$COLD_CACHE \
           timed "$OD" "-vox-backend $BACKEND" \
             'rm -rf "$COLD_CACHE"; mkdir -m 700 -p "$COLD_CACHE"'); \
                                                                 OR_COLD=$1; row "orig  verify, cold cache" "$1" "$2" "$3"
abort_if_failed

# --------------------------------------------- the known-in-advance control
# The same three verification rows, on a module carrying ONE obligation.  Its
# per-obligation content is 1/N of the real module's, so whatever these rows
# still cost is what the compiler pays per invocation rather than per
# obligation.  Measured, not assumed, and printed whether or not it is
# convenient.
_saved_unit=$UNIT; UNIT=$CONTROL_UNITS
set -- $(VOX_SOLVER_CACHE=0 timed "$CD" "-vox-no-verify");       CT_NOVF=$1
abort_if_failed
( VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$CTL_WARM_CACHE \
    timed "$CD" "-vox-backend $BACKEND" >/dev/null )
abort_if_failed
set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$CTL_WARM_CACHE \
           timed "$CD" "-vox-backend $BACKEND");                 CT_WARM=$1
abort_if_failed
set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$CTL_COLD_CACHE \
           timed "$CD" "-vox-backend $BACKEND" \
             'rm -rf "$CTL_COLD_CACHE"; mkdir -m 700 -p "$CTL_COLD_CACHE"'); \
                                                                 CT_COLD=$1
abort_if_failed
UNIT=$_saved_unit

# The default rows telescope to the cold total by construction, so their sum is
# an identity and not a check.  Measure the cold row a second time, after
# everything else, and let the two independent measurements of the same
# quantity be the residual.  Known in advance: they agree.
set -- $(VOX_SMT_SOLVER="$SOLVER" VOX_SOLVER_CACHE_DIR=$COLD_CACHE \
           timed "$OD" "-vox-backend $BACKEND" \
             'rm -rf "$COLD_CACHE"; mkdir -m 700 -p "$COLD_CACHE"'); \
                                                                 OR_COLD2=$1
abort_if_failed

# Both sides of each boundary below write the .cmo, so neither difference
# carries the dump row's emission asymmetry.
CT_TRANS=$(sub "$CT_WARM" "$CT_NOVF")
CT_SOLVE=$(sub "$CT_COLD" "$CT_WARM")
OR_TRANS=$(sub "$OR_WARM" "$OR_NOVF")
OR_SOLVE=$(sub "$OR_COLD" "$OR_WARM")

echo
echo "known in advance"
echo "  $NUNITS control module(s) carrying one obligation each, $CONTROL_OBLIGATIONS in all, put"
echo "  through the same rows and so making the same $NUNITS compiler invocation(s)."
echo "  Every row below is per-obligation work by its own name, and the timed"
echo "  row carries $OBLIGATIONS obligations against the control's $CONTROL_OBLIGATIONS, so on the"
echo "  control every one of them is expected to be near zero."
printf '  %-34s %8s   %s\n' "control  construct + translate + key" "$CT_TRANS" "expected ~0"
printf '  %-34s %8s   %s\n' "control  solving + teardown" "$CT_SOLVE" "expected ~0"
echo "  Whatever the control still pays here is a PER-INVOCATION charge, and"
echo "  the same row on the real module is paying it too.  It is separated out"
echo "  in the table below rather than left inside a row named after solving."
echo
printf '  %-34s %8s\n' "cold total, measured first" "$OR_COLD"
printf '  %-34s %8s\n' "cold total, re-measured last" "$OR_COLD2"
printf '  %-34s %8s   %s\n' "drift over the run" "$(sub "$OR_COLD2" "$OR_COLD")" \
  "expected ~0; this is the table's residual"
echo "  The attributed rows telescope to the FIRST cold measurement by"
echo "  construction, so their sum is an identity.  The two independent cold"
echo "  measurements above are the check that the run was stable enough for"
echo "  the differences between rows to mean anything."

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

attribute "fixed: cache key, per invocation"  "$CT_TRANS"
attribute "construction + translation + key, marginal" "$(sub "$OR_TRANS" "$CT_TRANS")"
attribute "fixed: solver teardown, per invocation" "$CT_SOLVE"
attribute "solving + 2nd emission, marginal"  "$(sub "$OR_SOLVE" "$CT_SOLVE")"
echo
echo "  The two 'fixed' rows are the control's own values: they are what this"
echo "  compiler spends per invocation before any obligation is considered."
echo "  On the base compiler they are a Digest.file over the compiler and"
echo "  solver binaries, and a flat 100 ms sleep in the solver teardown.  Both"
echo "  are removed on the cache lane -- a filesystem stamp and a 200 us"
echo "  backoff -- so on such a compiler both rows are absent and the rest of"
echo "  the table is unchanged.  Every absolute figure here describes the"
echo "  compiler named in the header, and nothing else."
echo
echo "  The control carries one obligation per invocation, so each fixed row"
echo "  above overstates the per-invocation charge by that one obligation's"
echo "  marginal cost, and the marginal row beside it understates it by the"
echo "  same amount."
echo
echo "  RAW ROW, BEFORE THE CONTROL IS SUBTRACTED.  cold minus warm is"
printf '  %-34s %8s   %s\n' "  cold - warm, raw" "$OR_SOLVE" "$(pct "$OR_SOLVE" "$OR_COLD")"
echo "  and this raw figure IS NOT SOLVING: most of it is the per-invocation"
echo "  teardown charge the control measures above.  Reading it as solver time"
echo "  is the mistake that produced a 44% 'solving' figure that has since"
echo "  been retracted; the split above is the corrected reading."
echo
echo "  The marginal solving row includes the second SMT emission a cold"
echo "  obligation pays, so it is an upper bound on solving proper and not a"
echo "  measurement of it.  Run --scaling to fit the same split over a range"
echo "  of obligation counts instead of against a one-obligation control."
echo
echo "  THE -vox-dump-vc BOUNDARY, AND WHY NO ROW ABOVE IS DERIVED FROM IT:"
printf '  %-34s %8s\n' "  dump - no-verify" "$(sub "$OR_DUMP" "$OR_NOVF")"
echo "  -vox-dump-vc builds and formats every obligation but exits before"
echo "  emitting bytecode -- it writes no .cmo, and -vox-no-verify does.  So"
echo "  this difference is (construction + formatting) MINUS (bytecode"
echo "  emission), which on a module this size are the same size, so it lands"
echo "  at the noise floor with an arbitrary sign.  Reading it as the cost of"
echo "  building obligations understates that cost by a whole .cmo emission,"
echo "  and charging the rest to the row above it overstates that by the same."
echo "  The table above therefore takes its boundary at -vox-no-verify, which"
echo "  emits bytecode exactly as the warm and cold rows do."
echo
echo "  a perfect cache saves the cold-minus-warm row, most of which is the"
echo "  teardown charge rather than the solver:"
printf '  %-34s %8s   %s\n' "cold total" "$OR_COLD" "$(pct "$OR_COLD" "$OR_COLD")"
printf '  %-34s %8s   %s\n' "warm total" "$OR_WARM" "$(pct "$OR_WARM" "$OR_COLD")"
