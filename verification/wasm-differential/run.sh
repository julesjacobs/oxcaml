#!/usr/bin/env bash
# Differential test of the Vox WebAssembly model against Node.
#
#   verification/wasm-differential/run.sh [--prefix DIR] [--jobs N] [TESTER OPTIONS]
#
# Builds testsuite/tests/vox/wasm_differential.ml and the model it tests with
# the compiler installed in DIR (default: _install), then runs it. Without
# tester options it runs the fixed configuration of the ocamltest test and
# compares the output with wasm_differential.reference (the fast check).
# Otherwise it passes the options to the tester (see the head of
# wasm_differential.ml: -count, -seed, -from, -fuel, -capacity, -pages,
# -timeout, -keep, -minimize, -detail, -coverage, -verbose); with --jobs N it
# splits [from, from + count) into N ranges run in parallel and adds up the
# tallies.
set -euo pipefail

root=$(cd "$(dirname "$0")/../.." && pwd)
prefix=$root/_install
jobs=1
while [[ $# -gt 0 ]]; do
  case $1 in
    --prefix) prefix=$2; shift 2 ;;
    --jobs) jobs=$2; shift 2 ;;
    *) break ;;
  esac
done
command -v node > /dev/null || { echo "node not found" >&2; exit 2; }
ulimit -s 65536 2> /dev/null || ulimit -s unlimited 2> /dev/null || true

tests=$root/testsuite/tests/vox
build=$root/_build/wasm-differential
mkdir -p "$build"
export VOX_VERIFY_CACHE=${VOX_VERIFY_CACHE-$root/_build/vox-verify-cache}
# The modules, in dependency order, as listed in the test's header, then the
# test itself.
prebuilt=$(sed -n 's/^ *prebuilt_modules = "\(.*\)";/\1/p' "$tests/wasm_differential.ml")
[[ -n $prebuilt ]] || { echo "no prebuilt_modules in wasm_differential.ml" >&2; exit 2; }
modules="$prebuilt wasm_differential.ml"
cp "$root/verification/library/pref.ml" "$root/verification/library/pref.mli" "$build/"
objects=()
dirty=
for file in $modules; do
  [[ $file == pref.ml* ]] || cmp -s "$tests/$file" "$build/$file" || cp "$tests/$file" "$build/$file"
  base=${file%.*}
  if [[ $file == *.mli ]]; then
    target=$build/$base.cmi
  else
    target=$build/$base.cmx; objects+=("$base.cmx")
  fi
  # The list is in dependency order: after one unit changes, recompile the rest.
  if [[ -n $dirty || ! -f $target || $build/$file -nt $target ]]; then
    dirty=1
    (cd "$build" && "$prefix/bin/ocamlopt.opt" -extension refinement_types -I . -c "$file")
  fi
done
(cd "$build" && "$prefix/bin/ocamlopt.opt" -I . "${objects[@]}" -o wasm_differential.exe)
harness=$tests/wasm_differential_harness.js

if [[ $# -eq 0 ]]; then
  output=$(mktemp)
  "$build/wasm_differential.exe" -harness "$harness" > "$output"
  if diff -u "$tests/wasm_differential.reference" "$output"; then
    echo "wasm differential check: passed"; rm -f "$output"
  else
    echo "wasm differential check: output differs from wasm_differential.reference" >&2
    rm -f "$output"; exit 1
  fi
  exit 0
fi

if [[ $jobs -le 1 ]]; then
  exec "$build/wasm_differential.exe" -harness "$harness" "$@"
fi

# Parallel: split the id range, then add up the tallies.
count=300; from=0; rest=()
while [[ $# -gt 0 ]]; do
  case $1 in
    -count) count=$2; shift 2 ;;
    -from) from=$2; shift 2 ;;
    *) rest+=("$1"); shift ;;
  esac
done
parts=$(mktemp -d)
share=$(( (count + jobs - 1) / jobs ))
for (( j = 0; j < jobs; j++ )); do
  start=$(( from + j * share ))
  n=$(( share < from + count - start ? share : from + count - start ))
  (( n > 0 )) || continue
  "$build/wasm_differential.exe" -harness "$harness" -from "$start" -count "$n" ${rest[@]+"${rest[@]}"} > "$parts/$j.txt" &
done
wait
python3 - "$count" "$from" "$parts"/*.txt <<'EOF'
import re, sys
count, start, files = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3:]
tally, steps, cases, model, total = {}, {}, [], '', 0
for name in files:
    section = None
    for line in open(name):
        line = line.rstrip('\n')
        if line.startswith('Model fuel'): model = line
        elif line.startswith('Differential'): section = tally
        elif line.startswith('Executed by the model'): section = steps
        elif line.startswith('Disagreements:'): total += int(line.split()[1]); section = 'cases'
        elif section == 'cases': cases.append(line)
        elif line.startswith('  ') and section is not None:
            m = re.match(r'  (.*?)\s+(\d+)$', line)
            section[m.group(1)] = section.get(m.group(1), 0) + int(m.group(2))
print(f'Differential test of the WebAssembly model against Node: {count} modules, ids {start}-{start + count - 1}')
print(model)
for k in sorted(tally): print(f'  {k:<78} {tally[k]:8}')
if steps:
    print('Executed by the model on agreeing cases (steps):')
    for k in sorted(steps): print(f'  {k:<40} {steps[k]:12}')
print(f'Disagreements: {total}')
for c in cases: print(c)
EOF
rm -rf "$parts"
