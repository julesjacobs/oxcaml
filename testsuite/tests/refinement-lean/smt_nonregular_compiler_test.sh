set -eu

ocamlrun=$1
ocamlc=$2
source=$3
json=$4
marker="$json.solver-invoked"
output="$json.output"

set +e
timeout --kill-after=1s 5s "$ocamlrun" "$ocamlc" \
  -vox-backend z3 \
  -vox-smt-solver "sh smt_nonregular_marker_solver.sh $marker" \
  -vox-dump-vc-json "$json" \
  -c "$source" >"$output" 2>&1
status=$?
set -e

test "$status" -eq 2
test -s "$json"
test ! -e "$marker"
detail='"detail": "non-regular recursive datatype t is not supported"'
message='"message": "non-regular recursive datatype t is not supported"'
test "$(grep -c "$detail" "$json")" -eq 1
test "$(grep -c "$message" "$json")" -eq 1
grep -q '"generated_lean": null' "$json"
