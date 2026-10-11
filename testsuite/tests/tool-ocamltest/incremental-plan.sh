#!/bin/sh
set -eu

sed 's/CHILDTEST/TEST/' \
  "$test_source_directory/incremental-plan.tsl" > incremental.ml
"$ocamlsrcdir/ocamltest/ocamltest" -plan-incremental incremental.ml > actual
cat > expected <<'EOF'
compilerlibs.ocamlbytecomp
compilerlibs.ocamlcommon
compilerlibs.ocamlfrontend
compilerlibs.ocamloptcomp
compilerlibs.ocamltoplevel
ocamlc.byte
ocamlopt.byte
EOF
diff -u expected actual

work=$(mktemp -d "${TMPDIR:-/tmp}/vox-affected-test.XXXXXX")
trap 'rm -rf "$work"' 0
mkdir -p "$work/testsuite/tests/vox/nested" \
  "$work/testsuite/tests/typing-refinement-types/deep" \
  "$work/verification/library"
for test in vox/root.ml vox/nested/test.ml vox/nested/linked.ml \
  typing-refinement-types/deep/test.ml \
  typing-refinement-types/deep/bad.ml; do
  cat > "$work/testsuite/tests/$test" <<'EOF'
(* TEST
 pass;
*)
EOF
done
cat > "$work/testsuite/tests/vox/nested/test.ml" <<'EOF'
(* TEST
 source_directories =
   "${test_source_directory}/../../../../verification/library";
 prebuilt_modules = "common.mli";
 flags = "-principal";
 pass;
*)
EOF
cat > "$work/testsuite/tests/vox/nested/linked.ml" <<'EOF'
(* TEST
 source_directories =
   "${test_source_directory}/../../../../verification/library";
 prebuilt_modules = "common.mli";
 pass;
*)
EOF
printf 'val x : int\n' > "$work/verification/library/common.mli"
printf 'type bad = { x : int | }\n' >> \
  "$work/testsuite/tests/typing-refinement-types/deep/bad.ml"
printf 'let x = Helper.x\n' >> "$work/testsuite/tests/vox/nested/test.ml"
printf 'let x = Linked_helper.x\n' >> \
  "$work/testsuite/tests/vox/nested/linked.ml"
printf 'let x = Transitive.x\n' > "$work/testsuite/tests/vox/nested/helper.ml"
printf 'let x = Transitive.x\n' > "$work/verification/library/linked_helper.ml"
ln -s ../../../../verification/library/linked_helper.ml \
  "$work/testsuite/tests/vox/nested/linked_helper.ml"
printf 'let x = 1\n' > "$work/verification/library/transitive.ml"
: > "$work/testsuite/tests/vox/nested/test.reference"
: > "$work/compiler.ml"
git -C "$work" init -q --template=
git -C "$work" add .
git -C "$work" -c user.name=Regression \
  -c user.email=regression@example.invalid -c commit.gpgsign=false \
  -c core.hooksPath=/dev/null commit -qm base

affected()
{
  sh "$test_source_directory/../../vox-affected-tests.sh" "$work" \
    "$ocamlsrcdir/ocamltest/ocamltest" "$ocamlsrcdir/ocamldep" HEAD > actual
  LC_ALL=C sort actual > actual.sorted
  diff -u expected actual.sorted
}

: > expected
affected
printf 'changed\n' > "$work/compiler.ml"
cat > expected <<'EOF'
typing-refinement-types/deep/bad.ml
typing-refinement-types/deep/test.ml
vox/nested/linked.ml
vox/nested/test.ml
vox/root.ml
EOF
affected
: > "$work/compiler.ml"
printf 'vox/nested/test.ml\n' > expected
printf 'changed\n' > "$work/testsuite/tests/vox/nested/test.reference"
affected
: > "$work/testsuite/tests/vox/nested/test.reference"
printf 'let x = Transitive.x + 1\n' > \
  "$work/testsuite/tests/vox/nested/helper.ml"
affected
printf 'let x = Transitive.x\n' > "$work/testsuite/tests/vox/nested/helper.ml"
printf 'let x = 2\n' > "$work/verification/library/transitive.ml"
printf 'vox/nested/linked.ml\nvox/nested/test.ml\n' > expected
affected
printf 'let x = 1\n' > "$work/verification/library/transitive.ml"
printf 'let x = 2\n' > "$work/verification/library/linked_helper.ml"
printf 'vox/nested/linked.ml\n' > expected
affected
printf 'let x = Transitive.x\n' > "$work/verification/library/linked_helper.ml"
printf 'let x = Helper.x + 1\n' >> \
  "$work/testsuite/tests/vox/nested/test.ml"
printf 'vox/nested/test.ml\n' > expected
affected
git -C "$work" show HEAD:testsuite/tests/vox/nested/test.ml > \
  "$work/testsuite/tests/vox/nested/test.ml"
sed '/prebuilt_modules/d; /flags =/d' \
  "$work/testsuite/tests/vox/nested/test.ml" > "$work/replacement"
mv "$work/replacement" "$work/testsuite/tests/vox/nested/test.ml"
printf 'vox/nested/linked.ml\nvox/nested/test.ml\n' > expected
affected
rm "$work/testsuite/tests/vox/nested/test.ml"
printf 'vox/nested/linked.ml\n' > expected
affected

lock_work=$work/lock
mkdir -p "$lock_work/_build"
lock_work=$(CDPATH= cd -P "$lock_work" && pwd)
git -C "$lock_work" init -q --template=
printf 'prefix=%s/_install\n' "$lock_work" > "$lock_work/Makefile.config"
# Exercise the actual acquisition code, pausing after the stale-owner check.
awk '
  /^[[:space:]]*stale=/ {
    print "    if [ -n \"${LOCK_TEST_PAUSE-}\" ]; then"
    print "      : > \"$LOCK_TEST_DIR/checked\""
    print "      tries=0"
    print "      while [ ! -f \"$LOCK_TEST_DIR/resume\" ]; do"
    print "        tries=$((tries + 1))"
    print "        [ \"$tries\" -lt 200 ] || exit 1"
    print "        sleep 0.05"
    print "      done"
    print "    fi"
  }
  { print }
  /^rm -f "\$lock_claim"$/ { exit }
' "$test_source_directory/../../../dev" > "$lock_work/dev"
cat >> "$lock_work/dev" <<'EOF'
: > "$LOCK_TEST_DIR/$LOCK_TEST_NAME.acquired"
tries=0
while [ ! -f "$LOCK_TEST_DIR/release" ]; do
  tries=$((tries + 1))
  [ "$tries" -lt 200 ] || exit 1
  sleep 0.05
done
EOF
chmod +x "$lock_work/dev"

wait_file()
{
  tries=0
  while [ ! -f "$1" ]; do
    tries=$((tries + 1))
    [ "$tries" -lt 200 ] || { echo "timed out waiting for $1" >&2; exit 1; }
    sleep 0.05
  done
}

export LOCK_TEST_DIR=$lock_work
printf '0|expired\n' > "$lock_work/_build/.vox-dev.lock"
LOCK_TEST_NAME=first LOCK_TEST_PAUSE=true "$lock_work/dev" init \
  > "$lock_work/first.log" 2>&1 &
first=$!
wait_file "$lock_work/checked"
LOCK_TEST_NAME=second "$lock_work/dev" init \
  > "$lock_work/second.log" 2>&1 &
second=$!
sleep 0.2
: > "$lock_work/resume"
wait_file "$lock_work/first.acquired"
status=0
wait "$second" || status=$?
[ "$status" -eq 75 ]
[ ! -f "$lock_work/second.acquired" ]
grep -Fq 'another command owns this worktree' "$lock_work/second.log"
: > "$lock_work/release"
wait "$first"
[ ! -f "$lock_work/_build/.vox-dev.lock" ]
LOCK_TEST_NAME=third "$lock_work/dev" init > "$lock_work/third.log" 2>&1
[ -f "$lock_work/third.acquired" ]
[ ! -f "$lock_work/_build/.vox-dev.lock" ]
