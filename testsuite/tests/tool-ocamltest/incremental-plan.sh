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

library_work=$work/library-lock
mkdir -p "$library_work/_build" "$library_work/testsuite"
library_work=$(CDPATH= cd -P "$library_work" && pwd)
git -C "$library_work" init -q --template=
printf 'prefix=%s/_install\n' "$library_work" > "$library_work/Makefile.config"
awk '{ print } /^rm -f "\$lock_claim"$/ { exit }' \
  "$test_source_directory/../../../dev" > "$library_work/dev"
awk '
  /^start_test_library\(\)/ { copying = 1 }
  /^# Prints the Vox tests/ { exit }
  copying { print }
' "$test_source_directory/../../../dev" >> "$library_work/dev"
cat >> "$library_work/dev" <<'EOF'
temp_dir=$(mktemp -d "$root/_build/.vox-dev.XXXXXX")
printf '%s\n' "$temp_dir" > "$root/$LIBRARY_TEST_NAME.temp"
run_dir=$root/$LIBRARY_TEST_NAME.run
mkdir "$run_dir"
test_files=$run_dir/tests
: > "$test_files"
planner=unused
OCAMLTEST_OCAMLC_OPT=unused
OCAMLTEST_OCAMLOPT_OPT=unused
start_test_library
run_locked true
: > "$root/$LIBRARY_TEST_NAME.foreground-finished"
finish_test_library
exit "$library_status"
EOF
chmod +x "$library_work/dev"
cat > "$library_work/testsuite/vox-test-library.sh" <<'EOF'
#!/bin/sh
set -eu
if [ "$LIBRARY_TEST_NAME" = descendant ]; then
  perl -MPOSIX=setpgid -e '
    $SIG{TERM} = "IGNORE";
    setpgid(0, 0) == 0 or die "setpgid: $!";
    my $root = shift;
    open my $ready, ">", "$root/descendant.ready" or die $!;
    print $ready "$$\n";
    close $ready;
    for (1..1000) {
      exit 0 if -f "$root/descendant.release";
      select undef, undef, undef, 0.01;
    }
    exit 1;
  ' "$LIBRARY_TEST_DIR" &
  exit 0
fi
: > "$LIBRARY_TEST_DIR/$LIBRARY_TEST_NAME.ready"
tries=0
while [ ! -f "$LIBRARY_TEST_DIR/$LIBRARY_TEST_NAME.release" ]; do
  tries=$((tries + 1))
  [ "$tries" -lt 200 ] || exit 1
  sleep 0.05
done
EOF

library_test_group=
library_test_child=
library_test_wrapper=
trap '
  if [ -n "$library_test_group" ]; then
    perl -e '\''kill "KILL", -$ARGV[0]'\'' "$library_test_group" || true
  fi
  [ -z "$library_test_child" ] || kill -KILL "$library_test_child" || true
  [ -z "$library_test_wrapper" ] || kill -KILL "$library_test_wrapper" || true
  rm -rf "$work"
' 0
export LIBRARY_TEST_DIR=$library_work

reject_library_contender()
{
  status=0
  LIBRARY_TEST_NAME=contender "$library_work/dev" init \
    > "$library_work/contender.log" 2>&1 || status=$?
  [ "$status" -eq 75 ]
  grep -Fq 'another command owns this worktree' "$library_work/contender.log"
}

wait_library_group()
{
  token=${1-}
  tries=0
  while ps -axo pgid= | awk -v group="$library_test_group" \
      '$1 == group { found = 1 } END { exit !found }' ||
    { [ -n "$token" ] && perl -MFcntl=:flock -e '
        open my $lock, ">>", shift or exit 1;
        exit(flock($lock, LOCK_EX | LOCK_NB) ? 1 : 0);
      ' "$token"; }; do
    tries=$((tries + 1))
    [ "$tries" -lt 200 ] || exit 1
    sleep 0.05
  done
  library_test_group=
}

LIBRARY_TEST_NAME=normal "$library_work/dev" init \
  > "$library_work/normal.log" 2>&1 &
owner=$!
wait_file "$library_work/normal.foreground-finished"
wait_file "$library_work/normal.ready"
library_test_group=$(cat "$library_work/_build/vox-test-library/.building")
grep -Eq "^group\|$library_test_group\|" \
  "$library_work/_build/.vox-dev.lock"
reject_library_contender
: > "$library_work/normal.release"
wait "$owner"
wait_library_group
[ ! -f "$library_work/_build/.vox-dev.lock" ]
[ ! -f "$library_work/_build/vox-test-library/.building" ]
[ ! -d "$(cat "$library_work/normal.temp")" ]

LIBRARY_TEST_NAME=orphan "$library_work/dev" init \
  > "$library_work/orphan.log" 2>&1 &
owner=$!
wait_file "$library_work/orphan.foreground-finished"
wait_file "$library_work/orphan.ready"
library_test_group=$(cat "$library_work/_build/vox-test-library/.building")
kill -KILL "$owner"
wait "$owner" 2>/dev/null || true
kill -0 "$library_test_group"
reject_library_contender
: > "$library_work/orphan.release"
wait_library_group

LIBRARY_TEST_NAME=descendant "$library_work/dev" init \
  > "$library_work/descendant.log" 2>&1 &
owner=$!
wait_file "$library_work/descendant.ready"
library_test_child=$(cat "$library_work/descendant.ready")
status=0
wait "$owner" || status=$?
[ "$status" -eq 1 ]
library_test_group=$(cat "$library_work/_build/vox-test-library/.building")
[ "$library_test_child" -ne "$library_test_group" ]
[ -f "$library_work/_build/.vox-dev.lock" ]
[ -d "$(cat "$library_work/descendant.temp")" ]
reject_library_contender
: > "$library_work/descendant.release"
wait_library_group "$(cat "$library_work/descendant.temp")/library-start.lock"
library_test_child=

: > "$library_work/recovered.release"
LIBRARY_TEST_NAME=recovered "$library_work/dev" init \
  > "$library_work/recovered.log" 2>&1
[ ! -f "$library_work/_build/.vox-dev.lock" ]

wrapper_work=$work/library-wrapper
mkdir "$wrapper_work"
printf '#!/bin/sh\nset -eu\n' > "$wrapper_work/build"
awk '
  /^if \[ -z "\$\{VOX_TEST_LIBRARY_LOCKED-/ { copying = 1 }
  copying { print }
  copying && /^fi$/ { exit }
' "$test_source_directory/../../vox-test-library.sh" >> "$wrapper_work/build"
cat >> "$wrapper_work/build" <<'EOF'
exec perl -MPOSIX=setpgid -e '
  setpgid(0, 0) == 0 or die "setpgid: $!";
  my $root = shift;
  open my $child, ">", "$root/child" or die $!;
  print $child "$$\n";
  close $child;
  open my $ready, ">", "$root/ready" or die $!;
  close $ready;
  for (1..1000) {
    exit 0 if -f "$root/release";
    select undef, undef, undef, 0.01;
  }
  exit 1;
' "$1"
EOF
chmod +x "$wrapper_work/build"
unset VOX_TEST_LIBRARY_LOCKED
"$wrapper_work/build" "$wrapper_work" > "$wrapper_work/log" 2>&1 &
library_test_wrapper=$!
wait_file "$wrapper_work/ready"
library_test_child=$(cat "$wrapper_work/child")

claim_library_wrapper()
{
  perl -MFcntl=:flock -e '
    open my $lock, ">>", shift or die $!;
    exit(flock($lock, LOCK_EX | LOCK_NB) ? 0 : 75);
  ' "$wrapper_work/.lock"
}

status=0
claim_library_wrapper || status=$?
[ "$status" -eq 75 ]
kill -KILL "$library_test_wrapper"
wait "$library_test_wrapper" 2>/dev/null || true
library_test_wrapper=
kill -0 "$library_test_child"
status=0
claim_library_wrapper || status=$?
[ "$status" -eq 75 ]
: > "$wrapper_work/release"
tries=0
until claim_library_wrapper; do
  tries=$((tries + 1))
  [ "$tries" -lt 200 ] || exit 1
  sleep 0.05
done
library_test_child=

foreground_work=$work/foreground-lock
mkdir -p "$foreground_work/_build"
foreground_work=$(CDPATH= cd -P "$foreground_work" && pwd)
git -C "$foreground_work" init -q --template=
printf 'prefix=%s/_install\n' "$foreground_work" > \
  "$foreground_work/Makefile.config"
awk '
  /^[[:space:]]*active_child=\$group_child$/ {
    print
    print "  if [ -n \"${FOREGROUND_TEST_PAUSE-}\" ]; then"
    print "    printf '\''%s\\n'\'' \"$active_child\" > \"$root/unpublished\""
    print "    while [ ! -f \"$root/resume\" ]; do sleep 0.05; done"
    print "  fi"
    next
  }
  { print }
  /^rm -f "\$lock_claim"$/ { exit }
' "$test_source_directory/../../../dev" > "$foreground_work/dev"
cat >> "$foreground_work/dev" <<'EOF'
temp_dir=$(mktemp -d "$root/_build/.vox-dev.XXXXXX")
printf '%s\n' "$temp_dir" > "$root/$FOREGROUND_TEST_NAME.temp"
case "$FOREGROUND_TEST_NAME" in
  startup)
    run_locked sh -c ': > "$1/started"' sh "$root"
    ;;
  descendant)
    if run_locked perl -MPOSIX=setpgid -e '
      my $root = shift;
      my $child = fork();
      die $! unless defined $child;
      if ($child == 0) {
        $SIG{TERM} = "IGNORE";
        setpgid(0, 0) == 0 or die "setpgid: $!";
        open my $ready, ">", "$root/descendant.ready" or die $!;
        print $ready "$$\n";
        close $ready;
        for (1..1000) {
          exit 0 if -f "$root/descendant.release";
          select undef, undef, undef, 0.01;
        }
        exit 1;
      }
      while (!-f "$root/descendant.ready") {
        select undef, undef, undef, 0.01;
      }
    ' "$root"; then
      exit 1
    fi
    run_locked sh -c ': > "$1/second-started"' sh "$root"
    ;;
  *)
    run_locked true
    status=0
    run_locked false || status=$?
    [ "$status" -eq 1 ]
    run_locked true
    ;;
esac
EOF
chmod +x "$foreground_work/dev"

FOREGROUND_TEST_NAME=startup FOREGROUND_TEST_PAUSE=true \
  "$foreground_work/dev" init > "$foreground_work/startup.log" 2>&1 &
owner=$!
wait_file "$foreground_work/unpublished"
library_test_group=$(cat "$foreground_work/unpublished")
[ ! -f "$foreground_work/started" ]
kill -KILL "$owner"
wait "$owner" 2>/dev/null || true
wait_library_group
[ ! -f "$foreground_work/started" ]
FOREGROUND_TEST_NAME=normal "$foreground_work/dev" init \
  > "$foreground_work/normal.log" 2>&1
[ ! -f "$foreground_work/_build/.vox-dev.lock" ]
[ ! -d "$(cat "$foreground_work/normal.temp")" ]

FOREGROUND_TEST_NAME=descendant "$foreground_work/dev" init \
  > "$foreground_work/descendant.log" 2>&1 &
owner=$!
wait_file "$foreground_work/descendant.ready"
library_test_child=$(cat "$foreground_work/descendant.ready")
status=0
wait "$owner" || status=$?
[ "$status" -eq 2 ]
grep -Fq 'command left running processes' "$foreground_work/descendant.log"
grep -Fq 'previous command still has running processes' \
  "$foreground_work/descendant.log"
library_test_group=$(sed -n 's/^group|\([0-9]*\)|.*/\1/p' \
  "$foreground_work/_build/.vox-dev.lock")
[ -n "$library_test_group" ]
[ "$library_test_child" -ne "$library_test_group" ]
[ ! -f "$foreground_work/second-started" ]
[ -d "$(cat "$foreground_work/descendant.temp")" ]
status=0
FOREGROUND_TEST_NAME=contender "$foreground_work/dev" init \
  > "$foreground_work/contender.log" 2>&1 || status=$?
[ "$status" -eq 75 ]
: > "$foreground_work/descendant.release"
wait_library_group "$(cat "$foreground_work/descendant.temp")/active-start.lock"
library_test_child=
FOREGROUND_TEST_NAME=recovered "$foreground_work/dev" init \
  > "$foreground_work/recovered.log" 2>&1
[ ! -f "$foreground_work/_build/.vox-dev.lock" ]
