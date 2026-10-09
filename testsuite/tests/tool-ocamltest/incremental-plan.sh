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
