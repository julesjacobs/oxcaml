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
