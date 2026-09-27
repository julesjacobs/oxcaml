#!/bin/sh
# Usage: run.sh OCAMLC STDLIB_DIR, from the test's build directory
OCAMLC="$1"; STDLIB="$2"
VOX_VERIFY_CACHE=; export VOX_VERIFY_CACHE
# The sources are read-only links; work on copies.
rm -rf work; mkdir work
cp axioms.ml client.ml hidden.ml hidden_client.ml work/
cd work
compile() {
  "$OCAMLC" -nostdlib -I "$STDLIB" -extension refinement_types "$@" 2>&1
}
# The audit lists what a unit and the units it depends on trust. Only the
# lines about this test's units are kept: the standard library's change with
# it.
audit() {
  compile -vox-audit "$@" |
    sed -e 's/the [0-9]* units it/the N units it/' |
    awk '/^Vox audit|^File|^Warning|^  [^ ]/ && !/\(library\)/ {
           if (header != "") print header; header = ""; print; next }
         /:$/ { header = $0 }'
}
echo "== Warnings for axioms.ml"
compile -c axioms.ml
echo "== Audit of client.ml"
audit -c client.ml
echo "== Assumptions that are easy to miss"
compile -extension layouts_alpha -c hidden.ml
audit -c hidden_client.ml
