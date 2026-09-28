#!/bin/sh
# Usage: run.sh OCAMLC STDLIB_DIR, from the test's build directory
OCAMLC="$1"; STDLIB="$2"
VOX_VERIFY_CACHE=; export VOX_VERIFY_CACHE
# The sources are read-only links; work on copies.
rm -rf work; mkdir work
cp axioms.ml client.ml helper.ml user.ml good.ml good_user.ml hidden.ml \
  hidden_client.ml asserted.ml asserted_user.ml work/
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
# A unit compiled with -smt-assume-verified is recorded as not verified, and
# a verified unit that imports its interface is warned about.
echo "== helper.ml, not verified; user.ml"
compile -smt-assume-verified -c helper.ml
compile -c user.ml
audit -c user.ml
# Unless a verified compilation of the same source against the same
# interfaces produced its .cmo or .cmi, as in the library build.
echo "== good.ml, verified and then compiled again without verifying"
compile -c good.ml && compile -smt-assume-verified -c good.ml
compile -c good_user.ml
echo "== good.ml, edited and compiled without verifying"
echo "let h = 1" >> good.ml
compile -smt-assume-verified -c good.ml
compile -c good_user.ml
echo "== Assumptions that are easy to miss"
compile -extension layouts_alpha -c hidden.ml
audit -c hidden_client.ml
# The verified compilation must also have had the same flags: -noassert
# removes the check that the proof relies on.
echo "== asserted.ml, verified and then compiled with -noassert, not verified"
compile -c asserted.ml && compile -noassert -smt-assume-verified -c asserted.ml
compile -c asserted_user.ml
