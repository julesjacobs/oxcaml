# Usage: run.sh OCAMLC STDLIB_DIR LIBRARY_DIR, from the test's build
# directory, where ./prebuilt holds Vox_sequence, Vox_sat_spec and
# Vox_sat_proof.
#
# Derives a MUTANT COPY of vox_cdcl_total_proof.ml from the library's current
# source, with the n+1 weight dropped from the termination measure, prints
# the lines the substitution changed, and compiles the real module and the
# mutant. The mutant exists only in this test's build directory.
OCAMLC="$1"; STDLIB="$2"; LIBRARY="$3"
FLAGS="-nostdlib -I $STDLIB -I ../prebuilt -extension refinement_types"
m=vox_cdcl_total_proof

for dir in real mutant; do
  mkdir -p $dir
  cp "$LIBRARY/$m.mli" $dir/
done
cp "$LIBRARY/$m.ml" real/
perl -0777 -pe \
  's/\(Bigint\.add \(Bigint\.of_int n\) 1Z\)\)(\n\s*\(unassigned bindings\))/1Z)$1/g' \
  "$LIBRARY/$m.ml" > mutant/$m.ml
echo "$m.ml, mutant copy:"
diff "$LIBRARY/$m.ml" mutant/$m.ml | grep '^[<>]' | sed 's/^\([<>]\) */\1 /'

for dir in real mutant; do
  echo "## $m.ml, $dir:"
  ( cd $dir && "$OCAMLC" $FLAGS -c $m.mli $m.ml 2>&1 )
  echo "exit $?"
done
