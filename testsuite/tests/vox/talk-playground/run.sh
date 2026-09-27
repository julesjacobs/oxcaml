# Usage: run.sh OCAMLC STDLIB_DIR EXAMPLES_DIR FILE..., from the test's
# build directory. Compiles each playground example, as the playground's
# native reference does, and prints the compiler's output and exit status.
OCAMLC="$1"; STDLIB="$2"; EXAMPLES="$3"; shift 3
for f in "$@"; do
  echo "## $f"
  cp "$EXAMPLES/$f" "$f"
  "$OCAMLC" -nostdlib -I "$STDLIB" -extension refinement_types -c "$f" 2>&1
  echo "exit $?"
done
