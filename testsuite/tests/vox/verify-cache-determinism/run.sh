# Usage: run.sh OCAMLC STDLIB_DIR, from the test's build directory
# Compiles each file with a cold query cache, then with a warm one, then
# after a warm-up by another file, and prints the output once if the
# compilations agree.
OCAMLC="$1"; STDLIB="$2"
VOX_VERIFY_CACHE="$PWD/verify-cache"; export VOX_VERIFY_CACHE
compile() {
  "$OCAMLC" -nostdlib -I "$STDLIB" -extension refinement_types -c "$1" 2>&1
}
fresh() { rm -rf "$VOX_VERIFY_CACHE"; mkdir "$VOX_VERIFY_CACHE"; }
# check FILE WARMUP: FILE's output with a cold cache, a warm one, and after
# compiling WARMUP into an empty cache.
check() {
  fresh; cold=$(compile "$1")
  warm=$(compile "$1")
  fresh; compile "$2" > /dev/null; warmed=$(compile "$1")
  printf '%s\n' "$cold"
  if [ "$cold" = "$warm" ] && [ "$cold" = "$warmed" ]
  then echo "$1: the same with a cold cache, a warm one and after $2"
  else
    echo "$1: warm cache:"; printf '%s\n' "$warm"
    echo "$1: after $2:"; printf '%s\n' "$warmed"
  fi
}
check y_names.ml x_names.ml
check x_names.ml y_names.ml
check two_groups.ml x_names.ml
