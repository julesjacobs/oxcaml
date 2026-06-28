#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/post_ra_root_verifier_amd64_generated.ml"
obj="$build_dir/post_ra_root_verifier_amd64_generated.cmx"

search_dir=$build_dir
ocamlopt=""
while [ "$search_dir" != "/" ]; do
  if [ -f "$search_dir/ocamlopt.opt" ] && [ -x "$search_dir/ocamlopt.opt" ]; then
    ocamlopt="$search_dir/ocamlopt.opt"
    break
  fi
  search_dir=$(dirname "$search_dir")
done

if [ -z "$ocamlopt" ]; then
  if [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/ocamlopt.opt" ]; then
    ocamlopt="$OCAMLSRCDIR/ocamlopt.opt"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

stdlib_flags=""
ocamlopt_dir=$(dirname "$ocamlopt")
for stdlib_dir in \
  "$ocamlopt_dir/lib/ocaml" \
  "$ocamlopt_dir/_install/lib/ocaml" \
  "$ocamlopt_dir/../lib/ocaml" \
  "$ocamlopt_dir/../../runtime_stdlib_install/lib/ocaml_runtime_stdlib"
do
  if [ -f "$stdlib_dir/stdlib.cmi" ]; then
    stdlib_flags="-I $stdlib_dir"
    break
  fi
done

cat > "$src" <<'EOF'
type t =
  { a : string;
    b : string;
    c : int
  }

let[@inline never] force () =
  Gc.minor ()

let[@inline never] keep (x : t) =
  Sys.opaque_identity x

let[@inline never] f a b c =
  let r = keep { a; b; c } in
  force ();
  let r = keep r in
  String.length r.a + String.length r.b + r.c
EOF

"$ocamlopt" $stdlib_flags -O3 -S -c -keep-llvmir -llvm-backend \
  -ccopt -mllvm -ccopt -oxcaml-gc-root-verifier \
  -ccopt -mllvm -ccopt -oxcaml-gc-root-verifier-fatal \
  -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -o "$obj" "$src"
