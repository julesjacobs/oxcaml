#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_cldemote_generated.ml"
out="$build_dir/amd64_cldemote_generated.o"
ir="$build_dir/amd64_cldemote_generated.ll"
asm="$build_dir/amd64_cldemote_generated.s"

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
  elif [ -n "${OCAMLSRCDIR:-}" ] && [ -x "$OCAMLSRCDIR/_build/main/oxcaml_main_native.exe" ]; then
    ocamlopt="$OCAMLSRCDIR/_build/main/oxcaml_main_native.exe"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

cat > "$src" <<'EOF'
external cldemote : nativeint -> unit
  = "" "caml_cldemote"
[@@noalloc] [@@builtin]

let[@inline never] test p = cldemote p
EOF

"$ocamlopt" -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

for generated_file in "$ir" "$asm"; do
  if [ ! -f "$generated_file" ]; then
    echo "expected generated file missing: $generated_file" >&2
    exit 1
  fi
done

grep -q 'call void asm sideeffect "cldemote (\$0)", "r"' "$ir"
grep -Eq '^[[:space:]]*cldemote[[:space:]]' "$asm"
