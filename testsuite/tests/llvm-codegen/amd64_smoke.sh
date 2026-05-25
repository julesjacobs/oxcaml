#!/bin/sh

set -eu

build_dir=$(pwd)

ocamlsrcdir="${OCAMLSRCDIR:-}"
search_dir=$build_dir
ocamlopt=""
while [ "$search_dir" != "/" ]; do
  if [ -f "$search_dir/ocamlopt.opt" ] && [ -x "$search_dir/ocamlopt.opt" ]; then
    ocamlopt="$search_dir/ocamlopt.opt"
    if [ -z "$ocamlsrcdir" ]; then
      ocamlsrcdir="$search_dir"
    fi
    break
  fi
  search_dir=$(dirname "$search_dir")
done

if [ -z "$ocamlopt" ]; then
  if [ -n "$ocamlsrcdir" ] && [ -x "$ocamlsrcdir/ocamlopt.opt" ]; then
    ocamlopt="$ocamlsrcdir/ocamlopt.opt"
  else
    ocamlopt="_build/install/main/bin/ocamlopt.opt"
  fi
fi

if [ -n "$ocamlsrcdir" ] && [ -d "$ocamlsrcdir/stdlib" ]; then
  stdlib_flags="-nostdlib -I $ocamlsrcdir/stdlib"
else
  stdlib_flags=""
fi

llvm_path="${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}"

cat > "$build_dir/amd64_scalar.ml" <<'EOF'
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
let alloc n = Array.init n (fun i -> string_of_int (i + fib 8))
let exn n = try if n = 0 then raise Exit else n with Exit -> 42
let () =
  let a = alloc 8 in
  Printf.printf "%d %s %.1f\n" (exn 0) a.(3) (sqrt 9.)
EOF

"$ocamlopt" $stdlib_flags -O3 -llvm-backend -llvm-path "$llvm_path" \
  -o "$build_dir/amd64_scalar.exe" "$build_dir/amd64_scalar.ml"
"$build_dir/amd64_scalar.exe" > "$build_dir/amd64_scalar.out"
grep -qx "42 24 3.0" "$build_dir/amd64_scalar.out"

cat > "$build_dir/amd64_float32.ml" <<'EOF'
external of_float : float -> float32 = "%float32offloat"
external to_float : float32 -> float = "%floatoffloat32"

let pack x y = of_float x, of_float y

let () =
  let x, y = pack 1.5 2.5 in
  Printf.printf "%.1f %.1f\n" (to_float x) (to_float y)
EOF

"$ocamlopt" $stdlib_flags -O3 -llvm-backend -llvm-path "$llvm_path" \
  -o "$build_dir/amd64_float32.exe" "$build_dir/amd64_float32.ml"
"$build_dir/amd64_float32.exe" > "$build_dir/amd64_float32.out"
grep -qx "1.5 2.5" "$build_dir/amd64_float32.out"

cat > "$build_dir/amd64_prefetch_stubs.c" <<'EOF'
#include <caml/mlvalues.h>

CAMLprim value caml_prefetch_read_high(value v)
{
  (void)v;
  return Val_unit;
}

CAMLprim value caml_prefetch_write_low(value v)
{
  (void)v;
  return Val_unit;
}

CAMLprim value caml_cldemote(value v)
{
  (void)v;
  return Val_unit;
}
EOF

cat > "$build_dir/amd64_prefetch.ml" <<'EOF'
external prefetch_read_high : 'a -> unit = "" "caml_prefetch_read_high"
  [@@noalloc] [@@builtin]
external prefetch_write_low : 'a -> unit = "" "caml_prefetch_write_low"
  [@@noalloc] [@@builtin]
external cldemote : 'a -> unit = "caml_no_bytecode_impl" "caml_cldemote"
  [@@noalloc] [@@builtin]

let r = ref 7

let () =
  prefetch_read_high r;
  prefetch_write_low r;
  cldemote r;
  Printf.printf "%d\n" !r
EOF

"$ocamlopt" $stdlib_flags -O3 -llvm-backend -llvm-path "$llvm_path" \
  -o "$build_dir/amd64_prefetch.exe" \
  "$build_dir/amd64_prefetch_stubs.c" "$build_dir/amd64_prefetch.ml"
"$build_dir/amd64_prefetch.exe" > "$build_dir/amd64_prefetch.out"
grep -qx "7" "$build_dir/amd64_prefetch.out"
