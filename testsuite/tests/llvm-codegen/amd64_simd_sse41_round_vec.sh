#!/bin/sh

set -eu

build_dir=$(pwd)
src="$build_dir/amd64_simd_sse41_round_vec_generated.ml"
out="$build_dir/amd64_simd_sse41_round_vec_generated.o"
ir="$build_dir/amd64_simd_sse41_round_vec_generated.ll"

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
external round32 : (int[@untagged]) -> float32x4# -> float32x4#
  = "" "caml_sse41_float32x4_round"
[@@noalloc] [@@builtin]

external round64 : (int[@untagged]) -> float64x2# -> float64x2#
  = "" "caml_sse41_float64x2_round"
[@@noalloc] [@@builtin]

let[@inline never] round32_nearest v = round32 0x8 v
let[@inline never] round32_floor v = round32 0x9 v
let[@inline never] round32_ceil v = round32 0xA v
let[@inline never] round32_trunc v = round32 0xB v
let[@inline never] round32_current v = round32 0xC v

let[@inline never] round64_nearest v = round64 0x8 v
let[@inline never] round64_floor v = round64 0x9 v
let[@inline never] round64_ceil v = round64 0xA v
let[@inline never] round64_trunc v = round64 0xB v
let[@inline never] round64_current v = round64 0xC v
EOF

"$ocamlopt" \
  -extension layouts_alpha -extension simd_beta -extension small_numbers \
  -O3 -llvm-backend -llvm-path "${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}" \
  -c -S -keep-llvmir -o "$out" "$src"

if [ ! -f "$ir" ]; then
  echo "expected generated IR missing: $ir" >&2
  exit 1
fi

test "$(grep -c '@llvm\.roundeven\.v4f32' "$ir")" -ge 1
test "$(grep -c '@llvm\.floor\.v4f32' "$ir")" -ge 1
test "$(grep -c '@llvm\.ceil\.v4f32' "$ir")" -ge 1
test "$(grep -c '@llvm\.trunc\.v4f32' "$ir")" -ge 1
test "$(grep -c '@llvm\.nearbyint\.v4f32' "$ir")" -ge 1
test "$(grep -c '@llvm\.roundeven\.v2f64' "$ir")" -ge 1
test "$(grep -c '@llvm\.floor\.v2f64' "$ir")" -ge 1
test "$(grep -c '@llvm\.ceil\.v2f64' "$ir")" -ge 1
test "$(grep -c '@llvm\.trunc\.v2f64' "$ir")" -ge 1
test "$(grep -c '@llvm\.nearbyint\.v2f64' "$ir")" -ge 1
