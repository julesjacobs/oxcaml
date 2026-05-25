#!/bin/sh

set -eu

build_dir=$(pwd)
llvm_path="${LLVM_PATH:-/tmp/oxcaml-clang-wrapper}"

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

compile_llvm () {
  "$ocamlopt" $stdlib_flags -O3 -g -llvm-backend -llvm-path "$llvm_path" "$@"
}

cat > "$build_dir/amd64_exception_recovery.ml" <<'EOF'
let[@inline never] make_payload n =
  Array.init 7 (fun i -> string_of_int (n + i))

let[@inline never] rec raise_from_deep_stack n acc =
  let payload = make_payload (n + acc) in
  if n = 0 then raise (Failure payload.(3));
  String.length payload.(0) + raise_from_deep_stack (n - 1) (acc + 1)

let () =
  Printexc.record_backtrace true;
  let result =
    try raise_from_deep_stack 4096 0 with
    | Failure s ->
      let bt = Printexc.get_backtrace () in
      if s <> "4099" then failwith ("bad exception payload: " ^ s);
      if String.length bt = 0 then failwith "missing backtrace";
      let after = Array.init 64 (fun i -> i + String.length s) in
      Array.fold_left ( + ) 0 after
  in
  Printf.printf "exception:%d\n" result
EOF

compile_llvm -o "$build_dir/amd64_exception_recovery.exe" \
  "$build_dir/amd64_exception_recovery.ml"
OCAMLRUNPARAM="b,l=1M" "$build_dir/amd64_exception_recovery.exe" \
  > "$build_dir/amd64_exception_recovery.out"
grep -qx "exception:2272" "$build_dir/amd64_exception_recovery.out"

cat > "$build_dir/amd64_liveness.ml" <<'EOF'
external float32_of_float : float -> float32 = "%float32offloat"
external float_of_float32 : float32 -> float = "%floatoffloat32"
external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

let[@inline never] force_alloc n =
  let r = ref 0 in
  for i = 0 to n do
    let a = Array.init 17 (fun j -> i + j) in
    r := !r + a.(i land 15)
  done;
  !r

let[@inline never] live_float32 f =
  let f = Sys.opaque_identity f in
  let n = force_alloc 2000 in
  float_of_float32 f +. float_of_int (n land 0)

let[@inline never] live_vec128 v =
  let v = Sys.opaque_identity v in
  let _ = force_alloc 2000 in
  let low = int64x2_low_int64 v in
  let high = int64x2_high_int64 v in
  Int64.add low high

let () =
  let f = live_float32 (float32_of_float 13.5) in
  let v = live_vec128 (int64x2_of_int64s 21L 34L) in
  Printf.printf "liveness:%.1f:%Ld\n" f v
EOF

cat > "$build_dir/amd64_vec_stubs.c" <<'EOF'
#include <stdint.h>
#include <smmintrin.h>

typedef __m128i int64x2_t;

int64x2_t vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}

int64_t vec128_low_int64(int64x2_t v)
{
  return _mm_extract_epi64(v, 0);
}

int64_t vec128_high_int64(int64x2_t v)
{
  return _mm_extract_epi64(v, 1);
}
EOF

compile_llvm -ccopt -msse4.2 -S -keep-llvmir \
  -o "$build_dir/amd64_liveness.exe" \
  "$build_dir/amd64_vec_stubs.c" "$build_dir/amd64_liveness.ml"
"$build_dir/amd64_liveness.exe" > "$build_dir/amd64_liveness.out"
grep -qx "liveness:13.5:55" "$build_dir/amd64_liveness.out"
grep -Eq 'caml_call_gc_(sse|avx|avx512)' "$build_dir/amd64_liveness.s"
grep -q '"no-realign-stack"' "$build_dir/amd64_liveness.ll"

cat > "$build_dir/amd64_c_abi_stubs.c" <<'EOF'
#include <stdint.h>
#include <smmintrin.h>

typedef __m128i int64x2_t;

static int64x2_t make_vec128(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}

static int64_t low_int64(int64x2_t v)
{
  return _mm_extract_epi64(v, 0);
}

static int64_t high_int64(int64x2_t v)
{
  return _mm_extract_epi64(v, 1);
}

int64x2_t amd64_mixed_abi(
  float f0, int64x2_t v0, int64_t i0, double d0,
  int64x2_t v1, float f1, int64_t i1, int64x2_t v2,
  double d1, int64_t i2, int64x2_t v3, float f2)
{
  int64_t low =
    low_int64(v0) + low_int64(v1)
    + low_int64(v2) + low_int64(v3)
    + i0 + i1 + i2 + (int64_t)f0 + (int64_t)f1 + (int64_t)f2
    + (int64_t)d0 + (int64_t)d1;
  int64_t high =
    high_int64(v0) + high_int64(v1)
    + high_int64(v2) + high_int64(v3);
  return make_vec128(low, high);
}

EOF

cat > "$build_dir/amd64_c_abi.ml" <<'EOF'
external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64" [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64" [@@noalloc] [@@unboxed]

external mixed_abi :
  float32 -> int64x2 -> int64 -> float ->
  int64x2 -> float32 -> int64 -> int64x2 ->
  float -> int64 -> int64x2 -> float32 ->
  int64x2
  = "" "amd64_mixed_abi" [@@noalloc] [@@unboxed]

external mixed_abi_alloc :
  float32 -> int64x2 -> int64 -> float ->
  int64x2 -> float32 -> int64 -> int64x2 ->
  float -> int64 -> int64x2 -> float32 ->
  int64x2
  = "" "amd64_mixed_abi" [@@unboxed]

external float32_of_float : float -> float32 = "%float32offloat"

let vec = int64x2_of_int64s

let check label r =
  let low = int64x2_low_int64 r in
  let high = int64x2_high_int64 r in
  Printf.printf "%s:%Ld:%Ld\n" label low high

let () =
  let r =
    mixed_abi (float32_of_float 1.0) (vec 10L 100L) 2L 3.0
      (vec 20L 200L) (float32_of_float 4.0) 5L (vec 30L 300L)
      6.0 7L (vec 40L 400L) (float32_of_float 8.0)
  in
  let r_alloc =
    mixed_abi_alloc (float32_of_float 1.0) (vec 10L 100L) 2L 3.0
      (vec 20L 200L) (float32_of_float 4.0) 5L (vec 30L 300L)
      6.0 7L (vec 40L 400L) (float32_of_float 8.0)
  in
  check "abi-noalloc" r;
  check "abi-alloc" r_alloc
EOF

compile_llvm -ccopt -msse4.2 -S -keep-llvmir \
  -o "$build_dir/amd64_c_abi.exe" \
  "$build_dir/amd64_vec_stubs.c" "$build_dir/amd64_c_abi_stubs.c" \
  "$build_dir/amd64_c_abi.ml"
"$build_dir/amd64_c_abi.exe" > "$build_dir/amd64_c_abi.out"
grep -qx "abi-noalloc:136:1000" "$build_dir/amd64_c_abi.out"
grep -qx "abi-alloc:136:1000" "$build_dir/amd64_c_abi.out"
grep -q 'caml_c_call_stack_args' "$build_dir/amd64_c_abi.ll"
grep -q '< 2 x i64 >' "$build_dir/amd64_c_abi.ll"
