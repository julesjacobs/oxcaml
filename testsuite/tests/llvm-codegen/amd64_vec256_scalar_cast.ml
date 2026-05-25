(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 modules = "amd64_vec256_scalar_cast_stubs.c";
 flags += " -O3 -llvm-backend";
 native;
*)

external int64x4_low_of_int64 : int64 -> int64x4
  = "" "caml_int64x4_low_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64x4_low_to_int64 : int64x4 -> int64
  = "" "caml_int64x4_low_to_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external float64x4_low_of_float : float -> float64x4
  = "" "caml_float64x4_low_of_float"
[@@noalloc] [@@unboxed] [@@builtin]

external float64x4_low_to_float : float64x4 -> float
  = "" "caml_float64x4_low_to_float"
[@@noalloc] [@@unboxed] [@@builtin]

let check_int64 got expected =
  if got <> expected
  then Printf.ksprintf failwith "%Lx <> %Lx" got expected

let check_float got expected =
  if got <> expected
  then Printf.ksprintf failwith "%h <> %h" got expected

let () =
  let i = 0x0123_4567_89ab_cdefL in
  let iv = Sys.opaque_identity (int64x4_low_of_int64 i) in
  check_int64 (int64x4_low_to_int64 iv) i;
  let f = -1234.5 in
  let fv = Sys.opaque_identity (float64x4_low_of_float f) in
  check_float (float64x4_low_to_float fv) f
