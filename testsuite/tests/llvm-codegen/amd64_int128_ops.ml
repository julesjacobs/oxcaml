(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 modules = "amd64_int128_ops_stubs.c";
 include stdlib_upstream_compatible;
 flags += " -O3 -llvm-backend";
 native;
*)

module I64 = Stdlib_upstream_compatible.Int64_u

external add_int128 :
  x0:int64# -> x1:int64# -> y0:int64# -> y1:int64# -> #(int64# * int64#)
  = "" "caml_int128_add"
[@@noalloc] [@@builtin]

external sub_int128 :
  x0:int64# -> x1:int64# -> y0:int64# -> y1:int64# -> #(int64# * int64#)
  = "" "caml_int128_sub"
[@@noalloc] [@@builtin]

external mul_int64 : int64# -> int64# -> #(int64# * int64#)
  = "" "caml_int64_mul128"
[@@noalloc] [@@builtin]

external unsigned_mul_int64 : int64# -> int64# -> #(int64# * int64#)
  = "" "caml_unsigned_int64_mul128"
[@@noalloc] [@@builtin]

let check got expected =
  if not (I64.equal got expected)
  then
    Printf.ksprintf failwith "%Lx <> %Lx" (I64.to_int64 got)
      (I64.to_int64 expected)

let max_i64 = #0x7fff_ffff_ffff_ffffL

let min_i64 = #0x8000_0000_0000_0000L

let () =
  let #(low, high) =
    add_int128 ~x0:(-#1L) ~x1:#0L ~y0:#1L ~y1:#0L
  in
  check low #0L;
  check high #1L;
  let #(low, high) =
    add_int128 ~x0:#0L ~x1:min_i64 ~y0:(-#1L) ~y1:(-#1L)
  in
  check low (-#1L);
  check high max_i64;
  let #(low, high) =
    sub_int128 ~x0:#0L ~x1:#1L ~y0:#1L ~y1:#0L
  in
  check low (-#1L);
  check high #0L;
  let #(low, high) =
    sub_int128 ~x0:#0L ~x1:min_i64 ~y0:#1L ~y1:#0L
  in
  check low (-#1L);
  check high max_i64;
  let #(low, high) = mul_int64 min_i64 #2L in
  check low #0L;
  check high (-#1L);
  let #(low, high) = mul_int64 max_i64 (-#2L) in
  check low #2L;
  check high (-#1L);
  let #(low, high) = unsigned_mul_int64 min_i64 #2L in
  check low #0L;
  check high #1L;
  let #(low, high) = unsigned_mul_int64 (-#1L) #2L in
  check low I64.(shift_left (-#1L) 1);
  check high #1L
