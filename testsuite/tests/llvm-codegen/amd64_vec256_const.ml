(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 modules = "amd64_vec256_const_stubs.c";
 flags += " -O3 -llvm-backend";
 native;
*)

external int64x4_const4 : int64 -> int64 -> int64 -> int64 -> int64x4
  = "" "caml_int64x4_const4"
[@@noalloc] [@@unboxed] [@@builtin]

let[@inline never] make () =
  int64x4_const4 0x0123_4567_89ab_cdefL 0xfedc_ba98_7654_3210L
    0x1122_3344_5566_7788L 0x99aa_bbcc_ddee_ff00L

let () = ignore (Sys.opaque_identity (make ()))
