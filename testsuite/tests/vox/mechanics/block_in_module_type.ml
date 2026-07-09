(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* W6: a [%%vox.lean] block in a MODULE TYPE declaration is likewise unit-level
   in the wrong place.  Previously [check_no_nested_blocks_signature] had no
   [Tsig_modtype]/[Tstr_modtype] case, so such a block was neither collected nor
   rejected -- silently dropped.  Now rejected with the standard message. *)

module type P = sig
  type t
  [%%vox.lean {lean|
  public opaque Z : Type
  |lean}]
  val f : (x : int) -> int
end
