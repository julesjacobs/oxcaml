(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The argument for a dependent parameter must be a variable. *)

let lt : (x : int) -> (y : int) -> {z:bool | z = (x < y)} =
  fun x y -> assume_ (x < y)

let bad (x : int) = lt 0 x
