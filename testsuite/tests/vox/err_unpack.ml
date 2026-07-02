(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Unpacking requires a refined scrutinee. *)

let f (x : int) = let refine_ w = x in w
