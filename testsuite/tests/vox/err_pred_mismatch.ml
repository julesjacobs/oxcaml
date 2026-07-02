(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Rigid refined types: predicates compare structurally, so v > 0 and
   0 < v are DIFFERENT types (a documented sharp edge). *)

let x : {v:int | v > 0} = refine_ 3
let y : {v:int | 0 < v} = x
