(* TEST
 flags = "-vox-dry-run";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* v0 refinements are supported at int and bool only. *)

let s : {v:string | v = v} = refine_ "hi"
