(* TEST
 flags = "-vox-dry-run";
 modules = "sortmm_i.mli sortmm_i.ml";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* An .mli/.ml pair must agree on [@@vox.sort]: sorts are computed
   per-compilation from the visible declaration, so a mismatch would
   let clients reason at one sort against an implementation verified
   at another.  The pair here has the attribute only in the interface;
   compilation of the implementation must fail. *)

let unused = 0
