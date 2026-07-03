(* TEST
 flags = "-vox-solver z3";
 script = "sh ${test_source_directory}/../has-z3.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: a false datatype obligation must fail under the z3 backend
   (the lean twin is lean_adt_fail.ml): K 4 and K 3 are distinct by
   injectivity in the datatype theory, so this equality must be
   refuted. *)

type t =
  | K of int
  | L

let bad : t{ _ = K 3 } = refine_ (K 4)
