(* TEST
 flags = "-vox-solver z3";
 script = "sh ${test_source_directory}/../has-z3.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* vox: a false obligation must fail under the z3 backend (the lean
   twin is lean_fail.ml).  z3's [sat] answer maps to a deterministic,
   compiler-rendered message, so this reference does not depend on
   z3's output text. *)

let bad : {v:int | v > 0} = refine_ 0
