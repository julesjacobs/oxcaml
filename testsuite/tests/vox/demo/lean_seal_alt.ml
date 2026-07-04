(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/step.mli ../lib/step_double/step.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The client body of lean_seal.ml, verbatim, against the OTHER
   implementation of step.mli.  Nothing here changed but the linked
   module; the sig module the proof runs against is byte-identical. *)

open Step

let grows : int{ 0 < _ } =
  let a = step 0 in
  let b = step a in
  b
