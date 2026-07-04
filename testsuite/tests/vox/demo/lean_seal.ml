(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/step.mli ../lib/step_incr/step.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Client of the sealed interface, linked against step_incr.
   lean_seal_alt.ml verifies this IDENTICAL client body against
   step_double: the proof below uses only the interface's law
   [step_gt] -- [step] itself is opaque, so no client fact can
   distinguish the implementations. *)

open Step

let grows : int{ 0 < _ } =
  let a = step 0 in
  let b = step a in
  b
