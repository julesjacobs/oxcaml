(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "../lib/step.mli ../lib/step_bad/step.ml";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The seal fails closed: step_bad implements step.mli's VALUE
   contract honestly (its [step x = x] verifies against its own
   [def step]), but proves no [step_gt] -- and could not, it is false
   of the identity.  The unit is refused at the seal, so this client
   is never reached: no implementation that cannot pay the
   interface's laws can ever serve a client that relies on them. *)

open Step

let would_grow : int{ 0 < _ } =
  let a = step 0 in
  let b = step a in
  b
