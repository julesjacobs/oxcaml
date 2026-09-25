(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sat.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sat.mli";
 ocamlc.byte;
 module = "sat_solver_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

open Vox_sat

let forged_unsat () =
  let report = {answer = Unsat; fuel_left = 0} in
  unsat_at 1 [[Positive 0]] report [true]
