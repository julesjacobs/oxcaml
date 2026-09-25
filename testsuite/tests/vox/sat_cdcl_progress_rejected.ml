(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sat_spec.mli vox_cdcl_total.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "vox_sat_spec.mli";
 ocamlc.byte;
 module = "vox_cdcl_total.mli";
 ocamlc.byte;
 module = "sat_cdcl_progress_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

open Vox_sat_spec

let (unsupported_cdcl_depth @ total) :
    (n : {n : int | 0 <= n && n <= 256}) ->
    (formula : {f : formula |
      valid_formula n f && clauses_fit 4096 f && literals_fit 65536 f}) ->
    {r : (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result |
      match r with
      | Error _ -> false
      | Ok report -> match report.answer with
        | Vox_cdcl_total.Unknown -> false
        | Vox_cdcl_total.Sat _ | Vox_cdcl_total.Unsat -> true} =
  fun n formula -> Vox_cdcl_total.solve (n + 1) n formula
