(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_http_spec.mli vox_http.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "vox_http_spec.mli";
 ocamlc.opt;
 module = "vox_http.mli";
 ocamlc.opt;
 module = "http_suffix_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)
open Vox_http_spec
open Vox_http

let (discard_pipeline @ total) (request : request) (suffix : bytes) :
    {u : unit | if well_formed request then
      (feed (initial ()) (Vox_sequence.append (serialize request) suffix)).rest === [] else true} =
  roundtrip request suffix;
  ()
