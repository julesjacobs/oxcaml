(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_http_spec.mli vox_http.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -vox-library";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "vox_http_spec.mli";
 ocamlc.opt;
 module = "vox_http.mli";
 ocamlc.opt; flags = "-extension refinement_types";
 module = "http_private_rejected.ml";
 ocamlc_opt_exit_status = "2";
 ocamlc.opt;
 check-ocamlc.opt-output;
*)
let hidden = Vox_http.Internal.feed
