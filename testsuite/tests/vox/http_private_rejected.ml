(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_http_spec.mli vox_http.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "vox_sequence.mli";
 ocamlc.byte;
 module = "vox_http_spec.mli";
 ocamlc.byte;
 module = "vox_http.mli";
 ocamlc.byte;
 module = "http_private_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
let hidden = Vox_http.Internal.feed
