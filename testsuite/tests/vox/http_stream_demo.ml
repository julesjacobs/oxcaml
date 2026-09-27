(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 source_directories += " ${test_source_directory}/../../../verification/demos";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_http_spec.mli";
 all_modules += " vox_http_spec.ml vox_http.mli vox_http.ml http_stream.ml";
 { bytecode; }
*)

(* Builds and runs the HTTP streaming example,
   verification/demos/http_stream.ml, and compares its output with
   http_stream_demo.reference. *)
