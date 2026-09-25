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
 module = "http_body_rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
open Vox_http_spec
open Vox_http

let (discard_body @ total) (input : bytes) :
    {u : unit | match (status (feed (initial ()) input).state) with
      | Complete request -> request.body === [] | _ -> true} =
  let refine_ result = parse input in
  let u = () in refine_ u
