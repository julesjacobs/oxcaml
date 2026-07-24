(* TEST
 readonly_files = "\
   no_verify_api.mli no_verify_api.ml no_verify_client.ml \
   no_verify_mismatch.mli no_verify_mismatch.ml \
 ";
 setup-ocamlc.byte-build-env;
 flags = "-vox-no-verify -vox-backend z3 -vox-smt-solver false";

 module = "no_verify_api.mli";
 ocamlc.byte;
 module = "no_verify_api.ml";
 ocamlc.byte;
 module = "no_verify_client.ml";
 ocamlc.byte;

 (* Skipping refinement verification does not skip ordinary interface
    conformance. *)
 module = "no_verify_mismatch.mli";
 ocamlc.byte;
 module = "no_verify_mismatch.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 (* Mutually exclusive modes and VC output requests reject in either command
    line order. *)
 module = "no_verify_client.ml";
 flags = "-vox-no-verify -vox-type-only";
 ocamlc.byte;
 flags = "-vox-type-only -vox-no-verify";
 ocamlc.byte;
 flags = "-vox-no-verify -vox-dump-vc-json vcs.json";
 ocamlc.byte;
 flags = "-vox-dump-vc-json vcs.json -vox-no-verify";
 ocamlc.byte;
 flags = "-vox-no-verify -vox-dump-vc-json-smt";
 ocamlc.byte;
 flags = "-vox-dump-vc-json-smt -vox-no-verify";
 ocamlc.byte;
 flags = "-vox-no-verify -vox-dump-vc";
 ocamlc.byte;
 flags = "-vox-dump-vc -vox-no-verify";
 ocamlc.byte;
*)

(* [-vox-no-verify] preserves normal compilation artifacts and cross-unit
   typing while performing no refinement discharge. *)
