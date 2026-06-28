(* TEST
 arch_amd64;
 readonly_files = "post_ra_root_verifier_amd64.sh";
 script = "sh ${test_source_directory}/post_ra_root_verifier_amd64.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for the post-register-allocation OxCaml root verifier. *)
