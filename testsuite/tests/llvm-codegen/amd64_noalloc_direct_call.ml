(* TEST
 arch_amd64;
 readonly_files = "amd64_noalloc_direct_call.sh";
 script = "sh ${test_source_directory}/amd64_noalloc_direct_call.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for lowering noalloc external calls as direct C ABI calls
   with hidden runtime-register dependencies, rather than generated wrappers. *)
