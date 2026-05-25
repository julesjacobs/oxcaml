(* TEST
 arch_amd64;
 readonly_files = "amd64_regressions.sh";
 script = "sh ${test_source_directory}/amd64_regressions.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script builds and runs focused AMD64 LLVM-backend regressions. *)
