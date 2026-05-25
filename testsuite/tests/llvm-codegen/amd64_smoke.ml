(* TEST
 arch_amd64;
 readonly_files = "amd64_smoke.sh";
 script = "sh ${test_source_directory}/amd64_smoke.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script builds and runs small AMD64 programs with [-llvm-backend]. *)
