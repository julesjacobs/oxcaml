(* TEST
 arch_amd64;
 runtime5;
 readonly_files = "stack_growth.ml stack_growth.reference stack_growth_amd64.sh";
 script = "sh ${test_source_directory}/stack_growth_amd64.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for LLVM stack growth through effect continuation capture. *)
