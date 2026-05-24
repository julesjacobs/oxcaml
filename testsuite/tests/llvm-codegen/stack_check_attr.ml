(* TEST
 macos;
 arch_arm64;
 readonly_files = "stack_check_attr.sh";
 script = "sh ${test_source_directory}/stack_check_attr.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that the LLVM backend only asks the AArch64 backend to
   emit a prologue stack check when the CFG contains a [Stack_check]. *)
