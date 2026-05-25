(* TEST
 macos;
 arch_arm64;
 stack-checks;
 readonly_files = "stack_check_size_contract.sh";
 script = "sh ${test_source_directory}/stack_check_size_contract.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks the OxCaml LLVM stack-check byte-count contract against the
   post-stack-check CFG dump, and characterizes the outgoing stack-argument
   adjustment for the noalloc extcall case. *)
