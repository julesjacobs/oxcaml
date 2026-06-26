(* TEST
 arch_amd64;
 stack-checks;
 readonly_files = "stack_check_size_contract.sh";
 script = "sh ${test_source_directory}/stack_check_size_contract.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* This variant keeps the LLVM stack-check byte-count contract covered on AMD64.
   The script checks both the CFG-to-LLVM contract and target-specific prologue
   stack checks emitted by the LLVM backend. *)
