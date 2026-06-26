(* TEST
 arch_amd64;
 stack-checks;
 readonly_files = "stack_check_size_contract.sh";
 script = "sh ${test_source_directory}/stack_check_size_contract.sh no-cfg-stack-checks";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* This variant checks that disabling CFG stack checks on AMD64 falls back to
   LLVM prologue stack checks without emitting a byte-count contract. *)
