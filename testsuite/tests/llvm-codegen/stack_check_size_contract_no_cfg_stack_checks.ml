(* TEST
 macos;
 arch_arm64;
 stack-checks;
 readonly_files = "stack_check_size_contract.sh";
 script = "sh ${test_source_directory}/stack_check_size_contract.sh no-cfg-stack-checks";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* This variant checks that `-no-cfg-stack-checks` does not emit a misleading
   byte-count contract from a CFG where stack-check insertion did not run. *)
