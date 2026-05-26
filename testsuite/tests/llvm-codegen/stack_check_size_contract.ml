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
   post-stack-check CFG dump. A nonzero byte-count contract means the CFG check
   is responsible for stack growth; LLVM should not add a duplicate prologue
   check. The legacy boolean stack-check request is still emitted for the
   no-CFG-stack-check path. *)
