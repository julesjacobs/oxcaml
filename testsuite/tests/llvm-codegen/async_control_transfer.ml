(* TEST
 macos;
 arch_arm64;
 readonly_files = "async_control_transfer.sh";
 script = "sh ${test_source_directory}/async_control_transfer.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that LLVM keeps async runtime transfers distinct from
   ordinary exception handlers. *)
