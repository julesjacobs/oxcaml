(* TEST
 arch_amd64;
 readonly_files = "trap_recovery_runtime.sh";
 script = "sh ${test_source_directory}/trap_recovery_runtime.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 runtime coverage for exception and trap recovery through LLVM-generated
   code.  The AArch64-specific trap-intrinsic and frame-table assertions remain
   target-gated in the shared script. *)
