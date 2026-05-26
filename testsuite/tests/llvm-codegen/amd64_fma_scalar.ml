(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_fma_scalar.sh";
 script = "sh ${test_source_directory}/amd64_fma_scalar.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 scalar FMA builtins under LLVM lowering. *)
