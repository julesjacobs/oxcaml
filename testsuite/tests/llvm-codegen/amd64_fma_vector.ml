(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_fma_vector.sh";
 script = "sh ${test_source_directory}/amd64_fma_vector.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 vector FMA builtins under LLVM lowering. *)
