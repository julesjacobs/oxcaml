(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_avg_unsigned.sh";
 script = "sh ${test_source_directory}/amd64_simd_avg_unsigned.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 unsigned average builtins under LLVM lowering. *)
