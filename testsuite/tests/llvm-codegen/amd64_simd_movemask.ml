(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_movemask.sh";
 script = "sh ${test_source_directory}/amd64_simd_movemask.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE/SSE2 movemask builtins under LLVM lowering. *)
