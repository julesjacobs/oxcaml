(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_int_compare.sh";
 script = "sh ${test_source_directory}/amd64_simd_int_compare.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 integer compare builtins under LLVM lowering. *)
