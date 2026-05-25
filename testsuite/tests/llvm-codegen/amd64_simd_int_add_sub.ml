(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_int_add_sub.sh";
 script = "sh ${test_source_directory}/amd64_simd_int_add_sub.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 integer add/sub builtins under LLVM lowering. *)
