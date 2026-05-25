(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_bitwise.sh";
 script = "sh ${test_source_directory}/amd64_simd_bitwise.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE vec128 bitwise builtins under LLVM lowering. *)
