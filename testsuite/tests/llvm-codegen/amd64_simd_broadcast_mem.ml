(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_broadcast_mem.sh";
 script = "sh ${test_source_directory}/amd64_simd_broadcast_mem.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 AVX SIMD memory broadcasts under LLVM lowering. *)
