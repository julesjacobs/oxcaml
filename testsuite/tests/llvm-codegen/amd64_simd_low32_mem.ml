(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_low32_mem.sh";
 script = "sh ${test_source_directory}/amd64_simd_low32_mem.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 low-32 SIMD memory builtins under LLVM lowering. *)
