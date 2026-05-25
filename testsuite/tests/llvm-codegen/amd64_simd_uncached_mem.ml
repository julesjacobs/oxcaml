(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_uncached_mem.sh";
 script = "sh ${test_source_directory}/amd64_simd_uncached_mem.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 uncached SIMD memory builtins under LLVM lowering. *)
