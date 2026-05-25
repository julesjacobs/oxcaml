(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_uncached_scalar_store.sh";
 script = "sh ${test_source_directory}/amd64_simd_uncached_scalar_store.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 scalar uncached stores under LLVM lowering. *)
