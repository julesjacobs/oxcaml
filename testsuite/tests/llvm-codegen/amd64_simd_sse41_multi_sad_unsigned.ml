(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_sse41_multi_sad_unsigned.sh";
 script = "sh ${test_source_directory}/amd64_simd_sse41_multi_sad_unsigned.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE4.1 unsigned multi-SAD builtin under LLVM lowering. *)
