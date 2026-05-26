(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_sse42_int64_cmpgt.sh";
 script = "sh ${test_source_directory}/amd64_simd_sse42_int64_cmpgt.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE4.2 int64x2 signed compare-greater-than under LLVM lowering. *)
