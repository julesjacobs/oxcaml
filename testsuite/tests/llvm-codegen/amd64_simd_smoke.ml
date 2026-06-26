(* TEST
 arch_amd64;
 readonly_files = "amd64_simd_smoke.sh";
 script = "sh ${test_source_directory}/amd64_simd_smoke.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks a small set of AMD64 SIMD builtins through the LLVM backend.
   These cover the AMD64-specific selection path plus LLVM lowering for SSE2
   scalar float min/max, SSE4.1 lane blends, and AVX 256-bit lane blends. *)
