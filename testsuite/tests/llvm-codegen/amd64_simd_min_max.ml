(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_min_max.sh";
 script = "sh ${test_source_directory}/amd64_simd_min_max.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 integer min/max builtins under LLVM lowering. *)
