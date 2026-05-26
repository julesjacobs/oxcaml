(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_sse3_dup.sh";
 script = "sh ${test_source_directory}/amd64_simd_sse3_dup.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
