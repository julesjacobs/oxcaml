(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_sse41_int_minmax_cmp.sh";
 script = "sh ${test_source_directory}/amd64_simd_sse41_int_minmax_cmp.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
