(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_ssse3_mul_round.sh";
 script = "sh ${test_source_directory}/amd64_simd_ssse3_mul_round.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)
