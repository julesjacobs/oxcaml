(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_int16_mul.sh";
 script = "sh ${test_source_directory}/amd64_simd_int16_mul.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

