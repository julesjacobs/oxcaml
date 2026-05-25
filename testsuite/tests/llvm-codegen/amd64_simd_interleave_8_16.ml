(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_interleave_8_16.sh";
 script = "sh ${test_source_directory}/amd64_simd_interleave_8_16.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

