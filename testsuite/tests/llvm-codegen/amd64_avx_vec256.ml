(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_avx_vec256.sh";
 script = "sh ${test_source_directory}/amd64_avx_vec256.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 AVX 256-bit vector builtins under LLVM lowering. *)
