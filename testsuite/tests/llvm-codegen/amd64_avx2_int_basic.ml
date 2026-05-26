(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_avx2_int_basic.sh";
 script = "sh ${test_source_directory}/amd64_avx2_int_basic.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 AVX2 packed integer basics under LLVM lowering. *)
