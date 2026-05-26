(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_avx_float.sh";
 script = "sh ${test_source_directory}/amd64_avx_float.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 AVX packed-float builtins under LLVM lowering. *)
