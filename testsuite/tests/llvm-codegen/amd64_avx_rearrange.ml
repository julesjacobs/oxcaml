(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_avx_rearrange.sh";
 script = "sh ${test_source_directory}/amd64_avx_rearrange.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 AVX vector rearrangement builtins under LLVM lowering. *)
