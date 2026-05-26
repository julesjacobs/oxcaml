(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_clmul.sh";
 script = "sh ${test_source_directory}/amd64_simd_clmul.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 CLMUL int64x2 carry-less multiply under LLVM lowering. *)
