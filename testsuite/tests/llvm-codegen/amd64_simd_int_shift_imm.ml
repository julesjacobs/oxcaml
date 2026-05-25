(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_int_shift_imm.sh";
 script = "sh ${test_source_directory}/amd64_simd_int_shift_imm.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 immediate integer shifts under LLVM lowering. *)
