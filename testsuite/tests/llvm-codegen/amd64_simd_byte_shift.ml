(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_byte_shift.sh";
 script = "sh ${test_source_directory}/amd64_simd_byte_shift.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 byte-shift builtins under LLVM lowering. *)
