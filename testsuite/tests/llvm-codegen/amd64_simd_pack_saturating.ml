(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_simd_pack_saturating.sh";
 script = "sh ${test_source_directory}/amd64_simd_pack_saturating.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 SSE2 pack/narrow builtins under LLVM lowering. *)
