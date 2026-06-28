(* TEST
 macos;
 arch_arm64;
 readonly_files = "simd_variable_shifts.sh";
 script = "sh ${test_source_directory}/simd_variable_shifts.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks that the LLVM backend lowers AArch64 SIMD variable shifts
   to the target intrinsics, then runs out-of-range shift-count cases. *)
