(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_bmi2.sh";
 script = "sh ${test_source_directory}/amd64_bmi2.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 BMI2 scalar builtins under LLVM lowering. *)
