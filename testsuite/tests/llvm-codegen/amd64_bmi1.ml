(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_bmi1.sh";
 script = "sh ${test_source_directory}/amd64_bmi1.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 BMI1 scalar builtins under LLVM lowering. *)
