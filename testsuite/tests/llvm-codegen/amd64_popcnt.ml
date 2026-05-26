(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_popcnt.sh";
 script = "sh ${test_source_directory}/amd64_popcnt.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 POPCNT scalar builtins under LLVM lowering. *)
