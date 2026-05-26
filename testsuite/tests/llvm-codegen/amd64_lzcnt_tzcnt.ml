(* TEST
 not-windows;
 not-macos;
 arch_amd64;
 readonly_files = "amd64_lzcnt_tzcnt.sh";
 script = "sh ${test_source_directory}/amd64_lzcnt_tzcnt.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script checks AMD64 LZCNT/TZCNT scalar builtins under LLVM lowering. *)
