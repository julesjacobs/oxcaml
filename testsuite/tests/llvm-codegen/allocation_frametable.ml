(* TEST
 macos;
 arch_arm64;
 readonly_files = "allocation_frametable.sh";
 script = "sh ${test_source_directory}/allocation_frametable.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script compiles a small allocation program with [-g -S] and checks the
   final LLVM-generated frametable, not just the LLVM IR. *)
