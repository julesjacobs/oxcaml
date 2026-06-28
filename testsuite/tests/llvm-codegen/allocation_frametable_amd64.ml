(* TEST
 arch_amd64;
 readonly_files = "allocation_frametable.sh";
 script = "sh ${test_source_directory}/allocation_frametable.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for LLVM allocation frametable records. *)
