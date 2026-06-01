(* TEST
 macos;
 arch_arm64;
 readonly_files = "long_frame_failure.sh";
 script = "sh ${test_source_directory}/long_frame_failure.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script generates a small LLVM IR input that exercises long-frame
   frametable encoding directly. *)
