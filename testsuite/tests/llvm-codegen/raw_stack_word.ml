(* TEST
 macos;
 arch_arm64;
 readonly_files = "raw_stack_word_stubs.c raw_stack_word_failure.sh";
 script = "sh ${test_source_directory}/raw_stack_word_failure.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* The script builds and runs the actual program so it can link the local C
   helper. *)
