(* TEST
 arch_amd64;
 readonly_files = "raw_stack_word_stubs.c raw_stack_word.sh";
 script = "sh ${test_source_directory}/raw_stack_word.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for preserving a raw stack-looking nativeint# across LLVM
   stack growth.  The script builds and runs the shared program so it can link
   the local C helper. *)
