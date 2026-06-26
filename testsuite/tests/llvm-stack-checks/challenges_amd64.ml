(* TEST
 arch_amd64;
 runtime5;
 readonly_files = "challenges.sh";
 script = "sh ${test_source_directory}/challenges.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for LLVM stack growth, roots across stack checks, traps,
   effects, and stack-overflow reporting. *)
