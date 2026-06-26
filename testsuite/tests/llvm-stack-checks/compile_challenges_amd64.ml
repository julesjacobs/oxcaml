(* TEST
 arch_amd64;
 runtime5;
 readonly_files = "compile_challenges.sh";
 script = "sh ${test_source_directory}/compile_challenges.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for compiling a deep generated program through the LLVM
   backend while the compiler itself runs close to stack limits. *)
