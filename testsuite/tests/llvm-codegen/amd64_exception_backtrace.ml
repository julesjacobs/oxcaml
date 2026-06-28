(* TEST
 arch_amd64;
 readonly_files = "amd64_exception_backtrace.sh";
 script = "sh ${test_source_directory}/amd64_exception_backtrace.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for source-level OCaml backtraces through LLVM-generated
   exception unwinding in the normal backend mode. *)
