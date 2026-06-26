(* TEST
 arch_amd64;
 readonly_files = "runtime_roots_amd64.sh allocation_slow_path_roots.ml allocation_slow_path_roots.reference closure_call_roots.ml closure_call_roots.reference live_values_roots.ml live_values_roots.reference trap_roots.ml trap_roots.reference";
 script = "sh ${test_source_directory}/runtime_roots_amd64.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 runtime coverage for LLVM GC roots across allocation slow paths,
   closure calls, ordinary live values, and trap handlers. *)
