(* TEST
 arch_amd64;
 readonly_files = "amd64_c_stack_args_roots.sh";
 script = "sh ${test_source_directory}/amd64_c_stack_args_roots.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 coverage for allocating C calls with outgoing stack arguments and an
   OCaml root live across the call. *)
