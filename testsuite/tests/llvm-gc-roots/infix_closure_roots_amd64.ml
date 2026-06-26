(* TEST
 arch_amd64;
 readonly_files = "infix_closure_roots.sh infix_closure_roots.ml";
 script = "sh ${test_source_directory}/infix_closure_roots.sh";
 setup-ocamlopt.opt-build-env;
 script;
*)

(* AMD64 runtime coverage for infix closures that stay live across GC. *)
