(* TEST
 has-z3;
 readonly_files = "x_names.ml y_names.ml two_groups.ml run.sh";
 arguments = "${test_source_directory}/run.sh ${ocamlc_opt} ${ocamlsrcdir}/stdlib";
 bytecode;
*)

(* A refuted query's counterexample is shown with the source names of its
   variables, and single obligations search for small values while batches do
   not; neither is in the query's SMT-LIB text, so the query cache is also
   keyed by them. The output of a compilation must not depend on what the
   cache already holds: run.sh compiles the same files with a cold cache, a
   warm one, and a cache warmed by another file (x_names.ml and y_names.ml
   send the same query text under other names). two_groups.ml sends a batch,
   whose refutation records the solver's unreduced model, before its single
   obligations. *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command "sh" arguments))
