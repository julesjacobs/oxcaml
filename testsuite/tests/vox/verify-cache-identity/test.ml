(* TEST
 has-z3;
 readonly_files = "checked.ml run.sh";
 arguments = "${test_source_directory}/run.sh ${ocamlc_byte} ${ocamlsrcdir}/stdlib";
 bytecode;
*)

(* The verification caches identify the solver by its version, not only by
   its name: a different solver installed at the same path must not replay
   the old solver's results. Without -smt-solver-any-version, only the
   expected Z3 version is used at all. The compilations are separate
   processes, so run.sh drives them. *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command "sh" arguments))
