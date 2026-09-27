(* TEST
 has-z3;
 readonly_files = "axioms.ml client.ml hidden.ml hidden_client.ml run.sh";
 arguments = "${test_source_directory}/run.sh ${ocamlc_byte} ${ocamlsrcdir}/stdlib";
 bytecode;
*)

(* What a unit's verification trusts is recorded in its compiled files:
   trusted externals, totality casts, unsafe features, and whether
   verification was skipped with -smt-assume-verified. -vox-audit lists it
   for a unit and the units it depends on, and a verified unit that imports
   an interface whose verification was skipped is warned about. The
   compilations are separate processes, so run.sh drives them. *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command "sh" arguments))
