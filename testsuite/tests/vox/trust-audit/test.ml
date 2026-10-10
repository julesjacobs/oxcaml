(* TEST
 has-z3;
 readonly_files = "axioms.ml client.ml helper.ml user.ml good.ml good_user.ml";
 readonly_files += " hidden.ml hidden_client.ml asserted.ml asserted_user.ml run.sh";
 {
   arguments = "${test_source_directory}/run.sh ${ocamlc_opt} ${ocamlsrcdir}/stdlib";
   bytecode;
 }
 {
   arguments = "${test_source_directory}/run.sh ${ocamlopt_byte} ${ocamlsrcdir}/stdlib";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   run;
   check-program-output;
 }
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
