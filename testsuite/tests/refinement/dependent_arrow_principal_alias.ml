(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 ocamlc.byte;
 flags = "-principal -c";
 compiler_output = "dependent_arrow_principal_alias.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/dependent_arrow_principal_alias.reference";
 check-ocamlc.byte-output;
*)

(* CR vox: This unannotated alias is accepted in default mode, but principal
   mode infers [whole] as logical where the result refinement expects a
   physical value. Reconcile the alias mode inference between the two modes. *)
let rec step (n as whole) : int{ _ = whole } =
  let _ = whole in
  if n = 0 then 0 else step (n - 1) + 1
