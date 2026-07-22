(* TEST
 readonly_files = "\
   parameter_import_provider.mli parameter_import_provider.ml \
   parameter_import_client.ml \
   parameter_import_unrelated.ml parameter_import_unrelated.reference \
   parameter_import_domain.mli parameter_import_domain.ml \
   parameter_import_domain_client.ml parameter_import_same_unit.ml \
 ";
 setup-ocamlc.byte-build-env;

 module = "parameter_import_provider.mli";
 ocamlc.byte;
 module = "parameter_import_provider.ml";
 ocamlc.byte;

 module = "parameter_import_client.ml";
 flags = "-vox-backend lean";
 ocamlc.byte;

 module = "parameter_import_unrelated.ml";
 flags = "-vox-backend lean";
 compiler_output = "parameter_import_unrelated.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "\
   ${test_source_directory}/parameter_import_unrelated.reference";
 check-ocamlc.byte-output;

 module = "parameter_import_domain.mli";
 flags = "";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "parameter_import_domain.ml";
 ocamlc.byte;
 module = "parameter_import_domain_client.ml";
 flags = "-vox-backend lean";
 ocamlc.byte;

 module = "parameter_import_same_unit.ml";
 ocamlc.byte;
*)

(* Importing a dependent function type keeps one parameter identity coherent
   across all of the type's refinement descriptors. *)
