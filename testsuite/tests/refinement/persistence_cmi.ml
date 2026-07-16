(* TEST
 readonly_files = "persistence_interface.mli persistence_import.ml";
 setup-ocamlc.byte-build-env;
 module = "persistence_interface.mli";
 ocamlc.byte;
 script = "mv persistence_interface.cmi first.cmi";
 script;
 module = "persistence_interface.mli";
 ocamlc.byte;
 program = "persistence_interface.cmi";
 program2 = "first.cmi";
 compare-binary-files;
 module = "persistence_import.ml";
 flags = "-i";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
