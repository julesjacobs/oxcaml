(* TEST
 readonly_files = "\
   iarray_model.mli iarray_model.ml iarray_set.mli iarray_set.ml client.ml \
 ";
 setup-ocamlc.byte-build-env;
 module = "${test_source_directory}/../set_group/set_intf.ml";
 flags = "-o set_intf.cmo";
 ocamlc.byte;
 module = "iarray_model.mli";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "iarray_model.ml";
 ocamlc.byte;
 module = "iarray_set.mli";
 ocamlc.byte;
 module = "iarray_set.ml";
 ocamlc.byte;
 module = "client.ml";
 ocamlc.byte;
*)
