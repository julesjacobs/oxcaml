(* TEST
 readonly_files = "bst.mli bst.ml client_positive.ml";
 setup-ocamlc.byte-build-env;
 module = "bst.mli";
 ocamlc.byte;
 module = "client_positive.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
 module = "bst.ml";
 flags = "";
 ocamlc.byte;
*)
