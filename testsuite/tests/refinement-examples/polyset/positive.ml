(* TEST
 readonly_files = "\
   polyset.mli polyset.ml ordered_int.mli ordered_int.ml client.ml \
 ";
 setup-ocamlc.byte-build-env;
 module = "polyset.mli";
 ocamlc.byte;
 module = "ordered_int.mli";
 ocamlc.byte;
 module = "ordered_int.ml";
 ocamlc.byte;
 module = "polyset.ml";
 ocamlc.byte;
 module = "client.ml";
 flags = "-I ocamlc.byte";
 ocamlc.byte;
*)
