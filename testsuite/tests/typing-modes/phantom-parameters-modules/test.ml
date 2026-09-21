(* TEST
 readonly_files = "handle.mli handle.ml consumer.ml";
 setup-ocamlc.byte-build-env;
 module = "handle.mli";
 ocamlc.byte;
 module = "handle.ml";
 ocamlc.byte;
 module = "consumer.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
