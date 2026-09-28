(* TEST
 readonly_files = "definition.mli definition.ml consumer.ml forge.mli forge.ml drop.mli drop.ml shared_aliases.ml";
 setup-ocamlc.byte-build-env;
 module = "shared_aliases.ml";
 ocamlc.byte;
 module = "definition.mli";
 ocamlc.byte;
 module = "definition.ml";
 ocamlc.byte;
 module = "consumer.ml";
 ocamlc.byte;
 module = "forge.mli";
 ocamlc.byte;
 module = "drop.mli";
 ocamlc.byte;
 ocamlc_byte_exit_status = "2";
 module = "forge.ml";
 ocamlc.byte;
 module = "drop.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
