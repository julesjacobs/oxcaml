(* TEST
 readonly_files = "producer.mli consumer.ml";
 setup-ocamlc.byte-build-env;

 flags = "-extension refinement_types";
 module = "producer.mli";
 ocamlc.byte;
 module = "consumer.ml";
 ocamlc.byte;

 flags = "-extension refinement_types -principal";
 module = "producer.mli";
 ocamlc.byte;
 module = "consumer.ml";
 ocamlc.byte;
*)
