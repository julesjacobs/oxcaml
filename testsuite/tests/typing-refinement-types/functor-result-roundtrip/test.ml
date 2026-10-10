(* TEST
 readonly_files = "producer.mli producer.ml consumer.ml inferred.ml inferred_consumer.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "producer.mli";
 ocamlc.byte;
 module = "producer.ml";
 ocamlc.byte;
 module = "consumer.ml";
 ocamlc.byte;
 module = "inferred.ml";
 ocamlc.byte;
 module = "inferred_consumer.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
