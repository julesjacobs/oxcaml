(* TEST
 readonly_files = "producer.mli consumer.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types";
 module = "producer.mli";
 ocamlc.byte;
 flags = "";
 module = "consumer.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
