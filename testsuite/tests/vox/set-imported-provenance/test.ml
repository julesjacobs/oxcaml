(* TEST
 has-z3;
 readonly_files = "provider.ml rejected.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal -bin-annot-cms";
 module = "provider.ml";
 ocamlc.byte;
 module = "rejected.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
