(* TEST
 readonly_files = "\
   direct_if_total_call_api.mli direct_if_total_call_api.ml \
   direct_if_total_call_client.ml \
 ";
 setup-ocamlc.byte-build-env;
 module = "direct_if_total_call_api.mli";
 ocamlc.byte;
 module = "direct_if_total_call_api.ml";
 ocamlc.byte;
 module = "direct_if_total_call_client.ml";
 ocamlc.byte;
*)

