(* TEST
 readonly_files = "vslice_model.mli vslice_model.ml vslice.mli vslice.ml";
 setup-ocamlc.byte-build-env;
 module = "vslice_model.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "vslice_model.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "vslice.mli";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 module = "vslice.ml";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
*)
