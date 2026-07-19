(* TEST
 readonly_files = "\
   labeled_parameter_duplicate.mli labeled_parameter_duplicate.ml \
   labeled_parameter_duplicate_swapped.mli \
   labeled_parameter_duplicate_swapped.ml \
   labeled_parameter_alpha_identity.ml \
   labeled_parameter_join_api.mli labeled_parameter_join_false.ml \
 ";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;

 module = "labeled_parameter_duplicate.mli";
 ocamlc.byte;
 module = "labeled_parameter_duplicate.ml";
 ocamlc.byte;

 module = "labeled_parameter_duplicate_swapped.mli";
 ocamlc.byte;
 module = "labeled_parameter_duplicate_swapped.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 ocamlc_byte_exit_status = "0";
 module = "labeled_parameter_alpha_identity.ml";
 ocamlc.byte;

 module = "labeled_parameter_join_api.mli";
 ocamlc.byte;
 module = "labeled_parameter_join_false.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)

(* Parameter identities in structural equality follow arrow positions.  The
   direct alpha-equality check makes this root distinguish the old
   label-discovered relation.  Branch joins retain only facts with identical
   parameter identities. *)
