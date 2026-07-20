(* TEST
 readonly_files = "\
   labeled_parameter_alias_unique.mli labeled_parameter_alias_unique.ml \
   labeled_parameter_alias_directions.mli \
   labeled_parameter_alias_directions.ml \
   labeled_parameter_alias_api.mli labeled_parameter_alias_client.mli \
   labeled_parameter_alias_client.ml \
   labeled_parameter_directions_unique.mli \
   labeled_parameter_directions_unique.ml \
   labeled_parameter_manifest_swapped.mli \
   labeled_parameter_manifest_swapped.ml \
 ";
 setup-ocamlc.byte-build-env;

 module = "labeled_parameter_alias_unique.mli";
 ocamlc.byte;
 module = "labeled_parameter_alias_unique.ml";
 ocamlc.byte;

 module = "labeled_parameter_alias_directions.mli";
 ocamlc.byte;
 module = "labeled_parameter_alias_directions.ml";
 ocamlc.byte;

 module = "labeled_parameter_alias_api.mli";
 ocamlc.byte;
 module = "labeled_parameter_alias_client.mli";
 ocamlc.byte;
 module = "labeled_parameter_alias_client.ml";
 ocamlc.byte;

 module = "labeled_parameter_directions_unique.mli";
 ocamlc.byte;
 module = "labeled_parameter_directions_unique.ml";
 ocamlc.byte;

 module = "labeled_parameter_manifest_swapped.mli";
 ocamlc.byte;
 module = "labeled_parameter_manifest_swapped.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
*)

(* Independently elaborated manifest arrows are equal positionally.  Alias
   expansion and both variance directions retain that decision, while a
   duplicate-label permutation remains unequal. *)
