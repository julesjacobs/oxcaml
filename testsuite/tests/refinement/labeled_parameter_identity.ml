(* TEST
 readonly_files = "\
   lpi_collision_other.mli lpi_collision_other.ml \
   lpi_collision_provider.mli lpi_collision_provider.ml \
   lpi_collision_client.ml lpi_collision_client.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "lpi_collision_other.mli";
 ocamlc.byte;
 module = "lpi_collision_other.ml";
 ocamlc.byte;
 module = "lpi_collision_provider.mli";
 ocamlc.byte;
 module = "lpi_collision_provider.ml";
 ocamlc.byte;
 module = "lpi_collision_client.ml";
 compiler_output = "lpi_collision_client.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/lpi_collision_client.reference";
 check-ocamlc.byte-output;
*)

(* Dependent labels retain an identity distinct from qualified values with the
   same final component. *)
