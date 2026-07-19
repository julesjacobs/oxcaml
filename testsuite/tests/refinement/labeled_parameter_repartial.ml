(* TEST
 readonly_files = "\
   lpi_order_api.mli lpi_order_api.ml \
   lpi_order_true.ml lpi_order_false.ml lpi_order_false.reference \
   lpi_order_commuted_false.ml lpi_order_commuted_false.reference \
 ";
 setup-ocamlc.byte-build-env;

 module = "lpi_order_api.mli";
 ocamlc.byte;
 module = "lpi_order_api.ml";
 ocamlc.byte;
 module = "lpi_order_true.ml";
 ocamlc.byte;

 module = "lpi_order_false.ml";
 compiler_output = "lpi_order_false.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/lpi_order_false.reference";
 check-ocamlc.byte-output;

 module = "lpi_order_commuted_false.ml";
 compiler_output = "lpi_order_commuted_false.output";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/lpi_order_commuted_false.reference";
 check-ocamlc.byte-output;
*)

(* Substitutions supplied before a partial application are retained until a
   later labeled application saturates the law, without changing argument
   order or emitting the result fact early. *)
