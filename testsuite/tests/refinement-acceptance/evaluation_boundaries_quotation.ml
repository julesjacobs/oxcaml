(* TEST
 readonly_files = "\
   evaluation_boundaries_quotation_vehicle.ml \
   evaluation_boundaries_quotation_completion.ml \
   evaluation_boundaries_quotation.reference \
   evaluation_boundaries_quotation_completion.reference \
 ";
 setup-ocamlc.byte-build-env;
 flags = "-extension runtime_metaprogramming";

 module = "evaluation_boundaries_quotation_vehicle.ml";
 ocamlc.byte;

 module = "evaluation_boundaries_quotation.ml";
 compiler_output = "evaluation_boundaries_quotation.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/evaluation_boundaries_quotation.reference";
 check-ocamlc.byte-output;

 module = "evaluation_boundaries_quotation_completion.ml";
 compiler_output = "evaluation_boundaries_quotation_completion.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/evaluation_boundaries_quotation_completion.reference";
 check-ocamlc.byte-output;
*)

#syntax quotations on

let quotation_body_fact_does_not_escape =
  let _code =
    <[ ignore (Evaluation_boundaries_quotation_vehicle.impossible ()) ]>
  in
  (0 : int{ false })
