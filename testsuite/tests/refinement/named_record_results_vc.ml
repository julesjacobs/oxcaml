(* TEST
 readonly_files = "\
   named_record_results_positive.ml \
   named_record_results_negative.ml \
   named_record_results_uninhabited.ml \
   named_record_results_distinct_calls.ml \
   named_record_results_variant.ml \
   named_record_results_check.py \
 ";
 setup-ocamlc.byte-build-env;

 module = "named_record_results_positive.ml";
 flags = "-vox-dump-vc-json positive.json -c";
 compiler_output = "positive.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 module = "named_record_results_negative.ml";
 flags = "-vox-dump-vc-json negative.json -c";
 compiler_output = "negative.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_uninhabited.ml";
 flags = "-vox-dump-vc-json uninhabited.json -c";
 compiler_output = "uninhabited.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_distinct_calls.ml";
 flags = "-vox-dump-vc-json distinct.json -c";
 compiler_output = "distinct.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_variant.ml";
 flags = "-vox-dump-vc-json variant.json -c";
 compiler_output = "variant.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_positive.ml";
 flags = "-vox-dump-vc -vox-dump-vc-json smt.json \
          -vox-dump-vc-json-smt -c";
 compiler_output = "smt.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_positive.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json z3-positive.json -c";
 compiler_output = "z3-positive.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 module = "named_record_results_negative.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json z3-negative.json -c";
 compiler_output = "z3-negative.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_uninhabited.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json z3-uninhabited.json -c";
 compiler_output = "z3-uninhabited.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_variant.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json z3-variant.json -c";
 compiler_output = "z3-variant.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_positive.ml";
 flags = "-vox-backend oxsmt -vox-dump-vc-json oxsmt-positive.json -c";
 compiler_output = "oxsmt-positive.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 module = "named_record_results_negative.ml";
 flags = "-vox-backend oxsmt -vox-dump-vc-json oxsmt-negative.json -c";
 compiler_output = "oxsmt-negative.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_uninhabited.ml";
 flags = "-vox-backend oxsmt \
          -vox-dump-vc-json oxsmt-uninhabited.json -c";
 compiler_output = "oxsmt-uninhabited.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 module = "named_record_results_variant.ml";
 flags = "-vox-backend oxsmt -vox-dump-vc-json oxsmt-variant.json -c";
 compiler_output = "oxsmt-variant.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;

 script = "python3 ${test_source_directory}/named_record_results_check.py";
 script;
*)
