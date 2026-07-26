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

 (* The positive source has no oxsmt arm.  Its obligations are bitvector
    arithmetic over a record of integers, and the in-process backend takes its
    bitvector path only when the whole assertion set is pure, so the record
    declaration beside the arithmetic loses it: z3 discharges all nine and
    oxsmt discharges one.  Neither has the false-field source, whose
    obligation the acceptance backend refutes while oxsmt answers
    inconclusively to both of its queries, so the verdict weakens and the
    values that show the failure are lost.  Both are the same cause, filed
    upstream as report 08, second and third cases.  Restore these two arms
    when a fixed revision is vendored; the remaining oxsmt arms below still
    run, and the acceptance backend covers both sources on its own. *)

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
