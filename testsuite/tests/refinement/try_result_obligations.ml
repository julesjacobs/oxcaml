(* TEST
 readonly_files = "try_result_obligations_check.py \
                   try_result_obligations_bad.ml \
                   try_result_obligations_bad.reference \
                   try_result_obligations_effect_nonresume.ml \
                   try_result_obligations_effect_resume.ml \
                   try_result_obligations_effect_resume.reference \
                   try_result_obligations_unsupported.ml \
                   try_result_obligations_unsupported.reference \
                   try_result_obligations_unmatched_gate.ml \
                   try_result_obligations_unmatched_leaf.ml \
                   try_result_obligations_unmatched_summary.ml \
                   try_result_obligations_dynamic.ml \
                   try_result_obligations_nested.ml \
                   try_result_obligations_nested_same_effect.ml \
                   try_result_obligations_nested_guarded_effect.ml \
                   try_result_obligations_nested_refutable_effect.ml \
                   try_result_obligations_nested_rebound_effect.ml \
                   try_result_obligations_nested_alias_effect.ml \
                   try_result_obligations_rebound_effect_summary.ml \
                   try_result_obligations_shadowed.ml \
                   try_result_obligations_shadowed_local.ml \
                   try_result_obligations_shadowed_functor.ml \
                   try_result_obligations_shadowed_match.ml \
                   try_result_obligations_shadowed_continue.ml \
                   try_result_obligations_continue_direct.ml \
                   try_result_obligations_continue_module_alias.ml \
                   try_result_obligations_continue_local_alias.ml \
                   try_result_obligations_continue_value_alias.ml \
                   try_result_obligations_continuation_data.ml \
                   try_result_obligations_continue_then_code.ml \
                   try_result_obligations_continue_nested.ml \
                   try_result_obligations_custom_stdlib.ml \
                   try_result_obligations_custom_stdlib_client.ml \
                   try_result_obligations_false.ml";
 setup-ocamlc.byte-build-env;

 flags = "-vox-dump-vc-json positive.json -c";
 compiler_output = "positive.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           positive positive.json \
           ${test_source_directory}/try_result_obligations.ml";
 script;

 module = "try_result_obligations_bad.ml";
 flags = "-vox-dump-vc-json negative.json -c";
 compiler_output = "negative.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/try_result_obligations_bad.reference";
 check-ocamlc.byte-output;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           negative negative.json \
           ${test_source_directory}/try_result_obligations_bad.ml";
 script;

 module = "try_result_obligations_effect_nonresume.ml";
 flags = "-vox-dump-vc-json effect-nonresume.json -c";
 compiler_output = "effect-nonresume.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           effect-nonresume effect-nonresume.json \
           ${test_source_directory}/try_result_obligations_effect_nonresume.ml";
 script;

 module = "try_result_obligations_effect_resume.ml";
 flags = "-vox-dump-vc-json effect-resume.json -c";
 compiler_output = "effect-resume.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/try_result_obligations_effect_resume.reference";
 check-ocamlc.byte-output;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           effect-resume effect-resume.json \
           ${test_source_directory}/try_result_obligations_effect_resume.ml";
 script;

 module = "try_result_obligations_unsupported.ml";
 flags = "-vox-dump-vc-json unsupported.json -c";
 compiler_output = "unsupported.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/try_result_obligations_unsupported.reference";
 check-ocamlc.byte-output;

 module = "try_result_obligations_unmatched_gate.ml";
 flags = "-vox-dump-vc-json unmatched-gate.json -c";
 compiler_output = "unmatched-gate.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           unmatched-gate unmatched-gate.json \
           ${test_source_directory}/try_result_obligations_unmatched_gate.ml";
 script;

 module = "try_result_obligations_unmatched_leaf.ml";
 flags = "-vox-dump-vc-json unmatched-leaf.json -c";
 compiler_output = "unmatched-leaf.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           unmatched-leaf unmatched-leaf.json \
           ${test_source_directory}/try_result_obligations_unmatched_leaf.ml";
 script;

 module = "try_result_obligations_unmatched_summary.ml";
 flags = "-vox-dump-vc-json unmatched-summary.json -c";
 compiler_output = "unmatched-summary.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           unmatched-summary unmatched-summary.json \
           ${test_source_directory}/try_result_obligations_unmatched_summary.ml";
 script;

 module = "try_result_obligations_dynamic.ml";
 flags = "-vox-dump-vc-json dynamic.json -c";
 compiler_output = "dynamic.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           dynamic dynamic.json \
           ${test_source_directory}/try_result_obligations_dynamic.ml";
 script;

 module = "try_result_obligations_nested.ml";
 flags = "-vox-dump-vc-json nested.json -c";
 compiler_output = "nested.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           nested nested.json \
           ${test_source_directory}/try_result_obligations_nested.ml";
 script;

 module = "try_result_obligations_nested_same_effect.ml";
 flags = "-vox-dump-vc-json nested-same-effect.json -c";
 compiler_output = "nested-same-effect.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           nested-same-effect nested-same-effect.json \
           ${test_source_directory}/try_result_obligations_nested_same_effect.ml";
 script;

 module = "try_result_obligations_nested_guarded_effect.ml";
 flags = "-vox-dump-vc-json nested-guarded-effect.json -c";
 compiler_output = "nested-guarded-effect.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           nested-guarded-effect nested-guarded-effect.json \
           ${test_source_directory}/try_result_obligations_nested_guarded_effect.ml";
 script;

 module = "try_result_obligations_nested_refutable_effect.ml";
 flags = "-vox-dump-vc-json nested-refutable-effect.json -c";
 compiler_output = "nested-refutable-effect.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           nested-refutable-effect nested-refutable-effect.json \
           ${test_source_directory}/try_result_obligations_nested_refutable_effect.ml";
 script;

 module = "try_result_obligations_nested_rebound_effect.ml";
 flags = "-vox-dump-vc-json nested-rebound-effect.json -c";
 compiler_output = "nested-rebound-effect.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           nested-rebound-effect nested-rebound-effect.json \
           ${test_source_directory}/try_result_obligations_nested_rebound_effect.ml";
 script;

 module = "try_result_obligations_nested_alias_effect.ml";
 flags = "-vox-dump-vc-json nested-alias-effect.json -c";
 compiler_output = "nested-alias-effect.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           nested-alias-effect nested-alias-effect.json \
           ${test_source_directory}/try_result_obligations_nested_alias_effect.ml";
 script;

 module = "try_result_obligations_rebound_effect_summary.ml";
 flags = "-vox-dump-vc-json rebound-effect-summary.json -c";
 compiler_output = "rebound-effect-summary.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           rebound-effect-summary rebound-effect-summary.json \
           ${test_source_directory}/try_result_obligations_rebound_effect_summary.ml";
 script;

 module = "try_result_obligations_shadowed.ml";
 flags = "-vox-dump-vc-json shadowed.json -c";
 compiler_output = "shadowed.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           shadowed-module shadowed.json \
           ${test_source_directory}/try_result_obligations_shadowed.ml";
 script;

 module = "try_result_obligations_shadowed_local.ml";
 flags = "-vox-dump-vc-json shadowed-local.json -c";
 compiler_output = "shadowed-local.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           shadowed-local shadowed-local.json \
           ${test_source_directory}/try_result_obligations_shadowed_local.ml";
 script;

 module = "try_result_obligations_shadowed_functor.ml";
 flags = "-vox-dump-vc-json shadowed-functor.json -c";
 compiler_output = "shadowed-functor.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           shadowed-functor shadowed-functor.json \
           ${test_source_directory}/try_result_obligations_shadowed_functor.ml";
 script;

 module = "try_result_obligations_shadowed_match.ml";
 flags = "-vox-dump-vc-json shadowed-match.json -c";
 compiler_output = "shadowed-match.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           shadowed-match shadowed-match.json \
           ${test_source_directory}/try_result_obligations_shadowed_match.ml";
 script;

 module = "try_result_obligations_shadowed_continue.ml";
 flags = "-vox-dump-vc-json shadowed-continue.json -c";
 compiler_output = "shadowed-continue.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           shadowed-continue shadowed-continue.json \
           ${test_source_directory}/try_result_obligations_shadowed_continue.ml";
 script;

 module = "try_result_obligations_continue_direct.ml";
 flags = "-vox-dump-vc-json continue-direct.json -c";
 compiler_output = "continue-direct.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continue-direct.json \
           ${test_source_directory}/try_result_obligations_continue_direct.ml";
 script;

 module = "try_result_obligations_continue_module_alias.ml";
 flags = "-vox-dump-vc-json continue-module-alias.json -c";
 compiler_output = "continue-module-alias.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continue-module-alias.json \
           ${test_source_directory}/try_result_obligations_continue_module_alias.ml";
 script;

 module = "try_result_obligations_continue_local_alias.ml";
 flags = "-vox-dump-vc-json continue-local-alias.json -c";
 compiler_output = "continue-local-alias.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continue-local-alias.json \
           ${test_source_directory}/try_result_obligations_continue_local_alias.ml";
 script;

 module = "try_result_obligations_continue_value_alias.ml";
 flags = "-vox-dump-vc-json continue-value-alias.json -c";
 compiler_output = "continue-value-alias.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continue-value-alias.json \
           ${test_source_directory}/try_result_obligations_continue_value_alias.ml";
 script;

 module = "try_result_obligations_continuation_data.ml";
 flags = "-vox-dump-vc-json continuation-data.json -c";
 compiler_output = "continuation-data.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continuation-data.json \
           ${test_source_directory}/try_result_obligations_continuation_data.ml";
 script;

 module = "try_result_obligations_continue_then_code.ml";
 flags = "-vox-dump-vc-json continue-then-code.json -c";
 compiler_output = "continue-then-code.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continue-then-code.json \
           ${test_source_directory}/try_result_obligations_continue_then_code.ml";
 script;

 module = "try_result_obligations_continue_nested.ml";
 flags = "-vox-dump-vc-json continue-nested.json -c";
 compiler_output = "continue-nested.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue continue-nested.json \
           ${test_source_directory}/try_result_obligations_continue_nested.ml";
 script;

 module = "try_result_obligations_false.ml";
 flags = "-vox-dump-vc-json false.json -c";
 compiler_output = "false.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           false false.json \
           ${test_source_directory}/try_result_obligations_false.ml";
 script;

 module = "try_result_obligations_false.ml";
 flags = "-vox-backend z3 -vox-smt-solver 'z3 -in' \
          -vox-dump-vc-json false-z3.json -c";
 compiler_output = "false-z3.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           false false-z3.json \
           ${test_source_directory}/try_result_obligations_false.ml";
 script;

 (* The refuted source has no oxsmt arm.  Its obligation is bitvector
    arithmetic in a scope that also mentions a datatype, and the in-process
    backend then answers unknown to both the prove and the disprove query, so
    the verdict weakens from refuted to not-proved and the counterexample is
    lost.  Recording that weaker verdict here would hide the loss, so the
    acceptance backend checks the refutation on its own and the weakening is
    filed upstream as report 08, second case.  Restore this arm when a fixed
    revision is vendored. *)

 (* Compile this replacement last: its [Stdlib.cmi] intentionally shadows the
    test compiler's standard library for the following client action. *)
 module = "try_result_obligations_custom_stdlib.ml";
 flags = "-nopervasives -o stdlib.cmo -c";
 compiler_output = "custom-stdlib.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;

 module = "try_result_obligations_custom_stdlib_client.ml";
 flags = "-nopervasives -I . -vox-dump-vc-json custom-stdlib.json -c";
 compiler_output = "custom-stdlib-client.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 script = "python3 ${test_source_directory}/try_result_obligations_check.py \
           conservative-continue custom-stdlib.json \
           ${test_source_directory}/try_result_obligations_custom_stdlib_client.ml";
 script;
*)

(* The bound keeps [x + 1] inside the machine range.  Without it the successor
   overflows at the maximum and the result claim is false; the try/with
   obligation shape this exercises is unaffected. *)
let body_and_handlers (x : int{ _ >= 0 && _ < 1000 }) : int{ _ >= 0 } =
  try
    let bumped = x + 1 in
    ignore bumped;
    let body_result = bumped in
    body_result
  with
  | Not_found ->
    let not_found_result = 0 in
    not_found_result
  | Exit ->
    ignore x;
    let exit_result = x in
    exit_result

let raise_only_handler (x : int{ _ >= 0 }) : int{ _ >= 0 } =
  try
    if x = 0 then raise Exit
    else
      let normal_result = x in
      normal_result
  with
  | Exit -> raise Exit
  | Not_found ->
    let fallback_result = x in
    fallback_result
