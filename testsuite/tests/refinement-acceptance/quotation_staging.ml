(* TEST
 readonly_files = "\
   quotation_staging_vehicle.mli quotation_staging_vehicle.ml \
   quotation_staging_leak.ml quotation_staging_leak.reference \
   quotation_staging_control.ml quotation_staging_control.reference \
   quotation_staging_future_bad.ml quotation_staging_future_bad.reference \
   quotation_staging_future_to_splice.ml \
   quotation_staging_future_to_splice.reference \
   quotation_staging_future_post.ml quotation_staging_future_post.reference \
   quotation_staging_wrapper_bad.ml quotation_staging_wrapper_bad.reference \
   quotation_staging_nested_inactive.ml \
   quotation_staging_nested_inactive.reference \
   quotation_staging_double_cancel_bad.ml \
   quotation_staging_double_cancel_bad.reference \
   quotation_staging_nonreturn_left.ml \
   quotation_staging_nonreturn_left.reference \
   quotation_staging_nonreturn_right.ml \
   quotation_staging_nonreturn_right.reference \
   quotation_staging_nonreturn_post.ml \
   quotation_staging_nonreturn_post.reference \
   quotation_staging_sibling_forward.ml \
   quotation_staging_sibling_forward.reference \
   quotation_staging_sibling_reverse.ml \
   quotation_staging_sibling_reverse.reference \
   quotation_staging_splice_to_future.ml \
   quotation_staging_splice_to_future.reference \
   quotation_staging_splice_post.ml quotation_staging_splice_post.reference \
   quotation_staging_construction_to_future.ml \
   quotation_staging_construction_to_future.reference \
   quotation_staging_nonquotation_unproved.ml \
   quotation_staging_nonquotation_unproved.reference \
   quotation_staging_result_bad.ml quotation_staging_result_bad.reference \
   quotation_staging_flow_integration.ml \
   quotation_staging_flow_integration.reference \
 ";
 setup-ocamlc.byte-build-env;
 flags = "-extension runtime_metaprogramming";

 module = "quotation_staging_vehicle.mli";
 ocamlc.byte;
 module = "quotation_staging_vehicle.ml";
 ocamlc.byte;

 module = "quotation_staging.ml";
 ocamlc.byte;

 module = "quotation_staging_leak.ml";
 compiler_output = "quotation_staging_leak.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_leak.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_control.ml";
 compiler_output = "quotation_staging_control.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_control.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_future_bad.ml";
 compiler_output = "quotation_staging_future_bad.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_future_bad.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_future_to_splice.ml";
 compiler_output = "quotation_staging_future_to_splice.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_future_to_splice.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_future_post.ml";
 compiler_output = "quotation_staging_future_post.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_future_post.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_wrapper_bad.ml";
 compiler_output = "quotation_staging_wrapper_bad.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_wrapper_bad.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_nested_inactive.ml";
 compiler_output = "quotation_staging_nested_inactive.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_nested_inactive.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_double_cancel_bad.ml";
 compiler_output = "quotation_staging_double_cancel_bad.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_double_cancel_bad.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_nonreturn_left.ml";
 compiler_output = "quotation_staging_nonreturn_left.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_nonreturn_left.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_nonreturn_right.ml";
 compiler_output = "quotation_staging_nonreturn_right.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_nonreturn_right.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_nonreturn_post.ml";
 compiler_output = "quotation_staging_nonreturn_post.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_nonreturn_post.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_sibling_forward.ml";
 compiler_output = "quotation_staging_sibling_forward.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_sibling_forward.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_sibling_reverse.ml";
 compiler_output = "quotation_staging_sibling_reverse.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_sibling_reverse.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_splice_to_future.ml";
 compiler_output = "quotation_staging_splice_to_future.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_splice_to_future.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_splice_post.ml";
 compiler_output = "quotation_staging_splice_post.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_splice_post.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_construction_to_future.ml";
 compiler_output = "quotation_staging_construction_to_future.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_construction_to_future.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_nonquotation_unproved.ml";
 compiler_output = "quotation_staging_nonquotation_unproved.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_nonquotation_unproved.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_result_bad.ml";
 compiler_output = "quotation_staging_result_bad.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_result_bad.reference";
 check-ocamlc.byte-output;

 module = "quotation_staging_flow_integration.ml";
 compiler_output = "quotation_staging_flow_integration.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/quotation_staging_flow_integration.reference";
 check-ocamlc.byte-output;
*)

#syntax quotations on

(* Ordinary sequencing remains available within one isolated splice. *)
let splice_local_sequencing () =
  <[
    $(ignore Quotation_staging_vehicle.law_p;
      ignore (() : unit{ Quotation_staging_vehicle.p = true });
      <[ 0 ]>)
  ]>

(* Generated code has an isolated but otherwise ordinary fact environment. *)
let future_local_sequencing () =
  <[
    ignore Quotation_staging_vehicle.law_p;
    (() : unit{ Quotation_staging_vehicle.p = true })
  ]>

let future_local_definition () =
  <[
    let local_proof = Quotation_staging_vehicle.law_p in
    ignore local_proof;
    (() : unit{ Quotation_staging_vehicle.p = true })
  ]>

(* Construction-entry facts remain available while checking each isolated
   current-stage splice. *)
let construction_entry_is_available_in_splice () =
  ignore Quotation_staging_vehicle.law_p;
  (<[
     $(ignore (() : unit{ Quotation_staging_vehicle.p = true }); <[ 0 ]>)
   ]>
   [@magic_staged_modes])

(* Active splices remain eager under quoted control constructs, but their
   postconditions stay local to each payload. *)
let eager_splices_are_checked_locally () =
  let _function =
    <[
      fun () ->
        $(ignore Quotation_staging_vehicle.law_function;
          ignore (() : unit{
            Quotation_staging_vehicle.in_function = true
          });
          <[ 0 ]>)
    ]>
  in
  let _lazy =
    <[
      lazy
        $(ignore Quotation_staging_vehicle.law_lazy;
          ignore (() : unit{ Quotation_staging_vehicle.in_lazy = true });
          <[ 0 ]>)
    ]>
  in
  let _loop =
    <[
      while false do
        $(ignore Quotation_staging_vehicle.law_loop;
          ignore (() : unit{ Quotation_staging_vehicle.in_loop = true });
          <[ () ]>)
      done
    ]>
  in
  <[
    if false then
      $(ignore Quotation_staging_vehicle.law_branch;
        ignore (() : unit{ Quotation_staging_vehicle.in_branch = true });
        <[ 0 ]>)
    else 0
  ]>

(* One cancellation beneath one extra quote remains future-stage work. *)
let nested_splice_is_inactive () =
  (<[ <[ $(ignore (0 : int{ _ >= 0 }); <[ 0 ]>) ]> ]>
   [@magic_staged_modes])

(* Non-quotation expressions retain their existing sequential behavior. *)
let nonquotation_sequencing_control () =
  ignore Quotation_staging_vehicle.law_p;
  ignore (() : unit{ Quotation_staging_vehicle.p = true })
