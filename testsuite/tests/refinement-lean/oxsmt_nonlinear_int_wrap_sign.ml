(* TEST
 flags = "-vox-backend oxsmt";
 setup-ocamlc.byte-build-env;
 compiler_output = "oxsmt_nonlinear_int_wrap_sign.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "\
   ${test_source_directory}/oxsmt_nonlinear_int_wrap_sign.reference";
 check-ocamlc.byte-output;
*)

(* This is true over mathematical integers but false for wrapping machine
   integers: a sufficiently large square can have a negative sign bit. *)
let square_is_nonnegative (value : int)
    : unit{ 0 <= value * value } =
  ()
