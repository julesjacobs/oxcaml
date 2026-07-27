(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "integer_division_zero.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/integer_division_zero.reference";
 check-ocamlc.byte-output;
*)

(* A zero divisor raises, so there is no value for the verifier to prove.
   Every backend guards the case and hands it to something uninterpreted,
   which matters because the bitvector theories do not: they answer -1 for
   [bvsdiv x 0] with a non-negative dividend, and the dividend itself for
   [bvsrem x 0].  Proving either would be the model disagreeing with a
   program that never returns at all. *)

let quotient_at_zero = ((1 / 0) : int{ _ = (-1) })
