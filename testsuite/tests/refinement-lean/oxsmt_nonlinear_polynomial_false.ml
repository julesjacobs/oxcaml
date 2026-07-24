(* TEST
 flags = "-keywords 5.3 -vox-backend oxsmt";
 setup-ocamlc.byte-build-env;
 compiler_output = "oxsmt_nonlinear_polynomial_false.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "\
   ${test_source_directory}/oxsmt_nonlinear_polynomial_false.reference";
 check-ocamlc.byte-output;
*)

let false_square_sum
    (left : Bigint.t @ logical)
    (right : Bigint.t @ logical)
    : unit{
        Bigint.equal
          (Bigint.mul (Bigint.add left right) (Bigint.add left right))
          (Bigint.add
             (Bigint.add
                (Bigint.mul left left)
                (Bigint.mul (Bigint.mul (Bigint.of_int 3) left) right))
             (Bigint.mul right right))
      } =
  ()
