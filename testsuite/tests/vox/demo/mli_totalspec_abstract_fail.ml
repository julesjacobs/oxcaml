(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 modules = "mli_totalspec.mli mli_totalspec.ml";
 script;
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* NAME-ONLY means ABSTRACT: a client cannot COMPUTE the measure over
   the datatype's constructors, because [len]'s equations never left
   the implementation.  Asserting the length of a literal list -- which
   would require unfolding [len] -- fails at the solver, exactly as the
   abstraction intends.  (Contrast mli_exposed_client.ml, where the
   equations are exported and the same shape verifies.) *)
let bad : Mli_totalspec.ilist{ Mli_totalspec.len _ = 2 } =
  Mli_totalspec.Cons (5, Mli_totalspec.Cons (6, Mli_totalspec.Nil))
