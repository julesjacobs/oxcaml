(* TEST
 setup-ocamlc.byte-build-env;
 compiler_output = "refined_function_self_reject.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/refined_function_self_reject.reference";
 check-ocamlc.byte-output;
*)

(* Batch (default-mode) regression for the modelability gate.  Reading a
   function-typed self in its own predicate is not modelable and must be
   rejected at elaboration.  The companion [%%expect] test in [elaboration.ml]
   runs under the principal-like toplevel; this file pins the SAME rejection
   under plain [ocamlc.byte] (non-principal, non-toplevel).  Before the gate
   was made principality-insensitive this compiled cleanly here -- the error
   only showed up under [-principal] -- so this file guards against that
   batch-vs-toplevel masking. *)

type fn_reentrant = (int -> int){ ((_ : int -> int) = _) }
