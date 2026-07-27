(* TEST
 readonly_files = "differential_cases.py differential_gate.py";
 setup-ocamlc.byte-build-env;
 script = "python3 ${test_source_directory}/differential_gate.py \
           --ocamlrun ${ocamlrun} --ocamlc ${ocamlc_byte} \
           --ocamlc-opt ${ocamlsrcdir}/ocamlc.opt \
           --ocamlopt-opt ${ocamlsrcdir}/ocamlopt.opt \
           --backend z3 --profile division --jobs 4";
 script;
*)

(* The same comparison against the other SMT path.  Running both is the
   point: [Vox_smt] builds oxsmt terms directly rather than through the
   printed SMT-LIB text, so the two are independent translations and a
   mistake in one does not reach the other.

   An obligation costs several times as much through an external solver, so
   this arm keeps the operation whose translation is newest -- division and
   remainder, at the operands where truncation, the sign of a remainder, the
   quotient that leaves the range and a divisor of zero would each show --
   and leaves the rest of the SMT-LIB table to the sweep, which is where a
   change to that emitter is expected to be checked. *)
