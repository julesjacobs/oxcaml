(* TEST
 readonly_files = "decreases_unit_measured.ml";
 setup-ocamlc.byte-build-env;
 all_modules = "decreases_unit_measured.ml decreases_unit_lifetime.ml";
 compile_only = "true";
 compiler_output = "decreases_unit_lifetime.output";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/decreases_unit_lifetime.reference";
 check-ocamlc.byte-output;
*)

(* A recursive binding that asks for no measure, in a unit compiled after one
   that records a measure.  Nothing here should acquire a termination
   obligation: this binding is partial, as an unmeasured recursion is, and it
   compiles.  It stops compiling if a measure recorded for the previous
   unit's binding is still in the table under a stamp this unit has reused. *)
let rec spin (n : int{ _ >= 0 }) : int = if n = 0 then 0 else spin n
