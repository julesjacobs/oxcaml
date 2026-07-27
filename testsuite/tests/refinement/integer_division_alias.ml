(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-c";
 compiler_output = "integer_division_alias.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference =
   "${test_source_directory}/integer_division_alias.reference";
 check-ocamlc.byte-output;
*)

(* Division is recognised at its canonical paths only.  An external declared
   elsewhere with the same primitive name is not this operation, and reading
   it as one is how a carrier the bitvector model does not describe would get
   a bitvector meaning.  Refusing costs an alias its proof and is what the
   bitwise primitives already do. *)

external elsewhere : int -> int -> int = "%divint"

let through_an_alias = ((elsewhere 6 2) : int{ _ = 3 })
