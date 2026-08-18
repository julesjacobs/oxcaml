(* TEST
 modules = "ghost_units_a.ml";
 {
   reference = "${test_source_directory}/ghost_units.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/ghost_units.reference";
   native;
 }
*)

(* A cross-unit use of a ghost parameter, exercising the .cmi round
   trip. *)
let g x = x + 1
let () = print_int (g 41); print_newline ()
let () =
  print_int (Ghost_units_a.f (ghost_ (failwith "no")) + Ghost_units_a.use ());
  print_newline ()
