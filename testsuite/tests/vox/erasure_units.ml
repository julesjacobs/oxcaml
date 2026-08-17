(* TEST
 modules = "erasure_units_a.ml";
 {
   reference = "${test_source_directory}/erasure_units.reference";
   bytecode;
 }{
   reference = "${test_source_directory}/erasure_units.reference";
   native;
 }
*)

(* A cross-unit use of an erased parameter, exercising the .cmi round
   trip. *)
let g x = x + 1
let () = print_int (g 41); print_newline ()
let () =
  print_int (Erasure_units_a.f (erased_ (failwith "no")) + Erasure_units_a.use ());
  print_newline ()
