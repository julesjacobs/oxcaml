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
  print_int (Ghost_units_a.f (ghost_ 42) + Ghost_units_a.use ());
  print_newline ()

(* the ghost field of a record declared in another unit takes no slot on
   this side either, and construction/update agree with the .cmi *)
let () =
  let r = Ghost_units_a.mk_rec 5 6 in
  let r2 = { r with Ghost_units_a.ga = 7 } in
  Printf.printf "%d %d\n" r2.Ghost_units_a.ga r2.Ghost_units_a.gb
