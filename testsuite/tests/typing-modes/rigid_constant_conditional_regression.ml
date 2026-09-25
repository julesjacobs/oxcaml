(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_constant_right_preserves_outer_dependency ()
     then "right preserved"
     else "right lost");
  print_endline
    (if Mode.For_testing.rigid_constant_left_preserves_outer_dependency ()
     then "left preserved"
     else "left lost")
