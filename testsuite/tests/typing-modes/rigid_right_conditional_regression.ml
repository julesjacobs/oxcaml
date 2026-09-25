(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_right_preserves_outer_dependency ()
     then "preserved"
     else "lost")
