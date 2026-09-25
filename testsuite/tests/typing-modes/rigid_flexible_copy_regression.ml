(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_flexible_copy_preserves_assertions ()
     then "preserved"
     else "lost")
