(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigidification_preserves_exact_constraint ()
     then "preserved"
     else "lost")
