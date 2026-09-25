(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.exception_rolls_back_mode_changes ()
     then "rolled back"
     else "retained")
