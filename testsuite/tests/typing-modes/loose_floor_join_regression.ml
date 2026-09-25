(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.loose_floor_of_join_is_lower ()
     then "accepted"
     else "rejected")
