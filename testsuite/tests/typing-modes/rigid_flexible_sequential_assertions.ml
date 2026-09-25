(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_flexible_sequential_assertions ()
     then "rejected"
     else "accepted")
