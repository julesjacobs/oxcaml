(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_flexible_regionality_accepts_invalid ()
     then "accepted"
     else "rejected")
