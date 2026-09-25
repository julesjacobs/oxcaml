(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_correlated_regionality_checks ()
     then "accepted"
     else "rejected")
