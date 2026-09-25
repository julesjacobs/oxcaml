(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_regionality_matches_oracle ()
     then "matched"
     else "mismatched")
