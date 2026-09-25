(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.exact_distinguishes_reused_persistent_ids ()
     then "distinct"
     else "collided")
