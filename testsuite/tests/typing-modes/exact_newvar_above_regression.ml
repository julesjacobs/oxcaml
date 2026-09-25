(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.exact_newvar_above_preserves_constraint ()
     then "preserved"
     else "lost")
