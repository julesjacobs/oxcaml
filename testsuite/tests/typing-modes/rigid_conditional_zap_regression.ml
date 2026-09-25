(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.rigid_conditional_zap_preserves_residual ()
     then "preserved"
     else "lost")
