(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.exact_gencopy_preserves_constraint ()
     then "preserved"
     else "lost");
  print_endline
    (if Mode.For_testing.exact_failures_have_diagnostics ()
     then "diagnostics registered"
     else "diagnostics missing")
