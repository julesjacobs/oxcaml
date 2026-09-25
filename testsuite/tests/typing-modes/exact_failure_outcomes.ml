(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.exact_no_constant_zap_is_rejected ()
     then "zap rejected"
     else "zap accepted");
  print_endline
    (if Mode.For_testing.exact_resource_limit_is_separate ()
     then "limit reported"
     else "limit lost");
  print_endline
    (if Mode.For_testing.exact_first_rigid_limit_is_separate ()
     then "first limit reported"
     else "first limit lost");
  print_endline
    (if Mode.For_testing.exact_inequality_failure_is_separate ()
     then "inequality reported"
     else "inequality lost")
