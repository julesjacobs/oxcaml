(* TEST
 include ocamlcommon;
 native;
*)

let () =
  print_endline
    (if Mode.For_testing.nested_scope_order_distinguished ()
     then "distinguished"
     else "collapsed")
