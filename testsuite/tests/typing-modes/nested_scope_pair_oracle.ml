(* TEST
 include ocamlcommon;
 native;
*)

let () =
  Printf.printf "%d mismatches\n"
    (Mode.For_testing.nested_scope_pairs_match_oracle ())
