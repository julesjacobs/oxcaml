(* TEST
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/comparison_external_runtime.byte";
 all_modules = "comparison_external_runtime.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

external equal : float -> float -> bool @@ total = "%equal"
external not_equal : float -> float -> bool @@ total = "%notequal"
external less : float -> float -> bool @@ total = "%lessthan"
external less_equal : float -> float -> bool @@ total = "%lessequal"
external greater : float -> float -> bool @@ total = "%greaterthan"
external greater_equal : float -> float -> bool @@ total = "%greaterequal"

let print_comparisons left right =
  Printf.printf "%b %b %b %b %b %b\n"
    (equal left right)
    (not_equal left right)
    (less left right)
    (less_equal left right)
    (greater left right)
    (greater_equal left right)

let () =
  let nan = 0.0 /. 0.0 in
  print_comparisons nan nan;
  print_comparisons (-0.0) 0.0
