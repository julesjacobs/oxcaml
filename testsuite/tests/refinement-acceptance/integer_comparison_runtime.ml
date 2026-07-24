(* TEST
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/integer_comparison_runtime.byte";
 all_modules = "integer_comparison_runtime.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

let (compare @ total) (left : int) (right : int) =
  left = right, left <> right,
  left < right, left <= right, left > right, left >= right

let print_comparison left right =
  let equal, not_equal, less, less_equal, greater, greater_equal =
    compare left right
  in
  Printf.printf "%b %b %b %b %b %b\n"
    equal not_equal less less_equal greater greater_equal

let () =
  print_comparison min_int min_int;
  print_comparison min_int max_int;
  print_comparison max_int min_int;
  print_comparison max_int max_int;
  print_comparison (-1) 0;
  print_comparison 0 (-1)
