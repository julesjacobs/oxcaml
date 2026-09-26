(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_big_credits.mli vox_big_credits.ml";
 readonly_files = "big_time_credits_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

module Reuse = struct
  module C = Vox_big_credits.Make ()
  let bad (token : {t : C.token | C.credits t > 0Z} @ unique total ghost) =
    let _ = C.tick token in
    C.tick token
end;;
[%%expect{|
Line 5, characters 11-16:
5 |     C.tick token
               ^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 19-24:
4 |     let _ = C.tick token in
                       ^^^^^

|}]

module Empty = struct
  module C = Vox_big_credits.Make ()
  let bad () =
    let token = C.empty () in
    C.tick (token)
end;;
[%%expect{|
Line 5, characters 11-18:
5 |     C.tick (token)
               ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Oversplit = struct
  module C = Vox_big_credits.Make ()
  let bad (token : {t : C.token | C.credits t = 3Z} @ unique total ghost) =
    let token = token in
    let amount = 4Z in
    C.split amount (token)
end;;
[%%expect{|
Line 6, characters 19-26:
6 |     C.split amount (token)
                       ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Negative = struct
  module C = Vox_big_credits.Make ()
  let bad () =
    let amount = Bigint.sub 0Z 1Z in
    C.Budget.create (amount)
end;;
[%%expect{|
Line 5, characters 20-28:
5 |     C.Budget.create (amount)
                        ^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Mint (C : Vox_big_credits.S) = struct
  let bad () = C.Budget.create (1Z)
end;;
[%%expect{|
Line 2, characters 15-23:
2 |   let bad () = C.Budget.create (1Z)
                   ^^^^^^^^
Error: Unbound module "C.Budget"
|}]
