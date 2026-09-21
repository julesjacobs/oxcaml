(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_big_credits.mli vox_big_credits.ml";
 readonly_files = "big_time_credits_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
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
    let refine_ token = C.empty () in
    C.tick (refine_ token)
end;;
[%%expect{|
Line 5, characters 11-26:
5 |     C.tick (refine_ token)
               ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Oversplit = struct
  module C = Vox_big_credits.Make ()
  let bad (token : {t : C.token | C.credits t = 3Z} @ unique total ghost) =
    let refine_ token = token in
    let amount = 4Z in
    C.split amount (refine_ token)
end;;
[%%expect{|
Line 6, characters 19-34:
6 |     C.split amount (refine_ token)
                       ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Negative = struct
  module C = Vox_big_credits.Make ()
  let bad () =
    let amount = Bigint.sub 0Z 1Z in
    C.Budget.create (refine_ amount)
end;;
[%%expect{|
Line 5, characters 20-36:
5 |     C.Budget.create (refine_ amount)
                        ^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Mint (C : Vox_big_credits.S) = struct
  let bad () = C.Budget.create (refine_ 1Z)
end;;
[%%expect{|
Line 2, characters 15-23:
2 |   let bad () = C.Budget.create (refine_ 1Z)
                   ^^^^^^^^
Error: Unbound module "C.Budget"
|}]
