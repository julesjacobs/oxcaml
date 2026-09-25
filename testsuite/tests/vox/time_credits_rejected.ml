(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_credits.mli vox_credits.ml";
 readonly_files = "time_credits_rejected.ml";
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
  module C = Vox_credits.Make ()
  let bad (token : {t : C.token | C.credits t > 0} @ unique total ghost) =
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
  module C = Vox_credits.Make ()
  let bad () =
    let zero = 0 in
    let initial : {n : int | n >= 0} = zero in
    let token = C.Budget.create initial in
    C.tick (token)
end;;
[%%expect{|
Line 7, characters 11-18:
7 |     C.tick (token)
               ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Oversplit = struct
  module C = Vox_credits.Make ()
  let bad (token : {t : C.token | C.credits t = 3} @ unique total ghost) =
    let token = token in
    let amount = 4 in
    C.split amount (token)
end;;
[%%expect{|
Line 6, characters 19-26:
6 |     C.split amount (token)
                       ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Duplicate_merge = struct
  module C = Vox_credits.Make ()
  let bad (token : C.token @ unique total ghost) =
    C.merge token (token)
end;;
[%%expect{|
Line 4, characters 18-25:
4 |     C.merge token (token)
                      ^^^^^^^
Error: This value is used here, but it is also being used as unique at:
Line 4, characters 12-17:
4 |     C.merge token (token)
                ^^^^^

|}]

module Overflow = struct
  module C = Vox_credits.Make ()
  let bad : (left : C.token) @ unique total ghost ->
      (right : {t : C.token | C.credits left > 0 && C.credits t > 0 &&
        C.credits left + C.credits t < 0}) @ unique total ghost ->
      C.token @ unique total ghost = fun left right ->
    let right = right in
    let result = C.merge left (right) in
    result
end;;
[%%expect{|
Line 8, characters 30-37:
8 |     let result = C.merge left (right) in
                                  ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Foreign_budget = struct
  module C = Vox_credits.Make ()
  module D = Vox_credits.Make ()
  let bad (token : {t : C.token | C.credits t > 0} @ unique total ghost) =
    D.tick token
end;;
[%%expect{|
Line 5, characters 11-16:
5 |     D.tick token
               ^^^^^
Error: The value "token" has type "C.token"
       but an expression was expected of type "D.token"
|}]

module Mint (C : Vox_credits.S) = struct
  let bad () = C.Budget.create 10
end;;
[%%expect{|
Line 2, characters 15-23:
2 |   let bad () = C.Budget.create 10
                   ^^^^^^^^
Error: Unbound module "C.Budget"
|}]

module Ghost_consume = struct
  module C = Vox_credits.Make ()
  let bad (token : {t : C.token | C.credits t > 0} @ unique total ghost) =
    ghost_ (C.tick token)
end;;
[%%expect{|
Line 4, characters 19-24:
4 |     ghost_ (C.tick token)
                       ^^^^^
Error: This value is "aliased"
         because it is used in an expression (at line 4, characters 4-25).
       However, the highlighted expression is expected to be "unique".
|}]

module Free_comparison = struct
  module C = Vox_credits.Make ()
  type result = #{ before : bool; state : C.token @@ ghost }
  let (bad @ total) : (left : int) -> (right : int) ->
      (token : {t : C.token | C.credits t > 0}) @ unique total ghost ->
      {r : result | let token = token in
        r.#before = (left <= right) &&
        C.credits r.#state = C.credits token - 1} @ unique =
      fun left right token ->
    let token = token in
    let result = #{ before = left <= right; state = token } in
    result
end;;
[%%expect{|
Line 12, characters 4-10:
12 |     result
         ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Duplicate_partition = struct
  module C = Vox_credits.Make ()
  let bad (parts : C.partition @ unique total) =
    let _ = C.merge parts.left (parts.right) in
    parts.left
end;;
[%%expect{|
Line 5, characters 4-14:
5 |     parts.left
        ^^^^^^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 20-30:
4 |     let _ = C.merge parts.left (parts.right) in
                        ^^^^^^^^^^

|}]

module Escaping_observation = struct
  module C = Vox_credits.Make ()
  let bad (token : {t : C.token | C.credits t > 0} @ unique total ghost) =
    let token = token in
    let observer = ghost_ (fun () -> C.credits token) in
    let _ = C.tick (token) in
    observer
end;;
[%%expect{|
Line 6, characters 19-26:
6 |     let _ = C.tick (token) in
                       ^^^^^^^
Error: This value is used here as unique, but it has already been used at:
Line 5, characters 47-52:
5 |     let observer = ghost_ (fun () -> C.credits token) in
                                                   ^^^^^

|}]

module Exhausted = struct
  module C = Vox_credits.Make ()
  let bad () =
    let two = 2 in
    let amount : {n : int | n >= 0} = two in
    let start = C.Budget.create amount in
    let first : {t : C.token | C.credits t > 0} = start in
    let after_one = C.tick first in
    let second : {t : C.token | C.credits t > 0} = after_one in
    let after_two = C.tick second in
    C.tick (after_two)
end;;
[%%expect{|
Line 11, characters 11-22:
11 |     C.tick (after_two)
                ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module False_after_tick = struct
  module C = Vox_credits.Make ()
  let bad () =
    let one = 1 in
    let amount : {n : int | n >= 0} = one in
    let token = C.Budget.create amount in
    let positive : {t : C.token | C.credits t > 0} = token in
    let spent = C.tick positive in
    let u = () in
    (u : {u : unit | false})
end;;
[%%expect{|
Line 8, characters 8-13:
8 |     let spent = C.tick positive in
            ^^^^^
Warning 26 [unused-var]: unused variable "spent".

Line 10, characters 5-6:
10 |     (u : {u : unit | false})
          ^
Error: Refinement could not be proved (counterexample)
|}]
