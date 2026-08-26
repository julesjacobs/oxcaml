(* TEST
 flags = "-extension-universe no_extensions";
 expect;
*)

type t = { x : int | true };;
[%%expect{|
Line 1, characters 9-27:
1 | type t = { x : int | true };;
             ^^^^^^^^^^^^^^^^^^
Error: The extension "refinement_types" is disabled and cannot be used
|}]

let introduced : int = refine_ 1;;
[%%expect{|
Line 1, characters 23-32:
1 | let introduced : int = refine_ 1;;
                           ^^^^^^^^^
Error: The extension "refinement_types" is disabled and cannot be used
|}]

let eliminated = let refine_ x = 1 in x;;
[%%expect{|
Line 1, characters 17-39:
1 | let eliminated = let refine_ x = 1 in x;;
                     ^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "refinement_types" is disabled and cannot be used
|}]

let assumed x : int = assume_ x;;
[%%expect{|
Line 1, characters 22-31:
1 | let assumed x : int = assume_ x;;
                          ^^^^^^^^^
Error: The extension "refinement_types" is disabled and cannot be used
|}]
