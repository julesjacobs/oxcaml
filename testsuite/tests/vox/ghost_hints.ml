(* TEST
 flags = "-extension refinement_types -smt-assume-verified";
 expect;
*)

(* The hint "wrap the enclosing expression in ghost_ (...)" appears only
   where wrapping can help. *)

(* The hint applies: the value is read by real code. *)
let reads (x : int @ ghost) = print_int x
[%%expect{|
Line 1, characters 40-41:
1 | let reads (x : int @ ghost) = print_int x
                                            ^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]

(* The ghost_ itself is read at run time. *)
let inner (x : int) = (ghost_ x) + 1
[%%expect{|
Line 1, characters 22-32:
1 | let inner (x : int) = (ghost_ x) + 1
                          ^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
Hint: "ghost_" makes this value ghost, but it is used here at run time.
Move "ghost_" outward to cover the code that uses it, or remove it.
|}]

(* assume_ checks its predicate at run time. *)
let runtime_ghost (x : int) : {y : int | y === ghost_ x} = assume_ x
[%%expect{|
Line 1, characters 54-55:
1 | let runtime_ghost (x : int) : {y : int | y === ghost_ x} = assume_ x
                                                          ^
Error: This value is "ghost" but is expected to be "real".
Hint: "assume_" checks this predicate at run time,
where ghost values are unavailable.
State the fact as a static refinement instead.
|}]

(* The result is real by annotation. *)
let annotated (x : int @ ghost) : int @ real = x
[%%expect{|
Line 1, characters 47-48:
1 | let annotated (x : int @ ghost) : int @ real = x
                                                   ^
Error: This value is "ghost" but is expected to be "real".
|}]

let annotated_type : int @ ghost -> int @ real = fun x -> x
[%%expect{|
Line 1, characters 58-59:
1 | let annotated_type : int @ ghost -> int @ real = fun x -> x
                                                              ^
Error: This value is "ghost" but is expected to be "real".
|}]
