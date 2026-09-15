(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

type r = #{ live : int; proof : string @@ ghost };;
[%%expect{|
type r = #{ live : int; proof : string @@ ghost; }
|}]
let make live = #{ live; proof = ghost_ "erased" };;
let get r = r.#live;;
let proof r = ghost_ r.#proof;;
let update r = #{ r with live = 42 };;
let unpack r = let #{ live; proof = _ } = r in live;;
[%%expect{|
val make : int -> r = <fun>
val get : r -> int = <fun>
val proof : r @ total -> string @ ghost = <fun>
val update : r -> r = <fun>
val unpack : r -> int = <fun>
|}]
let bad r = String.length r.#proof;;
[%%expect{|
Line 1, characters 26-34:
1 | let bad r = String.length r.#proof;;
                              ^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]
let bad r = let #{ live = _; proof } = r in String.length proof;;
[%%expect{|
Line 1, characters 58-63:
1 | let bad r = let #{ live = _; proof } = r in String.length proof;;
                                                              ^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]
type singleton = #{ proof : string @@ ghost };;
type all = #{ a : int @@ ghost; b : string @@ ghost };;
[%%expect{|
type singleton = #{ proof : string @@ ghost; }
type all = #{ a : int @@ ghost; b : string @@ ghost; }
|}]
let singleton = #{ proof = ghost_ "erased" };;
let all = #{ a = ghost_ 1; b = ghost_ "erased" };;
[%%expect{|
val singleton : singleton = #{proof = <void>}
val all : all = #{a = <void>; b = <void>}
|}]
let bad () = #{ live = 1; proof = ghost_ (print_endline "bad"; "x") };;
[%%expect{|
Line 1, characters 42-55:
1 | let bad () = #{ live = 1; proof = ghost_ (print_endline "bad"; "x") };;
                                              ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 34-67).
|}]

type opaque;;
type crossing : (value & void) mod portable = #{ live : int; hidden : opaque @@ ghost };;
[%%expect{|
type opaque
type crossing = #{ live : int; hidden : opaque @@ ghost; }
|}]

type indexed = { payload : r };;
let bad_index = (.payload.#proof);;
[%%expect{|
type indexed = { payload : r; }
Line 2, characters 27-32:
2 | let bad_index = (.payload.#proof);;
                               ^^^^^
Error: Block indices do not support ghost fields in records.
|}]
let live_index = (.payload.#live);;
[%%expect{|
val live_index : (indexed, int) idx_imm = <abstr>
|}]

let bad_match (r : r) =
  match r with #{ live = _; proof = "x" } -> true | _ -> false;;
[%%expect{|
Line 2, characters 36-39:
2 |   match r with #{ live = _; proof = "x" } -> true | _ -> false;;
                                        ^^^
Error: This value is "ghost" but is expected to be "real".
|}]
