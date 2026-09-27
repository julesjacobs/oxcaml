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
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]
let bad r = let #{ live = _; proof } = r in String.length proof;;
[%%expect{|
Line 1, characters 58-63:
1 | let bad r = let #{ live = _; proof } = r in String.length proof;;
                                                              ^^^^^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
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

(* A ghost field has no slot, but its type still bounds the record's mode
   crossing: a ghost-field read takes the record's mode. *)
type opaque;;
type crossing : (value & void) mod portable = #{ live : int; hidden : opaque @@ ghost };;
[%%expect{|
type opaque
Line 2, characters 0-87:
2 | type crossing : (value & void) mod portable = #{ live : int; hidden : opaque @@ ghost };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation
         value mod portable & void mod portable,
       because opaque is not mod portable.
|}]

type crossing : (value & void) mod portable = #{ live : int; hidden : string @@ ghost };;
[%%expect{|
type crossing = #{ live : int; hidden : string @@ ghost; }
|}]

(* A record of one ghost field is not an abbreviation of the field's type:
   its layout is void, also when checked against a signature. *)
module Singleton_value : sig type t : value end = struct
  type t = #{ proof : string @@ ghost }
end;;
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = #{ proof : string @@ ghost }
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ proof : string @@ ghost; } end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = #{ proof : string @@ ghost; }
       is not included in
         type t
       The layout of the first is void
         because of the definition of t at line 2, characters 2-39.
       But the layout of the first must be a value layout
         because of the definition of t at line 1, characters 29-43.
|}]

module Singleton_void : sig type t : void end = struct
  type t = #{ proof : string @@ ghost }
end;;
[%%expect{|
module Singleton_void : sig type t : void end
|}]

module Singleton_abbreviation : sig type t : value end = struct
  type g = #{ proof : string @@ ghost }
  type t = g
end;;
[%%expect{|
Lines 1-4, characters 57-3:
1 | .........................................................struct
2 |   type g = #{ proof : string @@ ghost }
3 |   type t = g
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type g = #{ proof : string @@ ghost; } type t = g end
       is not included in
         sig type t end
       Type declarations do not match: type t = g is not included in type t
       The layout of the first is void
         because of the definition of g at line 2, characters 2-39.
       But the layout of the first must be a value layout
         because of the definition of t at line 1, characters 36-50.
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
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]
