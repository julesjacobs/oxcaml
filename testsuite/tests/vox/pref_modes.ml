(* TEST
 flags = "-extension refinement_types";
 expect;
*)

type token : void mod total contended logical;;
external empty : unit -> token @ unique = "%unbox_unit";;
external consume : token @ unique -> unit @@ total =
  "caml_pref_own_bytecode" "caml_pref_own";;
let duplicate t = consume t; consume t;;
[%%expect{|
type token : void mod total contended logical
external empty : unit -> token @ unique = "%unbox_unit"
Lines 3-4, characters 0-42:
3 | external consume : token @ unique -> unit @@ total =
4 |   "caml_pref_own_bytecode" "caml_pref_own"..
Warning 228 [trusted-external]: The verifier assumes this external's totality;
  nothing checks it.

external consume : token @ unique -> unit = "caml_pref_own_bytecode"
  "caml_pref_own"
Line 5, characters 37-38:
5 | let duplicate t = consume t; consume t;;
                                         ^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 26-27:
5 | let duplicate t = consume t; consume t;;
                              ^

|}]

let ghost_consume (t : token @ unique) = ghost_ (consume t); t;;
[%%expect{|
Line 1, characters 57-58:
1 | let ghost_consume (t : token @ unique) = ghost_ (consume t); t;;
                                                             ^
Error: This value is "aliased"
         because it is used in an expression (at line 1, characters 41-59).
       However, the highlighted expression is expected to be "unique".
|}]

type holder = #{ state : token };;
let duplicate_field (x : holder @ unique) =
  consume x.#state; consume x.#state;;
[%%expect{|
type holder = #{ state : token; }
Line 3, characters 28-36:
3 |   consume x.#state; consume x.#state;;
                                ^^^^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 10-18:
3 |   consume x.#state; consume x.#state;;
              ^^^^^^^^

|}]

type ('a : immutable_data) cell;;
type functions = (unit -> unit) cell;;
[%%expect{|
type ('a : immutable_data) cell
Line 2, characters 18-30:
2 | type functions = (unit -> unit) cell;;
                      ^^^^^^^^^^^^
Error: This type "unit -> unit" should be an instance of type
         "('a : immutable_data)"
       The kind of unit -> unit is value non_float mod aliased immutable
         because it's a function type.
       But the kind of unit -> unit must be a subkind of immutable_data
         because of the definition of cell at line 1, characters 0-31.
|}]

type annotated_callback = {
  f : token @ unique -> token @ unique
    @@ many forkable unyielding total immutable
};;
type annotated_callbacks = annotated_callback cell;;
[%%expect{|
type annotated_callback = {
  f : token @ unique -> token @ unique @@ forkable unyielding many total
    immutable;
}
type annotated_callbacks = annotated_callback cell
|}]

(* A ghost function field has no slot, but its type still bounds the
   record's kind: a ghost-field read takes the record's mode, so a record
   holding a once or nonportable ghost function is not immutable data. *)
type ghost_callback = { n : int; f : (unit -> unit) @@ ghost };;
type ghost_callbacks = ghost_callback cell;;
[%%expect{|
type ghost_callback = { n : int; f : unit -> unit @@ ghost; }
Line 2, characters 23-37:
2 | type ghost_callbacks = ghost_callback cell;;
                           ^^^^^^^^^^^^^^
Error: This type "ghost_callback" should be an instance of type
         "('a : immutable_data)"
       The kind of ghost_callback is value non_float mod immutable logical
         because of the definition of ghost_callback at line 1, characters 0-62.
       But the kind of ghost_callback must be a subkind of immutable_data
         because of the definition of cell at line 1, characters 0-31.
|}]

type mutable_payload = { mutable n : int };;
type mutable_cells = mutable_payload cell;;
[%%expect{|
type mutable_payload = { mutable n : int; }
Line 2, characters 21-36:
2 | type mutable_cells = mutable_payload cell;;
                         ^^^^^^^^^^^^^^^
Error: This type "mutable_payload" should be an instance of type
         "('a : immutable_data)"
       The kind of mutable_payload is mutable_data mod logical
         because of the definition of mutable_payload at line 1, characters 0-42.
       But the kind of mutable_payload must be a subkind of immutable_data
         because of the definition of cell at line 1, characters 0-31.
|}]

type authority_payload = { dummy : int; state : token };;
type authority_cells = authority_payload cell;;
[%%expect{|
type authority_payload = { dummy : int; state : token; }
Line 2, characters 23-40:
2 | type authority_cells = authority_payload cell;;
                           ^^^^^^^^^^^^^^^^^
Error: This type "authority_payload" should be an instance of type
         "('a : immutable_data)"
       The kind of authority_payload is logical_data with token
         because of the definition of authority_payload at line 1, characters 0-55.
       But the kind of authority_payload must be a subkind of immutable_data
         because of the definition of cell at line 1, characters 0-31.
|}]
