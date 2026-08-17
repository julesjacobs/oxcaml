(* TEST
 expect;
*)

(* The @@ erased field modality: the one place erasure touches
   representation. An erased field occupies no slot in its record;
   reading it fabricates a placeholder at mode erased; writing it
   evaluates the expression and discards the value. *)

type r = { a : int; p : string @@ erased; b : int }
[%%expect{|
type r = { a : int; p : string @@ erased; b : int; }
|}]

(* Construction accepts retained and erased values alike. *)
let mk a b = { a; p = "kept for effects"; b }
let mk' a b = { a; p = (erased_ (String.concat "" ["never"; "runs"])); b }
[%%expect{|
val mk : int -> int -> r = <fun>
val mk' : int -> int -> r = <fun>
|}]

(* Projection is erased: usable only at erased positions. *)
let bad r = String.length r.p
[%%expect{|
Line 1, characters 26-29:
1 | let bad r = String.length r.p
                              ^^^
Error: This value is "erased" but is expected to be "retained".
|}]

let ok r = erased_ (String.length r.p)
[%%expect{|
val ok : r -> int @ erased = <fun>
|}]

(* Patterns bind placeholders: binding is fine, reading is not. *)
let bad r = let { p; _ } = r in String.length p
[%%expect{|
Line 1, characters 46-47:
1 | let bad r = let { p; _ } = r in String.length p
                                                  ^
Error: This value is "erased" but is expected to be "retained".
|}]

let ok r = let { a; p = _; b } = r in a + b
[%%expect{|
val ok : r -> int = <fun>
|}]

(* Functional update over erased fields. *)
let upd r = { r with a = 42 }
let upd2 r = { r with p = "replaced and dropped" }
[%%expect{|
val upd : r -> r = <fun>
val upd2 : r -> r = <fun>
|}]

(* A mutable field cannot be erased: there is nothing to mutate. *)
type bad = { mutable m : int @@ erased }
[%%expect{|
Line 1, characters 13-38:
1 | type bad = { mutable m : int @@ erased }
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The field "m" cannot be both mutable and erased: an erased field
       occupies no slot, so there is nothing to mutate.
|}]

(* The modality is only interpreted on boxed record fields; everywhere else
   it is rejected, not ignored. *)
type bad = { u : int @@ erased } [@@unboxed]
[%%expect{|
Line 1, characters 24-30:
1 | type bad = { u : int @@ erased } [@@unboxed]
                            ^^^^^^
Error: Unrecognized modality erased.
|}]

type bad = A of int @@ erased
[%%expect{|
Line 1, characters 23-29:
1 | type bad = A of int @@ erased
                           ^^^^^^
Error: Unrecognized modality erased.
|}]

type bad = A of { x : int @@ erased }
[%%expect{|
Line 1, characters 29-35:
1 | type bad = A of { x : int @@ erased }
                                 ^^^^^^
Error: Unrecognized modality erased.
|}]

module type Bad = sig
  val x : int @@ erased
end
[%%expect{|
Line 2, characters 17-23:
2 |   val x : int @@ erased
                     ^^^^^^
Error: Unrecognized modality erased.
|}]

(* An all-erased record is legal: its value is the immediate 0 and it has
   kind value, so it can go in ordinary data structures. This is what makes
   the Erased.t wrapper work. *)
type 'a box = { erased : 'a @@ erased }
let wrap x = { erased = x }
let unwrap b = erased_ b.erased
let boxes = [ wrap 1; wrap 2 ]
[%%expect{|
type 'a box = { erased : 'a @@ erased; }
val wrap : 'a -> 'a box = <fun>
val unwrap : 'a box -> 'a @ erased = <fun>
val boxes : int box list = [{erased = <erased>}; {erased = <erased>}]
|}]

(* An erased value can be stored through the wrapper. The construction
   accepts an erased argument directly; a wrapper function must say its
   parameter is erased, since an unannotated parameter is retained. *)
let wrap_e (x : 'a @ erased) = { erased = x }
let store () =
  let secret = erased_ (String.make 1000000 'x') in
  wrap_e secret
[%%expect{|
val wrap_e : 'a @ erased -> 'a box = <fun>
val store : unit -> string box = <fun>
|}]

(* Erasedness decides layout, so signatures must agree with structures,
   in both directions. *)
module Bad : sig
  type t = { x : int; y : string }
end = struct
  type t = { x : int; y : string @@ erased }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { x : int; y : string @@ erased }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { x : int; y : string @@ erased; } end
       is not included in
         sig type t = { x : int; y : string; } end
       Type declarations do not match:
         type t = { x : int; y : string @@ erased; }
       is not included in
         type t = { x : int; y : string; }
       Fields do not match:
         "y : string @@ erased;"
       is not the same as:
         "y : string;"
       The first is erased and the second is not.
       Erasedness decides the record's layout, so both sides must agree.
|}]

module Bad_rev : sig
  type t = { x : int; y : string @@ erased }
end = struct
  type t = { x : int; y : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { x : int; y : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { x : int; y : string; } end
       is not included in
         sig type t = { x : int; y : string @@ erased; } end
       Type declarations do not match:
         type t = { x : int; y : string; }
       is not included in
         type t = { x : int; y : string @@ erased; }
       Fields do not match:
         "y : string;"
       is not the same as:
         "y : string @@ erased;"
       The second is erased and the first is not.
       Erasedness decides the record's layout, so both sides must agree.
|}]

(* An all-void record is still rejected when the voidness does not come from
   erasure. *)
type v : void
type bad = { g : v }
[%%expect{|
type v : void
Line 2, characters 0-20:
2 | type bad = { g : v }
    ^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
