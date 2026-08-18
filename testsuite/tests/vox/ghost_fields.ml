(* TEST
 expect;
*)

(* The @@ ghost field modality: the one place ghostliness touches
   representation. A ghost field occupies no slot in its record;
   reading it fabricates a placeholder at mode ghost; writing it
   evaluates the expression and discards the value. *)

type r = { a : int; p : string @@ ghost; b : int }
[%%expect{|
type r = { a : int; p : string @@ ghost; b : int; }
|}]

(* Construction accepts real and ghost values alike. *)
let mk a b = { a; p = "kept for effects"; b }
let mk' a b = { a; p = (ghost_ (String.concat "" ["never"; "runs"])); b }
[%%expect{|
val mk : int -> int -> r = <fun>
val mk' : int -> int -> r = <fun>
|}]

(* Projection is ghost: usable only at ghost positions. *)
let bad r = String.length r.p
[%%expect{|
Line 1, characters 26-29:
1 | let bad r = String.length r.p
                              ^^^
Error: This value is "ghost" but is expected to be "real".
|}]

let ok r = ghost_ (String.length r.p)
[%%expect{|
val ok : r -> int @ ghost = <fun>
|}]

(* Patterns bind placeholders: binding is fine, reading is not. *)
let bad r = let { p; _ } = r in String.length p
[%%expect{|
Line 1, characters 46-47:
1 | let bad r = let { p; _ } = r in String.length p
                                                  ^
Error: This value is "ghost" but is expected to be "real".
|}]

let ok r = let { a; p = _; b } = r in a + b
[%%expect{|
val ok : r -> int = <fun>
|}]

(* Functional update over ghost fields. *)
let upd r = { r with a = 42 }
let upd2 r = { r with p = "replaced and dropped" }
[%%expect{|
val upd : r -> r = <fun>
val upd2 : r -> r = <fun>
|}]

(* A mutable field cannot be ghost: there is nothing to mutate. *)
type bad = { mutable m : int @@ ghost }
[%%expect{|
Line 1, characters 13-37:
1 | type bad = { mutable m : int @@ ghost }
                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The field "m" cannot be both mutable and ghost: a ghost field
       occupies no slot, so there is nothing to mutate.
|}]

(* The modality is only interpreted on boxed record fields; everywhere else
   it is rejected, not ignored. *)
type bad = { u : int @@ ghost } [@@unboxed]
[%%expect{|
Line 1, characters 24-29:
1 | type bad = { u : int @@ ghost } [@@unboxed]
                            ^^^^^
Error: Unrecognized modality ghost.
|}]

type bad = A of int @@ ghost
[%%expect{|
Line 1, characters 23-28:
1 | type bad = A of int @@ ghost
                           ^^^^^
Error: Unrecognized modality ghost.
|}]

type bad = A of { x : int @@ ghost }
[%%expect{|
Line 1, characters 29-34:
1 | type bad = A of { x : int @@ ghost }
                                 ^^^^^
Error: Unrecognized modality ghost.
|}]

module type Bad = sig
  val x : int @@ ghost
end
[%%expect{|
Line 2, characters 17-22:
2 |   val x : int @@ ghost
                     ^^^^^
Error: Unrecognized modality ghost.
|}]

(* An all-ghost record is legal: its value is the immediate 0 and it has
   kind value, so it can go in ordinary data structures. This is what makes
   the Ghost.t wrapper work. *)
type 'a box = { ghost : 'a @@ ghost }
let wrap x = { ghost = x }
let unwrap b = ghost_ b.ghost
let boxes = [ wrap 1; wrap 2 ]
[%%expect{|
type 'a box = { ghost : 'a @@ ghost; }
val wrap : 'a -> 'a box = <fun>
val unwrap : 'a box -> 'a @ ghost = <fun>
val boxes : int box list = [{ghost = <ghost>}; {ghost = <ghost>}]
|}]

(* A ghost value can be stored through the wrapper. The construction
   accepts a ghost argument directly; a wrapper function must say its
   parameter is ghost, since an unannotated parameter is real. *)
let wrap_e (x : 'a @ ghost) = { ghost = x }
let store () =
  let secret = ghost_ (String.make 1000000 'x') in
  wrap_e secret
[%%expect{|
val wrap_e : 'a @ ghost -> 'a box = <fun>
val store : unit -> string box = <fun>
|}]

(* Ghostliness decides layout, so signatures must agree with structures,
   in both directions. *)
module Bad : sig
  type t = { x : int; y : string }
end = struct
  type t = { x : int; y : string @@ ghost }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { x : int; y : string @@ ghost }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { x : int; y : string @@ ghost; } end
       is not included in
         sig type t = { x : int; y : string; } end
       Type declarations do not match:
         type t = { x : int; y : string @@ ghost; }
       is not included in
         type t = { x : int; y : string; }
       Fields do not match:
         "y : string @@ ghost;"
       is not the same as:
         "y : string;"
       The first is ghost and the second is not.
       Ghostliness decides the record's layout, so both sides must agree.
|}]

module Bad_rev : sig
  type t = { x : int; y : string @@ ghost }
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
         sig type t = { x : int; y : string @@ ghost; } end
       Type declarations do not match:
         type t = { x : int; y : string; }
       is not included in
         type t = { x : int; y : string @@ ghost; }
       Fields do not match:
         "y : string;"
       is not the same as:
         "y : string @@ ghost;"
       The second is ghost and the first is not.
       Ghostliness decides the record's layout, so both sides must agree.
|}]

(* An all-void record is still rejected when the voidness does not come from
   ghostliness. *)
type v : void
type bad = { g : v }
[%%expect{|
type v : void
Line 2, characters 0-20:
2 | type bad = { g : v }
    ^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]
