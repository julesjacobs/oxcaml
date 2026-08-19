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

(* An all-ghost record has kind void: nothing exists at run time, so it
   vanishes from ABIs — no register, no slot. This is what makes the
   Ghost.t wrapper erase what it wraps. *)
type 'a box = { ghost : 'a @@ ghost }
let wrap x = { ghost = x }
let unwrap b = ghost_ b.ghost
[%%expect{|
type 'a box = { ghost : 'a @@ ghost; }
val wrap : 'a -> 'a box = <fun>
val unwrap : 'a box -> 'a @ ghost = <fun>
|}]

(* Being void, it cannot inhabit value-polymorphic containers... *)
let bad = [ wrap 1; wrap 2 ]
[%%expect{|
Line 1, characters 12-18:
1 | let bad = [ wrap 1; wrap 2 ]
                ^^^^^^
Error: This expression has type "int box"
       but an expression was expected of type "('a : value_or_null)"
       The layout of int box is void
         because of the definition of box at line 1, characters 0-37.
       But the layout of int box must be a value layout
         because the type argument of list has layout value_or_null.
|}]

(* ...but a record field of a void type needs no modality and takes no
   slot: the type alone erases it. *)
type holder = { id : int; hidden : string box }
let mk id s = { id; hidden = wrap s }
let get h = h.id
[%%expect{|
type holder = { id : int; hidden : string box; }
val mk : int -> string -> holder = <fun>
val get : holder -> int = <fun>
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

(* Block indices have no route to a ghost field: there is no slot to
   index, and a fabricated index would bypass the ghost read mode (found
   by review: the accessor typed at real mode and computed an
   out-of-bounds offset). On an all-ghost record the void kind already
   forbids the base type. *)
type ir = { ia : int; ip : string @@ ghost; ib : int }
let bad = (.ip)
[%%expect{|
type ir = { ia : int; ip : string @@ ghost; ib : int; }
Line 2, characters 12-14:
2 | let bad = (.ip)
                ^^
Error: Block indices do not support ghost fields in records.
|}]

let bad = (.ghost)
[%%expect{|
Line 1, characters 12-17:
1 | let bad = (.ghost)
                ^^^^^
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "'b box"
       The layout of 'a box is void
         because of the definition of box at line 1, characters 0-37.
       But the layout of 'a box must be a value layout
         because it's the base type (the first type parameter) for a
         block index (idx or mut_idx).
|}]

(* A record mixing ghost fields with ordinary void-typed fields is not
   all-ghost: it still needs at least one runtime value (found by review:
   [List.exists] where the rule says all fields). *)
type hybrid = { marker : int @@ ghost; ordinary_void : string box }
[%%expect{|
Line 1, characters 0-67:
1 | type hybrid = { marker : int @@ ghost; ordinary_void : string box }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

(* The toplevel printer shows ghost fields without reading memory. *)
let shown = { a = 10; p = "unseen"; b = 20 }
[%%expect{|
val shown : r = {a = 10; p = <ghost>; b = 20}
|}]

(* A ghost read owes nothing to the record: the result is a fabricated
   constant, minimal on every axis but ghostliness. *)
let project (x : r @ local) : string @ global ghost = x.p
[%%expect{|
val project : r @ local -> string @ ghost = <fun>
|}]

(* A ghost field's type contributes no bounds to the record's kind:
   nothing of it is stored. *)
type opaque
type crossing : value mod portable = { live : int; hidden : opaque @@ ghost }
[%%expect{|
type opaque
type crossing = { live : int; hidden : opaque @@ ghost; }
|}]
