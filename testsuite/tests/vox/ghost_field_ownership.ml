(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml";
 readonly_files = "ghost_field_ownership.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* A ghost field is a component of its record for ownership: reading it
   inherits the record's uniqueness, linearity, visibility, contention and
   locality.
   Before this was enforced, a ghost-field read was always unique and
   read_write, so an ownership token stored in an aliased record could be
   taken twice, and a verified function could return a value contradicting
   its own refinement. *)

module G = Ghost_pref
type raw = { ptr : int G.t; tok : int G.token @@ ghost }
[%%expect{|
module G = Ghost_pref
type raw = { ptr : int G.t; tok : int G.token @@ ghost; }
|}]

(* The end-to-end exploit: two unique tokens for one cell. Compiled, it
   printed "verified to be 1: 2". *)
type cell = {r : raw | G.Heap.mem (G.own r.tok) r.ptr
                       && G.Heap.at (G.own r.tok) r.ptr === Some 1}

let take (c : cell @ aliased) : {t : int G.token | G.Heap.mem (G.own t) c.ptr
                                         && G.Heap.at (G.own t) c.ptr === Some 1}
                      @ unique =
  c.tok
[%%expect{|
type cell =
    {r : raw
      | (G.Heap.mem (G.own r.tok) r.ptr) &&
          ((G.Heap.at (G.own r.tok) r.ptr) === (Some 1))}
Line 7, characters 2-7:
7 |   c.tok
      ^^^^^
Error: This value is "aliased"
         because it is the field "tok" of the record at line 7, characters 2-3
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* Projection from an aliased record. *)
let take (r : raw @ aliased) : int G.token @ unique ghost = r.tok
[%%expect{|
Line 1, characters 60-65:
1 | let take (r : raw @ aliased) : int G.token @ unique ghost = r.tok
                                                                ^^^^^
Error: This value is "aliased"
         because it is the field "tok" of the record at line 1, characters 60-61
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* The pattern form. *)
let take_pat (r : raw @ aliased) : int G.token @ unique ghost =
  let { tok; _ } = r in tok
[%%expect{|
Line 2, characters 24-27:
2 |   let { tok; _ } = r in tok
                            ^^^
Error: This value is "aliased"
         because it is the field "tok" of the record at line 2, characters 6-16
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* Visibility: an immutable record yields an immutable field. *)
let take_imm (r : raw @ unique immutable) : int G.token @ unique read_write ghost =
  r.tok
[%%expect{|
Line 2, characters 2-7:
2 |   r.tok
      ^^^^^
Error: This value is "immutable"
         because it is the field "tok" of the record at line 2, characters 2-3
         which is "immutable".
       However, the highlighted expression is expected to be "read_write".
|}]

(* Unboxed records. *)
type u = #{ p : int G.t; t : int G.token @@ ghost }
[%%expect{|
type u = #{ p : int G.t; t : int G.token @@ ghost; }
|}]

let take_unboxed (c : u @ aliased) : int G.token @ unique ghost = c.#t
[%%expect{|
Line 1, characters 66-70:
1 | let take_unboxed (c : u @ aliased) : int G.token @ unique ghost = c.#t
                                                                      ^^^^
Error: This value is "aliased"
         because it is the field "t" of the record at line 1, characters 66-67
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

let take_unboxed_pat (c : u @ aliased) : int G.token @ unique ghost =
  let #{ t; _ } = c in t
[%%expect{|
Line 2, characters 23-24:
2 |   let #{ t; _ } = c in t
                           ^
Error: This value is "aliased"
         because it is the field "t" of the record at line 2, characters 6-15
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* Record update: the kept ghost field is read from the aliased record. *)
let copy (r : raw @ aliased) (p : int G.t @ unique) : raw @ unique =
  { r with ptr = p }
[%%expect{|
Line 2, characters 4-5:
2 |   { r with ptr = p }
        ^
Error: This value is "aliased"
         because it is the field "tok" of the record at line 2, characters 4-5
         which is "aliased".
       However, the highlighted expression is expected to be "unique"
         because it is the field "tok" of the record at line 2, characters 2-20
         which is expected to be "unique".
|}]

(* An exception payload is aliased. *)
exception Carry of raw
let extract (e : exn @ aliased) : int G.token @ unique ghost =
  match e with
  | Carry b -> b.tok
  | _ -> G.empty ()
[%%expect{|
exception Carry of raw
Line 4, characters 15-20:
4 |   | Carry b -> b.tok
                   ^^^^^
Error: This value is "aliased"
         because it is the field "tok" of the record at line 4, characters 15-16
         which is "aliased"
         because it is contained (via constructor "Carry") in the value at line 4, characters 4-11
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* Construction: an aliased token cannot be stored into a unique record and
   read back unique. *)
let launder (p : int G.t) (t : int G.token @ aliased ghost)
    : int G.token @ unique ghost =
  let r = { ptr = p; tok = t } in
  let { tok; _ } = r in tok
[%%expect{|
Line 4, characters 24-27:
4 |   let { tok; _ } = r in tok
                            ^^^
Error: This value is "aliased"
         because it is the field "tok" of the record at line 4, characters 6-16
         which is "aliased"
         because it is a record whose field "tok" is the expression at line 3, characters 27-28
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* Linearity: a once record yields a once field. *)
type 'a boxed = { v : int; g : 'a @@ ghost }
let take_many (b : (unit -> unit) boxed @ once) : (unit -> unit) @ many ghost =
  b.g
[%%expect{|
type 'a boxed = { v : int; g : 'a @@ ghost; }
Line 3, characters 2-5:
3 |   b.g
      ^^^
Error: This value is "once"
         because it is the field "g" of the record at line 3, characters 2-3
         which is "once".
       However, the highlighted expression is expected to be "many".
|}]

(* Accepted: a unique record gives up its ghost field uniquely. *)
let take (r : raw @ unique) : int G.token @ unique ghost = r.tok
let take_pat (r : raw @ unique) : int G.token @ unique ghost =
  let { tok; _ } = r in tok
let take_unboxed (c : u @ unique) : int G.token @ unique ghost = c.#t
let copy (r : raw @ unique) : raw @ unique = { r with ptr = r.ptr }
let store (p : int G.t) (t : int G.token @ unique ghost) : raw @ unique =
  { ptr = p; tok = t }
[%%expect{|
val take : raw @ unique -> int G.token @ unique ghost = <fun>
val take_pat : raw @ unique -> int G.token @ unique ghost = <fun>
val take_unboxed : u @ unique -> int G.token @ unique ghost = <fun>
val copy : raw @ unique -> raw @ unique = <fun>
val store : int G.t @ unique -> int G.token @ unique ghost -> raw @ unique =
  <fun>
|}]

(* Accepted: a ghost field of an aliased record can be borrowed. *)
let get (c : cell @ aliased) : {x : int | x = 1} = G.read c.ptr (borrow_ c.tok)
let get_pat (c : cell @ aliased) : {x : int | x = 1} =
  let { ptr; tok } = c in G.read ptr (borrow_ tok)
[%%expect{|
val get : cell -> {x : int | x = 1} = <fun>
val get_pat : cell -> {x : int | x = 1} = <fun>
|}]

(* Locality. The placeholder read from a ghost field is fabricated, so no
   memory is at stake, but locality is what confines [borrow_] to its region:
   a ghost token read from a borrowed record must stay local, or it outlives
   the borrow and still reads after the owner has written. Before this was
   enforced, a ghost read was always global and construction accepted local
   values. *)

(* Each route by which a ghost field is read or written. *)
let global_read (r : raw @ local unique) : int G.token @ global unique ghost =
  r.tok
[%%expect{|
Line 2, characters 2-7:
2 |   r.tok
      ^^^^^
Error: This value is "local" to the parent region
         because it is the field "tok" of the record at line 2, characters 2-3
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let global_read_pat (r : raw @ local unique)
    : int G.token @ global unique ghost =
  let { tok; _ } = r in tok
[%%expect{|
Line 3, characters 24-27:
3 |   let { tok; _ } = r in tok
                            ^^^
Error: This value is "local" to the parent region
         because it is the field "tok" of the record at line 3, characters 6-16
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let global_read_unboxed (c : u @ local unique)
    : int G.token @ global unique ghost =
  c.#t
[%%expect{|
Line 3, characters 2-6:
3 |   c.#t
      ^^^^
Error: This value is "local" to the parent region
         because it is the field "t" of the record at line 3, characters 2-3
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let global_read_unboxed_pat (c : u @ local unique)
    : int G.token @ global unique ghost =
  let #{ t; _ } = c in t
[%%expect{|
Line 3, characters 23-24:
3 |   let #{ t; _ } = c in t
                           ^
Error: This value is "local" to the parent region
         because it is the field "t" of the record at line 3, characters 6-15
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let store_local (p : int G.t) (t : int G.token @ local unique ghost)
    : raw @ global unique =
  { ptr = p; tok = t }
[%%expect{|
Line 3, characters 19-20:
3 |   { ptr = p; tok = t }
                       ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "tok" of the record at line 3, characters 2-22
         which is expected to be "global".
|}]

let store_local_unboxed (p : int G.t) (t : int G.token @ local unique ghost)
    : u @ global unique =
  #{ p; t }
[%%expect{|
Line 3, characters 8-9:
3 |   #{ p; t }
            ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "t" of the record at line 3, characters 2-11
         which is expected to be "global".
|}]

(* Record update: the other field crosses locality, the kept ghost field
   does not. *)
type counted = { n : int; ct : int G.token @@ ghost }
let copy_local (r : counted @ local unique) : counted @ global unique =
  { r with n = 0 }
[%%expect{|
type counted = { n : int; ct : int G.token @@ ghost; }
Line 3, characters 4-5:
3 |   { r with n = 0 }
        ^
Error: This value is "local" to the parent region
         because it is the field "ct" of the record at line 3, characters 4-5
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global"
         because it is the field "ct" of the record at line 3, characters 2-18
         which is expected to be "global".
|}]

(* A borrowed record's token, kept past the borrow. *)
let leak (c : raw @ unique) : int G.token @ global ghost =
  let b = borrow_ c in b.tok
[%%expect{|
Line 2, characters 23-28:
2 |   let b = borrow_ c in b.tok
                           ^^^^^
Error: This value is "local"
         because it is the field "tok" of the record at line 2, characters 23-24
         which is "local" because it is borrowed.
       However, the highlighted expression is expected to be "global".
|}]

(* The read-side exploit: [peek] hands out the token of a borrowed record,
   which is then used to read after the owner has written. Compiled, it
   printed "verified to be 1: 2". *)
type rawa = { aptr : int G.t @@ aliased; atok : int G.token @@ ghost }
let peek (p : int G.t)
    (c : {r : rawa | r.aptr === p && G.Heap.mem (G.own r.atok) p
                     && G.Heap.at (G.own r.atok) p === Some 1} @ local)
    : {t : int G.token | G.Heap.mem (G.own t) p
                         && G.Heap.at (G.own t) p === Some 1} @ local ghost =
  c.atok
let one () : {x : int | x = 1} =
  let s = G.alloc 1 (G.empty ()) in
  let p = s.G.value in
  let c = { aptr = p; atok = s.G.state } in
  let leaked = peek p (borrow_ c) in
  let _t = G.write p 2 c.atok in
  G.read p (borrow_ leaked)
[%%expect{|
type rawa = { aptr : int G.t @@ aliased; atok : int G.token @@ ghost; }
val peek :
  (p : int G.t) ->
  {r : rawa
    | (r.aptr === p) &&
        ((G.Heap.mem (G.own r.atok) p) &&
           ((G.Heap.at (G.own r.atok) p) === (Some 1)))} @ local ->
  {t : int G.token
    | (G.Heap.mem (G.own t) p) && ((G.Heap.at (G.own t) p) === (Some 1))} @ local
  ghost = <fun>
Line 12, characters 15-33:
12 |   let leaked = peek p (borrow_ c) in
                    ^^^^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it escapes the borrow region at line 12, characters 15-33.
|}]

(* The store-side exploit: a borrowed token is stored in a record that
   outlives the borrow. It also printed "verified to be 1: 2". *)
let box (p : int G.t)
    (t : {t : int G.token | G.Heap.mem (G.own t) p
                            && G.Heap.at (G.own t) p === Some 1} @ local ghost)
    : {r : rawa | r.aptr === p && G.Heap.mem (G.own r.atok) p
                  && G.Heap.at (G.own r.atok) p === Some 1} @ global =
  { aptr = p; atok = t }
[%%expect{|
Line 6, characters 21-22:
6 |   { aptr = p; atok = t }
                         ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "atok" of the record at line 6, characters 2-24
         which is expected to be "global".
|}]

(* Accepted: inside the borrow region, the borrowed token reads. *)
let get_borrowed (p : int G.t)
    (c : {r : raw | r.ptr === p && G.Heap.mem (G.own r.tok) p
                    && G.Heap.at (G.own r.tok) p === Some 1} @ unique)
    : {x : int | x = 1} =
  let b = borrow_ c in G.read p b.tok
[%%expect{|
val get_borrowed :
  (p : int G.t) ->
  {r : raw
    | (r.ptr === p) &&
        ((G.Heap.mem (G.own r.tok) p) &&
           ((G.Heap.at (G.own r.tok) p) === (Some 1)))} @ unique ->
  {x : int | x = 1} = <fun>
|}]

(* Accepted: a local record gives up its ghost field locally, and a local
   token can be stored in a local record. *)
let local_read (r : raw @ local unique) : int G.token @ local unique ghost =
  r.tok
let local_read_unboxed (c : u @ local unique)
    : int G.token @ local unique ghost =
  c.#t
let store_in_local (p : int G.t) (t : int G.token @ local unique ghost)
    : raw @ local unique =
  exclave_ { ptr = p; tok = t }
[%%expect{|
val local_read : raw @ local unique -> int G.token @ local unique ghost =
  <fun>
val local_read_unboxed : u @ local unique -> int G.token @ local unique ghost =
  <fun>
val store_in_local :
  int G.t @ unique -> int G.token @ local unique ghost -> raw @ local unique =
  <fun>
|}]

(* Accepted: a ghost field whose type crosses locality still crosses it, on
   reads and on construction. *)
let int_read (b : int boxed @ local) : int @ global ghost = b.g
let int_store (n : int @ local) : int boxed @ global = { v = 0; g = n }
[%%expect{|
val int_read : int boxed @ local -> int @ ghost = <fun>
val int_store : int @ local -> int boxed = <fun>
|}]

(* A [global] ghost field (which is also aliased) is read global from a
   local record. Construction then requires a global value, so a borrowed
   token cannot be stored in it. *)
type 'a logical = { w : int; model : 'a @@ ghost global }
let model_read (l : int list logical @ local) : int list @ global ghost =
  l.model
let model_store (m : int list) : int list logical @ global =
  { w = 0; model = m }
[%%expect{|
type 'a logical = { w : int; model : 'a @@ ghost global; }
val model_read : int list logical @ local -> int list @ ghost = <fun>
val model_store : int list -> int list logical = <fun>
|}]

let model_store_local (m : int list @ local) : int list logical @ global =
  { w = 0; model = m }
[%%expect{|
Line 2, characters 19-20:
2 |   { w = 0; model = m }
                       ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "model" (with some modality) of the record at line 2, characters 2-22.
|}]

(* Accepted: the non-borrowed uses of the exploit programs above. *)
let peek_global (c : rawa @ unique) : int G.token @ unique ghost = c.atok
let box_global (p : int G.t) (t : int G.token @ unique ghost)
    : rawa @ global unique =
  { aptr = p; atok = t }
[%%expect{|
val peek_global : rawa @ unique -> int G.token @ unique ghost = <fun>
val box_global : int G.t -> int G.token @ unique ghost -> rawa @ unique =
  <fun>
|}]

(* Kinds. A ghost field has no slot, but a record's kind must still account
   for its type: a ghost-field read takes the record's mode, so the record
   may cross an axis (uniqueness, locality, ...) only if its ghost fields'
   types do. Before this was enforced, the kind of a record left its ghost
   fields out. *)

(* The end-to-end program: an unboxed record holding a token, sealed as
   crossing every axis, gives up its token twice. Compiled, it printed
   "verified to be 1: 2". *)
let one () : {x : int | x = 1} =
  let s = G.alloc 1 (G.empty ()) in
  let p = s.G.value in
  let module M : sig
    type t : value mod everything & void mod everything
    val mk : {k : int G.token | G.Heap.mem (G.own k) p
                                && G.Heap.at (G.own k) p === Some 1}
             @ unique total stateful ghost -> t @ unique
    val take : t @ unique -> {k : int G.token | G.Heap.mem (G.own k) p
                                && G.Heap.at (G.own k) p === Some 1}
             @ unique ghost
  end = struct
    type tok1 = {k : int G.token | G.Heap.mem (G.own k) p
                                   && G.Heap.at (G.own k) p === Some 1}
    type t = #{ m : int; ut : tok1 @@ ghost }
    let mk (t : tok1 @ unique ghost) : t @ unique = #{ m = 0; ut = t }
    let take (r : t @ unique) : tok1 @ unique ghost = r.#ut
  end in
  let r = M.mk s.G.state in
  let a = M.take r in
  let b = M.take r in
  let _t = G.write p 2 a in
  G.read p (borrow_ b)
[%%expect{|
Lines 12-18, characters 8-5:
12 | ........struct
13 |     type tok1 = {k : int G.token | G.Heap.mem (G.own k) p
14 |                                    && G.Heap.at (G.own k) p === Some 1}
15 |     type t = #{ m : int; ut : tok1 @@ ghost }
16 |     let mk (t : tok1 @ unique ghost) : t @ unique = #{ m = 0; ut = t }
17 |     let take (r : t @ unique) : tok1 @ unique ghost = r.#ut
18 |   end...
Error: Signature mismatch:
       Modules do not match:
         sig
           type tok1 =
               {k : int G.token
                 | (G.Heap.mem (G.own k) p) &&
                     ((G.Heap.at (G.own k) p) === (Some 1))}
           type t = #{ m : int; ut : tok1 @@ ghost; }
           val mk : tok1 @ unique ghost -> t @ unique
           val take : t @ unique -> tok1 @ unique ghost
         end
       is not included in
         sig
           type t : value mod everything & void mod everything
           val mk :
             {k : int G.token
               | (G.Heap.mem (G.own k) p) &&
                   ((G.Heap.at (G.own k) p) === (Some 1))} @ unique
             total stateful ghost -> t @ unique
           val take :
             t @ unique ->
             {k : int G.token
               | (G.Heap.mem (G.own k) p) &&
                   ((G.Heap.at (G.own k) p) === (Some 1))} @ unique
             ghost
         end
       Type declarations do not match:
         type t = #{ m : int; ut : tok1 @@ ghost; }
       is not included in
         type t : value mod everything & void mod everything
       The kind of the first is
           immediate with tok1 @@ external_
           & void mod everything with tok1 @@ external_
         because of the definition of t at line 15, characters 4-45.
       But the kind of the first must be a subkind of
           value mod everything & void mod everything
         because of the definition of t at line 5, characters 4-55.

       The first mode-crosses less than the second along:
         locality: mod global with tok1 ≰ mod global
         uniqueness: mod aliased with tok1 ≰ mod aliased
         linearity: mod many with tok1 ≰ mod many
         forkable: mod forkable with tok1 ≰ mod forkable
         yielding: mod unyielding with tok1 ≰ mod unyielding
         visibility: mod immutable with tok1 ≰ mod immutable
|}]

(* A boxed record holding a token is not immutable data: it does not cross
   linearity or visibility. *)
module Boxed : sig
  type t : immutable_data
end = struct
  type t = { bp : int G.t; bt : int G.token @@ ghost }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { bp : int G.t; bt : int G.token @@ ghost }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { bp : int G.t; bt : int G.token @@ ghost; } end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type t = { bp : int G.t; bt : int G.token @@ ghost; }
       is not included in
         type t : immutable_data
       The kind of the first is logical_data with int G.t with int G.token
         because of the definition of t at line 4, characters 2-54.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-25.
|}]

(* A kind-annotated type variable. *)
type um = #{ m : int; ut : int G.token @@ ghost }
let promote (type a : value mod everything & void mod everything)
    (x : a @ local aliased) : a @ global unique = x
let lift (x : um @ local aliased) : um @ global unique = promote x
[%%expect{|
type um = #{ m : int; ut : int G.token @@ ghost; }
val promote :
  ('a : value mod everything & void mod everything).
    'a @ local -> 'a @ unique =
  <fun>
Line 4, characters 65-66:
4 | let lift (x : um @ local aliased) : um @ global unique = promote x
                                                                     ^
Error: The value "x" has type "um" but an expression was expected of type
         "('a : value mod everything & void mod everything)"
       The kind of um is
           immediate with int G.token & void mod everything with int G.token
         because of the definition of um at line 1, characters 0-49.
       But the kind of um must be a subkind of
           value mod everything & void mod everything
         because of the definition of promote at lines 2-3, characters 12-51.
|}]

(* Accepted: the record crosses the axes its ghost fields' types cross. A
   token crosses contention; an [int] crosses everything. *)
module Unboxed_contended : sig
  type t : value mod contended & void mod contended
end = struct
  type t = um
end
module Unboxed_int : sig
  type t : value mod everything & void mod everything
end = struct
  type t = #{ ui : int; ug : int @@ ghost }
end
module Boxed_int : sig
  type t : immutable_data
end = struct
  type t = { bi : int; bg : int @@ ghost }
end
[%%expect{|
module Unboxed_contended :
  sig type t : value mod contended & void mod contended end
module Unboxed_int :
  sig type t : value mod everything & void mod everything end
module Boxed_int : sig type t : immutable_data end
|}]

