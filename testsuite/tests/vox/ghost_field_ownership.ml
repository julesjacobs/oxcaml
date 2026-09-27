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
   inherits the record's uniqueness, linearity, visibility and contention.
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

(* Accepted: reading stays global and ghost; a local value may be stored. *)
let global_read (r : raw @ local unique) : int G.token @ global unique ghost =
  r.tok
let store_local (p : int G.t) (t : int G.token @ local unique ghost)
    : raw @ unique =
  { ptr = p; tok = t }
[%%expect{|
val global_read : raw @ local unique -> int G.token @ unique ghost = <fun>
val store_local :
  int G.t @ unique -> int G.token @ local unique ghost -> raw @ unique =
  <fun>
|}]
