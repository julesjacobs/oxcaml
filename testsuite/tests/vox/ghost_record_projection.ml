(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Projecting a ghost field reads no memory, so the record may be ghost. *)

type w = { m : int list @@ ghost aliased; r : int }
type v = { model : int list @@ ghost aliased; routes : int };;
[%%expect{|
type w = { m : int list @@ ghost aliased; r : int; }
type v = { model : int list @@ ghost aliased; routes : int; }
|}]

let (model_of @ total) (x : v @ ghost) : int list @ ghost = x.model;;
[%%expect{|
val model_of : v @ ghost -> int list @ ghost = <fun>
|}]

let (conv @ total) (x : v @ ghost) (routes : int) : w =
  { m = x.model; r = routes };;
[%%expect{|
val conv : v @ ghost -> int -> w = <fun>
|}]

(* A real field of a ghost record is still a run-time read. *)
let (routes_of @ total) (x : v @ ghost) : int = x.routes;;
[%%expect{|
Line 1, characters 48-49:
1 | let (routes_of @ total) (x : v @ ghost) : int = x.routes;;
                                                    ^
Error: This value is "ghost" but is expected to be "real".
Hint: if this is proof code, wrap the enclosing expression in "ghost_ (...)".
|}]

(* Ownership comes from the record, as before. *)
type token : value mod external_
type holder = { token : token @@ ghost };;
let take (h : holder @ ghost) : token @ unique ghost = h.token;;
[%%expect{|
type token : value mod external_
type holder = { token : token @@ ghost; }
val take : holder @ unique ghost -> token @ unique ghost = <fun>
|}]

let take_aliased (h : holder @ aliased ghost) : token @ unique ghost =
  h.token;;
[%%expect{|
Line 2, characters 2-9:
2 |   h.token;;
      ^^^^^^^
Error: This value is "aliased"
         because it is the field "token" of the record at line 2, characters 2-3
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]
