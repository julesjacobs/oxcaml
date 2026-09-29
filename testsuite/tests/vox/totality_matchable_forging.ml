(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Forging [@@total_matchable] through an alias. When the implementation
   reaches the signature through a module PATH -- sealing [Bad] directly, a
   functor argument, [include Bad] -- its declarations are strengthened
   aliases ([type u = Bad.u]). Inclusion must judge the aliased declaration:
   walking from the alias under its own name misses the recursion, and the
   knot closes (on jujacobs/vox/t9-matchability-20260929 each case below was
   accepted and [lie 1] evaluated to 1 at type [{r | r = 2}]). *)

module Bad = struct
  type u = t -> {v : unit | false}
  and t = Roll of u
  let app f x = f x
  let mk f = f
end;;
[%%expect{|
module Bad :
  sig
    type u = t -> {v : unit | false}
    and t = Roll of u
    val app : ('a -> 'b) -> 'a -> 'b
    val mk : 'a -> 'a
  end
|}]

(* 1. Sealing a module path. *)
module M : sig
  type u [@@total_matchable]
  type t = Roll of u
  val app : u -> t -> {v : unit | false} @@ total
  val mk : (t -> {v : unit | false}) -> u @@ total
end = Bad;;
[%%expect{|
Line 6, characters 6-9:
6 | end = Bad;;
          ^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           type u = t -> {v : unit | false}
           and t = Bad.t = Roll of u
           val app : ('a -> 'b) -> 'a -> 'b
           val mk : 'a -> 'a
         end
       is not included in
         sig
           type u
           type t = Roll of u
           val app : u -> t -> {v : unit | false} @@ total
           val mk : (t -> {v : unit | false}) -> u @@ total
         end
       Type declarations do not match:
         type u = t -> {v : unit | false}
       is not included in
         type u
       Their total-matchability guarantees differ;
       an interface may promise
       "[@@total_matchable]" only if the implementation's declaration does.
|}]

let (delta @ total) (x : M.t) : {v : unit | false} =
  match x with M.Roll f -> M.app f x
let (omega @ total) () : {v : unit | false} =
  delta (M.Roll (M.mk (fun y -> delta y)))
let (lie @ total) (x : int) : {r : int | r = x + 1} =
  ghost_ (omega ());
  x
let v = lie 1;;
[%%expect{|
Line 1, characters 25-26:
1 | let (delta @ total) (x : M.t) : {v : unit | false} =
                             ^
Error: Unbound module "M"
|}]

(* 2. Functor application. *)
module F (X : sig
  type u [@@total_matchable]
  type t = Roll of u
  val app : u -> t -> {v : unit | false} @@ total
  val mk : (t -> {v : unit | false}) -> u @@ total
end) = struct
  let (delta @ total) (x : X.t) : {v : unit | false} =
    match x with X.Roll f -> X.app f x
  let (omega @ total) () : {v : unit | false} =
    delta (X.Roll (X.mk (fun y -> delta y)))
end
module R = F (Bad)
let (lie2 @ total) (x : int) : {r : int | r = x + 1} =
  ghost_ (R.omega ());
  x
let v2 = lie2 1;;
[%%expect{|
module F :
  functor
    (X : sig
           type u
           type t = Roll of u
           val app : u -> t -> {v : unit | false} @@ total
           val mk : (t -> {v : unit | false}) -> u @@ total
         end)
    ->
    sig
      val delta : X.t -> {v : unit | false}
      val omega : unit -> {v : unit | false}
    end
Line 12, characters 11-18:
12 | module R = F (Bad)
                ^^^^^^^
Error: Modules do not match:
       sig
         type u = t -> {v : unit | false}
         and t = Bad.t = Roll of u
         val app : ('a -> 'b) -> 'a -> 'b
         val mk : 'a -> 'a
       end
     is not included in
       sig
         type u
         type t = Roll of u
         val app : u -> t -> {v : unit | false} @@ total
         val mk : (t -> {v : unit | false}) -> u @@ total
       end
     Type declarations do not match:
       type u = t -> {v : unit | false}
     is not included in
       type u
     Their total-matchability guarantees differ;
     an interface may promise
     "[@@total_matchable]" only if the implementation's declaration does.
|}]

(* 3. [include] of the path inside the sealed structure. *)
module N : sig
  type u [@@total_matchable]
  type t = Roll of u
  val app : u -> t -> {v : unit | false} @@ total
  val mk : (t -> {v : unit | false}) -> u @@ total
end = struct
  include Bad
end;;
[%%expect{|
Lines 6-8, characters 6-3:
6 | ......struct
7 |   include Bad
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type u = t -> {v : unit | false}
           and t = Bad.t = Roll of u
           val app : ('a -> 'b) -> 'a -> 'b
           val mk : 'a -> 'a
         end
       is not included in
         sig
           type u
           type t = Roll of u
           val app : u -> t -> {v : unit | false} @@ total
           val mk : (t -> {v : unit | false}) -> u @@ total
         end
       Type declarations do not match:
         type u = t -> {v : unit | false}
       is not included in
         type u
       Their total-matchability guarantees differ;
       an interface may promise
       "[@@total_matchable]" only if the implementation's declaration does.
|}]

(* 4. Direct sealing of the same structure (the branch's own test): rejected. *)
module D : sig
  type u [@@total_matchable]
  type t = Roll of u
end = struct
  type u = t -> {v : unit | false}
  and t = Roll of u
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type u = t -> {v : unit | false}
6 |   and t = Roll of u
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type u = t -> {v : unit | false} and t = Roll of u end
       is not included in
         sig type u type t = Roll of u end
       Type declarations do not match:
         type u = t -> {v : unit | false}
       is not included in
         type u
       Their total-matchability guarantees differ;
       an interface may promise
       "[@@total_matchable]" only if the implementation's declaration does.
|}]

(* Recursive module signatures may not assert the guarantee: inside the group
   the type is reached through the forward path [A.u], which the definition-
   site check does not recognise as the type itself. *)
module rec A : sig
  type u [@@total_matchable]
  type t = Roll of u
end = struct
  type u = A.t -> int [@@total_matchable]
  type t = Roll of u
end;;
[%%expect{|
Line 2, characters 2-28:
2 |   type u [@@total_matchable]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Recursive module signatures cannot assert "[@@total_matchable]".
|}]

(* An [@@inductive] matched type is trusted: its definition-site check saw
   the representation, and a functor parameter's payload was bound before it.
   (The trunk expectation of typing-modes/inductive_functors.ml.) *)
module Roller (Payload : sig type t end) = struct
  type t = Roll of (Payload.t -> int) [@@inductive]
  let (unroll @ total) = function Roll f -> f
end;;
[%%expect{|
module Roller :
  functor (Payload : sig type t end) ->
    sig
      type t = Roll of (Payload.t -> int)
      [@@inductive]
      val unroll : t -> Payload.t -> int
    end
|}]
