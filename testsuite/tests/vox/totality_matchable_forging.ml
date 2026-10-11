(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Forging [mod logical] through an alias. When the implementation reaches the
   signature through a module PATH -- sealing [Bad] directly, a functor
   argument, [include Bad] -- its declarations are strengthened aliases
   ([type u = Bad.u]). On jujacobs/vox/t9-matchability-20260929 each case
   below forged the [@@total_matchable] guarantee, and [lie 1] evaluated to 1
   at type [{r | r = 2}]. With logicality in the kind, the alias has the kind of
   [Bad.u], a function type whose argument is not logical, and ordinary kind
   inclusion refuses it. *)

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
  type u : value mod logical
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
           type u : value mod logical
           type t = Roll of u
           val app : u -> t -> {v : unit | false} @@ total
           val mk : (t -> {v : unit | false}) -> u @@ total
         end
       Type declarations do not match:
         type u = t -> {v : unit | false}
       is not included in
         type u : value mod logical
       The kind of the first is value non_float mod aliased immutable
         because it's a function type.
       But the kind of the first must be a subkind of value mod logical
         because of the definition of u at line 2, characters 2-28.
       The first is not logical: Bad.t is recursive but not [@@inductive].
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
  type u : value mod logical
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
           type u : value mod logical
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
         type u : value mod logical
         type t = Roll of u
         val app : u -> t -> {v : unit | false} @@ total
         val mk : (t -> {v : unit | false}) -> u @@ total
       end
     Type declarations do not match:
       type u = t -> {v : unit | false}
     is not included in
       type u : value mod logical
     The kind of the first is value non_float mod aliased immutable
       because it's a function type.
     But the kind of the first must be a subkind of value mod logical
       because of the definition of u at line 2, characters 2-28.
     The first is not logical: Bad.t is recursive but not [@@inductive].
|}]

(* 3. [include] of the path inside the sealed structure. *)
module N : sig
  type u : value mod logical
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
           type u : value mod logical
           type t = Roll of u
           val app : u -> t -> {v : unit | false} @@ total
           val mk : (t -> {v : unit | false}) -> u @@ total
         end
       Type declarations do not match:
         type u = t -> {v : unit | false}
       is not included in
         type u : value mod logical
       The kind of the first is value non_float mod aliased immutable
         because it's a function type.
       But the kind of the first must be a subkind of value mod logical
         because of the definition of u at line 2, characters 2-28.
       The first is not logical: Bad.t is recursive but not [@@inductive].
|}]

(* 4. Direct sealing of the same structure (the branch's own test): rejected. *)
module D : sig
  type u : value mod logical
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
         sig type u : value mod logical type t = Roll of u end
       Type declarations do not match:
         type u = t -> {v : unit | false}
       is not included in
         type u : value mod logical
       The kind of the first is value non_float mod aliased immutable
         because it's a function type.
       But the kind of the first must be a subkind of value mod logical
         because of the definition of u at line 2, characters 2-28.
       The first is not logical: t is recursive but not [@@inductive].
|}]

(* Recursive module signatures may not declare an abstract type logical: the
   signatures are typed under assumptions about each other, so the claim could
   justify itself through the recursion. *)
module rec A : sig
  type u : value mod logical
  type t = Roll of u
end = struct
  type u : value mod logical = A.t -> int
  type t = Roll of u
end;;
[%%expect{|
Line 2, characters 2-28:
2 |   type u : value mod logical
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Recursive module signatures cannot declare an abstract type whose kind is logical;
       drop "mod logical" from its kind.
|}]

(* An [@@inductive] type is logical only if its components are. A payload whose
   kind makes no claim is not known to be logical, so total code may not match
   [Roll]; with [value mod logical] it may (Roller_set below). The hybrid branch
   trusted [@@inductive] types outright here. *)
module Roller (Payload : sig type t end) = struct
  type t = Roll of (Payload.t -> int) [@@inductive]
  let (unroll @ total) = function Roll f -> f
end;;
[%%expect{|
Line 3, characters 34-40:
3 |   let (unroll @ total) = function Roll f -> f
                                      ^^^^^^
Error: The match on a value whose type is not logical (Payload.t is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 25-45
         which is expected to be "total".
|}]

module Roller_set (Payload : sig type t : value mod logical end) = struct
  type t = Roll of (Payload.t -> int) [@@inductive]
  let (unroll @ total) = function Roll f -> f
end;;
[%%expect{|
module Roller_set :
  functor (Payload : sig type t : value mod logical end) ->
    sig
      type t = Roll of (Payload.t -> int)
      [@@inductive]
      val unroll : t -> Payload.t -> int
    end
|}]
