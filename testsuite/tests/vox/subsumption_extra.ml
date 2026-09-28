(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Refinement subsumption cases beyond the design's suite
   (research/subsumption-design-20260927/IMPLEMENTATION.md). *)

(* A value inside a nested module: the declaration's refinement is proved
   from the implementation's. *)
module Outer : sig
  module N : sig val f : int -> {r : int | r >= 0} end
end = struct
  module N = struct
    let (f @ total) (x : int) : {r : int | r > 0} = if x > 0 then x else 1
  end
end;;
[%%expect{|
module Outer : sig module N : sig val f : int -> {r : int | r >= 0} end end
|}]

module Outer_bad : sig
  module N : sig val f : int -> {r : int | r > 0} end
end = struct
  module N = struct
    let (f @ total) (x : int) : {r : int | r >= 0} = if x > 0 then x else 0
  end
end;;
[%%expect{|
Line 5, characters 9-10:
5 |     let (f @ total) (x : int) : {r : int | r >= 0} = if x > 0 then x else 0
             ^
Error: The value "N.f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 2, characters 43-48:
2 |   module N : sig val f : int -> {r : int | r > 0} end
                                               ^^^^^
  The refinement is stated here.
|}]

(* A first-class module is proved with no facts (the verifier does not look
   into packs), which suffices when the value's type implies the
   declaration. *)
module type S = sig val f : (x : int) -> {r : int | r >= x} end
let packed =
  (module struct
    let f (y : int) : {s : int | s > y} =
      if y < max_int then y + 1 else failwith "top"
  end : S);;
[%%expect{|
module type S = sig val f : (x : int) -> {r : int | r >= x} end
val packed : (module S) = <module>
|}]

let packed_bad =
  (module struct
    let f (y : int) : {s : int | s >= 0} = if y >= 0 then y else 0
  end : S);;
[%%expect{|
Line 3, characters 8-9:
3 |     let f (y : int) : {s : int | s >= 0} = if y >= 0 then y else 0
            ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: r = 0, x = 1)
Line 1, characters 52-58:
1 | module type S = sig val f : (x : int) -> {r : int | r >= x} end
                                                        ^^^^^^
  The refinement is stated here.
|}]

(* Recursive modules keep comparing refinements syntactically. *)
module rec Rec : sig val f : int -> {r : int | r >= 0} end = struct
  let f (x : int) : {r : int | r > 0} = if x > 0 then x else 1
end;;
[%%expect{|
Lines 1-3, characters 61-3:
1 | .............................................................struct
2 |   let f (x : int) : {r : int | r > 0} = if x > 0 then x else 1
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> {r : int | r > 0} end
       is not included in
         sig val f : int -> {r : int | r >= 0} end
       Values do not match:
         val f : int -> {r : int | r > 0}
       is not included in
         val f : int -> {r : int | r >= 0}
       The type "int -> {r : int | r > 0}" is not compatible with the type
         "int -> {r : int | r >= 0}"
       Type "{r : int | r > 0}" is not compatible with type "{r : int | r >= 0}"
       Refinements differ here, and this check cannot generate a proof obligation.
|}]

(* Refinements inside polymorphic variants and objects are compared
   syntactically. *)
module Variant : sig val v : [ `A of {x : int | x >= 0} ] end = struct
  let v : [ `A of {x : int | x > 0} ] = `A 1
end;;
[%%expect{|
Lines 1-3, characters 64-3:
1 | ................................................................struct
2 |   let v : [ `A of {x : int | x > 0} ] = `A 1
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val v : [ `A of {x : int | x > 0} ] end
       is not included in
         sig val v : [ `A of {x : int | x >= 0} ] end
       Values do not match:
         val v : [ `A of {x : int | x > 0} ]
       is not included in
         val v : [ `A of {x : int | x >= 0} ]
       The type "[ `A of {x : int | x > 0} ]" is not compatible with the type
         "[ `A of {x : int | x >= 0} ]"
       Type "{x : int | x > 0}" is not compatible with type "{x : int | x >= 0}"
       Refinements in this invariant position must be equal.
|}]

(* (e :> T) with a refinement added at the top: proved for the value, in
   the state of the coercion. *)
let three = (3 :> {x : int | x > 0});;
[%%expect{|
val three : int = 3
|}, Principal{|
val three : {x : int | x > 0} = 3
|}]

let zero = (0 :> {x : int | x > 0});;
[%%expect{|
Line 1, characters 11-35:
1 | let zero = (0 :> {x : int | x > 0});;
               ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 28-33:
1 | let zero = (0 :> {x : int | x > 0});;
                                ^^^^^
  The refinement is stated here.
|}]

let positive (y : int) = if y > 0 then (y :> {x : int | x > 0}) else 1;;
[%%expect{|
val positive : int -> int = <fun>
|}]

let unchecked (y : int) = (y :> {x : int | x > 0});;
[%%expect{|
Line 1, characters 26-50:
1 | let unchecked (y : int) = (y :> {x : int | x > 0});;
                              ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: y = 0)
Line 1, characters 43-48:
1 | let unchecked (y : int) = (y :> {x : int | x > 0});;
                                               ^^^^^
  The refinement is stated here.
|}]

(* The parameter of a callback parameter is a covariant position: the
   implementation may call the callback with any int, so a declaration whose
   callbacks accept only positive ints is not implemented.  Refining it the
   other way round is fine. *)
module Callback_arg : sig
  val apply : ({x : int | x > 0} -> int) -> int
end = struct
  let apply (k : int -> int) = k 1
end;;
[%%expect{|
Line 4, characters 6-11:
4 |   let apply (k : int -> int) = k 1
          ^^^^^
Error: The value "apply" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 2, characters 26-31:
2 |   val apply : ({x : int | x > 0} -> int) -> int
                              ^^^^^
  The refinement is stated here.
|}]

module Callback_result : sig
  val apply : (int -> {r : int | r > 0}) -> int
end = struct
  let apply (k : int -> int) = k 1
end;;
[%%expect{|
module Callback_result :
  sig val apply : (int -> {r : int | r > 0}) -> int end
|}]

(* A value of a nested module of a module given by its path: the obligation
   is about A.N.x (0), not A.x (1). *)
module A = struct
  let x = 1
  module N = struct let x = 0 end
end
module B = (A : sig module N : sig val x : {n : int | n > 0} end end);;
[%%expect{|
module A : sig val x : int module N : sig val x : int end end
Line 5, characters 12-13:
5 | module B = (A : sig module N : sig val x : {n : int | n > 0} end end);;
                ^
Error: The value "A.N.x" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 5, characters 54-59:
5 | module B = (A : sig module N : sig val x : {n : int | n > 0} end end);;
                                                          ^^^^^
  The refinement is stated here.
|}]

(* A covariant type parameter need not be a component of the value (in
   [type +'a producer = Producer of (cell -> 'a)] it is the result of a
   function inside it), so the value's mode says nothing about the modes of
   the parameter's values.  They are taken as unknown, and a closure under a
   type constructor cannot enter a refined type, even when it is total. *)
module Closure_list_total : sig
  val fs : {f : int -> int | true} list
end = struct
  let (id @ total) (x : int) = x
  let (fs @ total) = [ id ]
end;;
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let (id @ total) (x : int) = x
5 |   let (fs @ total) = [ id ]
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val id : int -> int val fs : (int -> int) list end
       is not included in
         sig val fs : {f : int -> int | true} list end
       Values do not match:
         val fs : (int -> int) list
       is not included in
         val fs : {f : int -> int | true} list
       The type "(int -> int) list" is not compatible with the type
         "{f : int -> int | true} list"
       Type "int -> int" is not compatible with type "{f : int -> int | true}"
       The refined type "{f : int -> int | true}" requires values that are
       "total", "stateless" and "portable", but at this position they may be
       "partial".
|}]

(* Tuple components are components of the value, and inherit its mode. *)
module Closure_pair : sig
  val p : {f : int -> int | true} * int
end = struct
  let (id @ total) (x : int) = x
  let (p @ total) = (id, 0)
end;;
[%%expect{|
module Closure_pair : sig val p : {f : int -> int | true} * int end
|}]

(* Predicate types at a universal variable are compared with the other side
   unless that side is a variable too. *)
module type Q = sig
  type ('a : immutable_data) t : immutable_data
  val empty : ('a : immutable_data). 'a t @@ total immutable
  val is_empty : 'a t -> bool @@ total
  val law_int : unit -> {u : unit | is_empty (empty : int t)} @@ total
end
module Generalize_val (Q : Q) : sig
  val law : ('a : immutable_data). unit -> {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
end = struct
  let law = Q.law_int
end;;
[%%expect{|
module type Q =
  sig
    type ('a : immutable_data) t : immutable_data
    val empty : ('a : immutable_data). 'a t @@ total immutable
    val is_empty : ('a : immutable_data). 'a t -> bool @@ total
    val law_int : unit -> {u : unit | is_empty (empty : int t)} @@ total
  end
Line 10, characters 6-9:
10 |   let law = Q.law_int
           ^^^
Error: The value "law" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 8, characters 55-84:
8 |   val law : ('a : immutable_data). unit -> {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Generalize_let (Q : Q) = struct
  let law : ('a : immutable_data). unit -> {u : unit | Q.is_empty (Q.empty : 'a Q.t)} =
    Q.law_int
end;;
[%%expect{|
Line 3, characters 4-13:
3 |     Q.law_int
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 55-84:
2 |   let law : ('a : immutable_data). unit -> {u : unit | Q.is_empty (Q.empty : 'a Q.t)} =
                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* A signature can carry the idents of another module ([module type of]):
   the obligation for N.x is about M's x (0), found by its path, not about
   Original's x (1), which has the same ident. *)
module Original = struct let x = 1 end
module type Of_original = module type of Original
module M : Of_original = struct let x = 0 end
module N = (M : sig val x : {x : int | x > 0} end);;
[%%expect{|
module Original : sig val x : int end
module type Of_original = sig val x : int @@ total end
module M : Of_original
Line 4, characters 12-13:
4 | module N = (M : sig val x : {x : int | x > 0} end);;
                ^
Error: The value "M.x" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 4, characters 39-44:
4 | module N = (M : sig val x : {x : int | x > 0} end);;
                                           ^^^^^
  The refinement is stated here.
|}]

(* A termination check runs the verifier over the body while typing; the
   local module's obligation is still proved by the verification of the
   phrase. *)
let rec countdown (n : int) : int =
  let module L : sig val x : {x : int | x > 0} end = struct let x = 0 end in
  if n > 0 then countdown (n - 1) else L.x
[@@decreases n];;
[%%expect{|
Line 2, characters 64-65:
2 |   let module L : sig val x : {x : int | x > 0} end = struct let x = 0 end in
                                                                    ^
Error: The value "x" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 2, characters 40-45:
2 |   let module L : sig val x : {x : int | x > 0} end = struct let x = 0 end in
                                            ^^^^^
  The refinement is stated here.
|}]

(* Distinct universal variables of a polymorphic method stay distinct in
   predicates: the law about 'a is not a law about 'b. *)
module type Q' = sig
  type 'a t : immutable_data
  val empty : 'a t @@ total immutable
  val is_empty : 'a t -> bool @@ total
end
module Method_univars (Q : Q') = struct
  type s = < law : 'a 'b. 'a Q.t -> 'b Q.t ->
               {u : unit | Q.is_empty (Q.empty : 'a Q.t)} ->
               {u : unit | Q.is_empty (Q.empty : 'a Q.t)} >
  type t = < law : 'a 'b. 'a Q.t -> 'b Q.t ->
               {u : unit | Q.is_empty (Q.empty : 'a Q.t)} ->
               {u : unit | Q.is_empty (Q.empty : 'b Q.t)} >
  let cast (x : s) : t = x
end;;
[%%expect{|
module type Q' =
  sig
    type 'a t : immutable_data
    val empty : 'a t @@ total immutable
    val is_empty : 'a t -> bool @@ total
  end
Line 13, characters 25-26:
13 |   let cast (x : s) : t = x
                              ^
Error: The value "x" has type
         "s" =
           "< law : 'a 'b.
                     'a Q.t ->
                     'b Q.t ->
                     {u : unit | Q.is_empty (Q.empty : 'a Q.t)} ->
                     {u : unit | Q.is_empty (Q.empty : 'a Q.t)} >"
       but an expression was expected of type
         "t" =
           "< law : 'a 'b.
                     'a Q.t ->
                     'b Q.t ->
                     {u : unit | Q.is_empty (Q.empty : 'a Q.t)} ->
                     {u : unit | Q.is_empty (Q.empty : 'b Q.t)} >"
       The method "law" has type
       "'a 'b.
         'a Q.t ->
         'b Q.t ->
         {u : unit | Q.is_empty (Q.empty : 'a Q.t)} ->
         {u : unit | Q.is_empty (Q.empty : 'a Q.t)}",
       but the expected method type was
       "'a 'b.
         'a Q.t ->
         'b Q.t ->
         {u : unit | Q.is_empty (Q.empty : 'a Q.t)} ->
         {u : unit | Q.is_empty (Q.empty : 'b Q.t)}"
|}]

(* The module an inclusion compares is the exported one, not a hidden module
   of the same name introduced by [open struct ... end]. *)
module Hidden : sig module N : sig val x : {x : int | x > 0} end end = struct
  module Good = struct let x = 1 end
  module N = struct let x = 0 end
  open struct module N = Good end
end;;
[%%expect{|
Line 3, characters 24-25:
3 |   module N = struct let x = 0 end
                            ^
Error: The value "N.x" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 1, characters 54-59:
1 | module Hidden : sig module N : sig val x : {x : int | x > 0} end end = struct
                                                          ^^^^^
  The refinement is stated here.
|}]

(* Declaration parameters are not renamed when two declarations are compared:
   a law about the first parameter is not a law about the second. *)
module Parameter_laws (Q : Q) = struct
  module M : sig
    type ('a, 'b) t = {v : 'a * 'b | Q.is_empty (Q.empty : 'b Q.t)}
  end = struct
    type ('a, 'b) t = {v : 'a * 'b | Q.is_empty (Q.empty : 'a Q.t)}
  end
end;;
[%%expect{|
Lines 4-6, characters 8-5:
4 | ........struct
5 |     type ('a, 'b) t = {v : 'a * 'b | Q.is_empty (Q.empty : 'a Q.t)}
6 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immutable_data, 'b) t =
               {v : 'a * 'b | Q.is_empty (Q.empty : 'a Q.t)}
         end
       is not included in
         sig
           type ('a, 'b : immutable_data) t =
               {v : 'a * 'b | Q.is_empty (Q.empty : 'b Q.t)}
         end
       Type declarations do not match:
         type ('a : immutable_data, 'b) t =
             {v : 'a * 'b | Q.is_empty (Q.empty : 'a Q.t)}
       is not included in
         type ('a, 'b : immutable_data) t =
             {v : 'a * 'b | Q.is_empty (Q.empty : 'b Q.t)}
       The type "{v : 'a * 'b | Q.is_empty (Q.empty : 'a Q.t)}"
       is not equal to the type "{v : 'a * 'b | Q.is_empty (Q.empty : 'b Q.t)}"
       Type "'a" is not equal to type "'b"
|}]

(* A universal variable in scope is related to an ordinary type variable as
   usual. *)
module Univar_param (Q : Q') = struct
  type 'b s = < law : 'a. 'a Q.t ->
                  {u : unit | Q.is_empty (Q.empty : 'b Q.t)} ->
                  {u : unit | Q.is_empty (Q.empty : 'b Q.t)} >
  type 'b t = < law : 'a. 'a Q.t ->
                  {u : unit | Q.is_empty (Q.empty : 'b Q.t)} ->
                  {u : unit | Q.is_empty (Q.empty : 'a Q.t)} >
  let cast (x : 'b s) : 'b t = x
end;;
[%%expect{|
Line 8, characters 31-32:
8 |   let cast (x : 'b s) : 'b t = x
                                   ^
Error: The value "x" has type "'b s" but an expression was expected of type "'b t"
       The method "law" has type
       "'a.
         'a Q.t ->
         {u : unit | Q.is_empty (Q.empty : 'b Q.t)} ->
         {u : unit | Q.is_empty (Q.empty : 'b Q.t)}",
       but the expected method type was
       "'a.
         'a Q.t ->
         {u : unit | Q.is_empty (Q.empty : 'b Q.t)} ->
         {u : unit | Q.is_empty (Q.empty : 'a Q.t)}"
       The universal variable "'a" would escape its scope
|}]

(* A type parameter constrained to a compound type: the variables of the
   constraint are parameters too, and are not renamed. *)
module Constrained_parameter (Q : Q) = struct
  module M : sig
    type 'p t = {u : unit | Q.is_empty (Q.empty : 'b Q.t)}
      constraint 'p = ('a : immutable_data) * ('b : immutable_data)
  end = struct
    type 'p t = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
      constraint 'p = ('a : immutable_data) * ('b : immutable_data)
  end
end;;
[%%expect{|
Lines 5-8, characters 8-5:
5 | ........struct
6 |     type 'p t = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
7 |       constraint 'p = ('a : immutable_data) * ('b : immutable_data)
8 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type 'c t = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
             constraint 'c = 'a * 'b
         end
       is not included in
         sig
           type 'c t = {u : unit | Q.is_empty (Q.empty : 'b Q.t)}
             constraint 'c = 'a * 'b
         end
       Type declarations do not match:
         type 'c t = {u : unit | Q.is_empty (Q.empty : 'a Q.t)}
           constraint 'c = 'a * 'b
       is not included in
         type 'c t = {u : unit | Q.is_empty (Q.empty : 'b Q.t)}
           constraint 'c = 'a * 'b
       The type "{u : unit | Q.is_empty (Q.empty : 'a Q.t)}"
       is not equal to the type "{u : unit | Q.is_empty (Q.empty : 'b Q.t)}"
       Type "'a" is not equal to type "'b"
|}]

(* A polymorphic parameter, equal on both sides. *)
module Polymorphic_parameter : sig
  val f : ('a. 'a -> 'a) -> {r : int | r >= 0}
end = struct
  let (f @ total) (_id : 'a. 'a -> 'a) : {r : int | r > 0} = 1
end;;
[%%expect{|
module Polymorphic_parameter :
  sig val f : ('a. 'a -> 'a) -> {r : int | r >= 0} end
|}]

(* The caller's parameter refinement holds of the argument even when both
   sides state it. *)
module Same_parameter (X : sig
  val f : {x : int | x < 100} -> {r : int | r > 100}
end) : sig
  val f : (x : {x : int | x < 100}) -> {r : int | r > x}
end = X;;
[%%expect{|
module Same_parameter :
  functor (X : sig val f : {x : int | x < 100} -> {r : int | r > 100} end) ->
    sig val f : (x : {x : int | x < 100}) -> {r : int | r > x} end
|}]

(* A constraint on a module path (a module alias) is proved with the facts of
   that module. *)
module Wrap = struct
  module A2 = struct
    let x = 1
    module N = struct let x = 0 end
  end
  module Aliased =
    (A2 : sig module N : sig val x : {n : int | n >= 0} end end)
end;;
[%%expect{|
module Wrap :
  sig
    module A2 : sig val x : int module N : sig val x : int end end
    module Aliased : sig module N : sig val x : {n : int | n >= 0} end end
  end
|}]

(* Predicates are compared with the refinements the verifier assumes while
   evaluating them: of a [let refine_] binding, of a pattern, and of a
   function parameter.  Two predicates with the same syntax whose exposed
   refinements differ are different predicates.  Here the declaration's u is
   {v >= 0} and the implementation's {v > 0}: after the inclusion
   substitution the declared type w names the implementation's u, but its
   predicate's nodes keep the declaration's types, so [let refine_ y = u]
   assumes y >= 0 on one side and y > 0 on the other.  Type declarations
   compare predicates exactly, so this is rejected; with equal refinements
   it is accepted. *)
module Refine_exposed_same : sig
  val u : {v : int | v > 0}
  type w = {r : int | let refine_ y = u in r = y}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : int | let refine_ y = u in r = y}
end;;
[%%expect{|
module Refine_exposed_same :
  sig
    val u : {v : int | v > 0}
    type w = {r : int | let refine_ y = u in r = y}
  end
|}]

module Refine_exposed_different : sig
  val u : {v : int | v >= 0}
  type w = {r : int | let refine_ y = u in r = y}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : int | let refine_ y = u in r = y}
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   let u : {v : int | v > 0} = 1
6 |   type w = {r : int | let refine_ y = u in r = y}
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           val u : {v : int | v > 0}
           type w = {r : int | let refine_ y = u in r = y}
         end
       is not included in
         sig
           val u : {v : int | v >= 0}
           type w = {r : int | let refine_ y = u in r = y}
         end
       Type declarations do not match:
         type w = {r : int | let refine_ y = u in r = y}
       is not included in
         type w = {r : int | let refine_ y = u in r = y}
       The type "{r : int | let refine_ y = u in r = y}"
       is not equal to the type "{r : int | let refine_ y = u in r = y}"
       Type "{v : int | v > 0}" is not equal to type "{v : int | v >= 0}"
|}]

(* In [match u with y -> ...] and [(fun y -> ...) u], u is used at int, so
   the pattern and the parameter have type int on both sides.  The
   eliminations of u record its two declared types, but the verifier does
   not assume what an elimination records: it assumes the refinement of u's
   type where the predicate is evaluated.  So the two predicates are equal,
   and the [_different] declarations are accepted. *)
module Pattern_exposed_same : sig
  val u : {v : int | v > 0}
  type w = {r : bool | r = (match u with y -> y > 0)}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : bool | r = (match u with y -> y > 0)}
end;;
[%%expect{|
module Pattern_exposed_same :
  sig
    val u : {v : int | v > 0}
    type w = {r : bool | r = (match u with | y -> y > 0)}
  end
|}]

module Pattern_exposed_different : sig
  val u : {v : int | v >= 0}
  type w = {r : bool | r = (match u with y -> y > 0)}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : bool | r = (match u with y -> y > 0)}
end;;
[%%expect{|
module Pattern_exposed_different :
  sig
    val u : {v : int | v >= 0}
    type w = {r : bool | r = (match u with | y -> y > 0)}
  end
|}]

module Lambda_exposed_same : sig
  val u : {v : int | v > 0}
  type w = {r : bool | r = (fun y -> y > 0) u}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : bool | r = (fun y -> y > 0) u}
end;;
[%%expect{|
module Lambda_exposed_same :
  sig
    val u : {v : int | v > 0}
    type w = {r : bool | r = ((fun y -> y > 0) u)}
  end
|}]

module Lambda_exposed_different : sig
  val u : {v : int | v >= 0}
  type w = {r : bool | r = (fun y -> y > 0) u}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : bool | r = (fun y -> y > 0) u}
end;;
[%%expect{|
module Lambda_exposed_different :
  sig
    val u : {v : int | v >= 0}
    type w = {r : bool | r = ((fun y -> y > 0) u)}
  end
|}]

(* An elimination present in only one of two otherwise equal predicates.
   Predicates are compared up to eliminations; the verifier assumes the
   refinement of the eliminated subexpression's type where the predicate is
   evaluated, not the type the elimination records.  Verdicts only. *)

(* Accepted. *)
module Wrapper_declared_only : sig
  val u : {v : int | v > 0}
  type w = {r : int | r = u + 0}
end = struct
  let u = 5
  type w = {r : int | r = u + 0}
end;;
[%%expect{|
module Wrapper_declared_only :
  sig val u : {v : int | v > 0} type w = {r : int | r = (u + 0)} end
|}]

(* Rejected. *)
module Wrapper_declared_only_false : sig
  val u : {v : int | v > 0}
  type w = {r : int | r = u + 0}
end = struct
  let u = -5
  type w = {r : int | r = u + 0}
end;;
[%%expect{|
Line 5, characters 6-7:
5 |   let u = -5
          ^
Error: The value "u" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 2, characters 21-26:
2 |   val u : {v : int | v > 0}
                         ^^^^^
  The refinement is stated here.
|}]

(* Accepted. *)
module Wrapper_implementation_only : sig
  val u : int
  type w = {r : int | r = u + 0}
end = struct
  let u : {v : int | v > 0} = 5
  type w = {r : int | r = u + 0}
end;;
[%%expect{|
module Wrapper_implementation_only :
  sig val u : int type w = {r : int | r = (u + 0)} end
|}]

(* Accepted. *)
module Wrapper_target_parameter : sig
  val f : (x : {v : int | v > 0}) -> {r : int | r = x + 0}
end = struct
  let (f @ total) (x : int) : {r : int | r = x + 0} = x + 0
end;;
[%%expect{|
module Wrapper_target_parameter :
  sig val f : (x : {v : int | v > 0}) -> {r : int | r = (x + 0)} end
|}]

(* Rejected. *)
module Wrapper_source_parameter : sig
  val f : (x : int) -> {r : int | r = x + 0}
end = struct
  let (f @ total) (x : {v : int | v > 0}) : {r : int | r = x + 0} = x + 0
end;;
[%%expect{|
Line 4, characters 7-8:
4 |   let (f @ total) (x : {v : int | v > 0}) : {r : int | r = x + 0} = x + 0
           ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 4, characters 34-39:
4 |   let (f @ total) (x : {v : int | v > 0}) : {r : int | r = x + 0} = x + 0
                                      ^^^^^
  The refinement is stated here.
|}]

(* Accepted. *)
module Wrapper_source_payload : sig
  val x : {v : int | v + 0 = 3}
end = struct
  let x : {v : {w : int | w > 0} | v + 0 = 3} = 3
end;;
[%%expect{|
module Wrapper_source_payload : sig val x : {v : int | (v + 0) = 3} end
|}]

(* Rejected. *)
module Wrapper_target_payload (A : sig val x : {v : int | v + 0 = -3} end)
  : sig val x : {v : {w : int | w > 0} | v + 0 = -3} end = A;;
[%%expect{|
Line 2, characters 59-60:
2 |   : sig val x : {v : {w : int | w > 0} | v + 0 = -3} end = A;;
                                                               ^
Error: The value "A.x" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 2, characters 32-37:
2 |   : sig val x : {v : {w : int | w > 0} | v + 0 = -3} end = A;;
                                    ^^^^^
  The refinement is stated here.
|}]

(* Rejected. *)
type _ wrapper_witness =
  | Wrapper_positive : {v : int | v > 0} wrapper_witness
  | Wrapper_any : int wrapper_witness

let (wrapper_branches @ total) (type a) (w : a wrapper_witness) (y : a)
    : {z : int | z > 0} =
  let r =
    match w with
    | Wrapper_positive -> (y + 0 : {r : int | r = y + 0})
    | Wrapper_any -> (y + 0 : {r : int | r = y + 0})
  in
  r;;
[%%expect{|
type _ wrapper_witness =
    Wrapper_positive : {v : int | v > 0} wrapper_witness
  | Wrapper_any : int wrapper_witness
Line 12, characters 2-3:
12 |   r;;
       ^
Error: Refinement could not be proved (counterexample: y = 0)
Line 6, characters 17-22:
6 |     : {z : int | z > 0} =
                     ^^^^^
  The refinement is stated here.
|}]

(* Since predicates are compared up to eliminations, what the verifier
   assumes of an eliminated subexpression comes from where the predicate is
   evaluated: the declared type of a value, the type of an applied function,
   or a record declaration.  Verdicts only. *)

(* Accepted: both declarations of w are equal up to eliminations, and a
   client assumes u >= 0, from the declared type of u. *)
module Recorded_stronger : sig
  val u : {v : int | v >= 0}
  type w = {r : int | r = u + 0}
end = struct
  let u : {v : int | v > 0} = 1
  type w = {r : int | r = u + 0}
end

let (recorded_stronger @ total) (x : Recorded_stronger.w)
    : {z : int | z >= 0} = x;;
[%%expect{|
module Recorded_stronger :
  sig val u : {v : int | v >= 0} type w = {r : int | r = (u + 0)} end
val recorded_stronger : Recorded_stronger.w -> {z : int | z >= 0} = <fun>
|}]

(* Rejected: the implementation's elimination recorded u > 0, but the
   client only knows u >= 0. *)
let (recorded_stronger_false @ total) (x : Recorded_stronger.w)
    : {z : int | z > 0} = x;;
[%%expect{|
Line 2, characters 26-27:
2 |     : {z : int | z > 0} = x;;
                              ^
Error: Refinement could not be proved (counterexample: x = 0)
Line 2, characters 17-22:
2 |     : {z : int | z > 0} = x;;
                     ^^^^^
  The refinement is stated here.
|}]

(* Accepted: the refinement of an eliminated application comes from the
   type of the function applied. *)
let (successor @ total) (x : {v : int | 0 <= v && v < 100})
    : {r : int | r > x} = x + 1

let (successor_known @ total) (x : {v : int | 0 <= v && v < 100})
    : {u : unit | successor x + 0 > x} = ();;
[%%expect{|
val successor : (x : {v : int | (0 <= v) && (v < 100)}) -> {r : int | r > x} =
  <fun>
val successor_known :
  (x : {v : int | (0 <= v) && (v < 100)}) ->
  {u : unit | ((successor x) + 0) > x} = <fun>
|}]

(* Rejected. *)
let (successor_known_false @ total) (x : {v : int | 0 <= v && v < 100})
    : {u : unit | successor x + 0 > x + 1} = ();;
[%%expect{|
Line 2, characters 45-47:
2 |     : {u : unit | successor x + 0 > x + 1} = ();;
                                                 ^^
Error: Refinement could not be proved (counterexample: x = 0)
Line 2, characters 18-41:
2 |     : {u : unit | successor x + 0 > x + 1} = ();;
                      ^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Accepted: the refinement of an eliminated field comes from the record
   declaration. *)
type limb_pair = {lo : {n : int | 0 <= n && n < 256}; hi : int}

let (limb_known @ total) (p : limb_pair) : {u : unit | p.lo + 0 < 256} = ();;
[%%expect{|
type limb_pair = { lo : {n : int | (0 <= n) && (n < 256)}; hi : int; }
val limb_known : (p : limb_pair) -> {u : unit | (p.lo + 0) < 256} = <fun>
|}]

(* Rejected. *)
let (limb_known_false @ total) (p : limb_pair)
    : {u : unit | p.hi + 0 < 256} = ();;
[%%expect{|
Line 2, characters 36-38:
2 |     : {u : unit | p.hi + 0 < 256} = ();;
                                        ^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 18-32:
2 |     : {u : unit | p.hi + 0 < 256} = ();;
                      ^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Accepted: the record's type, and so the field's, comes from the declared
   type of s. *)
type 'a box = {contents : 'a}

let (box_known @ total) (s : {v : int | v > 0} box)
    : {u : unit | s.contents + 0 > 0} = ();;
[%%expect{|
type 'a box = { contents : 'a; }
val box_known :
  (s : {v : int | v > 0} box) -> {u : unit | (s.contents + 0) > 0} = <fun>
|}]

(* Rejected. *)
let (box_known_false @ total) (s : int box)
    : {u : unit | s.contents + 0 > 0} = ();;
[%%expect{|
Line 2, characters 40-42:
2 |     : {u : unit | s.contents + 0 > 0} = ();;
                                            ^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 18-36:
2 |     : {u : unit | s.contents + 0 > 0} = ();;
                      ^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Accepted: the argument 5 replaces the parameter y, whose elimination in
   the result type stays around it.  The declared result type has no
   elimination. *)
let (positive_id @ total) (y : {v : int | v > 0}) : {r : int | r = y + 0} =
  y + 0

let (positive_five @ total) (u : unit) : {r : int | r = 5 + 0} =
  positive_id 5;;
[%%expect{|
val positive_id : (y : {v : int | v > 0}) -> {r : int | r = (y + 0)} = <fun>
val positive_five : unit -> {r : int | r = (5 + 0)} = <fun>
|}]

(* Rejected. *)
let (positive_five_false @ total) (u : unit) : {r : int | r = 6 + 0} =
  positive_id 5;;
[%%expect{|
Line 2, characters 2-15:
2 |   positive_id 5;;
      ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 58-67:
1 | let (positive_five_false @ total) (u : unit) : {r : int | r = 6 + 0} =
                                                              ^^^^^^^^^
  The refinement is stated here.
|}]

(* A type variable on one side, instantiated on the other with a type that
   has a refinement inside a constructor: xs is eliminated in the
   declaration's predicate only. *)
let[@def] (is_nil @ total) (xs : 'a list @ immutable) : bool =
  match xs with [] -> true | _ :: _ -> false

(* Accepted. *)
module Variable_instance : sig
  val f :
    (xs : {l : {v : int | v > 0} list | not (l === [])}) @ immutable ->
    {r : bool | r = is_nil xs}
end = struct
  let (f @ total) (xs : 'a list @ immutable) : {r : bool | r = is_nil xs} =
    is_nil xs
end;;
[%%expect{|
val is_nil : 'a list @ immutable -> bool = <fun>
val is_nil_def :
  (xs : 'a list) @ immutable ->
  {u : unit
    | (is_nil xs) === (match xs with | [] -> true | _::_ -> false : bool)} =
  <fun>
module Variable_instance :
  sig
    val f :
      (xs : {l : {v : int | v > 0} list | not (l === [])}) @ immutable ->
      {r : bool | r = (is_nil xs)}
  end
|}]

(* Rejected. *)
module Variable_instance_false : sig
  val f :
    (xs : {l : {v : int | v > 0} list | not (l === [])}) @ immutable ->
    {r : bool | r = is_nil xs && r}
end = struct
  let (f @ total) (xs : 'a list @ immutable) : {r : bool | r = is_nil xs} =
    is_nil xs
end;;
[%%expect{|
Line 6, characters 7-8:
6 |   let (f @ total) (xs : 'a list @ immutable) : {r : bool | r = is_nil xs} =
           ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample)
Line 4, characters 33-34:
4 |     {r : bool | r = is_nil xs && r}
                                     ^
  The refinement is stated here.
|}]

(* Accepted: the implementation's x : 'a is x : {v > 0} in the
   declaration, whose predicate eliminates it. *)
module Variable_refined : sig
  val g : (x : {v : int | v > 0}) -> {r : int | r === x}
end = struct
  let (g @ total) (x : 'a) : {r : 'a | r === x} = x
end;;
[%%expect{|
module Variable_refined :
  sig val g : (x : {v : int | v > 0}) -> {r : int | r === x} end
|}]

(* Rejected. *)
module Variable_refined_false : sig
  val g : (x : int) -> {r : {v : int | v > 0} | r === x}
end = struct
  let (g @ total) (x : 'a) : {r : 'a | r === x} = x
end;;
[%%expect{|
Line 4, characters 7-8:
4 |   let (g @ total) (x : 'a) : {r : 'a | r === x} = x
           ^
Error: The value "g" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 2, characters 39-44:
2 |   val g : (x : int) -> {r : {v : int | v > 0} | r === x}
                                           ^^^^^
  The refinement is stated here.
|}]

(* Rejected: in [at_count]'s parameter type for i, s is eliminated, and the
   elimination records s.count > 5.  The caller's type for a is equal to it
   up to that elimination, but there s has type counter, and nothing checks
   s.count > 5.  (Accepted while the verifier assumed what eliminations
   record: it assumed s.count > 5 when it used a.) *)
type counter = {count : int}

let (at_count @ total) (s : {s : counter | s.count > 5})
    (i : {i : int | i = s.count}) : int = i

let (unchecked_call @ total) (s : counter) (a : {i : int | i = s.count})
    : int =
  at_count s a;;
[%%expect{|
type counter = { count : int; }
val at_count :
  (s : {s : counter | s.count > 5}) -> {i : int | i = s.count} -> int = <fun>
Line 8, characters 11-12:
8 |   at_count s a;;
               ^
Error: Refinement could not be proved (counterexample)
Line 3, characters 43-54:
3 | let (at_count @ total) (s : {s : counter | s.count > 5})
                                               ^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Rejected, for the same reason. *)
let (unchecked_premise @ total) (s : counter) (a : {i : int | i = s.count})
    : {u : unit | s.count > 5} =
  let _ = at_count s a in ();;
[%%expect{|
Line 3, characters 19-20:
3 |   let _ = at_count s a in ();;
                       ^
Error: Refinement could not be proved (counterexample)
Line 3, characters 43-54:
3 | let (at_count @ total) (s : {s : counter | s.count > 5})
                                               ^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Accepted. *)
let (checked_call @ total) (s : {s : counter | s.count > 5})
    (a : {i : int | i = s.count}) : int =
  at_count s a;;
[%%expect{|
val checked_call :
  (s : {s : counter | s.count > 5}) -> {i : int | i = s.count} -> int = <fun>
|}]
