(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Error-message quality for refinement subsumption (DESIGN.md 3.3.4 names,
   3.3.6 errors).  The wording of the new messages is part of the
   specification; the order of the values in a counterexample is the
   solver's.  Each block is the output after stages 1-4; comments say what
   trunk does. *)

(* [stage 3] Counterexamples use the declaration's names (x, r), not the
   implementation's (y, s).  f is partial, so its result is a fresh symbol
   named after the declaration's result binder.
   Currently: rejected at typing, 'Type "{s : int | s >= x}" is not compatible
   with type "{r : int | r > x}"'. *)
module Names : sig val f : (x : int) -> {r : int | r > x} end = struct
  let f (y : int) : {s : int | s >= y} = if y = 0 then failwith "zero" else y
end;;
[%%expect{|
Line 2, characters 6-7:
2 |   let f (y : int) : {s : int | s >= y} = if y = 0 then failwith "zero" else y
          ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: r = 0, x = 0)
Line 1, characters 51-56:
1 | module Names : sig val f : (x : int) -> {r : int | r > x} end = struct
                                                       ^^^^^
  The refinement is stated here.
|}]

(* [stage 3] A failing precondition: the goal is the implementation's
   predicate (m > 0), so that is where "stated here" points; the argument is
   named after the declaration's refinement binder n (neither arrow has a
   binder).
   Currently: rejected at typing, 'Type "{m : int | m > 0}" is not compatible
   with type "{n : int | n >= 0}"'. *)
module Precondition : sig val g : {n : int | n >= 0} -> int end = struct
  let g (m : {m : int | m > 0}) = m
end;;
[%%expect{|
Line 2, characters 6-7:
2 |   let g (m : {m : int | m > 0}) = m
          ^
Error: The value "g" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: n = 0)
Line 2, characters 24-29:
2 |   let g (m : {m : int | m > 0}) = m
                            ^^^^^
  The refinement is stated here.
|}]

(* [stage 3] Two failing values: the site's batch fails, each obligation is
   retried alone (vox_vc.ml verify_batch), and the first failure in
   declaration order is reported.
   Currently: rejected at typing, reporting f only ('Type "{r : int | r >= x}"
   is not compatible with type "{r : int | r > x}"'). *)
module Two : sig
  val f : (x : int) -> {r : int | r > x}
  val g : (x : int) -> {r : int | r > x + 1}
end = struct
  let (f @ total) (x : int) : {r : int | r >= x} = x
  let (g @ total) (x : int) : {r : int | r >= x} = x
end;;
[%%expect{|
Line 5, characters 7-8:
5 |   let (f @ total) (x : int) : {r : int | r >= x} = x
           ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 2, characters 34-39:
2 |   val f : (x : int) -> {r : int | r > x}
                                      ^^^^^
  The refinement is stated here.
|}]

(* [stage 3] A functor applied to a module path: the value is not inside the
   application, so the primary location is the application and the headline
   names the value.  The expect tool verifies each module of this phrase
   separately, so Weak.f is not known to be total here and its result is a
   fresh symbol r (in a compilation unit, r is the reflected f x).
   Currently: rejected at typing, 'The type "int -> {s : int | s >= 0}" is not
   compatible with the type "(x : int) -> {r : int | r >= x}"'. *)
module type S = sig val f : (x : int) -> {r : int | r >= x} end
module F (X : S) = struct let g = X.f 3 end
module Weak = struct
  let (f @ total) (y : int) : {s : int | s >= 0} = if y >= 0 then y else 0
end
module Bad = F (Weak);;
[%%expect{|
module type S = sig val f : (x : int) -> {r : int | r >= x} end
module F : functor (X : S) -> sig val g : int end
module Weak : sig val f : int -> {s : int | s >= 0} end
Line 6, characters 13-21:
6 | module Bad = F (Weak);;
                 ^^^^^^^^
Error: The value "Weak.f" does not satisfy the functor's parameter.
       Refinement could not be proved (counterexample: r = 0, x = 1)
Line 1, characters 52-58:
1 | module type S = sig val f : (x : int) -> {r : int | r >= x} end
                                                        ^^^^^^
  The refinement is stated here.
|}]

(* [stage 1] Module type equality stays syntactic (Directionality.in_eq);
   the new last line says why no proof was attempted.
   Currently: the same message without the last line. *)
module type S1 = sig val f : int -> {r : int | r > 0} end
module Z : sig module type S = S1 end = struct
  module type S = sig val f : int -> {r : int | r >= 1} end
end;;
[%%expect{|
module type S1 = sig val f : int -> {r : int | r > 0} end
Lines 2-4, characters 40-3:
2 | ........................................struct
3 |   module type S = sig val f : int -> {r : int | r >= 1} end
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig val f : int -> {r : int | r >= 1} end end
       is not included in
         sig module type S = S1 end
       Module type declarations do not match:
         module type S = sig val f : int -> {r : int | r >= 1} end
       does not match
         module type S = S1
       At position "module type S = <here>"
       Module types do not match:
         sig val f : int -> {r : int | r >= 1} end
       is not equal to
         S1
       At position "module type S = <here>"
       Values do not match:
         val f : int -> {r : int | r >= 1}
       is not included in
         val f : int -> {r : int | r > 0}
       The type "int -> {r : int | r >= 1}" is not compatible with the type
         "int -> {r : int | r > 0}"
       Type "{r : int | r >= 1}" is not compatible with type "{r : int | r > 0}"
       Refinements differ here, and this check cannot generate a proof obligation.
|}]

(* [stage 1] The invariance line names the invariant constructor (ref),
   even though the refinement sits under a covariant one (box).
   Currently: the same message without the last line. *)
type ('a : immutable_data) box = { v : 'a }
module Hint : sig val c : {x : int | x >= 0} box ref end = struct
  let c : {x : int | x > 0} box ref = { contents = { v = 1 } }
end;;
[%%expect{|
type ('a : immutable_data) box = { v : 'a; }
Lines 2-4, characters 59-3:
2 | ...........................................................struct
3 |   let c : {x : int | x > 0} box ref = { contents = { v = 1 } }
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val c : {x : int | x > 0} box ref end
       is not included in
         sig val c : {x : int | x >= 0} box ref end
       Values do not match:
         val c : {x : int | x > 0} box ref
       is not included in
         val c : {x : int | x >= 0} box ref
       The type "{x : int | x > 0} box ref" is not compatible with the type
         "{x : int | x >= 0} box ref"
       Type "{x : int | x > 0}" is not compatible with type "{x : int | x >= 0}"
       Refinements under the invariant type constructor "ref" must be equal.
|}]
