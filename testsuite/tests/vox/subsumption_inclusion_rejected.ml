(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Inclusions that must be rejected after stages 1-4 (DESIGN.md 2, 3.3.6).
   Semantic failures are reported by the verifier at the implementation's
   value, naming the declaration's site, with a counterexample in the
   interface's names and "The refinement is stated here." at the goal
   predicate.  Syntactic failures (invariant positions, payload mismatches)
   stay type errors, with a new explanation line for invariant positions.
   Counterexample values are predictions of the solver's smallest-model
   search (vox_verify.enabled.ml, smaller_model).  Each block is the intended
   output after stages 1-4; the comment says what trunk does today. *)

(* [stage 3] A weaker postcondition.  f is total, so its result is the
   reflected term f x and only x is shown.
   Currently: rejected at typing, 'Type "{r : int | r >= x}" is not compatible
   with type "{r : int | r > x}"'. *)
module Weaker_post : sig val f : (x : int) -> {r : int | r > x} end = struct
  let (f @ total) (x : int) : {r : int | r >= x} = x
end;;
[%%expect{|
Line 2, characters 7-8:
2 |   let (f @ total) (x : int) : {r : int | r >= x} = x
           ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 1, characters 57-62:
1 | module Weaker_post : sig val f : (x : int) -> {r : int | r > x} end = struct
                                                             ^^^^^
  The refinement is stated here.
|}]

(* [stage 3] The implementation's precondition is stronger than the
   declaration's: the goal is the implementation's own predicate, so "stated
   here" points into the structure.
   Currently: rejected at typing, 'Type "{x : int | x >= 0}" is not compatible
   with type "{x : int | x > 0}"'. *)
module Weaker_pre : sig val g : {x : int | x >= 0} -> int end = struct
  let g (x : {x : int | x > 0}) = x
end;;
[%%expect{|
Line 2, characters 6-7:
2 |   let g (x : {x : int | x > 0}) = x
          ^
Error: The value "g" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = 0)
Line 2, characters 24-29:
2 |   let g (x : {x : int | x > 0}) = x
                            ^^^^^
  The refinement is stated here.
|}]

(* [stage 1 + 3] A binder only on the implementation side.  Stage 1 compares
   the codomains unchanged instead of failing in align_arrow_codomains; the
   implementation's r >= x does not imply r >= 0.  The argument is named after
   the implementation's binder, since the declaration has none.
   Currently: rejected at typing, 'The type "(x : int) -> {r : int | r >= x}"
   is not compatible with the type "int -> {r : int | r >= 0}"'. *)
module Leaked_binder : sig val f : int -> {r : int | r >= 0} end = struct
  let (f @ total) (x : int) : {r : int | r >= x} = x
end;;
[%%expect{|
Line 2, characters 7-8:
2 |   let (f @ total) (x : int) : {r : int | r >= x} = x
           ^
Error: The value "f" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: x = -1)
Line 1, characters 53-59:
1 | module Leaked_binder : sig val f : int -> {r : int | r >= 0} end = struct
                                                         ^^^^^^
  The refinement is stated here.
|}]

(* [stage 3] Machine integers wrap: b = a + 1 does not imply b > a at
   a = max_int.  The same holds for the implication b > a + 1 => b > a
   (subsumption.md e18), whose counterexample leaves b unconstrained and is
   therefore not pinned here.
   Currently: rejected at typing, 'Type "{b : int | b = (a + 1)}" is not
   compatible with type "{b : int | b > a}"'. *)
module Wrapping : sig val next : (a : int) -> {b : int | b > a} end = struct
  let (next @ total) (a : int) : {b : int | b = a + 1} = a + 1
end;;
[%%expect{|
Line 2, characters 7-11:
2 |   let (next @ total) (a : int) : {b : int | b = a + 1} = a + 1
           ^^^^
Error: The value "next" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: a = 4611686018427387903)
Line 1, characters 57-62:
1 | module Wrapping : sig val next : (a : int) -> {b : int | b > a} end = struct
                                                             ^^^^^
  The refinement is stated here.
|}]

(* [stage 1] ref is invariant: refinements must be equal, even though
   both directions would be sound.  Stays a type error, with a new
   explanation line.
   Currently: rejected, same message without the last line. *)
module Invariant_ref : sig val r : {x : int | x >= 0} ref end = struct
  let r : {x : int | x > 0} ref = { contents = 1 }
end;;
[%%expect{|
Lines 1-3, characters 64-3:
1 | ................................................................struct
2 |   let r : {x : int | x > 0} ref = { contents = 1 }
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val r : {x : int | x > 0} ref end
       is not included in
         sig val r : {x : int | x >= 0} ref end
       Values do not match:
         val r : {x : int | x > 0} ref
       is not included in
         val r : {x : int | x >= 0} ref
       The type "{x : int | x > 0} ref" is not compatible with the type
         "{x : int | x >= 0} ref"
       Type "{x : int | x > 0}" is not compatible with type "{x : int | x >= 0}"
       Refinements under the invariant type constructor "ref" must be equal.
|}]

(* [stage 1] Dropping under array is not allowed either (invariant).
   Currently: rejected, same message without the last line. *)
module Invariant_array : sig val a : int array end = struct
  let a : {x : int | x > 0} array = [| 1; 2 |]
end;;
[%%expect{|
Lines 1-3, characters 53-3:
1 | .....................................................struct
2 |   let a : {x : int | x > 0} array = [| 1; 2 |]
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val a : {x : int | x > 0} array end
       is not included in
         sig val a : int array end
       Values do not match:
         val a : {x : int | x > 0} array
       is not included in
         val a : int array
       The type "{x : int | x > 0} array" is not compatible with the type
         "int array"
       Type "{x : int | x > 0}" is not compatible with type "int"
       Refinements under the invariant type constructor "array" must be equal.
|}]

(* [stage 1] An abstract type without a variance annotation is invariant.
   Currently: rejected, same message without the last line. *)
type 'a abs
module Invariant_abstract : sig val v : int abs end = struct
  let v : {x : int | x > 0} abs = Obj.magic 0
end;;
[%%expect{|
type 'a abs
Lines 2-4, characters 54-3:
2 | ......................................................struct
3 |   let v : {x : int | x > 0} abs = Obj.magic 0
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val v : {x : int | x > 0} abs end
       is not included in
         sig val v : int abs end
       Values do not match:
         val v : {x : int | x > 0} abs
       is not included in
         val v : int abs
       The type "{x : int | x > 0} abs" is not compatible with the type "int abs"
       Type "{x : int | x > 0}" is not compatible with type "int"
       Refinements under the invariant type constructor "abs" must be equal.
|}]

(* [stage 1] A GADT index is invariant, so adding a refinement to the
   parameter (which is otherwise free, contravariantly) is rejected.
   Currently: rejected, same message without the last line. *)
type _ g = Int : {x : int | x > 0} g
module Invariant_gadt : sig val w : int g -> unit end = struct
  let w (_ : {x : int | x > 0} g) = ()
end;;
[%%expect{|
type _ g = Int : {x : int | x > 0} g
Lines 2-4, characters 56-3:
2 | ........................................................struct
3 |   let w (_ : {x : int | x > 0} g) = ()
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val w : {x : int | x > 0} g -> unit end
       is not included in
         sig val w : int g -> unit end
       Values do not match:
         val w : {x : int | x > 0} g -> unit
       is not included in
         val w : int g -> unit
       The type "{x : int | x > 0} g -> unit" is not compatible with the type
         "int g -> unit"
       Type "{x : int | x > 0}" is not compatible with type "int"
       Refinements under the invariant type constructor "g" must be equal.
|}]

(* [stage 1] A mutable field makes its parameter invariant.
   Currently: rejected, same message without the last line. *)
type 'a cell = { mutable c : 'a }
module Invariant_mutable : sig val c : int cell end = struct
  let c : {x : int | x > 0} cell = { c = 1 }
end;;
[%%expect{|
type 'a cell = { mutable c : 'a; }
Lines 2-4, characters 54-3:
2 | ......................................................struct
3 |   let c : {x : int | x > 0} cell = { c = 1 }
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val c : {x : int | x > 0} cell end
       is not included in
         sig val c : int cell end
       Values do not match:
         val c : {x : int | x > 0} cell
       is not included in
         val c : int cell
       The type "{x : int | x > 0} cell" is not compatible with the type
         "int cell"
       Type "{x : int | x > 0}" is not compatible with type "int"
       Refinements under the invariant type constructor "cell" must be equal.
|}]

(* [unchanged] Refinements over different payload types: a plain type
   error, since the skeletons differ.
   Currently: rejected with the same message. *)
module Payload : sig val f : int -> {r : bool | r} end = struct
  let f (x : int) : {r : int | r > 0} = if x > 0 then x else 1
end;;
[%%expect{|
Lines 1-3, characters 57-3:
1 | .........................................................struct
2 |   let f (x : int) : {r : int | r > 0} = if x > 0 then x else 1
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> {r : int | r > 0} end
       is not included in
         sig val f : int -> {r : bool | r} end
       Values do not match:
         val f : int -> {r : int | r > 0}
       is not included in
         val f : int -> {r : bool | r}
       The type "int -> {r : int | r > 0}" is not compatible with the type
         "int -> {r : bool | r}"
       Type "int" is not compatible with type "bool"
|}]

(* [stage 3] A functor argument weaker than the parameter.  The primary
   location is the value inside the argument; a sub-message points at the
   application.
   Currently: rejected at the application, 'The type "int -> {s : int | s >= 0}"
   is not compatible with the type "(x : int) -> {r : int | r >= x}"'. *)
module type S = sig val f : (x : int) -> {r : int | r >= x} end
module F (X : S) = struct let g = X.f 3 end
module Bad = F (struct
  let (f @ total) (y : int) : {s : int | s >= 0} = if y >= 0 then y else 0
end);;
[%%expect{|
module type S = sig val f : (x : int) -> {r : int | r >= x} end
module F : functor (X : S) -> sig val g : int end
Line 4, characters 7-8:
4 |   let (f @ total) (y : int) : {s : int | s >= 0} = if y >= 0 then y else 0
           ^
Error: The value "f" does not satisfy the functor's parameter.
       Refinement could not be proved (counterexample: x = 1)
Line 1, characters 52-58:
1 | module type S = sig val f : (x : int) -> {r : int | r >= x} end
                                                        ^^^^^^
  The refinement is stated here.
Lines 3-5, characters 13-4:
3 | .............F (struct
4 |   let (f @ total) (y : int) : {s : int | s >= 0} = if y >= 0 then y else 0
5 | end)..
  Required by this functor application.
|}]

(* [stage 3] The same local module as in inclusion_semantic.ml, without the
   path condition k > 0: the obligation fails.  "above" is partial, so its
   result is a fresh symbol named after the declaration's binder r.
   Currently: rejected at typing, 'Type "{r : int | r > k}" is not compatible
   with type "{r : int | r > 0}"'. *)
let local_bad (k : int) =
  let module M : sig val above : int -> {r : int | r > 0} end = struct
    let above (_ : int) : {r : int | r > k} =
      if k < max_int then k + 1 else failwith "top"
  end in
  M.above 0;;
[%%expect{|
Line 3, characters 8-13:
3 |     let above (_ : int) : {r : int | r > k} =
            ^^^^^
Error: The value "above" does not satisfy its declaration in the signature.
       Refinement could not be proved (counterexample: r = 0, k = -1)
Line 2, characters 51-56:
2 |   let module M : sig val above : int -> {r : int | r > 0} end = struct
                                                       ^^^^^
  The refinement is stated here.
|}]

(* [stage 1] Package constraints are type equalities, so refinements in them
   are compared invariantly even though the value position is covariant
   (DESIGN.md 2.6).  With (Drop) here, a client could unpack E.p and apply
   use to 0, obtaining a "pos" equal to 0 (found by the Codex review).
   Currently: rejected with the same message without the last line. *)
type pos = {x : int | x > 0}
module type S = sig type t val use : t -> pos end
module P = struct type t = pos let use (x : t) : pos = x end
module Package_equation : sig val p : (module S with type t = int) end = struct
  let p = (module P : S with type t = pos)
end;;
[%%expect{|
type pos = {x : int | x > 0}
module type S = sig type t val use : t -> pos end
module P : sig type t = pos val use : t -> pos end
Lines 4-6, characters 73-3:
4 | .........................................................................struct
5 |   let p = (module P : S with type t = pos)
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val p : (module S with type t = pos) end
       is not included in
         sig val p : (module S with type t = int) end
       Values do not match:
         val p : (module S with type t = pos)
       is not included in
         val p : (module S with type t = int)
       The type "(module S with type t = pos)" is not compatible with the type
         "(module S with type t = int)"
       Type "pos" = "{x : int | x > 0}" is not compatible with type "int"
       Refinements in package type constraints must be equal.
|}]
