(* TEST
 expect;
*)

(* Equal predicates remain structural and require no seal implication. *)
module Equal : sig
  val x : int{ _ = 2 }
end = struct
  let x = (2 : int{ _ = 2 })
end
[%%expect {|
module Equal : sig val x : int{ _ = 2 } end
|}]

(* A stronger implementation contract may be weakened at the seal. *)
module Stronger : sig
  val x : int{ _ >= 0 }
end = struct
  let x = (2 : int{ _ = 2 })
end
[%%expect {|
module Stronger : sig val x : int{ _ >= 0 } end
|}]

(* The reverse implication is false and must be rejected by verification. *)
module Weaker : sig
  val x : int{ _ = 2 }
end = struct
  let x = (2 : int{ _ >= 0 })
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = (2 : int{ _ >= 0 })
5 | end
Error: Refinement verification failed at module seal for value "x" (not-proved)
Line 2, characters 2-22:
2 |   val x : int{ _ = 2 }
      ^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value x
Line 4, characters 6-7:
4 |   let x = (2 : int{ _ >= 0 })
          ^
  Implementation declaration for value x
|}]

(* A refinement may be silently forgotten in a covariant seal position. *)
module Refined_behind_bare : sig
  val x : int
end = struct
  let x = (2 : int{ _ = 2 })
end
[%%expect {|
module Refined_behind_bare : sig val x : int end
|}]

(* Q-001 remains fail-closed: a bare implementation cannot acquire a
   refinement from its interface. *)
module Bare_behind_refined : sig
  val x : int{ _ = 2 }
end = struct
  let x = 2
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = 2
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : int end
       is not included in
         sig val x : int{ _ = 2 } end
       Values do not match:
         val x : int
       is not included in
         val x : int{ _ = 2 }
       The type "int" is not compatible with the type "int{ _ = 2 }"
|}]

module type Positive = sig
  val x : int{ _ > 0 }
end

module type Nonnegative = sig
  val x : int{ _ >= 0 }
end

module Accepts_nonnegative (X : Nonnegative) = struct end

(* Functor parameters are contravariant: [Positive] implies [Nonnegative]. *)
module Functor_direction_accept
    : functor (X : Positive) -> sig end = Accepts_nonnegative
[%%expect {|
module type Positive = sig val x : int{ _ > 0 } end
module type Nonnegative = sig val x : int{ _ >= 0 } end
module Accepts_nonnegative : functor (X : Nonnegative) -> sig end
module Functor_direction_accept : functor (X : Positive) -> sig end
|}]

module Accepts_positive (X : Positive) = struct end

(* The opposite parameter implication is false. *)
module Functor_direction_reject
    : functor (X : Nonnegative) -> sig end = Accepts_positive
[%%expect {|
module Accepts_positive : functor (X : Positive) -> sig end
Line 5, characters 45-61:
5 |     : functor (X : Nonnegative) -> sig end = Accepts_positive
                                                 ^^^^^^^^^^^^^^^^
Error: Refinement verification failed at module seal for value "x" (not-proved)
Line 6, characters 2-23:
6 |   val x : int{ _ >= 0 }
      ^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value x
Line 2, characters 2-22:
2 |   val x : int{ _ > 0 }
      ^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value x
|}]

module type Sibling_equal = sig
  val base : int
  val x : int{ _ = base }
end

module type Sibling_lower_bound = sig
  val base : int
  val x : int{ _ >= base }
end

(* Rename-immune sibling heads participate in a true seal implication. *)
module Sibling_true (X : Sibling_equal) : Sibling_lower_bound = X
[%%expect {|
module type Sibling_equal = sig val base : int val x : int{ _ = base } end
module type Sibling_lower_bound =
  sig val base : int val x : int{ _ >= base } end
module Sibling_true : functor (X : Sibling_equal) -> Sibling_lower_bound
|}]

(* The reverse sibling implication remains a sound rejection. *)
module Sibling_false (X : Sibling_lower_bound) : Sibling_equal = X
[%%expect {|
Line 1, characters 65-66:
1 | module Sibling_false (X : Sibling_lower_bound) : Sibling_equal = X
                                                                     ^
Error: Refinement verification failed at module seal for value "x" (not-proved)
Line 3, characters 2-25:
3 |   val x : int{ _ = base }
      ^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value x
Line 8, characters 2-26:
8 |   val x : int{ _ >= base }
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value x
|}]

(* A declaration UID identifies the field declaration in the functor body,
   not the runtime value produced by a particular application.  Seal
   reconciliation must therefore retain the complete instance path. *)
module Instance_functor (M : sig val value : int end) = struct
  let field = M.value
end

module Instance_x = struct let value = 1 end
module Instance_y = struct let value = 2 end
module Instance_a = Instance_functor (Instance_x)
module Instance_b = Instance_functor (Instance_y)
module Instance_a_again = Instance_functor (Instance_x)
module Instance_alias = Instance_a
[%%expect {|
module Instance_functor :
  functor (M : sig val value : int end) -> sig val field : int end
module Instance_x : sig val value : int end
module Instance_y : sig val value : int end
module Instance_a : sig val field : int end
module Instance_b : sig val field : int end
module Instance_a_again : sig val field : int end
module Instance_alias = Instance_a
|}]

(* Exact paths and genuine aliases still reconcile. *)
module Instance_same : sig
  val field : int{ _ = Instance_a.field }
end = struct
  let field : int{ _ = Instance_a.field } = Instance_a.field
end

module Instance_alias_same : sig
  val field : int{ _ = Instance_a.field }
end = struct
  let field : int{ _ = Instance_alias.field } = Instance_alias.field
end
[%%expect {|
module Instance_same : sig val field : int{ _ = Instance_a.field } end
module Instance_alias_same : sig val field : int{ _ = Instance_a.field } end
|}]

(* Different functor arguments must remain distinct at a covariant result
   seal, even though both fields carry the UID of [Instance_functor.field]. *)
module Instance_argument_rejected : sig
  val field : int{ _ = Instance_a.field }
end = struct
  let field : int{ _ = Instance_b.field } = Instance_b.field
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let field : int{ _ = Instance_b.field } = Instance_b.field
5 | end
Error: Refinement verification failed at module seal for value "field" (not-proved)
Line 2, characters 2-41:
2 |   val field : int{ _ = Instance_a.field }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value field
Line 4, characters 6-11:
4 |   let field : int{ _ = Instance_b.field } = Instance_b.field
          ^^^^^
  Implementation declaration for value field
|}]

(* Re-evaluating an applicative functor at the same argument can still produce
   a different term value; applicativity equates types, not values. *)
module Instance_reapplication_rejected : sig
  val field : int{ _ = Instance_a.field }
end = struct
  let field : int{ _ = Instance_a_again.field } = Instance_a_again.field
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let field : int{ _ = Instance_a_again.field } = Instance_a_again.field
5 | end
Error: Refinement verification failed at module seal for value "field" (not-proved)
Line 2, characters 2-41:
2 |   val field : int{ _ = Instance_a.field }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value field
Line 4, characters 6-11:
4 |   let field : int{ _ = Instance_a_again.field } = Instance_a_again.field
          ^^^^^
  Implementation declaration for value field
|}]

module Generative_functor () = struct
  let field = 0
end

module Generative_a = Generative_functor ()
module Generative_b = Generative_functor ()
[%%expect {|
module Generative_functor : functor () -> sig val field : int end
module Generative_a : sig val field : int end
module Generative_b : sig val field : int end
|}]

module Generative_rejected : sig
  val field : int{ _ = Generative_a.field }
end = struct
  let field : int{ _ = Generative_b.field } = Generative_b.field
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let field : int{ _ = Generative_b.field } = Generative_b.field
5 | end
Error: Refinement verification failed at module seal for value "field" (not-proved)
Line 2, characters 2-43:
2 |   val field : int{ _ = Generative_a.field }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value field
Line 4, characters 6-11:
4 |   let field : int{ _ = Generative_b.field } = Generative_b.field
          ^^^^^
  Implementation declaration for value field
|}]

(* The same distinction is required when the seal implication reverses in a
   function argument. *)
module Instance_domain_rejected : sig
  val consume : int{ _ = Instance_a.field } -> unit
end = struct
  let consume (_ : int{ _ = Instance_b.field }) = ()
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let consume (_ : int{ _ = Instance_b.field }) = ()
5 | end
Error: Refinement verification failed at module seal for value "consume" (not-proved)
Line 2, characters 2-51:
2 |   val consume : int{ _ = Instance_a.field } -> unit
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value consume
Line 4, characters 6-13:
4 |   let consume (_ : int{ _ = Instance_b.field }) = ()
          ^^^^^^^
  Implementation declaration for value consume
|}]

(* Module type declarations are compared by rigid equality, even when the
   containing modules meet at a seal. *)
module Module_type_equality : sig
  module type T = sig
    val x : int{ _ >= 0 }
  end
end = struct
  module type T = sig
    val x : int{ _ = 2 }
  end
end
[%%expect {|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   module type T = sig
7 |     val x : int{ _ = 2 }
8 |   end
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type T = sig val x : int{ _ = 2 } end end
       is not included in
         sig module type T = sig val x : int{ _ >= 0 } end end
       Module type declarations do not match:
         module type T = sig val x : int{ _ = 2 } end
       does not match
         module type T = sig val x : int{ _ >= 0 } end
       At position "module type T = <here>"
       Module types do not match:
         sig val x : int{ _ = 2 } end
       is not equal to
         sig val x : int{ _ >= 0 } end
       At position "module type T = <here>"
       Values do not match:
         val x : int{ _ = 2 }
       is not included in
         val x : int{ _ >= 0 }
       The type "int{ _ = 2 }" is not compatible with the type "int{ _ >= 0 }"
|}]

module type Fun_dom_strong = sig
  val f : int{ _ > 5 } -> int
end

module type Fun_dom_weak = sig
  val f : int{ _ > 0 } -> int
end

(* Function-argument contravariance: at a refined function domain the seal
   implication reverses.  Sealing an implementation that assumes only
   [_ > 0] of its argument behind an interface that promises callers pass
   [_ > 5] accepts, because the interface domain implies the implementation
   domain ([_ > 5] |- [_ > 0]). *)
module Fun_dom_accept (X : Fun_dom_weak) : Fun_dom_strong = X
[%%expect {|
module type Fun_dom_strong = sig val f : int{ _ > 5 } -> int end
module type Fun_dom_weak = sig val f : int{ _ > 0 } -> int end
module Fun_dom_accept : functor (X : Fun_dom_weak) -> Fun_dom_strong
|}]

(* The reverse is unsound and rejects: an implementation that assumes
   [_ > 5] behind an interface promising only [_ > 0] would receive
   arguments it cannot handle ([_ > 0] does not imply [_ > 5]). *)
module Fun_dom_reject (X : Fun_dom_strong) : Fun_dom_weak = X
[%%expect {|
Line 1, characters 60-61:
1 | module Fun_dom_reject (X : Fun_dom_strong) : Fun_dom_weak = X
                                                                ^
Error: Refinement verification failed at module seal for value "f" (not-proved)
Line 6, characters 2-29:
6 |   val f : int{ _ > 0 } -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Interface declaration for value f
Line 2, characters 2-29:
2 |   val f : int{ _ > 5 } -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Implementation declaration for value f
|}]

(* @acc id=seal_launder_unsound final=REJECT today=REJECT stable=yes
   KNOWN GAP, deferred to the Seals stage: an UNCONSTRAINED implementation
   ([Obj.magic 0] : the value-restricted [Tvar] result) behind a refined
   interface laundered the refinement through signature inclusion, with no
   obligation queued.  A concrete bare implementation is already rigidly
   rejected (see seal_conforming); only this [Tvar]-through-seal case slips.
   Signature-boundary obligations are the Seals stage -- the verification pass
   over the structure has no visibility into the ascribing signature -- so this
   was an ANCHOR recording the then-unsound ACCEPT.  The Seals merge closed the
   [Tvar]-through-seal guard, so the laundered ['a] is now rejected structurally
   at signature inclusion, reaching the final REJECT. *)
module Seal_launder : sig
  val x : int{ _ = 1 }
end = struct
  let x = Obj.magic 0
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = Obj.magic 0
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : 'a end
       is not included in
         sig val x : int{ _ = 1 } end
       Values do not match:
         val x : 'a
       is not included in
         val x : int{ _ = 1 }
       The type "'a" is not compatible with the type "int{ _ = 1 }"
|}]
