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
module Equal : sig val x : int{ (app[Stdlib!.=] _ 2) } end
|}]

(* A stronger implementation contract may be weakened at the seal. *)
module Stronger : sig
  val x : int{ _ >= 0 }
end = struct
  let x = (2 : int{ _ = 2 })
end
[%%expect {|
module Stronger : sig val x : int{ (app[Stdlib!.>=] _ 0) } end
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
         sig val x : int{ (app[Stdlib!.=] _ 2) } end
       Values do not match:
         val x : int
       is not included in
         val x : int{ (app[Stdlib!.=] _ 2) }
       The type "int" is not compatible with the type
         "int{ (app[Stdlib!.=] _ 2) }"
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
module type Positive = sig val x : int{ (app[Stdlib!.>] _ 0) } end
module type Nonnegative = sig val x : int{ (app[Stdlib!.>=] _ 0) } end
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
module type Sibling_equal =
  sig val base : int val x : int{ (app[Stdlib!.=] _ sibling[base]) } end
module type Sibling_lower_bound =
  sig val base : int val x : int{ (app[Stdlib!.>=] _ sibling[base]) } end
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
         sig module type T = sig val x : int{ (app[Stdlib!.=] _ 2) } end end
       is not included in
         sig module type T = sig val x : int{ (app[Stdlib!.>=] _ 0) } end end
       Module type declarations do not match:
         module type T = sig val x : int{ (app[Stdlib!.=] _ 2) } end
       does not match
         module type T = sig val x : int{ (app[Stdlib!.>=] _ 0) } end
       At position "module type T = <here>"
       Module types do not match:
         sig val x : int{ (app[Stdlib!.=] _ 2) } end
       is not equal to
         sig val x : int{ (app[Stdlib!.>=] _ 0) } end
       At position "module type T = <here>"
       Values do not match:
         val x : int{ (app[Stdlib!.=] _ 2) }
       is not included in
         val x : int{ (app[Stdlib!.>=] _ 0) }
       The type "int{ (app[Stdlib!.=] _ 2) }" is not compatible with the type
         "int{ (app[Stdlib!.>=] _ 0) }"
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
module type Fun_dom_strong =
  sig val f : int{ (app[Stdlib!.>] _ 5) } -> int end
module type Fun_dom_weak = sig val f : int{ (app[Stdlib!.>] _ 0) } -> int end
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

(* @acc id=seal_launder_unsound final=REJECT today=ACCEPT stable=no unlocks=seals
   KNOWN GAP, deferred to the Seals stage: an UNCONSTRAINED implementation
   ([Obj.magic 0] : the value-restricted [Tvar] result) behind a refined
   interface laundered the refinement through signature inclusion, with no
   obligation queued.  A concrete bare implementation is already rigidly
   rejected (see seal_conforming); only this [Tvar]-through-seal case slips.
   Signature-boundary obligations are the Seals stage -- the verification pass
   over the structure has no visibility into the ascribing signature -- so this
   is an ANCHOR recording the current unsound ACCEPT.  It flips to REJECT when
   Seals queues the directed implication VC. *)
module Seal_launder : sig
  val x : int{ _ = 1 }
end = struct
  let x = Obj.magic 0
end
[%%expect {|
module Seal_launder : sig val x : int{ (app[Stdlib!.=] _ 1) } end
|}]
