(* An arbitrary key type with a comparison, and a set over it.

   For an [int] key the comparison is a primitive the solver interprets: it
   knows on its own that [<] is a strict total order.  Here [compare] is an
   uninterpreted function, and everything the solver may assume about it is
   the three laws below, each of which has to be instantiated by hand at the
   pair or triple where it is needed.

   Trichotomy comes free, in exactly one sense and no wider: [compare]
   returns an [int], so the result of a SINGLE call is negative, zero or
   positive whatever [compare] means.  Nothing relating two calls is free.

   Three laws are enough.  Writing [R x y] for [compare x y < 0]: [R] is
   irreflexive because the equality law at a repeated argument gives
   [compare x x = 0]; asymmetric because [R x y] and sign reversal give
   [compare y x > 0]; total on distinct keys because the equality law makes
   [compare x y] non-zero and an [int] is then negative or positive, with
   sign reversal turning the positive case round; and transitive by the third
   law.  So the three imply a strict total order, provided [compare] is a
   function of its arguments at all -- which is not one of the three, see
   below.  Two independent passes over this directory also searched for a
   pure deterministic comparison satisfying all three that is nevertheless
   not an order, and neither found one.

   WHAT FORCES THEM, and the distinction matters.  A law is a value of type
   [unit{ P }].  Module matching requires a key module to supply a value of
   that type and will not take a plain [unit]-returning stub in its place:
   supplying [let compare_zero_iff_equal ~left:_ ~right:_ = ()] is a
   signature mismatch, "the type left:'a -> right:'b -> unit is not
   compatible with ... unit{ compare left right = 0 = (left = right) }".  So
   a key module cannot decline to answer.  It can answer in two ways, and
   they are not the same thing:

   - An ordinary OCaml definition.  Module matching forces the field and its
     refined type; the VERIFIER then proves [P] where the value is written,
     and compilation of the key module fails if it cannot.  Every key in this
     directory takes this route, and for it the laws really are discharged
     before [Make] is reached.
   - A trusted [external] carrying the same refined type.  The compiler
     admits the refinement as an axiom and emits no obligation at all -- a
     key module written that way dumps ZERO verification conditions.  This is
     the repository's ordinary trusted boundary, the same one the iarray work
     uses, and the interface cannot tell the two routes apart.

   So "the type system forces the laws" is right about the question being
   ASKED and wrong about the question being ANSWERED.  There is no path from
   "no proof" to "accepted key" for an ordinary definition; there is one
   through a trusted declaration, and it is the same one the rest of the
   corpus has.

   Two properties the three laws do not state, and do not need to.
   Determinism and freedom from observable effects come from the [@@ total]
   mode on [compare] together with its logical, [immutable_data] arguments,
   and they are refused at TYPING, before any obligation is built: a
   [compare] that reads or writes a mutable cell is rejected with "The first
   is partial because it contains a usage (of the value counter ...) which is
   expected to be physical".  A trusted external can still declare [@@ total]
   and lie.

   WHAT A FALSE LAW COSTS.  Not merely that the structure is misnamed: an
   exported set law becomes false of the running program.  Measured, with a
   key whose [compare] is integer addition -- so [compare 1 (-1) = 0] though
   the keys differ -- and whose three laws are trusted externals.  It is
   accepted, [Gen_sorted.Make] applies to it, and at
   [inserted = 1, set = empty, query = -1] the exported [insert_law] says
   [member query (insert inserted set) = ((query = inserted) || member query
   set)] where the left side computes [true] and the right side [false].  The
   verifier proves [member (-1) (insert 1 empty) = false] for a program that
   computes [true].  Every client holding that refined unit holds a false
   fact.  That is why nothing in this directory declares a refined
   [external], and why it should stay that way.

   All three laws are load-bearing here, measured by weakening each to
   [unit{ true }] and recompiling: the equality law is needed by all four
   implementations, and sign reversal and transitivity by the three ordered
   ones.  [gen_ulist] still verifies without those two, since a unique list
   compares for equality and never for order. *)

module type ORDERED_KEY = sig
  type t : immutable_data

  (* A ground value of the key type.  Mentioning it is how a proof tells
     the solver the abstract type is inhabited: an abstract type the solver
     does not know is inhabited makes the obligation an error rather than a
     vacuous truth.  Twelve proofs across the four implementations have to
     say so by hand, three each, all of the form
     [K.compare_zero_iff_equal ~left:K.witness ~right:K.witness].  A
     functor-parameter type is not treated as inhabited on its own the way
     [iarray] is; if a declared witness were enough to mark it so, those
     twelve mentions could go. *)
  val witness : t

  val compare :
    t @ local logical -> t @ local logical -> int @@ total

  val compare_zero_iff_equal :
    left:t @ logical -> right:t @ logical ->
    unit{ (compare left right = 0) = (left = right) } @@ total

  val compare_sign_reversal :
    left:t @ logical -> right:t @ logical ->
    unit{
      (compare left right < 0) = (compare right left > 0)
    } @@ total

  val compare_negative_transitive :
    first:t @ logical -> second:t @ logical -> third:t @ logical ->
    unit{
      not (compare first second < 0)
      || not (compare second third < 0)
      || compare first third < 0
    } @@ total
end

(* The integer-set interface of [set_group], over an arbitrary key.  The
   representation invariant is abstract: a client learns only that [empty]
   satisfies it and that [insert] preserves it.

   How much of that invariant the interface forces was measured by weakening
   each component to [true], deleting the invariant-preservation machinery
   the weakened module no longer needs, and recompiling.  Two of the five
   components across the four implementations are forced:

     gen_sorted  sorted    forced   -- insert_law fails at [insert_sorted]
     gen_avl     ordered   forced   -- insert_law fails at [insert_ordered]
     gen_avl     balanced  not forced
     gen_bst     ordered   not forced
     gen_ulist   unique    not forced

   Deleting the unneeded machinery is not optional.  Weakening a component
   and changing nothing else leaves [insert_invariant] still calling the
   preservation lemma for a component the invariant no longer mentions, and
   all five then "fail" -- at that call, not at anything the interface
   requires.  Only a refusal inside [insert_law] or [insert_invariant]'s own
   obligation counts.

   The rule is not "membership exits early on the comparison": [gen_bst]
   exits early too and is unforced.  It is whether that implementation's
   proof of [insert_law] reaches for the component.  A different proof of the
   same law would give a different answer with no change to the interface, so
   this is a measurement of these four implementations as written, and has to
   be re-measured after any substantive change to a proof.

   The invariant is exported for all four anyway, because a search structure
   without its invariant is not that structure -- not because the interface
   extracts it. *)
module type SET = sig
  type key : immutable_data
  type t

  val key_witness : key

  val empty : t @@ total
  val insert : key @ logical -> t @ logical -> t @@ total
  val member :
    key @ local logical -> t @ local logical -> bool @@ total
  val equal :
    t @ local logical -> t @ local logical -> bool @@ total

  val invariant : t @ local logical -> bool @@ total

  val empty_invariant : unit{ invariant empty = true } @@ total

  val insert_invariant :
    inserted:key @ logical ->
    set:t @ logical ->
    well_formed:unit{ invariant set = true } ->
    unit{ invariant (insert inserted set) = true } @@ total

  val empty_law :
    query:key @ logical ->
    unit{ member query empty = false } @@ total

  val insert_law :
    inserted:key @ logical ->
    set:t @ logical ->
    query:key @ logical ->
    well_formed:unit{ invariant set = true } ->
    unit{
      member query (insert inserted set)
      = ((query = inserted) || member query set)
    } @@ total

  val equal_forward_law :
    left:t @ logical ->
    right:t @ logical ->
    equal_sets:unit{ equal left right = true } ->
    query:key @ logical ->
    unit{ member query left = member query right } @@ total

  val equal_backward_law :
    left:t @ logical ->
    right:t @ logical ->
    pointwise:
      (query:key @ logical ->
       unit{ member query left = member query right }) @ total ->
    unit{ equal left right = true } @@ total
end
