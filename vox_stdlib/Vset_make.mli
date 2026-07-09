(* Vset_make: a verified finite-SET as an ORD FUNCTOR over an
   int-representable, ordered element type -- the [Set.Make(ORD)] shape,
   productized from the proven functor probe
   (testsuite/tests/vox/mechanics/lean_functor_bst.ml).  ADDITIVE to the
   flat [Vset] face: same op names ([empty]/[add]/[mem]/[singleton]), a
   parallel model vocabulary, so a future unification is mechanical.

   WHAT THE FUNCTOR EXPRESSES (crisp boundary):
   - The element type [O.t] carries [@@vox.sort int]: its logical model is
     an int key, so the order is [Int]'s [<] (grind-native).  [O.compare]'s
     refinement is the ORDERED CONTRACT tying the result sign to that order
     -- an obligation EVERY instantiation must discharge (a lawful [IntOrd]
     seals green; a sign-flipped comparator is DISPROVED -- see
     clients/smoke_vset_make.ml and the cross-unit demo).
   - The set MODEL is a CHARACTERISTIC FUNCTION [ISet := Int -> Prop].  Its
     equality (via [funext]) IS extensional set equality, so unlike the flat
     [Vset] (inductive-list model, membership-agreement specs), [add] here
     carries a STRUCTURAL postcondition [_ = ins x s].  This is the cleaner,
     stronger spec shape -- but the char-function model has NO support size,
     so [cardinal] is NOT expressible here (see notes/vset_make.md; that is
     the deliberate model trade against Vset, which ships cardinal).
   - The Lean model + tree bridge live at FILE TOP LEVEL, never inside the
     functor ([%%vox.lean] blocks are unit-level, vox_verify enforces it).
     The .mli block ships only the CLIENT-FACING set vocabulary (ISet,
     mem_s, ins, empty_s + the three membership laws); the tree BST
     invariant + abstraction [elems] are .ml-only (they name the functor's
     own [Vox_Vset_make_tree]).

   The model OPS are opaque (obligation pattern): [ins]/[mem_s]/[empty_s]
   are declared but not defined in the interface, so a client computes only
   through the three shipped laws ([mem_s_ins], [mem_s_ins_ne],
   [mem_s_empty]) -- exposing the char-function defs would let grind
   beta-discharge them and the algebra would be dead.  Each law is proven
   load-bearing by deletion (clients/smoke_vset_make.ml).  Zero trust: the
   .ml proves every spec honestly through a [refine_] unpack; zero
   [assume_unchecked_]. *)

type iset [@@vox.sort lean "ISet"]

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t)
    -> int{ (_ < 0 -> x < y) && (_ = 0 -> x = y) && (_ > 0 -> y < x) }
end

[%%vox.lean {lean|
-- The exported SET model: a set of int keys as a characteristic function.
-- Its funext-equality IS set equality, so op specs are STRUCTURAL (_ = ins ...).
-- ISet's sort is transparent, but the model OPS are exported OPAQUE (the
-- oset obligation pattern): [ins] is a non-recursive point-update and
-- [mem_s] one application, so exposed defs would let a client's grind
-- discharge the membership laws by beta-unfolding and the shipped algebra
-- would be dead.  Opaque ops keep the three laws LIVE.  The .ml pays them
-- as obligations over the concrete char-function defs.
public abbrev ISet := Int -> Prop
public axiom mem_s : Int -> ISet -> Prop
public axiom ins : Int -> ISet -> ISet
public axiom empty_s : ISet
-- The algebra clients reason with (all three LIVE under opaque model ops).
public axiom mem_s_ins (x : Int) (s : ISet) : mem_s x (ins x s)
grind_pattern mem_s_ins => mem_s x (ins x s)
public axiom mem_s_ins_ne (x y : Int) (s : ISet) (h : x ≠ y) :
    mem_s y (ins x s) = mem_s y s
grind_pattern mem_s_ins_ne => mem_s y (ins x s)
public axiom mem_s_empty (x : Int) : ¬ mem_s x empty_s
grind_pattern mem_s_empty => mem_s x empty_s
|lean}]

module type SET = sig
  type elt
  type t : value refines (iset)
  val empty : (u : unit) -> t{ _ = empty_s }
  val add : (x : elt) -> (s : t) -> t{ _ = ins x s }
  val mem : (x : elt) -> (s : t) -> bool{ _ = mem_s x s }
  (* [singleton x] is the one-element set (add over empty). *)
  val singleton : (x : elt) -> t{ _ = ins x empty_s }
end

module Make : functor (O : ORD) -> SET with type elt = O.t
