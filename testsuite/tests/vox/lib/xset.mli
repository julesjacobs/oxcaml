(* A finite set behind a via-abstracted sealed interface -- the SAME
   shape as [via_set], but with an EXTENSIONAL model.  Where [via_set]
   models [ISet] as an inductive list ([cons]/[nil]) -- an ordinary
   datatype-style target with decidable observables (it can expose
   [card] and print concrete counterexamples, but it distinguishes
   values that are equal as sets: [cons x (cons x s) <> cons x s]) --
   [xset] models [ISet] as Lean's actual set type [Int -> Prop].  The
   payoff is EXTENSIONALITY: [ins x (ins x s) = ins x s] holds (see
   [ins_idem]), so a client can prove a set-level EQUALITY through the
   abstraction (adding the same element twice yields the same denotation
   as adding it once).  The trade-off: [card] is not definable on
   [Int -> Prop], so this model gives witness-free failures where
   [via_set]'s gives arithmetic counterexamples.  The two libraries are
   the two ends of the via modeling spectrum.

   As in [via_set], the .mli hides the representation entirely: [t] is
   [refines (iset)], the client reasons in set vocabulary, and the .ml
   PROVES these specs honestly (no [assume_unchecked_]) by unpacking the
   image binder with [refine_].

   Kept MONOMORPHIC ([Int -> Prop]).  The parameterized generalization
   is mechanical -- [ISet (a : Type) := a -> Prop], mirroring [pset]'s
   inductive parameterization -- and adds no new modeling point over
   this one, so it is not duplicated here. *)
type iset [@@vox.sort lean "ISet"]
type t : value refines (iset)

[%%vox.lean {lean|
@[expose] public def ISet := Int -> Prop

@[grind, expose] public def mem (x : Int) (s : ISet) : Prop := s x
@[grind, expose] public def emp : ISet := fun _ => False
@[grind, expose] public def ins (x : Int) (s : ISet) : ISet :=
  fun y => y = x ∨ s y
@[grind, expose] public def uni (a b : ISet) : ISet := fun y => a y ∨ b y

@[grind] public theorem mem_emp (x : Int) : ¬ mem x emp := by grind
@[grind] public theorem mem_ins_iff (x y : Int) (s : ISet) :
    mem x (ins y s) ↔ (x = y ∨ mem x s) := by grind
@[grind] public theorem mem_uni (x : Int) (a b : ISet) :
    mem x (uni a b) ↔ (mem x a ∨ mem x b) := by grind
@[grind] public theorem mem_ins (x : Int) (s : ISet) : mem x (ins x s) := by
  grind

-- EXTENSIONALITY: sets equal iff they have the same members.  This is
-- what the [Int -> Prop] model buys over the inductive list model.
public theorem iset_ext (a b : ISet) (h : ∀ x, mem x a ↔ mem x b) : a = b := by
  funext x
  have hx : a x ↔ b x := h x
  exact propext hx
@[grind] public theorem ins_idem (x : Int) (s : ISet) :
    ins x (ins x s) = ins x s := iset_ext _ _ (by grind)
|lean}]

val add : (x : int) -> (s : t) -> t{ _ = ins x s }
val member : (x : int) -> (s : t) -> bool{ _ = mem x s }
