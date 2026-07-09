(* A set behind an OPAQUE Lean model, exposed with the INLINE refines
   form [refines ([%lean "MyT"])] -- no ghost-sort intermediary type. *)
type t : value refines ([%lean "MyT"])

[%%vox.lean {lean|
public opaque MyT : Type
public axiom mem : Int -> MyT -> Prop
public axiom ins : Int -> MyT -> MyT
public axiom empty_s : MyT
public axiom mem_empty (x : Int) : ¬ mem x empty_s
grind_pattern mem_empty => mem x empty_s
public axiom mem_ins (x y : Int) (s : MyT) :
    mem y (ins x s) ↔ (y = x ∨ mem y s)
grind_pattern mem_ins => mem y (ins x s)
|lean}]

val empty : unit -> t{ _ = empty_s }
val add : (x : int) -> (s : t) -> t{ _ = ins x s }
val member : (x : int) -> (s : t) -> bool{ _ = mem x s }
