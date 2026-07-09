(* Auxiliary sealed functor library for obst_client.ml / obst_wrong.ml.
   [ORD]'s [compare] carries an ELEMENT-MENTIONING contract
   ([_ = 0 -> x = y] refers to both dependent-arrow binders), so
   instantiating [Make] cross-unit coerces the client's argument arrow
   against this .cmi-imported one -- the case that used to mis-pair on a
   binder stamp collision (see stamp_collide.ml, gap C). *)

type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
public inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet
@[grind, expose] public def mem_s (x : Int) : ISet -> Prop
  | .nil => False
  | .cons y s => x = y ∨ mem_s x s
@[grind, expose] public def ins (x : Int) (s : ISet) : ISet := ISet.cons x s
|lean}]

module type ORD = sig
  type t [@@vox.sort int]
  val compare : (x : t) -> (y : t) -> int{ (_ = 0 -> x = y) }
end

module type S = sig
  type elt
  type t : value refines (iset)
  val add : (x : elt) -> (s : t) -> t{ _ = ins x s }
end

module Make (Ord : ORD) : S with type elt = Ord.t
