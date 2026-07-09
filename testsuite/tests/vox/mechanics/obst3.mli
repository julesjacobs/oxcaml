(* Auxiliary for obst3_client.ml: [ORD.compare]'s refinement is
   RESULT-ONLY ([_ >= -1 && _ <= 1]); it mentions no binder, so the
   binder is normalized away.  Regression for the no-collision path with
   a refinement present. *)

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
  val compare : (x : t) -> (y : t) -> int{ _ >= (-1) && _ <= 1 }
end

module type S = sig
  type elt
  type t : value refines (iset)
  val add : (x : elt) -> (s : t) -> t{ _ = ins x s }
end

module Make (Ord : ORD) : S with type elt = Ord.t
