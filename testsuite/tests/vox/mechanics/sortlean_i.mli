(* interface side of the lean-ghost-sort mli/ml agreement test: a ghost
   sort at Lean type "ISet" here, at "IBag" on the implementation *)
type t [@@vox.sort lean "ISet"]

val mk : (x : int) -> t
