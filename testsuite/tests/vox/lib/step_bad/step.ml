(* An implementation that cannot pay the interface's obligation:
   [step x = x] satisfies its OWN definition (the value contract
   verifies), but no [step_gt] theorem is provable -- and none is even
   stated.  The seal refuses the unit; see
   mechanics/lean_seal_fail.ml. *)

[%%vox.lean {lean|
@[grind] def step (x : Int) : Int := x
|lean}]

let step : (x : int) -> int{ _ = step x } = fun x -> x
