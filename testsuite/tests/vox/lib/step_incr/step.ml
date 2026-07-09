(* Successor: the simplest payment of step.mli's obligation.  The
   block OWNS the interface's constants -- [def step] implements the
   opaque, [theorem step_gt] proves the axiom -- and the seal checks
   both by name and statement. *)

[%%vox.lean {lean|
@[grind] def step (x : Int) : Int := x + 1

theorem step_gt (x : Int) : x < step x := by grind
|lean}]

let step : (x : int) -> int{ _ = step x } = fun x -> x + 1
