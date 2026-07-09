(* A different function under the same sig: grows by doubling once
   past zero.  The observable values differ from step_incr's on every
   positive input; the CONTRACT is the law alone, and the identical
   client (demo/lean_seal.ml vs demo/lean_seal_alt.ml) verifies
   against either implementation unchanged. *)

[%%vox.lean {lean|
@[grind] def step (x : Int) : Int := if x < 0 then x + 1 else 2 * x + 1

theorem step_gt (x : Int) : x < step x := by grind
|lean}]

let step : (x : int) -> int{ _ = step x } = fun x -> if x < 0 then x + 1 else (2 * x) + 1
