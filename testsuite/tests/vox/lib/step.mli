(* One sig, two implementations (step_incr/step.ml and
   step_double/step.ml) -- the F* interface discipline, made checkable.

   The model constant [step] is OPAQUE and its law is an AXIOM: a
   CLIENT verifies against this text alone (it arrives as the
   compiled VoxSig_Step module, built from this .mli before any
   implementation exists), so nothing about [step]'s value beyond the
   law can ever be proved on the client side.  For an IMPLEMENTATION
   the same axiom is an OBLIGATION: the seal appended to its solver
   input re-elaborates this block and demands a same-named PROVED
   theorem for the law and a same-typed definition for the opaque --
   an implementation that cannot pay is refused (see
   mechanics/lean_seal_fail.ml).  Both implementations discharge the
   same text; every client proof holds for whichever is linked. *)

[%%vox.lean {lean|
public opaque step : Int -> Int
public axiom step_gt (x : Int) : x < step x
grind_pattern step_gt => step x
|lean}]

val step : (x : int) -> int{ _ = step x }
