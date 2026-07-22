(* Provider compiled as a separate unit.  Arrow-bound parameters, sibling
   values, and captured locals remain distinct through CMI import. *)
let identity (x : int) : int{ _ = x } = x          (* result mentions the PARAMETER *)
let k = 5
let fk (x : int) : int{ _ = k } = k                (* result mentions a SIBLING *)
let capf = let cap = 7 in fun (x : int) : int{ _ = cap } -> cap
                                                   (* result mentions a CAPTURED local *)
let five : int{ _ = 5 } = 5                        (* NON-dependent refinement *)
