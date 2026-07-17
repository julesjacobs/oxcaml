(* Provider compiled as a SEPARATE unit.  Its parameters, siblings, and captured
   locals mentioned in result refinements are all lowered as free local Pidents
   carrying this unit's stamps; on import they must not be conflated with the
   caller's binders. *)
let identity (x : int) : int{ _ = x } = x          (* result mentions the PARAMETER *)
let k = 5
let fk (x : int) : int{ _ = k } = k                (* result mentions a SIBLING *)
let capf = let cap = 7 in fun (x : int) : int{ _ = cap } -> cap
                                                   (* result mentions a CAPTURED local *)
let five : int{ _ = 5 } = 5                        (* NON-dependent refinement *)
