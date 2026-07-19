(* Router for the DT + LIA combination (task #47, bugreport 03). Ownership and the
   ℤ-trichotomy split are IDENTICAL to {!Uflia_router}: a datatype (dis)equality already
   routes to the congruence child [A] (a [Theory_view.Equality] over a datatype sort -> A,
   like an uninterpreted-sort equality), a LIA order atom [Le_zero] to the arithmetic
   child [B], and an Int equality to [Both] — the same structural dispatch EUF+LIA uses.
   So the congruence child (here the DT theory) and LIA share every Int-sorted boundary
   term, and a DT-derived Int equality (e.g. selector evaluation [key (Node _ k _) = k])
   reaches LIA through the classic disagreement/trichotomy path exactly as [f x = x + y]
   does in QF_UFLIA.

   What differs is the two combination flags: the congruence child IS the DT theory
   ([congruence_models_datatypes] — so datatype-sorted terms are not degraded at Sat
   certification; the child's axiom-validating Final has already certified them), and it
   does not implement the fabric-live congruence seam, so force the classic no-fabric path
   ([fabric_disabled]) regardless of [OXSMT_NO_FABRIC]. *)
include Uflia_router

let fabric_disabled = true
let congruence_models_datatypes = true
