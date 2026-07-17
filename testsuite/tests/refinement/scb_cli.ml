module A = (val Scb_prov.m1 : Scb_prov.SIG)
module B = (val Scb_prov.m2 : Scb_prov.SIG)
(* Cross-instance false claim through first-class modules.  Must be rejected. *)
let bad = A.f (B.g ())
