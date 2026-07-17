(* Different sibling names (basea vs baseb): no conflation possible; stays
   rejected before and after the fix. *)
let bad = Scn_prov.P.f (Scn_prov.Q.g ())
