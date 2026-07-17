(* P.f needs P.cap; Q.g () delivers Q.cap.  Different sigs, same name.
   Must be rejected. *)
let bad = Scd_prov.P.f (Scd_prov.Q.g ())
