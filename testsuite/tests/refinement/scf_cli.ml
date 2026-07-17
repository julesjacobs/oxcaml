module A = Scf_prov.Make (struct let cap = 1 end)
module B = Scf_prov.Make (struct let cap = 2 end)
(* A.f needs A.cap; B.g () delivers B.cap.  Distinct.  Must be rejected. *)
let bad = A.f (B.g ())
