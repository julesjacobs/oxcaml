module A = Scf_prov.Make (struct let cap = 1 end)
(* 5 not known to equal A.cap: contract genuinely unmet.  Must be rejected. *)
let bad = A.f 5
