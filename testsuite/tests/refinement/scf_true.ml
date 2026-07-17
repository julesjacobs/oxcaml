module A = Scf_prov.Make (struct let cap = 1 end)
(* Same instance: A.g () = A.cap, A.f needs A.cap.  Genuinely true. *)
let ok = A.f (A.g ())
