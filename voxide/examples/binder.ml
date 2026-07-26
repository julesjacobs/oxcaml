(* A refined let-binder is remembered as a fact.

   [x] is bound at [int{ _ = 7 }]; downstream, that fact discharges the
   stronger-looking obligation [x > 0]. *)

let bump () =
  let x = (7 : int{ _ = 7 }) in
  (x : int{ _ > 0 })
