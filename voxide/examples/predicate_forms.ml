(* Predicates beyond a single comparison.

   A refinement predicate can be any boolean expression over the hole
   [_]: a conjunction, an [if]/[then]/[else], or a [let] .. [in].  Each
   call below discharges its obligation; move the cursor onto the
   argument to read the instantiated predicate in the proof pane. *)

let conj (x : int{ _ > 0 && _ < 10 }) = x

let ite (x : int{ if _ > 0 then true else false }) = x

let lin (x : int{ let y = _ in y > 0 }) = x

let use_conj = conj 5
let use_ite = ite 5
let use_lin = lin 5
