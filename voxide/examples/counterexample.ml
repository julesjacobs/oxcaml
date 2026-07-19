(* When the specification is wrong.

   [need_one] demands its argument equal 1.  The call [need_one 2] asks
   the verifier to prove [2 = 1]; it cannot, and reports the obligation
   as disproved.  This is the one example here that does NOT verify --
   by design. *)

let need_one (x : int{ _ = 1 }) = x

let wrong = need_one 2
