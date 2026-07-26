(* Sixty seconds with refinement types.

   A refinement type like [int{ _ > 0 }] is an ordinary int carrying a
   proof obligation: every value that flows into it must satisfy the
   predicate, and the verifier discharges that obligation for you.  The
   [_] stands for the value being constrained. *)

let positive (x : int{ _ > 0 }) = x

(* The call site carries the obligation [7 > 0], discharged here. *)
let seven = positive 7
