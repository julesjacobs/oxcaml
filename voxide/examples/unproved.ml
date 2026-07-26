(* An open goal: true, but automation gives up.

   [_ >= 0] holds for [x * x], but the goal is nonlinear and grind does
   not discharge it -- and finds no counterexample either.  The verdict
   is UNPROVED, distinct from DISPROVED: the goal may still hold.  This
   buffer therefore does NOT verify -- by design. *)

let square (x : int) : int{ _ >= 0 } = x * x
