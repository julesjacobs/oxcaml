(* A tour of the proof pane.

   Move the cursor onto each refined call to read its obligation in the
   proof pane on the right: the goal behind a turnstile, a verdict badge,
   the hypotheses in scope, and -- for a failure -- the solver's own
   diagnostic and the generated Lean behind a disclosure.  The per-VC
   source underlines are coloured by verdict.

   The automatic check-and-verify round proves the first call because the
   [if] guard [y > 0] becomes a hypothesis on the then-branch.  It disproves
   the second because [0 > 0] is false.  Discharge stops there, so this buffer
   does NOT verify -- by design. *)

let need_pos (x : int{ _ > 0 }) = x

let proved (y : int) = if y > 0 then need_pos y else 1

let disproved = need_pos 0
