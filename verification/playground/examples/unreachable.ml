(* [unreachable_ ()] claims that a branch cannot be taken, and Vox proves
   it: the facts on the path to it must be contradictory. [head] takes only
   nonempty lists, so its [[]] case is impossible. In [last], nothing rules
   out the empty list. *)

let (head @ total) (xs : {xs : int list | not (xs === [])}) : int =
  match xs with x :: _ -> x | [] -> unreachable_ ()

let (sign @ total) (x : int) : int =
  if x > 0 then 1 else if x < 0 then -1 else if x = 0 then 0 else unreachable_ ()

let (last @ total) (xs : int list) : int =
  match List.rev xs with x :: _ -> x | [] -> unreachable_ ()
