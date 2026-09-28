(* Ghost code is checked, then erased from the compiled program.
   - [ghost_ e] is type-checked (and must be total) but never evaluated.
   - A record field marked [@@ ghost] occupies no memory.
   A summary carries, as ghost data, the list it is the sum of; the proofs
   use it, the program does not. Show the erased program to see the Lambda
   code: the [sum_def] calls and the [items] field are gone ([items] became
   an empty unboxed product, and [ghost_] a placeholder). *)

let[@def] rec (sum @ total) (xs : int list) : int =
  match xs with [] -> 0 | x :: rest -> x + sum rest

type summary = { total : int; items : int list @@ ghost }

let (empty @ total) () : {s : summary | s.total = sum s.items} =
  ghost_ (sum_def []);
  { total = 0; items = ghost_ [] }

let (add @ total) (x : int) (s : {s : summary | s.total = sum s.items}) :
    {r : summary | r.total = sum r.items} =
  ghost_ (sum_def (x :: s.items));
  { total = x + s.total; items = ghost_ (x :: s.items) }
