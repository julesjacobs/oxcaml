(* The mutable twin of [decreases_tot_record.ml], which stays refused.

   [left] can be assigned, so the value the pattern binds need not be a part
   of the value matched at all: write the value into its own field and the
   recursion is a loop.  A mutable field therefore gives no descent, for the
   same reason an array element gives none.

   The refusal is a mode one rather than a message of its own, because the
   field's mutability is visible where the shape is read: the binding is left
   partial, and it is asking for it to be total that fails. *)
let (expects_total @ total) (f @ total) = f

type tree =
  | Leaf
  | Node of { mutable left : tree; right : tree }

let rec size (t : tree) : int =
  match t with
  | Leaf -> 1
  | Node { left; right } -> size left + size right

let total_use () = expects_total size
