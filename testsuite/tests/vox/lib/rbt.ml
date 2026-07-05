(* Implementation of rbt.mli, checked against its interface's model
   (the .cmi carries the definitions and theorems; no local prelude,
   no assumption).  [balance] and [ins] are proved to build exactly
   the model trees; [add] blackens the root and inherits the RB
   invariant and the membership characterisation from the sealed
   theorems; [mem] decides whole-tree membership on one path. *)

type color = Red | Black
type tree = Leaf | Node of color * tree * int * tree
type set = tree{ rb _ }

let empty : set{ _ = Leaf } = Leaf

let rec mem (x : int) (t : set) : bool{ _ = mem x t } =
  match t with
  | Leaf -> false
  | Node (_, l, y, r) ->
    if x = y then true
    else if x < y then mem x l
    else mem x r

(* Okasaki balance in two halves.  Colours are matched EXPLICITLY (as
   their own shallow scrutinees) because vox surfaces a match arm's
   negative information -- "an earlier arm failed" -- only for a
   single-constructor pattern over variables (a nullary colour
   qualifies; a deep [Node (Red, Node (Red, ..), ..)] does not).  With
   the colours pinned positively, the model [balance] reduces to a
   unique case and the arm's result is proved equal to it.  The
   right-rotation cases are factored into [balance_r], whose left
   argument is known conflict-free ([notRR]) at every call, letting the
   checker rule out the left rotations there. *)
let balance_r (l : tree) (x : int) (r : tree)
  : tree{ _ = balanceR Black l x r } =
  match r with
  | Leaf -> Node (Black, l, x, r)
  | Node (Black, _, _, _) -> Node (Black, l, x, r)
  | Node (Red, rl, xr, rr) ->
    match rl with
    | Node (Red, b, y, cc) ->
      Node (Red, Node (Black, l, x, b), y, Node (Black, cc, xr, rr))
    | Leaf ->
      (match rr with
       | Node (Red, cc, z, d) ->
         Node (Red, Node (Black, l, x, rl), xr, Node (Black, cc, z, d))
       | Leaf -> Node (Black, l, x, r)
       | Node (Black, _, _, _) -> Node (Black, l, x, r))
    | Node (Black, _, _, _) ->
      (match rr with
       | Node (Red, cc, z, d) ->
         Node (Red, Node (Black, l, x, rl), xr, Node (Black, cc, z, d))
       | Leaf -> Node (Black, l, x, r)
       | Node (Black, _, _, _) -> Node (Black, l, x, r))

let balance (c : color) (l : tree) (x : int) (r : tree)
  : tree{ _ = balance c l x r } =
  match c with
  | Red -> Node (Red, l, x, r)
  | Black ->
    match l with
    | Leaf -> balance_r l x r
    | Node (Black, _, _, _) -> balance_r l x r
    | Node (Red, ll, xl, lr) ->
      match ll with
      | Node (Red, a, xa, b) ->
        Node (Red, Node (Black, a, xa, b), xl, Node (Black, lr, x, r))
      | Leaf ->
        (match lr with
         | Node (Red, b, y, cc) ->
           Node (Red, Node (Black, ll, xl, b), y, Node (Black, cc, x, r))
         | Leaf -> balance_r l x r
         | Node (Black, _, _, _) -> balance_r l x r)
      | Node (Black, _, _, _) ->
        (match lr with
         | Node (Red, b, y, cc) ->
           Node (Red, Node (Black, ll, xl, b), y, Node (Black, cc, x, r))
         | Leaf -> balance_r l x r
         | Node (Black, _, _, _) -> balance_r l x r)

let rec ins (x : int) (t : tree) : tree{ _ = ins x t } =
  match t with
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (c, l, y, r) ->
    if x < y then begin
      let l' = ins x l in
      balance c l' y r
    end
    else if y < x then begin
      let r' = ins x r in
      balance c l y r'
    end
    else Node (c, l, y, r)

let paint_black (t : tree) : tree{ _ = paintBlack t } =
  match t with
  | Leaf -> Leaf
  | Node (_, l, x, r) -> Node (Black, l, x, r)

let add (x : int) (t : set) : set{ _ = add x t && mem x _ } =
  let t' = ins x t in
  paint_black t'

