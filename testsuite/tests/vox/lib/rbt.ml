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

(* Okasaki balance in its NATURAL four-rotation-plus-fall-through form,
   matched as a 4-tuple exactly like the model.  The earlier arms leave
   the subtree corners (the [a]/[b]/[c]/[d] and pivots) as variables, so
   the model [balance] overlaps their cases; each later arm learns the
   deep-pattern NEGATIVE of every guard-free earlier arm
   ([not (c = Black && exists f.., l = Node (Red, Node (Red, f..), f, f))]),
   and its VC discharges by splitting the model match and refuting the
   overlapping case with that negative.  No explicit colour scrutinees or
   [balance_r] factoring is needed. *)
let balance (c : color) (l : tree) (x : int) (r : tree)
  : tree{ _ = balance c l x r } =
  match c, l, x, r with
  | Black, Node (Red, Node (Red, a, xa, b), y, cc), z, d ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, cc, z, d))
  | Black, Node (Red, a, xa, Node (Red, b, y, cc)), z, d ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, cc, z, d))
  | Black, a, xa, Node (Red, Node (Red, b, y, cc), z, d) ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, cc, z, d))
  | Black, a, xa, Node (Red, b, y, Node (Red, c, z, d)) ->
    Node (Red, Node (Black, a, xa, b), y, Node (Black, c, z, d))
  | c, l, x, r -> Node (c, l, x, r)

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
