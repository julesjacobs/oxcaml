(* Each branch introduces the scrutinee's own components as facts.  Before the
   filter these arrived accompanied by [x = x] entries that hold at every
   instantiation and so constrain nothing. *)
type tree =
  | Leaf
  | Node of tree * int * tree

let rec depth (t : tree) : int{ _ >= 0 } =
  match t with
  | Leaf -> 0
  | Node (left, key, right) ->
    ignore key;
    let dl = depth left in
    let dr = depth right in
    if dl >= dr then dl + 1 else dr + 1

let root_or (t : tree) (default : int) : int =
  match t with
  | Leaf -> default
  | Node (left, key, right) ->
    ignore left;
    ignore right;
    key
