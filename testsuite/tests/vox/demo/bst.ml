(* Implementation of bst.mli, checked against its own interface's
   model (the .cmi carries the definitions and theorems; there is no
   local prelude and no assumption).  Every match arm carries its own
   obligation: [member]'s arms prove that inspecting ONE path decides
   membership in the WHOLE tree, and [insert]'s arms prove the code
   builds exactly the model's tree, from which its interface facts
   follow. *)

type tree =
  | Leaf
  | Node of tree * int * tree

let empty : tree{ _ = Leaf } = Leaf

let rec member : (x : int) -> (t : tree{ bst _ }) -> bool{ _ = mem x t } =
  fun x t ->
    match t with
    | Leaf -> false
    | Node (l, v, r) ->
      if x = v then true
      else if x < v then begin
        let b = member x l in
        b
      end
      else begin
        let b = member x r in
        b
      end

let rec insert
  : (x : int) -> (t : tree{ bst _ }) -> tree{ _ = insert x t && bst _ && mem x _ }
  =
  fun x t ->
    match t with
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) ->
      if x = v then t
      else if x < v then begin
        let l' = insert x l in
        Node (l', v, r)
      end
      else begin
        let r' = insert x r in
        Node (l, v, r')
      end
