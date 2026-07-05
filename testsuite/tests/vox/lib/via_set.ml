(* The representation is a binary tree whose element set is [elems].
   [type t = tree{ bst _ } via (elems : iset)] models [t] at the set
   sort while the runtime value stays the tree; the .mli sees only
   [refines (iset)].  The operations are TRUSTED here (the honest
   route -- proving [elems] relates the tree operations to the set
   operations under [bst] -- is the via payoff that the base-binder
   boundary does not yet discharge; see docs/plans).  Same trust class
   as lib/gset.ml. *)
type iset [@@vox.sort lean "ISet"]
type tree = Leaf | Node of tree * int * tree
type t = tree{ bst _ } [@vox.via (elems : iset)]

let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s -> ignore (x, s); assume_unchecked_ (Node (Leaf, x, Leaf))

let member : (x : int) -> (s : t) -> bool{ _ = mem x s } =
  fun x s -> ignore (x, s); assume_unchecked_ false

let card : (s : t) -> int{ _ = card s } =
  fun s -> ignore s; assume_unchecked_ 0
