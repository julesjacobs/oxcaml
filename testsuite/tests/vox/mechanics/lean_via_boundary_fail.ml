(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* STAGE 3 fail-closed: an interface OVERCLAIM over a via manifest is
   rejected at the implementation's VC.  Under image-binder the honest
   boundary PROVES (see lean_via_seal.ml); here the sealed spec claims
   [add] returns [ins x (ins x s)] (two inserts) but the implementation
   builds a single node, so the VC [elems (Node (t0,x,Leaf)) = ins x
   (ins x s)] reduces to [cons x s = cons x (cons x s)] and the solver
   rejects it -- fail-closed, never a silent pass. *)

type tree = Leaf | Node of tree * int * tree
type iset [@@vox.sort lean "ISet"]

[%%vox.lean {lean|
inductive ISet where
  | nil : ISet
  | cons : Int -> ISet -> ISet

@[grind] def ins (x : Int) (s : ISet) : ISet := ISet.cons x s

@[grind] def bst : Vox_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l

@[grind] def elems : Vox_tree -> ISet
  | .Leaf => .nil
  | .Node l v _ => .cons v (elems l)
|lean}]

module M : sig
  type t : value refines (iset)
  val add : (x : int) -> (s : t) -> t{ _ = ins x (ins x s) }
end = struct
  type t = tree{ bst _ } [@vox.via (elems : iset)]
  let add : (x : int) -> (s : t) -> t{ _ = ins x (ins x s) } =
    fun x s ->
      let refine_ t0 = s in
      (Node (t0, x, Leaf) : t{ _ = ins x (ins x s) })
end
[%%expect{|
type tree = Leaf | Node of tree * int * tree
type iset
Line 28, characters 7-25:
28 |       (Node (t0, x, Leaf) : t{ _ = ins x (ins x s) })
            ^^^^^^^^^^^^^^^^^^
Error: vox: verification failed (lean).
       Goal: bst (Node (t0, x, Leaf)) && elems (Node (t0, x, Leaf)) = ins x (ins x s)
Hypotheses:
  bst t0
  elems t0 = s
(lean: error: `grind` failed)
|}]
