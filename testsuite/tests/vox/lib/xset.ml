type iset [@@vox.sort lean "ISet"]
type tree = Leaf | Node of tree * int * tree
type t = tree{ bst _ } [@vox.via (elems : iset)]

[%%vox.lean {lean|
@[expose] def ISet := Int -> Prop

@[grind] def mem (x : Int) (s : ISet) : Prop := s x
@[grind] def emp : ISet := fun _ => False
@[grind] def ins (x : Int) (s : ISet) : ISet := fun y => y = x ∨ s y
@[grind] def uni (a b : ISet) : ISet := fun y => a y ∨ b y

@[grind] theorem mem_emp (x : Int) : ¬ mem x emp := by grind
@[grind] theorem mem_ins_iff (x y : Int) (s : ISet) :
    mem x (ins y s) ↔ (x = y ∨ mem x s) := by grind
@[grind] theorem mem_uni (x : Int) (a b : ISet) :
    mem x (uni a b) ↔ (mem x a ∨ mem x b) := by grind
@[grind] theorem mem_ins (x : Int) (s : ISet) : mem x (ins x s) := by grind

theorem iset_ext (a b : ISet) (h : ∀ x, mem x a ↔ mem x b) : a = b := by
  funext x
  have hx : a x ↔ b x := h x
  exact propext hx
@[grind] theorem ins_idem (x : Int) (s : ISet) : ins x (ins x s) = ins x s :=
  iset_ext _ _ (by grind)
@[grind] theorem uni_emp (s : ISet) : uni s emp = s := iset_ext _ _ (by grind)

-- The representation: a binary tree whose set of elements is the UNION
-- over both subtrees (a bona fide set, not the left-spine simplification
-- of via_set).  [elems] is the abstraction function into [Int -> Prop].
@[grind] def bst : Vox_Xset_tree -> Prop
  | .Leaf => True
  | .Node l _ _ => bst l

@[grind] def elems : Vox_Xset_tree -> ISet
  | .Leaf => emp
  | .Node l v r => ins v (uni (elems l) (elems r))

-- A Bool membership test on the tree, bridged to model membership so
-- the recursive [member] can discharge its spec.
@[grind] def tmem (x : Int) : Vox_Xset_tree -> Bool
  | .Leaf => false
  | .Node l v r => if x = v then true else (tmem x l || tmem x r)

@[grind] theorem tmem_elems (x : Int) (u : Vox_Xset_tree) :
    (tmem x u = true) = mem x (elems u) := by
  induction u <;> grind
grind_pattern tmem_elems => mem x (elems u)
|lean}]

(* Honest proof (no [assume_unchecked_]): unpack the image binder to its
   tree with the link [elems t0 = s], rebuild, and the image-vocab
   contract [elems (Node ..) = ins x s] discharges through the link, the
   membership-commutation lemmas, and [uni_emp]. *)
let add : (x : int) -> (s : t) -> t{ _ = ins x s } =
  fun x s ->
    let refine_ t0 = s in
    (Node (t0, x, Leaf) : t{ _ = ins x s })

(* [member] recurses over both subtrees; each recursive result is
   let-bound so its spec fact ([bl = mem x (elems l)]) is recorded at a
   name (a refined-bool used as a bare [if] condition does not thread its
   fact into the branch). *)
let member : (x : int) -> (s : t) -> bool{ _ = mem x s } =
  fun x s ->
    let refine_ t0 = s in
    let rec go : (u : tree) -> bool{ _ = mem x (elems u) } =
      fun u ->
        match u with
        | Leaf -> false
        | Node (l, v, r) ->
          if x = v then true
          else
            let bl = go l in
            if bl then true else go r
    in
    go t0
