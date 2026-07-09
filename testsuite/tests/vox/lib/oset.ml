(* Pays oset.mli's obligations over the CONCRETE representation: in
   this unit the interface's opaque sort Vox_Oset_t is the real
   inductive (the concrete declaration registers under the SAME solver
   name), so the sealed axioms land on it and [isDefEq] matches the
   payments with no coupling machinery.  Everything below the defs --
   the ordering helpers and the one-path search bridge -- stays
   private to the implementation. *)

type t =
  | Leaf
  | Node of t * int * t

[%%vox.lean {lean|
@[grind] def mem : Int -> Vox_Oset_t -> Prop
  | _, .Leaf => False
  | x, .Node l v r => x = v ∨ mem x l ∨ mem x r

@[grind] def all_lt : Vox_Oset_t -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v < b ∧ all_lt l b ∧ all_lt r b

@[grind] def all_gt : Vox_Oset_t -> Int -> Prop
  | .Leaf, _ => True
  | .Node l v r, b => v > b ∧ all_gt l b ∧ all_gt r b

@[grind] def bst : Vox_Oset_t -> Prop
  | .Leaf => True
  | .Node l v r => all_lt l v ∧ all_gt r v ∧ bst l ∧ bst r

@[grind] def no_mem (t : Vox_Oset_t) : Prop := ∀ x, ¬ mem x t

@[grind] def insert : Int -> Vox_Oset_t -> Vox_Oset_t
  | x, .Leaf => .Node .Leaf x .Leaf
  | x, .Node l v r =>
      if x = v then .Node l v r
      else if x < v then .Node (insert x l) v r
      else .Node l v (insert x r)

theorem not_mem_lt (x b : Int) (t : Vox_Oset_t)
    (h : all_lt t b) (hx : b <= x) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_lt => mem x t, all_lt t b

theorem not_mem_gt (x b : Int) (t : Vox_Oset_t)
    (h : all_gt t b) (hx : x <= b) : ¬ mem x t := by
  induction t <;> grind
grind_pattern not_mem_gt => mem x t, all_gt t b

theorem all_lt_insert (x b : Int) (t : Vox_Oset_t)
    (h : all_lt t b) (hx : x < b) : all_lt (insert x t) b := by
  induction t <;> grind
grind_pattern all_lt_insert => all_lt (insert x t) b

theorem all_gt_insert (x b : Int) (t : Vox_Oset_t)
    (h : all_gt t b) (hx : b < x) : all_gt (insert x t) b := by
  induction t <;> grind
grind_pattern all_gt_insert => all_gt (insert x t) b

theorem no_mem_spec (x : Int) (t : Vox_Oset_t)
    (h : no_mem t) : ¬ mem x t := by grind

theorem bst_insert (x : Int) (t : Vox_Oset_t)
    (h : bst t) : bst (insert x t) := by
  induction t <;> grind
grind_pattern bst_insert => bst (insert x t)

theorem mem_insert (x y : Int) (t : Vox_Oset_t) :
    mem y (insert x t) ↔ (y = x ∨ mem y t) := by
  induction t <;> grind
grind_pattern mem_insert => mem y (insert x t)
|lean}]

type set = t{ bst _ }

let empty : set{ no_mem _ } = Leaf

let rec member : (x : int) -> (t : set) -> bool{ _ = mem x t } =
  fun x t ->
    match t with
    | Leaf -> false
    | Node (l, v, r) ->
      if x = v then true
      else if x < v then member x l
      else member x r

let rec insert : (x : int) -> (t : set) -> set{ _ = insert x t } =
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
