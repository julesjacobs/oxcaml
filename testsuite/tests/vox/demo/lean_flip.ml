(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Verified imperative tree flip: a mutable tree with a pure ghost
   MODEL; flip swaps child pointers in place, recursively, and its
   dependent signature model _ = mirror (model h) makes each
   recursive call the induction hypothesis.  Flipping twice provably
   returns the original tree (mirror_mirror is PROVED in the prelude
   by induction -- no new trust). *)

type tree =
  | Leaf
  | Node of tree * int * tree

[%%vox.lean {lean|
@[grind] def mirror : Vox_Lean_flip_tree -> Vox_Lean_flip_tree
  | .Leaf => .Leaf
  | .Node l v r => .Node (mirror r) v (mirror l)
@[grind] def isnode : Vox_Lean_flip_tree -> Prop
  | .Leaf => False
  | .Node _ _ _ => True
@[grind] def lsub : Vox_Lean_flip_tree -> Vox_Lean_flip_tree
  | .Leaf => .Leaf
  | .Node l _ _ => l
@[grind] def rsub : Vox_Lean_flip_tree -> Vox_Lean_flip_tree
  | .Leaf => .Leaf
  | .Node _ _ r => r
@[grind] def nval : Vox_Lean_flip_tree -> Int
  | .Leaf => 0
  | .Node _ v _ => v
@[grind] def plugl : Vox_Lean_flip_tree -> Vox_Lean_flip_tree -> Vox_Lean_flip_tree
  | .Leaf, _ => .Leaf
  | .Node _ v r, c => .Node c v r
@[grind] def plugr : Vox_Lean_flip_tree -> Vox_Lean_flip_tree -> Vox_Lean_flip_tree
  | .Leaf, _ => .Leaf
  | .Node l v _, c => .Node l v c
@[grind] def swapc : Vox_Lean_flip_tree -> Vox_Lean_flip_tree
  | .Leaf => .Leaf
  | .Node l v r => .Node r v l
axiom model : VoxU -> Vox_Lean_flip_tree
axiom hml : VoxU -> Vox_Lean_flip_tree
axiom hmr : VoxU -> Vox_Lean_flip_tree
@[grind] theorem node_eta :
    forall t, isnode t -> t = .Node (lsub t) (nval t) (rsub t) := by
  intro t h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [lsub, nval, rsub]
@[grind] theorem leaf_eta : forall t, ¬ isnode t -> t = .Leaf := by
  intro t h
  cases t with
  | Leaf => rfl
  | Node l v r => simp [isnode] at h
@[grind] theorem plugl_isnode : forall t c, isnode t -> isnode (plugl t c) := by
  intro t c h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [plugl, isnode]
@[grind] theorem plugr_isnode : forall t c, isnode t -> isnode (plugr t c) := by
  intro t c h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [plugr, isnode]
@[grind] theorem rsub_plugl : forall t c, rsub (plugl t c) = rsub t := by
  intro t c
  cases t <;> simp [plugl, rsub]
@[grind] theorem flip_step :
    forall t c d, isnode t ->
      swapc (plugr (plugl t c) d) = .Node d (nval t) c := by
  intro t c d h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [plugl, plugr, swapc, nval]
@[grind] theorem mirror_step :
    forall t, isnode t ->
      mirror t = .Node (mirror (rsub t)) (nval t) (mirror (lsub t)) := by
  intro t h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [mirror, rsub, nval, lsub]
@[grind] theorem mirror_mirror : forall t, mirror (mirror t) = t := by
  intro t
  induction t with
  | Leaf => simp [mirror]
  | Node l v r ihl ihr => simp [mirror, ihl, ihr]
|lean}]

module T : sig
  type vtree
  type lhole
  type rhole

  val of_pure : (t : tree) -> vtree{ model _ = t } @ unique
  val to_pure : (h : vtree) @ unique -> tree{ _ = model h }

  val is_node :
    (h : vtree) @ unique ->
    (bool{ _ = isnode (model h) } * vtree{ model _ = model h }) @ unique

  val take_left :
    (h : vtree{ isnode (model _) }) @ unique ->
    (vtree{ model _ = lsub (model h) } * lhole{ hml _ = model h }) @ unique
  val put_left :
    (k : lhole) @ unique -> (c : vtree) @ unique ->
    vtree{ model _ = plugl (hml k) (model c) } @ unique

  val take_right :
    (h : vtree{ isnode (model _) }) @ unique ->
    (vtree{ model _ = rsub (model h) } * rhole{ hmr _ = model h }) @ unique
  val put_right :
    (k : rhole) @ unique -> (c : vtree) @ unique ->
    vtree{ model _ = plugr (hmr k) (model c) } @ unique

  val swap_kids :
    (h : vtree{ isnode (model _) }) @ unique ->
    vtree{ model _ = swapc (model h) } @ unique
end = struct
  type vtree = { mutable sh : shape }
  and shape =
    | SL
    | SN of vtree * int * vtree

  type lhole = L of { global_ node : vtree }
  type rhole = R of { global_ node : vtree }

  let rec build (t : tree) : vtree =
    match t with
    | Leaf -> { sh = SL }
    | Node (l, v, r) -> { sh = SN (build l, v, build r) }

  let rec harvest (h : vtree) : tree =
    match h.sh with
    | SL -> Leaf
    | SN (l, v, r) -> Node (harvest l, v, harvest r)

  let of_pure : (t : tree) -> vtree{ model _ = t } @ unique =
    fun t -> assume_unchecked_ (Obj.magic_unique (build t))

  let to_pure : (h : vtree) @ unique -> tree{ _ = model h } =
    fun h -> assume_unchecked_ (harvest h)

  let is_node :
    (h : vtree) @ unique ->
    (bool{ _ = isnode (model h) } * vtree{ model _ = model h }) @ unique =
    fun h ->
      let b = (match h.sh with SL -> false | SN _ -> true) in
      Obj.magic_unique
        ((assume_unchecked_ b : bool{ _ = isnode (model h) }),
         (assume_unchecked_ (Obj.magic h) : vtree{ model _ = model h }))

  let take_left :
    (h : vtree{ isnode (model _) }) @ unique ->
    (vtree{ model _ = lsub (model h) } * lhole{ hml _ = model h }) @ unique =
    fun h ->
      (match h.sh with
       | SN (l, v, r) ->
         h.sh <- SN ({ sh = SL }, v, r);
         Obj.magic_unique
           ((assume_unchecked_ l : vtree{ model _ = lsub (model h) }),
            (assume_unchecked_ (L { node = h }) : lhole{ hml _ = model h }))
       | SL -> failwith "take_left: leaf (contract-unreachable)")

  let put_left :
    (k : lhole) @ unique -> (c : vtree) @ unique ->
    vtree{ model _ = plugl (hml k) (model c) } @ unique =
    fun k c ->
      let (L { node }) = k in
      (match node.sh with
       | SN (_, v, r) ->
         node.sh <- SN (c, v, r);
         (assume_unchecked_ (Obj.magic_unique (Obj.magic node))
           : vtree{ model _ = plugl (hml k) (model c) })
       | SL -> failwith "put_left: leaf (contract-unreachable)")

  let take_right :
    (h : vtree{ isnode (model _) }) @ unique ->
    (vtree{ model _ = rsub (model h) } * rhole{ hmr _ = model h }) @ unique =
    fun h ->
      (match h.sh with
       | SN (l, v, r) ->
         h.sh <- SN (l, v, { sh = SL });
         Obj.magic_unique
           ((assume_unchecked_ r : vtree{ model _ = rsub (model h) }),
            (assume_unchecked_ (R { node = h }) : rhole{ hmr _ = model h }))
       | SL -> failwith "take_right: leaf (contract-unreachable)")

  let put_right :
    (k : rhole) @ unique -> (c : vtree) @ unique ->
    vtree{ model _ = plugr (hmr k) (model c) } @ unique =
    fun k c ->
      let (R { node }) = k in
      (match node.sh with
       | SN (l, v, _) ->
         node.sh <- SN (l, v, c);
         (assume_unchecked_ (Obj.magic_unique (Obj.magic node))
           : vtree{ model _ = plugr (hmr k) (model c) })
       | SL -> failwith "put_right: leaf (contract-unreachable)")

  let swap_kids :
    (h : vtree{ isnode (model _) }) @ unique ->
    vtree{ model _ = swapc (model h) } @ unique =
    fun h ->
      (match h.sh with
       | SN (l, v, r) ->
         h.sh <- SN (r, v, l);
         (assume_unchecked_ (Obj.magic_unique (Obj.magic h))
           : vtree{ model _ = swapc (model h) })
       | SL -> failwith "swap_kids: leaf (contract-unreachable)")
end

open T

(* The VERIFIED imperative flip: in-place pointer swaps, one node at a
   time; the recursive calls are the induction hypotheses. *)
let rec flip : (h : vtree) @ unique -> vtree{ model _ = mirror (model h) } @ unique =
  fun h ->
  let (b, h1) = is_node h in
  if b
  then (
    let (l, k) = take_left h1 in
    let l' = flip l in
    let h2 = put_left k l' in
    let (r, k2) = take_right h2 in
    let r' = flip r in
    let h3 = put_right k2 r' in
    let h4 = swap_kids h3 in
    h4)
  else h1

(* Flip twice: provably the tree you started with. *)
let roundtrip : (t : tree) -> tree{ _ = t } =
  fun t ->
  let h = of_pure t in
  let h1 = flip h in
  let h2 = flip h1 in
  let r = to_pure h2 in
  r
