(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Verified imperative tree flip, PROPHECY EDITION: the flip works on
   a LOAN of a tree slot; children are borrowed IN PLACE (reborrows
   into the loan's target), dataflow is mutation only, and the logic
   flows through tree-valued prophecies resolved at drops.  flip's
   contract is a resolution promise: tfin m = mirror (tnow m). *)

type tree =
  | Leaf
  | Node of tree * int * tree

[%%vox.lean {lean|
@[grind] def mirror : Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree
  | .Leaf => .Leaf
  | .Node l v r => .Node (mirror r) v (mirror l)
@[grind] def isnode : Vox_Lean_flip_proph_tree -> Prop
  | .Leaf => False
  | .Node _ _ _ => True
@[grind] def lsub : Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree
  | .Leaf => .Leaf
  | .Node l _ _ => l
@[grind] def rsub : Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree
  | .Leaf => .Leaf
  | .Node _ _ r => r
@[grind] def nval : Vox_Lean_flip_proph_tree -> Int
  | .Leaf => 0
  | .Node _ v _ => v
@[grind] def plugl : Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree
  | .Leaf, _ => .Leaf
  | .Node _ v r, c => .Node c v r
@[grind] def plugr : Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree
  | .Leaf, _ => .Leaf
  | .Node l v _, c => .Node l v c
@[grind] def swapc : Vox_Lean_flip_proph_tree -> Vox_Lean_flip_proph_tree
  | .Leaf => .Leaf
  | .Node l v r => .Node r v l
axiom model : VoxU -> Vox_Lean_flip_proph_tree
axiom tnow : VoxU -> Vox_Lean_flip_proph_tree
axiom tfin : VoxU -> Vox_Lean_flip_proph_tree
axiom tpv : VoxU -> Vox_Lean_flip_proph_tree
@[grind] theorem plugl_isnode :
    forall t c, isnode t -> isnode (plugl t c) := by
  intro t c h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [plugl, isnode]
@[grind] theorem plugr_isnode :
    forall t c, isnode t -> isnode (plugr t c) := by
  intro t c h
  cases t with
  | Leaf => simp [isnode] at h
  | Node l v r => simp [plugr, isnode]
@[grind] theorem rsub_plugl : forall t c, rsub (plugl t c) = rsub t := by
  intro t c
  cases t <;> simp [plugl, rsub]
@[grind] theorem lsub_leaf : forall t, ¬ isnode t -> t = .Leaf := by
  intro t h
  cases t with
  | Leaf => rfl
  | Node l v r => simp [isnode] at h
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

module P : sig
  type vtree
  type tproph
  type tmut

  val of_pure : (t : tree) -> vtree{ model _ = t } @ unique
  val to_pure : (h : vtree) @ unique -> tree{ _ = model h }
  val new_tproph : unit -> tproph @ unique

  (* borrow the whole owned tree: the residual's model is the
     prophecy *)
  val borrow_tree :
    (p : tproph) @ unique -> (h : vtree) @ unique ->
    ((m : tmut{ tnow _ = model h && tfin _ = tpv p }) @ local unique
      -> 'b @ unique) @ once local ->
    (vtree{ model _ = tpv p } * 'b) @ unique

  val tis_node :
    (m : tmut) @ local unique ->
    (bool{ _ = isnode (tnow m) }
     * tmut{ tnow _ = tnow m && tfin _ = tfin m }) @ local unique

  (* REBORROW the left child slot of a loan: dataflow by mutation --
     the child never leaves the tree; the parent residual's NOW is
     updated with the child's prophecy *)
  val borrow_left :
    (p : tproph) @ unique -> (m : tmut{ isnode (tnow _) }) @ local unique ->
    ((c : tmut{ tnow _ = lsub (tnow m) && tfin _ = tpv p }) @ local unique
      -> 'b @ unique) @ once local ->
    (tmut{ tnow _ = plugl (tnow m) (tpv p) && tfin _ = tfin m } * 'b)
      @ local unique

  val borrow_right :
    (p : tproph) @ unique -> (m : tmut{ isnode (tnow _) }) @ local unique ->
    ((c : tmut{ tnow _ = rsub (tnow m) && tfin _ = tpv p }) @ local unique
      -> 'b @ unique) @ once local ->
    (tmut{ tnow _ = plugr (tnow m) (tpv p) && tfin _ = tfin m } * 'b)
      @ local unique

  (* swap the child pointers through the loan *)
  val tswap_kids :
    (m : tmut{ isnode (tnow _) }) @ local unique ->
    tmut{ tnow _ = swapc (tnow m) && tfin _ = tfin m } @ local unique

  (* resolve the prophecy: the loan's final value is its current one *)
  val tdrop : (m : tmut) @ local unique -> unit{ tfin m = tnow m }
end = struct
  type vtree = { mutable sh : shape }
  and shape =
    | SL
    | SN of vtree * int * vtree

  type tproph = TP of { u : unit }
  type tmut = TM of { global_ tgt : vtree }

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

  let new_tproph : unit -> tproph @ unique =
    fun () -> Obj.magic_unique (TP { u = () })

  let borrow_tree :
    (p : tproph) @ unique -> (h : vtree) @ unique ->
    ((m : tmut{ tnow _ = model h && tfin _ = tpv p }) @ local unique
      -> 'b @ unique) @ once local ->
    (vtree{ model _ = tpv p } * 'b) @ unique =
    fun p h k ->
      let (TP _) = p in
      let m0 =
        (assume_unchecked_ (Obj.magic_unique (TM { tgt = h }))
          : tmut{ tnow _ = model h && tfin _ = tpv p })
      in
      let b = k m0 in
      Obj.magic_unique
        ((assume_unchecked_ (Obj.magic h) : vtree{ model _ = tpv p }), b)

  let tis_node :
    (m : tmut) @ local unique ->
    (bool{ _ = isnode (tnow m) }
     * tmut{ tnow _ = tnow m && tfin _ = tfin m }) @ local unique =
    fun m ->
      let (TM { tgt }) = m in
      let b = (match tgt.sh with SL -> false | SN _ -> true) in
      exclave_
        (Obj.magic_unique
           ((assume_unchecked_ b : bool{ _ = isnode (tnow m) }),
            (assume_unchecked_ (TM { tgt })
              : tmut{ tnow _ = tnow m && tfin _ = tfin m })))

  let borrow_left :
    (p : tproph) @ unique -> (m : tmut{ isnode (tnow _) }) @ local unique ->
    ((c : tmut{ tnow _ = lsub (tnow m) && tfin _ = tpv p }) @ local unique
      -> 'b @ unique) @ once local ->
    (tmut{ tnow _ = plugl (tnow m) (tpv p) && tfin _ = tfin m } * 'b)
      @ local unique =
    fun p m k ->
      let (TP _) = p in
      let (TM { tgt }) = m in
      (match tgt.sh with
       | SN (l, _, _) ->
         let c0 =
           (assume_unchecked_ (Obj.magic_unique (TM { tgt = l }))
             : tmut{ tnow _ = lsub (tnow m) && tfin _ = tpv p })
         in
         let b = k c0 in
         exclave_
           (Obj.magic_unique
              ((assume_unchecked_ (TM { tgt })
                 : tmut{ tnow _ = plugl (tnow m) (tpv p)
                      && tfin _ = tfin m }),
               b))
       | SL -> failwith "borrow_left: leaf (contract-unreachable)")

  let borrow_right :
    (p : tproph) @ unique -> (m : tmut{ isnode (tnow _) }) @ local unique ->
    ((c : tmut{ tnow _ = rsub (tnow m) && tfin _ = tpv p }) @ local unique
      -> 'b @ unique) @ once local ->
    (tmut{ tnow _ = plugr (tnow m) (tpv p) && tfin _ = tfin m } * 'b)
      @ local unique =
    fun p m k ->
      let (TP _) = p in
      let (TM { tgt }) = m in
      (match tgt.sh with
       | SN (_, _, r) ->
         let c0 =
           (assume_unchecked_ (Obj.magic_unique (TM { tgt = r }))
             : tmut{ tnow _ = rsub (tnow m) && tfin _ = tpv p })
         in
         let b = k c0 in
         exclave_
           (Obj.magic_unique
              ((assume_unchecked_ (TM { tgt })
                 : tmut{ tnow _ = plugr (tnow m) (tpv p)
                      && tfin _ = tfin m }),
               b))
       | SL -> failwith "borrow_right: leaf (contract-unreachable)")

  let tswap_kids :
    (m : tmut{ isnode (tnow _) }) @ local unique ->
    tmut{ tnow _ = swapc (tnow m) && tfin _ = tfin m } @ local unique =
    fun m ->
      let (TM { tgt }) = m in
      (match tgt.sh with
       | SN (l, v, r) ->
         tgt.sh <- SN (r, v, l);
         exclave_
           (Obj.magic_unique
              (assume_unchecked_ (TM { tgt })
                : tmut{ tnow _ = swapc (tnow m) && tfin _ = tfin m }))
       | SL -> failwith "tswap_kids: leaf (contract-unreachable)")

  let tdrop : (m : tmut) @ local unique -> unit{ tfin m = tnow m } =
    fun m ->
      let (TM _) = m in
      assume_unchecked_ ()
end

open P

(* The verified flip over a LOAN: mutation-only dataflow, prophecy
   logic; each recursive call is the induction hypothesis, phrased as
   a resolution promise. *)
let rec flip : (m : tmut) @ local unique -> unit{ tfin m = mirror (tnow m) } =
  fun m ->
  let (b, m1) = tis_node m in
  if b
  then (
    let pl = new_tproph () in
    let (m2, ul) = borrow_left pl m1 (fun cl ->
      let _u = flip cl in
      (() : unit{ tpv pl = mirror (lsub (tnow m1)) }))
    in
    ignore ul;
    let pr = new_tproph () in
    let (m3, ur) = borrow_right pr m2 (fun cr ->
      let _u = flip cr in
      (() : unit{ tpv pr = mirror (rsub (tnow m2)) }))
    in
    ignore ur;
    let m4 = tswap_kids m3 in
    let u = tdrop m4 in
    u)
  else (
    let u = tdrop m1 in
    u)

(* Flip twice through two whole-tree borrows: provably the identity. *)
let roundtrip : (t : tree) -> tree{ _ = t } =
  fun t ->
  let h = of_pure t in
  let p1 = new_tproph () in
  let (h1, u1) = borrow_tree p1 h (fun m ->
    let _u = flip m in
    (() : unit{ tpv p1 = mirror t }))
  in
  ignore u1;
  let p2 = new_tproph () in
  let (h2, u2) = borrow_tree p2 h1 (fun m ->
    let _u = flip m in
    (() : unit{ tpv p2 = mirror (tpv p1) }))
  in
  ignore u2;
  let r = to_pure h2 in
  r
