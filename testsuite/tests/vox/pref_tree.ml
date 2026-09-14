(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_tree.ml";
 { bytecode; }
 { native; }
*)

module H = Pref.Heap

type node : immutable_data = {
  value : int;
  left : node option Pref.t;
  right : node option Pref.t;
}

type tree = Empty | Branch of node * tree * tree [@@inductive]

let[@def] (root @ total) (tree : tree @ immutable) =
  match tree with Empty -> None | Branch (n, _, _) -> Some n

let[@def] (left_tree @ total) (tree : tree @ immutable) =
  match tree with Empty -> Empty | Branch (_, l, _) -> l

let[@def] (right_tree @ total) (tree : tree @ immutable) =
  match tree with Empty -> Empty | Branch (_, _, r) -> r

let[@def] (links @ total) (n : node @ immutable)
    (l : node option @ immutable) (r : node option @ immutable) =
  ghost_ (H.put (H.put (H.empty ()) n.left l) n.right r)

let[@def] rec (heap @ total) (tree : tree @ immutable) =
  ghost_ (match tree with
  | Empty -> H.empty ()
  | Branch (n, l, r) -> H.union (links n (root l) (root r))
      (H.union (heap l) (heap r)))

let[@def] rec (valid @ total) (tree : tree @ immutable) =
  ghost_ (match tree with
  | Empty -> true
  | Branch (n, l, r) -> valid l && valid r
      && not (n.left === n.right)
      && H.disjoint (links n (root l) (root r))
           (H.union (heap l) (heap r))
      && H.disjoint (heap l) (heap r))

let[@def] rec (flipped @ total) (tree : tree @ immutable) =
  match tree with
  | Empty -> Empty
  | Branch (n, l, r) -> Branch (n, flipped r, flipped l)

let (root_flipped @ total) (tree : tree @ immutable)
    : {u : unit | root (flipped tree) === root tree} =
  let refine_ a = flipped_def tree in
  let f = flipped tree in
  let refine_ b = root_def f in
  let refine_ c = root_def tree in
  let u = () in refine_ u

let set_links (n : node @ immutable)
    (old_l : node option @ immutable ghost)
    (old_r : node option @ immutable ghost)
    (new_l : node option @ immutable)
    (new_r : node option @ immutable)
    (t : {t : node option Pref.token | Pref.own t === links n old_l old_r
      && not (n.left === n.right)} @ unique)
    : {t : node option Pref.token | Pref.own t === links n new_l new_r
        && H.same_domain (Pref.own t) (links n old_l old_r)} @ unique =
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let p = n.left in
  let q = n.right in
  let proof = ghost_ (
    let refine_ a = links_def n old_l old_r in
    let refine_ b = links_def n new_l new_r in
    let e = H.empty () in
    let h = H.put e p old_l in
    let refine_ c = H.commute_law h q old_r p new_l in
    let refine_ d = H.put_law e p old_l new_l in
    let h = H.put e p new_l in
    let refine_ e = H.put_law h q old_r new_r in
    let h1 = H.put before p new_l in
    let h2 = H.put h1 q new_r in
    let refine_ f = H.put_law before p new_l new_l in
    let refine_ g = H.put_law h1 q new_r new_r in
    let refine_ i = H.domain_law h2 h1 before in
    let u = () in
    let result : {u : unit |
      H.put (H.put before p new_l) q new_r === links n new_l new_r
      && H.same_domain (H.put (H.put before p new_l) q new_r) before} =
      refine_ u in result) in
  let refine_ proof = proof in
  let t : {t : node option Pref.token | H.mem (Pref.own t) p} = refine_ t in
  let refine_ t = Pref.write p new_l t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) q} = refine_ t in
  let refine_ t = Pref.write q new_r t in
  refine_ t

let rec mirror :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique ->
    {t : node option Pref.token | Pref.own t === heap (flipped model)
      && valid (flipped model)
      && H.same_domain (Pref.own t) (heap model)} @ unique =
  fun pointer model t ->
  let refine_ t = t in
  let facts = ghost_ (
    let refine_ a = valid_def model in
    let refine_ b = heap_def model in
    let refine_ c = root_def model in
    let refine_ d = flipped_def model in
    let refine_ e = left_tree_def model in
    let refine_ f = right_tree_def model in
    let u = () in
    let facts : {u : unit |
      (match model with
      | Empty -> pointer === None && heap model === H.empty ()
          && flipped model === Empty
      | Branch (n, l, r) -> pointer === Some n
          && left_tree model === l && right_tree model === r
          && flipped model === Branch (n, flipped r, flipped l)
          && valid l && valid r && not (n.left === n.right)
          && heap model === H.union (links n (root l) (root r))
               (H.union (heap l) (heap r))
          && H.disjoint (links n (root l) (root r))
               (H.union (heap l) (heap r))
          && H.disjoint (heap l) (heap r))} = refine_ u in facts) in
  let refine_ facts = facts in
  match pointer with
  | None ->
    let proof = ghost_ (
      let empty = Empty in
      let refine_ a = heap_def empty in
      let h = heap model in
      let refine_ b = H.domain_law h h h in
      let u = () in
      let proof : {u : unit | heap (flipped model) === heap model
        && H.same_domain (heap model) (heap model)} = refine_ u in proof) in
    let refine_ proof = proof in
    refine_ t
  | Some n ->
    let lm = ghost_ (left_tree model) in
    let rm = ghost_ (right_tree model) in
    let lp = ghost_ (root lm) in
    let rp = ghost_ (root rm) in
    let nh = ghost_ (links n lp rp) in
    let lh = ghost_ (heap lm) in
    let rh = ghost_ (heap rm) in
    let ch = ghost_ (H.union lh rh) in
    let proof = ghost_ (
      let refine_ a = H.partition_law nh ch in
      let refine_ b = H.partition_law lh rh in
      let refine_ c = links_def n lp rp in
      let u = () in
      let _links : {u : unit |
        nh === H.put (H.put (H.empty ()) n.left lp) n.right rp} = refine_ u in
      let proof : {u : unit |
        H.restrict (heap model) nh === nh
        && H.exclude (heap model) nh === ch
        && H.restrict ch lh === lh && H.exclude ch lh === rh
        && H.mem nh n.left && H.mem nh n.right
        && H.at nh n.left === Some lp && H.at nh n.right === Some rp} =
        refine_ u in proof) in
    let refine_ proof = proof in
    let refine_ parts = Pref.split nh t in
    let nt = parts.#left in
    let ct = parts.#right in
    let p = n.left in
    let q = n.right in
    let l : {l : node option | l === root lm} =
      let b = borrow_ nt in
      let b : {b : node option Pref.token | H.mem (Pref.own b) p} = refine_ b in
      let refine_ l = Pref.read p b in
      refine_ l in
    let refine_ l = l in
    let r : {r : node option | r === root rm} =
      let b = borrow_ nt in
      let b : {b : node option Pref.token | H.mem (Pref.own b) q} = refine_ b in
      let refine_ r = Pref.read q b in
      refine_ r in
    let refine_ r = r in
    let refine_ children = Pref.split lh ct in
    let lt = children.#left in
    let rt = children.#right in
    let lt : {t : node option Pref.token | valid lm && root lm === l
        && Pref.own t === heap lm} = refine_ lt in
    let refine_ lt = mirror l lm lt in
    let rt : {t : node option Pref.token | valid rm && root rm === r
        && Pref.own t === heap rm} = refine_ rt in
    let refine_ rt = mirror r rm rt in
    let nt : {t : node option Pref.token | Pref.own t === links n lp rp
        && not (n.left === n.right)} = refine_ nt in
    let refine_ nt = set_links n lp rp r l nt in
    let new_nh = ghost_ (Pref.own (borrow_ nt)) in
    let new_lh = ghost_ (Pref.own (borrow_ lt)) in
    let new_rh = ghost_ (Pref.own (borrow_ rt)) in
    let refine_ ct = Pref.join rt lt in
    let refine_ t = Pref.join nt ct in
    let after = ghost_ (Pref.own (borrow_ t)) in
    let proof = ghost_ (
      let f = flipped model in
      let refine_ a = root_flipped lm in
      let refine_ b = root_flipped rm in
      let refine_ c = heap_def f in
      let refine_ validity = valid_def f in
      let nc = H.union new_rh new_lh in
      let rc = H.union rh lh in
      let refine_ d = H.union_domain_law new_rh rh new_lh lh in
      let refine_ e = H.union_law rh lh rh in
      let refine_ g = H.domain_law nc rc ch in
      let refine_ h = H.union_domain_law new_nh nh nc ch in
      let u = () in
      let proof : {u : unit | after === heap (flipped model)
        && valid (flipped model)
        && H.same_domain after (heap model)} = refine_ u in proof) in
    let refine_ proof = proof in
    refine_ t

let mirror_with_frame :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (frame : node option Pref.heap) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && H.disjoint (heap model) frame
      && Pref.own t === H.union (heap model) frame}) @ unique ->
    {t : node option Pref.token | Pref.own t === H.union (heap (flipped model)) frame
      && valid (flipped model)
      && H.disjoint (heap (flipped model)) frame}
      @ unique = fun pointer model frame t ->
  let refine_ t = t in
  let mh = ghost_ (heap model) in
  let proof = ghost_ (H.partition_law mh frame) in
  let refine_ proof = proof in
  let refine_ parts = Pref.split mh t in
  let tree = parts.#left in
  let frame_token = parts.#right in
  let tree : {t : node option Pref.token | valid model && root model === pointer
    && Pref.own t === heap model} = refine_ tree in
  let refine_ tree = mirror pointer model tree in
  let refine_ result = Pref.join tree frame_token in
  refine_ result

type built = {
  pointer : node option @@ aliased;
  model : tree @@ aliased ghost;
  state : node option Pref.token;
}

let empty () : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model && b.model === Empty} @ unique =
  let model = Empty in
  let facts = ghost_ (
    let refine_ a = root_def model in
    let refine_ b = heap_def model in
    let refine_ c = valid_def model in
    let u = () in
    let facts : {u : unit | root model === None && heap model === H.empty ()
        && valid model} = refine_ u in facts) in
  let refine_ facts = facts in
  let refine_ state = Pref.empty () in
  let b = {pointer = None; model; state} in
  refine_ b

let branch (value : int)
    (l : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    (r : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    : {b : built | let refine_ l = l in let refine_ r = r in
      b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && (match b.model with Empty -> false | Branch (n, lm, rm) ->
        n.value = value && lm === l.model && rm === r.model)} @ unique =
  let refine_ l = l in
  let refine_ r = r in
  let lp = l.pointer in
  let rp = r.pointer in
  let lm = l.model in
  let rm = r.model in
  let lt = l.state in
  let rt = r.state in
  let refine_ t = Pref.join lt rt in
  let ch = ghost_ (Pref.own (borrow_ t)) in
  let refine_ a = Pref.alloc lp t in
  let p = a.value in
  let t = a.state in
  let refine_ b = Pref.alloc rp t in
  let q = b.value in
  let state = b.state in
  let n = {value; left = p; right = q} in
  let model = ghost_ (Branch (n, lm, rm)) in
  let proof = ghost_ (
    let refine_ a = root_def model in
    let refine_ b = valid_def model in
    let refine_ c = heap_def model in
    let refine_ d = links_def n lp rp in
    let e = H.empty () in
    let s = H.put e p lp in
    let refine_ f = H.union_law ch e e in
    let refine_ g = H.put_union_law e ch p lp in
    let refine_ h = H.put_union_law s ch q rp in
    let u = () in
    let _contents : {u : unit |
      H.put (H.put ch p lp) q rp === heap model} = refine_ u in
    let _distinct : {u : unit | not (p === q)} = refine_ u in
    let _separate : {u : unit | H.disjoint (links n lp rp) ch} =
      refine_ u in
    let proof : {u : unit | valid model && root model === Some n
      && H.put (H.put ch p lp) q rp === heap model} = refine_ u in proof) in
  let refine_ proof = proof in
  let result = {pointer = Some n; model; state} in
  refine_ result

let leaf (value : int)
    : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique =
  let refine_ l = empty () in
  let refine_ r = empty () in
  let l : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = refine_ l in
  let r : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = refine_ r in
  let refine_ b = branch value l r in
  refine_ b

type shape = Tip | Fork of int * shape * shape

let rec observe :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique -> shape =
  fun pointer model t ->
  let refine_ t = t in
  let facts = ghost_ (
    let refine_ a = valid_def model in
    let refine_ b = heap_def model in
    let refine_ c = root_def model in
    let refine_ d = flipped_def model in
    let refine_ e = left_tree_def model in
    let refine_ f = right_tree_def model in
    let u = () in
    let facts : {u : unit |
      (match model with
      | Empty -> pointer === None && heap model === H.empty ()
          && flipped model === Empty
      | Branch (n, l, r) -> pointer === Some n
          && left_tree model === l && right_tree model === r
          && flipped model === Branch (n, flipped r, flipped l)
          && valid l && valid r && not (n.left === n.right)
          && heap model === H.union (links n (root l) (root r))
               (H.union (heap l) (heap r))
          && H.disjoint (links n (root l) (root r))
               (H.union (heap l) (heap r))
          && H.disjoint (heap l) (heap r))} = refine_ u in facts) in
  let refine_ facts = facts in
  match pointer with
  | None -> Tip
  | Some n ->
    let lm = ghost_ (left_tree model) in
    let rm = ghost_ (right_tree model) in
    let lp = ghost_ (root lm) in
    let rp = ghost_ (root rm) in
    let nh = ghost_ (links n lp rp) in
    let lh = ghost_ (heap lm) in
    let rh = ghost_ (heap rm) in
    let ch = ghost_ (H.union lh rh) in
    let proof = ghost_ (
      let refine_ a = H.partition_law nh ch in
      let refine_ b = H.partition_law lh rh in
      let refine_ c = links_def n lp rp in
      let u = () in
      let _links : {u : unit |
        nh === H.put (H.put (H.empty ()) n.left lp) n.right rp} = refine_ u in
      let proof : {u : unit |
        H.restrict (heap model) nh === nh
        && H.exclude (heap model) nh === ch
        && H.restrict ch lh === lh && H.exclude ch lh === rh
        && H.mem nh n.left && H.mem nh n.right
        && H.at nh n.left === Some lp && H.at nh n.right === Some rp} =
        refine_ u in proof) in
    let refine_ proof = proof in
    let refine_ parts = Pref.split nh t in
    let nt = parts.#left in
    let ct = parts.#right in
    let p = n.left in
    let q = n.right in
    let l : {l : node option | l === root lm} =
      let b = borrow_ nt in
      let b : {b : node option Pref.token | H.mem (Pref.own b) p} = refine_ b in
      let refine_ l = Pref.read p b in
      refine_ l in
    let refine_ l = l in
    let r : {r : node option | r === root rm} =
      let b = borrow_ nt in
      let b : {b : node option Pref.token | H.mem (Pref.own b) q} = refine_ b in
      let refine_ r = Pref.read q b in
      refine_ r in
    let refine_ r = r in
    let refine_ children = Pref.split lh ct in
    let lt = children.#left in
    let rt = children.#right in
    let lt : {t : node option Pref.token | valid lm && root lm === l
      && Pref.own t === heap lm} = refine_ lt in
    let rt : {t : node option Pref.token | valid rm && root rm === r
      && Pref.own t === heap rm} = refine_ rt in
    let left = observe l lm lt in
    let right = observe r rm rt in
    Fork (n.value, left, right)

let () =
  let four = 4 in
  let two = 2 in
  let three = 3 in
  let one = 1 in
  let refine_ ll = leaf four in
  let refine_ lr = empty () in
  let lr : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = refine_ lr in
  let ll : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = refine_ ll in
  let refine_ l = branch two ll lr in
  let refine_ r = leaf three in
  let l : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = refine_ l in
  let r : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = refine_ r in
  let refine_ tree = branch one l r in
  let pointer = tree.pointer in
  let model = tree.model in
  let t = tree.state in
  let refine_ frame_token = Pref.empty () in
  let empty : node option = None in
  let refine_ extra = Pref.alloc empty frame_token in
  let unrelated = extra.value in
  let frame_token = extra.state in
  let frame = ghost_ (Pref.own (borrow_ frame_token)) in
  let refine_ t = Pref.join t frame_token in
  let t : {t : node option Pref.token | valid model && root model === pointer
    && H.disjoint (heap model) frame
    && Pref.own t === H.union (heap model) frame} = refine_ t in
  let refine_ t = mirror_with_frame pointer model frame t in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let result : {v : node option | v === None} =
    let b = borrow_ t in
    let b : {b : node option Pref.token | H.mem (Pref.own b) unrelated} = refine_ b in
    let refine_ result = Pref.read unrelated b in
    refine_ result in
  let refine_ result = result in
  assert (result = None);
  let flipped_model = ghost_ (flipped model) in
  let selection = ghost_ (heap flipped_model) in
  let proof = ghost_ (
    let refine_ a = root_flipped model in
    let refine_ b = H.partition_law selection frame in
    let u = () in
    let proof : {u : unit | root flipped_model === pointer
      && H.restrict after selection === selection} = refine_ u in proof) in
  let refine_ proof = proof in
  let refine_ parts = Pref.split selection t in
  let tree_token = parts.#left in
  let tree_token : {t : node option Pref.token | valid flipped_model
    && root flipped_model === pointer
    && Pref.own t === heap flipped_model} = refine_ tree_token in
  let actual = observe pointer flipped_model tree_token in
  assert (actual = Fork (1, Fork (3, Tip, Tip),
    Fork (2, Tip, Fork (4, Tip, Tip))))
