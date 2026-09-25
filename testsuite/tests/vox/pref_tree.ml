(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_tree.mli pref_tree.ml pref_tree_client.ml";
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
  let _ = flipped_def tree in
  let f = flipped tree in
  let _ = root_def f in
  let _ = root_def tree in
  ()

let set_links (n : node @ immutable)
    (old_l : node option @ immutable ghost)
    (old_r : node option @ immutable ghost)
    (new_l : node option @ immutable)
    (new_r : node option @ immutable)
    (t : {t : Pref.token | Pref.own t === links n old_l old_r
      && not (n.left === n.right)} @ unique)
    : {t : Pref.token | Pref.own t === links n new_l new_r
        && H.same_domain (Pref.own t) (links n old_l old_r)} @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  let p = n.left in
  let q = n.right in
  let _ = ghost_ (
    let _ = links_def n old_l old_r in
    let _ = links_def n new_l new_r in
    let e = H.empty () in
    let h = H.put e p old_l in
    let _ = H.commute_law h q old_r p new_l in
    let _ = H.put_law e p old_l new_l in
    let h = H.put e p new_l in
    let _ = H.put_law h q old_r new_r in
    let h1 = H.put before p new_l in
    let h2 = H.put h1 q new_r in
    let _ = H.put_law before p new_l new_l in
    let _ = H.put_law h1 q new_r new_r in
    let _ = H.domain_law h2 h1 before in
    let u = () in
    let result : {u : unit |
      H.put (H.put before p new_l) q new_r === links n new_l new_r
      && H.same_domain (H.put (H.put before p new_l) q new_r) before} =
      u in result) in
  let t : {t : Pref.token | H.mem (Pref.own t) p} = t in
  let t = Pref.write p new_l t in
  let t : {t : Pref.token | H.mem (Pref.own t) q} = t in
  let t = Pref.write q new_r t in
  t

let rec mirror :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique ->
    {t : Pref.token | Pref.own t === heap (flipped model)
      && valid (flipped model)
      && H.same_domain (Pref.own t) (heap model)} @ unique =
  fun pointer model t ->
  let _ = ghost_ (
    let _ = valid_def model in
    let _ = heap_def model in
    let _ = root_def model in
    let _ = flipped_def model in
    let _ = left_tree_def model in
    let _ = right_tree_def model in
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
          && H.disjoint (heap l) (heap r))} = u in facts) in
  match pointer with
  | None ->
    let _ = ghost_ (
      let empty = Empty in
      let _ = heap_def empty in
      let h = heap model in
      let _ = H.domain_law h h h in
      let u = () in
      let proof : {u : unit | heap (flipped model) === heap model
        && H.same_domain (heap model) (heap model)} = u in proof) in
    t
  | Some n ->
    let lm = ghost_ (left_tree model) in
    let rm = ghost_ (right_tree model) in
    let lp = ghost_ (root lm) in
    let rp = ghost_ (root rm) in
    let nh = ghost_ (links n lp rp) in
    let lh = ghost_ (heap lm) in
    let rh = ghost_ (heap rm) in
    let ch = ghost_ (H.union lh rh) in
    let _ = ghost_ (
      let _ = H.partition_law nh ch in
      let _ = H.partition_law lh rh in
      let _ = links_def n lp rp in
      let u = () in
      let _links : {u : unit |
        nh === H.put (H.put (H.empty ()) n.left lp) n.right rp} = u in
      let proof : {u : unit |
        H.restrict (heap model) nh === nh
        && H.exclude (heap model) nh === ch
        && H.restrict ch lh === lh && H.exclude ch lh === rh
        && H.mem nh n.left && H.mem nh n.right
        && H.at nh n.left === Some lp && H.at nh n.right === Some rp} =
        u in proof) in
    let parts = Pref.split nh t in
    let nt = parts.#left in
    let ct = parts.#right in
    let p = n.left in
    let q = n.right in
    let l : {l : node option | l === root lm} =
      let b = borrow_ nt in
      let b : {b : Pref.token | H.mem (Pref.own b) p} = b in
      let l = Pref.read p b in
      l in
    let r : {r : node option | r === root rm} =
      let b = borrow_ nt in
      let b : {b : Pref.token | H.mem (Pref.own b) q} = b in
      let r = Pref.read q b in
      r in
    let children = Pref.split lh ct in
    let lt = children.#left in
    let rt = children.#right in
    let lt : {t : Pref.token | valid lm && root lm === l
        && Pref.own t === heap lm} = lt in
    let lt = mirror l lm lt in
    let rt : {t : Pref.token | valid rm && root rm === r
        && Pref.own t === heap rm} = rt in
    let rt = mirror r rm rt in
    let nt : {t : Pref.token | Pref.own t === links n lp rp
        && not (n.left === n.right)} = nt in
    let nt = set_links n lp rp r l nt in
    let new_nh = ghost_ (Pref.own (borrow_ nt)) in
    let new_lh = ghost_ (Pref.own (borrow_ lt)) in
    let new_rh = ghost_ (Pref.own (borrow_ rt)) in
    let ct = Pref.join rt lt in
    let t = Pref.join nt ct in
    let after = ghost_ (Pref.own (borrow_ t)) in
    let _ = ghost_ (
      let f = flipped model in
      let _ = root_flipped lm in
      let _ = root_flipped rm in
      let _ = heap_def f in
      let _ = valid_def f in
      let nc = H.union new_rh new_lh in
      let rc = H.union rh lh in
      let _ = H.union_domain_law new_rh rh new_lh lh in
      let _ = H.union_law rh lh rh in
      let _ = H.domain_law nc rc ch in
      let _ = H.union_domain_law new_nh nh nc ch in
      let u = () in
      let proof : {u : unit | after === heap (flipped model)
        && valid (flipped model)
        && H.same_domain after (heap model)} = u in proof) in
    t

let mirror_with_frame :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (frame : Pref.heap) @ immutable ghost ->
    (t : {t : Pref.token | valid model && root model === pointer
      && H.disjoint (heap model) frame
      && Pref.own t === H.union (heap model) frame}) @ unique ->
    {t : Pref.token | Pref.own t === H.union (heap (flipped model)) frame
      && valid (flipped model)
      && H.disjoint (heap (flipped model)) frame}
      @ unique = fun pointer model frame t ->
  let mh = ghost_ (heap model) in
  let _ = ghost_ (H.partition_law mh frame) in
  let parts = Pref.split mh t in
  let tree = parts.#left in
  let frame_token = parts.#right in
  let tree : {t : Pref.token | valid model && root model === pointer
    && Pref.own t === heap model} = tree in
  let tree = mirror pointer model tree in
  let result = Pref.join tree frame_token in
  result

type built = {
  pointer : node option @@ aliased;
  model : tree @@ aliased ghost;
  state : Pref.token;
}

let empty () : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model && b.model === Empty} @ unique =
  let model = Empty in
  let _ = ghost_ (
    let _ = root_def model in
    let _ = heap_def model in
    let _ = valid_def model in
    let u = () in
    let facts : {u : unit | root model === None && heap model === H.empty ()
        && valid model} = u in facts) in
  let state = Pref.empty () in
  let b = {pointer = None; model; state} in
  b

let branch (value : int)
    (l : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    (r : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && (match b.model with Empty -> false | Branch (n, lm, rm) ->
        n.value = value && lm === l.model && rm === r.model)} @ unique =
  let lp = l.pointer in
  let rp = r.pointer in
  let lm = l.model in
  let rm = r.model in
  let lt = l.state in
  let rt = r.state in
  let t = Pref.join lt rt in
  let ch = ghost_ (Pref.own (borrow_ t)) in
  let a = Pref.alloc lp t in
  let p = a.value in
  let t = a.state in
  let b = Pref.alloc rp t in
  let q = b.value in
  let state = b.state in
  let n = {value; left = p; right = q} in
  let model = ghost_ (Branch (n, lm, rm)) in
  let _ = ghost_ (
    let _ = root_def model in
    let _ = valid_def model in
    let _ = heap_def model in
    let _ = links_def n lp rp in
    let e = H.empty () in
    let s = H.put e p lp in
    let _ = H.union_law ch e e in
    let _ = H.put_union_law e ch p lp in
    let _ = H.put_union_law s ch q rp in
    let u = () in
    let _contents : {u : unit |
      H.put (H.put ch p lp) q rp === heap model} = u in
    let _distinct : {u : unit | not (p === q)} = u in
    let _separate : {u : unit | H.disjoint (links n lp rp) ch} =
      u in
    let proof : {u : unit | valid model && root model === Some n
      && H.put (H.put ch p lp) q rp === heap model} = u in proof) in
  let result = {pointer = Some n; model; state} in
  result

let leaf (value : int)
    : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique =
  let l = empty () in
  let r = empty () in
  let l : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = l in
  let r : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = r in
  let b = branch value l r in
  b

type shape = Tip | Fork of int * shape * shape

let rec observe :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique -> shape =
  fun pointer model t ->
  let _ = ghost_ (
    let _ = valid_def model in
    let _ = heap_def model in
    let _ = root_def model in
    let _ = flipped_def model in
    let _ = left_tree_def model in
    let _ = right_tree_def model in
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
          && H.disjoint (heap l) (heap r))} = u in facts) in
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
    let _ = ghost_ (
      let _ = H.partition_law nh ch in
      let _ = H.partition_law lh rh in
      let _ = links_def n lp rp in
      let u = () in
      let _links : {u : unit |
        nh === H.put (H.put (H.empty ()) n.left lp) n.right rp} = u in
      let proof : {u : unit |
        H.restrict (heap model) nh === nh
        && H.exclude (heap model) nh === ch
        && H.restrict ch lh === lh && H.exclude ch lh === rh
        && H.mem nh n.left && H.mem nh n.right
        && H.at nh n.left === Some lp && H.at nh n.right === Some rp} =
        u in proof) in
    let parts = Pref.split nh t in
    let nt = parts.#left in
    let ct = parts.#right in
    let p = n.left in
    let q = n.right in
    let l : {l : node option | l === root lm} =
      let b = borrow_ nt in
      let b : {b : Pref.token | H.mem (Pref.own b) p} = b in
      let l = Pref.read p b in
      l in
    let r : {r : node option | r === root rm} =
      let b = borrow_ nt in
      let b : {b : Pref.token | H.mem (Pref.own b) q} = b in
      let r = Pref.read q b in
      r in
    let children = Pref.split lh ct in
    let lt = children.#left in
    let rt = children.#right in
    let lt : {t : Pref.token | valid lm && root lm === l
      && Pref.own t === heap lm} = lt in
    let rt : {t : Pref.token | valid rm && root rm === r
      && Pref.own t === heap rm} = rt in
    let left = observe l lm lt in
    let right = observe r rm rt in
    Fork (n.value, left, right)
