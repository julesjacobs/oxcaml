(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_pref_semantics.mli vox_pref_semantics.ml pref_tree.mli pref_tree.ml pref_tree_client.ml";
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

let (set_links_law @ total) (n : node @ immutable)
    (old_l : node option @ immutable) (old_r : node option @ immutable)
    (new_l : node option @ immutable) (new_r : node option @ immutable) :
    {u : unit | let before = links n old_l old_r in
      if not (n.left === n.right) then
        H.mem before n.left && H.mem (H.put before n.left new_l) n.right &&
        H.put (H.put before n.left new_l) n.right new_r === links n new_l new_r &&
        H.same_domain (H.put (H.put before n.left new_l) n.right new_r) before
      else true} @ ghost = ghost_ (
  let before = links n old_l old_r in
  let p = n.left in let q = n.right in
  links_def n old_l old_r; links_def n new_l new_r;
  let e = H.empty () in
  H.commute_law (H.put e p old_l) q old_r p new_l;
  H.put_law e p old_l new_l;
  H.put_law (H.put e p new_l) q old_r new_r;
  let h1 = H.put before p new_l in
  let h2 = H.put h1 q new_r in
  H.put_law before p new_l new_l; H.put_law h1 q new_r new_r;
  H.domain_law h2 h1 before; ())

let (mirror_view @ total) (model : tree @ immutable) (pointer : node option @ immutable) :
    {u : unit | if valid model && root model === pointer then
      (match model with
      | Empty -> pointer === None && heap model === H.empty () && flipped model === Empty
      | Branch (n, l, r) -> pointer === Some n &&
          left_tree model === l && right_tree model === r &&
          flipped model === Branch (n, flipped r, flipped l) &&
          valid l && valid r && not (n.left === n.right) &&
          heap model === H.union (links n (root l) (root r)) (H.union (heap l) (heap r)) &&
          H.disjoint (links n (root l) (root r)) (H.union (heap l) (heap r)) &&
          H.disjoint (heap l) (heap r)) else true} @ ghost = ghost_ (
  valid_def model; heap_def model; root_def model; flipped_def model;
  left_tree_def model; right_tree_def model; ())

let (mirror_partition @ total) (model : tree @ immutable) (n : node @ immutable) :
    {u : unit | let lm = left_tree model in let rm = right_tree model in
      let nh = links n (root lm) (root rm) in
      let lh = heap lm in let rh = heap rm in let ch = H.union lh rh in
      if valid model && root model === Some n then
        H.restrict (heap model) nh === nh && H.exclude (heap model) nh === ch &&
        H.restrict ch lh === lh && H.exclude ch lh === rh &&
        H.mem nh n.left && H.mem nh n.right &&
        H.at nh n.left === Some (root lm) && H.at nh n.right === Some (root rm)
      else true} @ ghost = ghost_ (
  mirror_view model (Some n);
  let lm = left_tree model in let rm = right_tree model in
  let nh = links n (root lm) (root rm) in
  H.partition_law nh (H.union (heap lm) (heap rm));
  H.partition_law (heap lm) (heap rm);
  links_def n (root lm) (root rm);
  Vox_pref_semantics.put (H.empty ()) n.left (root lm) n.left;
  Vox_pref_semantics.put (H.put (H.empty ()) n.left (root lm)) n.right (root rm) n.left;
  Vox_pref_semantics.put (H.put (H.empty ()) n.left (root lm)) n.right (root rm) n.right; ())

let set_links (n : node @ immutable)
    (old_l : node option @ immutable ghost)
    (old_r : node option @ immutable ghost)
    (new_l : node option @ immutable)
    (new_r : node option @ immutable)
    (t : {t : node option Pref.token | Pref.own t === links n old_l old_r
      && not (n.left === n.right)} @ unique)
    : {t : node option Pref.token | Pref.own t === links n new_l new_r
        && H.same_domain (Pref.own t) (links n old_l old_r)} @ unique =
  let p = n.left in
  let q = n.right in
  ghost_ (set_links_law n old_l old_r new_l new_r);
  let t : {t : node option Pref.token | H.mem (Pref.own t) p} = t in
  let t = Pref.write p new_l t in
  let t : {t : node option Pref.token | H.mem (Pref.own t) q} = t in
  let t = Pref.write q new_r t in
  t

let (mirror_rebuild @ total) (model : tree @ immutable) (n : node @ immutable)
    (new_node : node option Pref.heap @ immutable) (new_left : node option Pref.heap @ immutable)
    (new_right : node option Pref.heap @ immutable) (after : node option Pref.heap @ immutable) :
    {u : unit | let lm = left_tree model in let rm = right_tree model in
      let nh = links n (root lm) (root rm) in
      if valid model && root model === Some n &&
        valid (flipped lm) && valid (flipped rm) &&
        new_left === heap (flipped lm) && new_right === heap (flipped rm) &&
        new_node === links n (root rm) (root lm) &&
        H.same_domain new_left (heap lm) && H.same_domain new_right (heap rm) &&
        H.same_domain new_node nh && H.disjoint new_right new_left &&
        H.disjoint new_node (H.union new_right new_left) &&
        after === H.union new_node (H.union new_right new_left) then
          after === heap (flipped model) && valid (flipped model) &&
          H.same_domain after (heap model)
        else true} @ ghost = ghost_ (
  mirror_view model (Some n);
  let lm = left_tree model in let rm = right_tree model in
  let lh = heap lm in let rh = heap rm in
  let nh = links n (root lm) (root rm) in
  let ch = H.union lh rh in
  let f = flipped model in
  root_flipped lm; root_flipped rm; heap_def f; valid_def f;
  let nc = H.union new_right new_left in
  let rc = H.union rh lh in
  H.union_domain_law new_right rh new_left lh;
  H.union_law rh lh rh;
  H.domain_law nc rc ch;
  H.union_domain_law new_node nh nc ch; ())

let rec mirror :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique ->
    {t : node option Pref.token | Pref.own t === heap (flipped model)
      && valid (flipped model)
      && H.same_domain (Pref.own t) (heap model)} @ unique =
  fun pointer model t ->
  ghost_ (mirror_view model pointer);
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
    ghost_ (mirror_partition model n);
    let parts = Pref.split nh t in
    let nt = parts.#left in
    let ct = parts.#right in
    let p = n.left in
    let q = n.right in
    let l : {l : node option | l === root lm} =
      let b = borrow_ nt in
      let b : {b : node option Pref.token | H.mem (Pref.own b) p} = b in
      let l = Pref.read p b in
      l in
    let r : {r : node option | r === root rm} =
      let b = borrow_ nt in
      let b : {b : node option Pref.token | H.mem (Pref.own b) q} = b in
      let r = Pref.read q b in
      r in
    let children = Pref.split lh ct in
    let lt = children.#left in
    let rt = children.#right in
    let lt : {t : node option Pref.token | valid lm && root lm === l
        && Pref.own t === heap lm} = lt in
    let lt = mirror l lm lt in
    let rt : {t : node option Pref.token | valid rm && root rm === r
        && Pref.own t === heap rm} = rt in
    let rt = mirror r rm rt in
    let nt : {t : node option Pref.token | Pref.own t === links n lp rp
        && not (n.left === n.right)} = nt in
    let nt = set_links n lp rp r l nt in
    let new_nh = ghost_ (Pref.own (borrow_ nt)) in
    let new_lh = ghost_ (Pref.own (borrow_ lt)) in
    let new_rh = ghost_ (Pref.own (borrow_ rt)) in
    let ct = Pref.join rt lt in
    let t = Pref.join nt ct in
    let after = ghost_ (Pref.own (borrow_ t)) in
    ghost_ (mirror_rebuild model n new_nh new_lh new_rh after);
    t

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
  let mh = ghost_ (heap model) in
  let _ = ghost_ (H.partition_law mh frame) in
  let parts = Pref.split mh t in
  let tree = parts.#left in
  let frame_token = parts.#right in
  let tree : {t : node option Pref.token | valid model && root model === pointer
    && Pref.own t === heap model} = tree in
  let tree = mirror pointer model tree in
  let result = Pref.join tree frame_token in
  result

type built = {
  pointer : node option @@ aliased;
  model : tree @@ aliased ghost;
  state : node option Pref.token;
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

type parts = #{pointer : node option @@ global;
  model : tree @@ global ghost; state : node option Pref.token}

let branch_parts (value : int)
    (l : {b : parts | b.#pointer === root b.#model && valid b.#model
      && Pref.own b.#state === heap b.#model} @ unique)
    (r : {b : parts | b.#pointer === root b.#model && valid b.#model
      && Pref.own b.#state === heap b.#model} @ unique)
    : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && (match b.model with Empty -> false | Branch (n, lm, rm) ->
        n.value = value && lm === l.#model && rm === r.#model)} @ unique =
  let lp = l.#pointer in
  let rp = r.#pointer in
  let lm = l.#model in
  let rm = r.#model in
  let lt = l.#state in
  let rt = r.#state in
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

let branch (value : int)
    (l : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    (r : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model} @ unique)
    : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && (match b.model with Empty -> false | Branch (n, lm, rm) ->
        n.value = value && lm === l.model && rm === r.model)} @ unique =
  let left = #{pointer = l.pointer; model = l.model; state = l.state} in
  let right = #{pointer = r.pointer; model = r.model; state = r.state} in
  branch_parts value left right

type shape = Tip | Fork of int * shape * shape
[@@inductive]

let[@def] rec (shape_of @ total) (model : tree @ immutable) =
  match model with
  | Empty -> Tip
  | Branch (n, l, r) -> Fork (n.value, shape_of l, shape_of r)

let leaf (value : int)
    : {b : built | b.pointer === root b.model && valid b.model
      && Pref.own b.state === heap b.model
      && shape_of b.model === Fork (value, Tip, Tip)} @ unique =
  let l = empty () in
  let r = empty () in
  let l : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = l in
  let r : {b : built | b.pointer === root b.model && valid b.model
    && Pref.own b.state === heap b.model} = r in
  let b = branch value l r in
  ghost_ (let view = borrow_ b in
    shape_of_def view.model; shape_of_def Empty);
  b

let (observation_frames @ total)
    (node : node option Pref.heap @ immutable) (left : node option Pref.heap @ immutable)
    (right : node option Pref.heap @ immutable) (frame : node option Pref.heap @ immutable) :
    {u : unit | if H.disjoint node (H.union left right) && H.disjoint left right then
      H.union (H.union node (H.union left right)) frame ===
        H.union left (H.union node (H.union right frame)) &&
      H.union (H.union node (H.union left right)) frame ===
        H.union right (H.union node (H.union left frame)) else true} @ ghost =
  ghost_ (H.union_law node left right;
    H.union_law node (H.union left right) frame;
    H.union_law left right frame;
    H.union_law left node (H.union right frame);
    H.union_law right left frame;
    H.union_law right node (H.union left frame);
    H.union_law node right left;
    H.union_law node left (H.union right frame);
    H.union_law node right (H.union left frame); ())

let (node_observations @ total) (n : node @ immutable)
    (lm : tree @ immutable) (rm : tree @ immutable) (frame : node option Pref.heap @ immutable) :
    {u : unit | let memory = H.union
      (H.union (links n (root lm) (root rm)) (H.union (heap lm) (heap rm))) frame in
      if not (n.left === n.right) then
        H.mem memory n.left && H.at memory n.left === Some (root lm) &&
        H.mem memory n.right && H.at memory n.right === Some (root rm)
      else true} @ ghost = ghost_ (
  let nh = links n (root lm) (root rm) in
  let children = H.union (heap lm) (heap rm) in
  links_def n (root lm) (root rm);
  Vox_pref_semantics.put (H.empty ()) n.left (root lm) n.left;
  Vox_pref_semantics.put (H.put (H.empty ()) n.left (root lm))
    n.right (root rm) n.left;
  Vox_pref_semantics.put (H.put (H.empty ()) n.left (root lm))
    n.right (root rm) n.right;
  Vox_pref_semantics.union nh children n.left;
  Vox_pref_semantics.union nh children n.right;
  Vox_pref_semantics.union (H.union nh children) frame n.left;
  Vox_pref_semantics.union (H.union nh children) frame n.right; ())

let rec observe_framed :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (frame : node option Pref.heap) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === H.union (heap model) frame}) @ local read ->
    {result : shape | result === shape_of model} = fun pointer model frame t ->
  ghost_ (valid_def model; heap_def model; root_def model;
    left_tree_def model; right_tree_def model; shape_of_def model);
  match pointer with
  | None -> Tip
  | Some n ->
    let lm = ghost_ (left_tree model) in
    let rm = ghost_ (right_tree model) in
    let nh = ghost_ (links n (root lm) (root rm)) in
    let lh = ghost_ (heap lm) in
    let rh = ghost_ (heap rm) in
    let left_frame = ghost_ (H.union nh (H.union rh frame)) in
    let right_frame = ghost_ (H.union nh (H.union lh frame)) in
    ghost_ (observation_frames nh lh rh frame;
      node_observations n lm rm frame);
    let p = n.left in
    let q = n.right in
    let l : {v : node option | v === root lm} = Pref.read p t in
    let r : {v : node option | v === root rm} = Pref.read q t in
    let left = observe_framed l lm left_frame t in
    let right = observe_framed r rm right_frame t in
    Fork (n.value, left, right)

let observe_read :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ local read ->
    {result : shape | result === shape_of model} = fun pointer model t ->
  let frame = ghost_ (H.empty ()) in
  ghost_ (H.union_law (heap model) frame frame);
  observe_framed pointer model frame t

let observe :
    (pointer : node option) @ immutable -> (model : tree) @ immutable ghost ->
    (t : {t : node option Pref.token | valid model && root model === pointer
      && Pref.own t === heap model}) @ unique ->
    {result : shape | result === shape_of model} = fun pointer model t ->
  observe_read pointer model (borrow_ t)

module Owned = struct
  type payload = parts
  type t = #{owned : {b : payload | b.#pointer === root b.#model &&
    valid b.#model && Pref.own b.#state === heap b.#model}}

  let[@def] model (state : t @ local immutable total ghost) = ghost_ state.#owned.#model

  let adopt : (b : {b : built | b.pointer === root b.model && valid b.model &&
      Pref.own b.state === heap b.model}) @ unique ->
      {state : t | model state === b.model} @ unique = fun b ->
    let owned = #{pointer = b.pointer; model = b.model; state = b.state} in
    let state : t = #{owned} in
    ghost_ (model_def (borrow_ state));
    state

  let release : (state : t) @ unique ->
      {b : built | b.pointer === root b.model && valid b.model &&
        Pref.own b.state === heap b.model && b.model === model state} @ unique =
    fun state ->
    ghost_ (model_def (borrow_ state));
    let b = state.#owned in
    {pointer = b.#pointer; model = b.#model; state = b.#state}

  let empty () : {state : t | model state === Empty} @ unique = adopt (empty ())

  let leaf (value : int) :
      {state : t | shape_of (model state) === Fork (value, Tip, Tip)} @ unique =
    adopt (leaf value)

  let branch : (value : int) -> (left : t) @ unique -> (right : t) @ unique ->
      {state : t | match model state with Empty -> false | Branch (n, l, r) ->
        n.value = value && l === model left && r === model right} @ unique =
    fun value left right ->
    ghost_ (model_def (borrow_ left); model_def (borrow_ right));
    adopt (branch_parts value left.#owned right.#owned)

  let mirror : (state : t) @ unique ->
      {next : t | model next === flipped (model state)} @ unique = fun state ->
    ghost_ (model_def (borrow_ state));
    let b = state.#owned in
    let pointer = b.#pointer in
    let before = ghost_ b.#model in
    let frame = ghost_ (H.empty ()) in
    ghost_ (H.union_law (heap before) frame frame);
    let token = mirror_with_frame pointer before frame b.#state in
    let after = ghost_ (flipped before) in
    ghost_ (root_flipped before; H.union_law (heap after) frame frame);
    let owned = #{pointer; model = after; state = token} in
    let next : t = #{owned} in
    ghost_ (model_def (borrow_ next));
    next

  let observe : (state : t) @ local read total forkable unyielding ->
      {result : shape | result === shape_of (model state)} = fun state ->
    ghost_ (model_def (borrow_ state));
    let view = state.#owned in
    observe_read view.#pointer view.#model (borrow_ view.#state)
end
