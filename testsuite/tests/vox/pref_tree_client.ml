open Pref_tree

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
  let forty_two = 42 in
  let refine_ extra = Pref.alloc forty_two frame_token in
  let unrelated = extra.value in
  let frame_token = extra.state in
  let frame = ghost_ (Pref.own (borrow_ frame_token)) in
  let refine_ t = Pref.join t frame_token in
  let t : {t : Pref.token | valid model && root model === pointer
    && H.disjoint (heap model) frame
    && Pref.own t === H.union (heap model) frame} = refine_ t in
  let refine_ t = mirror_with_frame pointer model frame t in
  let after = ghost_ (Pref.own (borrow_ t)) in
  let result : {v : int | v = 42} =
    let b = borrow_ t in
    let b : {b : Pref.token | H.mem (Pref.own b) unrelated} = refine_ b in
    let refine_ result = Pref.read unrelated b in
    refine_ result in
  let refine_ result = result in
  assert (result = 42);
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
  let tree_token : {t : Pref.token | valid flipped_model
    && root flipped_model === pointer
    && Pref.own t === heap flipped_model} = refine_ tree_token in
  let actual = observe pointer flipped_model tree_token in
  assert (actual = Fork (1, Fork (3, Tip, Tip),
    Fork (2, Tip, Fork (4, Tip, Tip))))
