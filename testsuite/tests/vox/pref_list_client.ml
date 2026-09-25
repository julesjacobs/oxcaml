open Pref_list

let check values =
  let b = of_list values in
  let pointer = b.pointer in
  let xs = b.model in
  let t = b.state in
  let t : {t : Pref.token | valid xs && root xs === pointer
    && Pref.own t === heap xs} = t in
  let original = observe pointer xs t in
  let original_nodes = original.nodes in
  let t = original.state in
  let frame_token = Pref.empty () in
  let forty_two = 42 in
  let extra = Pref.alloc forty_two frame_token in
  let unrelated = extra.value in
  let frame_token = extra.state in
  let frame = ghost_ (Pref.own (borrow_ frame_token)) in
  let u = () in
  let frame_value : {u : unit | H.mem frame unrelated
    && H.at frame unrelated === Some 42} = u in
  let t = Pref.join t frame_token in
  let t : {t : Pref.token | valid xs && root xs === pointer
    && H.disjoint (heap xs) frame
    && Pref.own t === H.union (heap xs) frame} = t in
  let reversed = reverse pointer xs frame t in
  let pointer = reversed.pointer in
  let t = reversed.state in
  let ys = ghost_ (rev_append xs Nil) in
  let selection = ghost_ (heap ys) in
  let proof = ghost_ (H.partition_law selection frame) in
  let parts = Pref.split selection t in
  let list = parts.#left in
  let frame_token = parts.#right in
  let untouched : {v : int | v = 42} =
    let b = borrow_ frame_token in
    let b : {b : Pref.token | H.mem (Pref.own b) unrelated} = b in
    let v = Pref.read unrelated b in
    v in
  assert (untouched = 42);
  let list : {t : Pref.token | valid ys && root ys === pointer
    && Pref.own t === heap ys} = list in
  let observed = observe pointer ys list in
  let reversed_nodes = observed.nodes in
  let state = observed.state in
  assert (List.map (fun n -> n.value) reversed_nodes = List.rev values);
  assert (List.for_all2 ( == ) reversed_nodes (List.rev original_nodes));
  let t = Pref.join state frame_token in
  let t : {t : Pref.token | valid ys && root ys === pointer
    && H.disjoint (heap ys) frame
    && Pref.own t === H.union (heap ys) frame} = t in
  let restored = reverse pointer ys frame t in
  let pointer = restored.pointer in
  let t = restored.state in
  let zs = ghost_ (rev_append ys Nil) in
  let selection = ghost_ (heap zs) in
  let proof = ghost_ (H.partition_law selection frame) in
  let parts = Pref.split selection t in
  let list = parts.#left in
  let list : {t : Pref.token | valid zs && root zs === pointer
    && Pref.own t === heap zs} = list in
  let observed = observe pointer zs list in
  assert (List.for_all2 ( == ) observed.nodes original_nodes)

let () =
  List.iter check [[]; [7]; [1; 2]; [1; 2; 3; 4]; [7; 7; 7]];
  check (List.init 1000 Fun.id)
