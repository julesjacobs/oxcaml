(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_split.ml";
 { native; }
*)

let () =
  let seven = 7 in
  let forty_two = 42 in
  let refine_ t = Pref.empty () in
  let refine_ first = Pref.alloc seven t in
  let p = first.value in
  let t = first.state in
  let refine_ second = Pref.alloc forty_two t in
  let q = second.value in
  let t = second.state in
  let hash = Hashtbl.hash p in
  let distinct = p <> q in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let selection = ghost_ (Pref.Heap.put (Pref.Heap.empty ()) p seven) in
  let refine_ parts = Pref.split selection t in
  let left = parts.#left in
  let right = parts.#right in
  let left : {t : int Pref.token | Pref.Heap.mem (Pref.own t) p} = refine_ left in
  let value = 8 in
  let refine_ left = Pref.write p value left in
  let refine_ next = Pref.join left right in
  let after = ghost_ (Pref.own (borrow_ next)) in
  let y : {y : int | Some y === Pref.Heap.at after q} =
    let b = borrow_ next in
    let b : {b : int Pref.token | Pref.Heap.mem (Pref.own b) q} = refine_ b in
    let refine_ y = Pref.read q b in
    refine_ y in
  let refine_ y = y in
  let z : {z : int | Some z === Pref.Heap.at after p} =
    let b = borrow_ next in
    let b : {b : int Pref.token | Pref.Heap.mem (Pref.own b) p} = refine_ b in
    let refine_ z = Pref.read p b in
    refine_ z in
  let refine_ z = z in
  let u = () in
  let claim : {u : unit |
    y = 42 && z = 8 && Pref.Heap.at before p === Some 7} = refine_ u in
  Gc.full_major ();
  Gc.compact ();
  assert (y = 42 && z = 8 && distinct && p <> q);
  assert (Hashtbl.hash p = hash);
  ignore claim
