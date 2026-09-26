(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_heterogeneous.ml";
 { native; }
*)

let () =
  let seven = 7 in
  let refine_ t = Pref.empty () in
  let refine_ first = Pref.alloc seven t in
  let p = first.value in
  let t = first.state in
  let payload = ("frame", p) in
  let refine_ frame_token = Pref.empty () in
  let refine_ second = Pref.alloc payload frame_token in
  let q = second.value in
  let frame_token = second.state in
  let frame = ghost_ (Pref.own (borrow_ frame_token)) in
  let hash = Hashtbl.hash p in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let x : {x : int | Some x === Pref.Heap.at before p} =
    let b = borrow_ t in
    let b : {b : int Pref.token | Pref.Heap.mem (Pref.own b) p} = refine_ b in
    let refine_ x = Pref.read p b in
    refine_ x in
  let refine_ x = x in
  let value = x + 1 in
  let t : {t : int Pref.token | Pref.Heap.mem (Pref.own t) p} = refine_ t in
  let refine_ next = Pref.write p value t in
  let after = ghost_ (Pref.own (borrow_ next)) in
  let y : {y : string * int Pref.t | Some y === Pref.Heap.at frame q} =
    let b = borrow_ frame_token in
    let b : {b : (string * int Pref.t) Pref.token | Pref.Heap.mem (Pref.own b) q} = refine_ b in
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
  let claim : {u : unit | y === payload && z = 8 && Pref.Heap.at before p === Some 7} = refine_ u in
  Gc.full_major ();
  Gc.compact ();
  assert (y = payload && z = 8);
  assert (Hashtbl.hash p = hash);
  ignore claim
