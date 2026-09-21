(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_swap.ml";
 { bytecode; }
 { native; }
*)

let swap (p : int Pref.t @ immutable) (q : int Pref.t @ immutable)
    (t : {t : int Pref.token |
      Pref.Heap.mem (Pref.own t) p && Pref.Heap.mem (Pref.own t) q} @ unique)
    : {u : int Pref.token | let refine_ t = t in
      match Pref.Heap.at (Pref.own t) p, Pref.Heap.at (Pref.own t) q with
      | Some x, Some y ->
        Pref.own u === Pref.Heap.put (Pref.Heap.put (Pref.own t) p y) q x
      | _ -> false} @ unique =
  let refine_ t = t in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let x : {x : int | Some x === Pref.Heap.at before p} =
    let b = borrow_ t in
    let b : {b : int Pref.token | Pref.Heap.mem (Pref.own b) p} = refine_ b in
    let refine_ x = Pref.read p b in
    refine_ x in
  let refine_ x = x in
  let y : {y : int | Some y === Pref.Heap.at before q} =
    let b = borrow_ t in
    let b : {b : int Pref.token | Pref.Heap.mem (Pref.own b) q} = refine_ b in
    let refine_ y = Pref.read q b in
    refine_ y in
  let refine_ y = y in
  let t : {t : int Pref.token | Pref.Heap.mem (Pref.own t) p} = refine_ t in
  let refine_ t = Pref.write p y t in
  let t : {t : int Pref.token | Pref.Heap.mem (Pref.own t) q} = refine_ t in
  let refine_ t = Pref.write q x t in
  refine_ t

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
  let t : {t : int Pref.token | Pref.Heap.mem (Pref.own t) p
    && Pref.Heap.mem (Pref.own t) q} = refine_ t in
  let refine_ next = swap p q t in
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
  let claim : {u : unit | y = 7 && z = 42 && Pref.Heap.at before p === Some 7} = refine_ u in
  Gc.full_major ();
  Gc.compact ();
  assert (y = 7 && z = 42 && distinct && p <> q);
  assert (Hashtbl.hash p = hash);
  ignore claim
