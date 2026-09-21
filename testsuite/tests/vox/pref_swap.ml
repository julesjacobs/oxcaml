(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_swap.ml";
 { bytecode; }
 { native; }
*)

let swap (p : int Pref.t @ immutable) (q : int Pref.t @ immutable)
    (t : {t : Pref.token |
      Pref.Heap.mem (Pref.own t) p && Pref.Heap.mem (Pref.own t) q} @ unique)
    : {u : Pref.token | match Pref.Heap.at (Pref.own t) p, Pref.Heap.at (Pref.own t) q with
      | Some x, Some y ->
        Pref.own u === Pref.Heap.put (Pref.Heap.put (Pref.own t) p y) q x
      | _ -> false} @ unique =
  let before = ghost_ (Pref.own (borrow_ t)) in
  let x : {x : int | Some x === Pref.Heap.at before p} =
    let b = borrow_ t in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) p} = b in
    let x = Pref.read p b in
    x in
  let y : {y : int | Some y === Pref.Heap.at before q} =
    let b = borrow_ t in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) q} = b in
    let y = Pref.read q b in
    y in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
  let t = Pref.write p y t in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) q} = t in
  let t = Pref.write q x t in
  t

let () =
  let seven = 7 in
  let forty_two = 42 in
  let t = Pref.empty () in
  let first = Pref.alloc seven t in
  let p = first.value in
  let t = first.state in
  let second = Pref.alloc forty_two t in
  let q = second.value in
  let t = second.state in
  let hash = Hashtbl.hash p in
  let distinct = p <> q in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p
    && Pref.Heap.mem (Pref.own t) q} = t in
  let next = swap p q t in
  let after = ghost_ (Pref.own (borrow_ next)) in
  let y : {y : int | Some y === Pref.Heap.at after q} =
    let b = borrow_ next in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) q} = b in
    let y = Pref.read q b in
    y in
  let z : {z : int | Some z === Pref.Heap.at after p} =
    let b = borrow_ next in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) p} = b in
    let z = Pref.read p b in
    z in
  let u = () in
  let claim : {u : unit | y = 7 && z = 42 && Pref.Heap.at before p === Some 7} = u in
  Gc.full_major ();
  Gc.compact ();
  assert (y = 7 && z = 42 && distinct && p <> q);
  assert (Hashtbl.hash p = hash);
  ignore claim
