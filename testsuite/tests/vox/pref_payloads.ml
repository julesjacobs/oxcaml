(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml pref_payloads.ml";
 { bytecode; }
 { native; }
*)

let () =
  let seven = [7] in
  let forty_two = 42 in
  let t = Pref.empty () in
  let first = Pref.alloc seven t in
  let p = first.value in
  let t = first.state in
  let second = Pref.alloc forty_two t in
  let q = second.value in
  let t = second.state in
  let hash = Hashtbl.hash p in
  let before = ghost_ (Pref.own (borrow_ t)) in
  let x : {x : int list | Some x === Pref.Heap.at before p} =
    let b = borrow_ t in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) p} = b in
    let x = Pref.read p b in
    x in
  let x = x in
  Gc.full_major ();
  let value = List.map (fun n -> n + 1) x in
  let t : {t : Pref.token | Pref.Heap.mem (Pref.own t) p} = t in
  let next = Pref.write p value t in
  Gc.minor ();
  let after = ghost_ (Pref.own (borrow_ next)) in
  let y : {y : int | Some y === Pref.Heap.at after q} =
    let b = borrow_ next in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) q} = b in
    let y = Pref.read q b in
    y in
  let y = y in
  let z : {z : int list | Some z === Pref.Heap.at after p} =
    let b = borrow_ next in
    let b : {b : Pref.token | Pref.Heap.mem (Pref.own b) p} = b in
    let z = Pref.read p b in
    z in
  let z = z in
  let u = () in
  let claim : {u : unit | y = 42 && z === value && Pref.Heap.at before p === Some [7]} = u in
  Gc.full_major ();
  Gc.compact ();
  assert (y = 42 && z = [8]);
  assert (Hashtbl.hash p = hash);
  ignore claim
