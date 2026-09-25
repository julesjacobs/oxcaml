(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml raw_memory_demo.ml";
 { bytecode; }
 { native; }
*)
module P = Ghost_pref
module H = P.Heap
module M = Raw_memory

let lifecycle : (token : P.token) @ unique ghost ->
    {s : P.token | P.own s === P.own token} @ unique ghost = fun token ->
  let before = ghost_ (P.own (borrow_ token)) in
  let r = M.malloc 2 token in
  match r.value with
  | None -> r.state
  | Some p ->
    let fp = ghost_ (M.footprint p) in
    ghost_ (M.allocated_covers p before);
    ghost_ (M.footprint_at p (-1));
    ghost_ (M.footprint_at p 0);
    ghost_ (M.footprint_at p 1);
    let h = ghost_ (P.own (borrow_ r.state)) in
    ghost_ (M.split_covers h p 0 1 2);
    ghost_ (M.range_at p 0 1 (-1));
    ghost_ (M.range_at p 0 1 0);
    ghost_ (M.range_at p 0 1 1);
    let selection = ghost_ (M.range p 0 1) in
    let split = P.split selection r.state in
    let left0 = ghost_ (P.own (borrow_ split.left)) in
    let right0 = ghost_ (P.own (borrow_ split.right)) in
    let left = M.write p 0 40 split.left in
    let right = M.write p 1 2 split.right in
    Gc.full_major ();
    Gc.compact ();
    ghost_ (
      let lh = H.put left0 (M.location p 0) (Some 40) in
      let rh = H.put right0 (M.location p 1) (Some 2) in
      let _ = H.at lh (M.location p 0) in
      let _ = H.at rh (M.location p 1) in
      let _ = H.mem lh (M.location p 0) in
      let _ = H.mem rh (M.location p 1) in ());
    let x : {v : int | v = 40} = M.read p 0 (borrow_ left) in
    let y : {v : int | v = 2} = M.read p 1 (borrow_ right) in
    assert (x + y = 42);
    ghost_ (M.write_covers left0 p 0 1 0 40);
    ghost_ (M.write_covers right0 p 1 2 1 2);
    let lh = ghost_ (P.own (borrow_ left)) in
    let rh = ghost_ (P.own (borrow_ right)) in
    ghost_ (M.join_covers lh rh p 0 1 2);
    ghost_ (M.location_law p p 0 (-1));
    ghost_ (M.location_law p p 1 (-1));
    ghost_ (H.exclude_put_law left0 fp (M.location p 0) (Some 40));
    ghost_ (H.exclude_put_law right0 fp (M.location p 1) (Some 2));
    ghost_ (H.exclude_union_law lh rh fp);
    ghost_ (H.exclude_union_law left0 right0 fp);
    ghost_ (H.split_law h selection);
    ghost_ (H.partition_law fp before);
    ghost_ (
      let marker = M.location p (-1) in
      let _ = H.mem (H.union fp before) marker in
      let _ = H.mem (H.exclude h selection) marker in
      let _ = H.mem (H.put right0 (M.location p 1) (Some 2)) marker in
      let _ = H.mem (H.union lh rh) marker in
      ());
    let joined = P.join left right in
    M.free p joined

let () =
  let r = P.alloc 7 (P.empty ()) in
  let token = lifecycle r.state in
  let seven : {v : int | v = 7} = P.read r.value (borrow_ token) in
  assert (seven = 7)

let free_uninitialized : (n : {n : int | n >= 0}) ->
    (token : P.token) @ unique ghost ->
    {s : P.token | P.own s === P.own token} @ unique ghost = fun n token ->
  let before = ghost_ (P.own (borrow_ token)) in
  let r = M.malloc n token in
  match r.value with
  | None -> r.state
  | Some p ->
    ghost_ (M.allocated_covers p before);
    ghost_ (M.footprint_at p (-1));
    ghost_ (H.partition_law (M.footprint p) before);
    M.free p r.state

let () =
  let token = free_uninitialized 0 (P.empty ()) in
  let token = free_uninitialized 4096 token in
  let _ = free_uninitialized 17 token in
  ()

let partitioned : (n : {n : int | n >= 0}) ->
    (cut : {i : int | 0 <= i && i <= n}) ->
    (token : P.token) @ unique ghost ->
    {s : P.token | P.own s === P.own token} @ unique ghost =
    fun n cut token ->
  let before = ghost_ (P.own (borrow_ token)) in
  let r = M.malloc n token in
  match r.value with
  | None -> r.state
  | Some p ->
    let h = ghost_ (P.own (borrow_ r.state)) in
    let selection = ghost_ (M.range p 0 cut) in
    ghost_ (M.allocated_covers p before);
    ghost_ (M.footprint_at p (-1));
    ghost_ (H.split_law h selection);
    ghost_ (H.partition_law (M.footprint p) before);
    let split = P.split selection r.state in
    let token = P.join split.left split.right in
    M.free p token

let () =
  for cut = 0 to 32 do
    if 0 <= cut && cut <= 32 then
      let _ = partitioned 32 cut (P.empty ()) in ()
  done

let freed_handle () =
  let r = M.malloc 1 (P.empty ()) in
  match r.value with
  | None -> None
  | Some p ->
    ghost_ (M.allocated_covers p (H.empty ()));
    ghost_ (M.footprint_at p (-1));
    let _ = M.free p r.state in
    Some p

let () =
  match freed_handle () with
  | None -> ()
  | Some p ->
    let hash = Hashtbl.hash p in
    Gc.full_major ();
    Gc.compact ();
    assert (M.length p = 1);
    for _ = 1 to 100 do
      match freed_handle () with
      | None -> ()
      | Some q -> assert (not (M.equal p q) && p <> q && Hashtbl.hash p = hash)
    done

let last_byte (n : {n : int | n > 0}) =
  let r = M.malloc n (P.empty ()) in
  match r.value with
  | None -> ()
  | Some p ->
    let i = n - 1 in
    ghost_ (M.footprint_at p i);
    let before = ghost_ (P.own (borrow_ r.state)) in
    let token = M.write p i 255 r.state in
    Gc.full_major ();
    let value : {v : int | v = 255} = M.read p i (borrow_ token) in
    assert (value = 255);
    ghost_ (M.allocated_covers p (H.empty ()));
    ghost_ (M.write_covers before p 0 n i 255);
    ghost_ (M.footprint_at p (-1));
    ghost_ (M.location_law p p i (-1));
    ghost_ (
      let marker = M.location p (-1) in
      let _ = H.mem (H.union (M.footprint p) (H.empty ())) marker in
      let _ = H.mem (H.put before (M.location p i) (Some 255)) marker in
      ());
    let _ = M.free p token in ()

let () = last_byte 1; last_byte 4096
