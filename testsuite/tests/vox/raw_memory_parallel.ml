(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml one_shot.mli one_shot.ml raw_memory_parallel.ml";
 { native; }
*)
module P = Ghost_pref
module H = P.Heap
module M = Raw_memory
module C = One_shot

type region = { block : M.t @@ aliased; permission : M.contents P.token @@ ghost }
type input = {r : region | M.length r.block = 2 &&
  H.mem (P.own r.permission) (M.location r.block 0)}
type output = {r : region | M.length r.block = 2 &&
  H.mem (P.own r.permission) (M.location r.block 0) &&
  (match H.at (P.own r.permission) (M.location r.block 0) with
   | Some (Some v) -> v = 42 | _ -> false)}

let fill : (r : input) @ unique ->
    {s : output | s.block === r.block} @ unique = fun r ->
  let { block; permission } = r in
  let permission = M.write block 0 42 permission in
  { block; permission }

let run () =
  let allocation = M.malloc 2 (P.empty ()) in
  match allocation.value with
  | None -> ()
  | Some p ->
    ghost_ (M.footprint_at p 0);
    ghost_ (M.footprint_at p 1);
    ghost_ (M.footprint_at p (-1));
    ghost_ (M.range_at p 0 1 0);
    ghost_ (M.range_at p 0 1 1);
    ghost_ (M.range_at p 0 1 (-1));
    let h = ghost_ (P.own (borrow_ allocation.state)) in
    ghost_ (M.allocated_covers p (H.empty ()));
    ghost_ (M.split_covers h p 0 1 2);
    let split = P.split (ghost_ (M.range p 0 1)) allocation.state in
    ghost_ (M.covers_get (P.own (borrow_ split.left)) p 0 1 0);
    ghost_ (M.covers_get (P.own (borrow_ split.right)) p 1 2 1);
    let (reply, answer : {r : output | r.block === p} C.send *
        {r : output | r.block === p} C.recv) = C.create () in
    let (tx, rx : {r : input | r.block === p} C.send *
        {r : input | r.block === p} C.recv) = C.create () in
    let worker = Domain.Safe.spawn (fun () ->
      let r = C.recv rx in
      let r = fill r in
      C.send reply r) in
    C.send tx ({ block = p; permission = split.left } :
      {r : input | r.block === p});
    let right_before = ghost_ (P.own (borrow_ split.right)) in
    let right = M.write p 1 7 split.right in
    let received = C.recv answer in
    let { block; permission = left } = received in
    let value : {v : int | v = 42} = M.read block 0 (borrow_ left) in
    assert (value = 42);
    ghost_ (M.location_law p p 1 (-1));
    let rh = ghost_ (P.own (borrow_ right)) in
    let lh = ghost_ (P.own (borrow_ left)) in
    ghost_ (
      let marker = M.location p (-1) in
      let _ = H.mem (H.union (M.footprint p) (H.empty ())) marker in
      let _ = H.mem (H.exclude h (M.range p 0 1)) marker in
      let _ = H.mem (H.put right_before (M.location p 1) (Some 7)) marker in
      let _ = H.mem (H.union lh rh) marker in
      let _ = H.mem (H.put right_before (M.location p 1) (Some 7))
        (M.location p 1) in
      M.covers_intro (H.union lh rh) p 0 2 (fun i ->
        if i = 0 then () else if i = 1 then () else ()));
    let joined = P.join left right in
    let _ = M.free p joined in
    Domain.join worker

let () = for _ = 1 to 100 do run () done
