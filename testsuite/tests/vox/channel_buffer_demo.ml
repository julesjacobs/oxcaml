(* TEST
 has-z3;
 multicore;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml one_shot.mli one_shot.ml channel_buffer.mli channel_buffer.ml channel_buffer_demo.ml";
 { native; }
*)
open Channel_buffer

let run () =
  let allocation = M.malloc 2 (P.empty ()) in
  match allocation.value with
  | None -> failwith "Could not allocate the demo buffer"
  | Some block ->
    let initial = ghost_ (P.own (borrow_ allocation.state)) in
    ghost_ (M.allocated_covers block (H.empty ()));
    ghost_ (M.split_covers initial block 0 1 2);
    let first = P.split (ghost_ (M.range block 0 1)) allocation.state in
    let rest = ghost_ (P.own (borrow_ first.right)) in
    ghost_ (M.split_covers rest block 1 2 2);
    let second = P.split (ghost_ (M.range block 1 2)) first.right in
    ghost_ (M.covers_get (P.own (borrow_ first.left)) block 0 1 0);
    ghost_ (M.covers_get (P.own (borrow_ second.left)) block 1 2 1);
    let { answer = left; worker = worker_left } =
      dispatch block 0 79 first.left in
    let { answer = right; worker = worker_right } =
      dispatch block 1 75 second.left in
    let a, left = read_receipt (C.recv left) in
    let b, right = read_receipt (C.recv right) in
    let a : {v : int | v = 79} = a in
    let b : {v : int | v = 75} = b in
    Printf.printf "Two workers filled one buffer: %c%c\n"
      (Char.chr a) (Char.chr b);
    let lh = ghost_ (P.own (borrow_ left.permission)) in
    let rh = ghost_ (P.own (borrow_ right.permission)) in
    let marker = ghost_ (P.own (borrow_ second.right)) in
    ghost_ (
      M.covers_intro (H.union lh rh) block 0 2 (fun i ->
        if i = 0 then () else if i = 1 then () else ());
      M.covers_intro (H.union (H.union lh rh) marker) block 0 2 (fun i ->
        M.covers_get (H.union lh rh) block 0 2 i);
      M.footprint_at block (-1);
      M.range_at block 0 1 (-1);
      M.range_at block 1 2 (-1);
      let key = M.location block (-1) in
      let _ = H.mem (H.union (M.footprint block) (H.empty ())) key in
      let _ = H.mem (H.exclude initial (M.range block 0 1)) key in
      let _ = H.mem (H.exclude rest (M.range block 1 2)) key in
      let _ = H.mem (H.union (H.union lh rh) marker) key in ());
    let bytes = P.join left.permission right.permission in
    let all = P.join bytes second.right in
    let _ = M.free block all in
    Domain.join worker_left;
    Domain.join worker_right;
    print_endline "Both ownership tokens returned; buffer freed."

let () = run ()
