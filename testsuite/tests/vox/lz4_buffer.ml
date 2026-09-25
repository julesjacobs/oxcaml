(* TEST
 has-z3;
 flags = "-extension refinement_types -principal";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml \
                raw_memory.mli raw_memory.ml vox_lz4_spec_storage.ml vox_lz4_buffer.ml \
                lz4_buffer.ml";
 { bytecode; }
 { native; }
*)

module B = Vox_lz4_buffer
module P = Ghost_pref
module H = P.Heap
module M = Raw_memory

let run () =
  let allocated = M.malloc 2 (P.empty ()) in
  match allocated.value with
  | None -> ()
  | Some block ->
    let token = allocated.state in
    let before = ghost_ (P.own (borrow_ token)) in
    ghost_ (M.allocated_covers block (H.empty ()));
    ghost_ (B.append_initialized before block 0 97);
    ghost_ (M.footprint_at block 0);
    let token = M.write block 0 97 token in
    let middle = ghost_ (P.own (borrow_ token)) in
    ghost_ (M.write_covers before block 0 2 0 97);
    ghost_ (M.covers_get middle block 0 2 1);
    ghost_ (B.append_initialized middle block 1 98);
    let token = M.write block 1 98 token in
    let after = ghost_ (P.own (borrow_ token)) in
    ghost_ (M.write_covers middle block 0 2 1 98);
    ghost_ (B.initialized_get after block 2 0);
    ghost_ (B.initialized_get after block 2 1);
    let a = M.read block 0 (borrow_ token) in
    let b = M.read block 1 (borrow_ token) in
    assert (a = 97 && b = 98);
    ghost_ (M.footprint_at block (-1));
    ghost_ (M.location_law block block 0 (-1));
    ghost_ (M.location_law block block 1 (-1));
    ghost_ (
      let marker = M.location block (-1) in
      let _ = H.mem before marker in
      let _ = H.mem middle marker in
      let _ = H.mem after marker in
      ());
    let _ = M.free block token in ()

let () = run ()

let () =
  match B.create 2 with
  | None -> ()
  | Some buffer ->
    let buffer = B.append buffer 97 in
    let buffer = B.append buffer 98 in
    let first, buffer = B.get buffer 0 in
    let second, buffer = B.get buffer 1 in
    assert (first = 97 && second = 98);
    B.release buffer
