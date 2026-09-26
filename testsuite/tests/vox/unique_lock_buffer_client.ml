(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml raw_memory.mli raw_memory.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml unique_lock.mli unique_lock.ml unique_lock_buffer_client.ml";
 { bytecode; }
 { native; }
*)
module P = Ghost_pref
module H = P.Heap
module M = Raw_memory
module Data = struct
  type buffer : value mod portable contended = {
    block : M.t @@ aliased;
    permission : M.contents P.token @@ ghost;
  }
  type t : value mod portable contended = {b : buffer |
    M.length b.block = 1 && H.mem (P.own b.permission) (M.location b.block 0)
    && H.mem (P.own b.permission) (M.location b.block (-1))}
  type model = int
  let[@def] (snapshot @ total) (x : t @ local immutable) = ghost_ 0
end
module L = Unique_lock.Make(Data)

let take : (a : L.t) ->
    {r : Data.t L.step | P.own r.state ===
      H.put (H.empty ()) (L.location a) None} @ unique = fun a ->
  let r = L.try_acquire a in
  if r.value then begin
    let h = ghost_ (P.own (borrow_ r.state)) in
    ghost_ (L.owned_def a h);
    let old = ghost_ (match H.at h (L.location a) with
      | Some v -> v | None -> None) in
    let result = L.take a r.state in
    ghost_ (H.put_law (H.empty ()) (L.location a) old None);
    result
  end else failwith "unexpected busy lock"

let restore : (a : L.t) -> Data.t @ unique ->
    {t : L.contents P.token | P.own t ===
      H.put (H.empty ()) (L.location a) None} @ unique ghost ->
    {t : L.contents P.token | P.own t === H.empty ()} @ unique ghost =
  fun a value t ->
  let _h = ghost_ (P.own (borrow_ t)) in
  ghost_ (Data.snapshot_def (borrow_ value));
  let t = L.put a value t in
  let after = ghost_ (P.own (borrow_ t)) in
  ghost_ (H.put_law (H.empty ()) (L.location a) (Some 0) None);
  ghost_ (H.put_law (H.empty ()) (L.location a) None (Some 0));
  ghost_ (L.owned_def a after);
  L.release a t

let () =
  let allocation = M.malloc 1 (P.empty ()) in
  match allocation.value with
  | None -> failwith "buffer allocation failed"
  | Some block ->
    ghost_ (M.allocated_covers block (H.empty ());
      M.covers_get (P.own (borrow_ allocation.state)) block 0 1 0;
      M.footprint_at block (-1));
    let initial : Data.t = { block; permission = allocation.state } in
    let a = L.make initial in
    let r = take a in
    let { Data.block; permission } = r.value in
    let permission = M.write block 0 42 permission in
    let actual : {n : int | n = 42} = M.read block 0 (borrow_ permission) in
    assert (actual = 42);
    let _ = restore a { block; permission } r.state in
    let r = take a in
    let { Data.block; permission } = r.value in
    ghost_ (M.covers_intro (P.own (borrow_ permission)) block 0 1
      (fun i -> ()));
    let _ = M.free block permission in ()
