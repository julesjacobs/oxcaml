(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml unique_cell.mli unique_cell.ml unique_lock.mli unique_lock.ml unique_lock_demo.ml";
 { native; }
*)

module Data = struct
  type t = int
  type model = int
  let (snapshot @ total) (x : int @ local immutable) = ghost_ x
end
module L = Unique_lock.Make(Data)
module C = L
module P = Ghost_pref

let (try_increment @ portable) (a : L.t) =
  let acquired = L.try_acquire a in
  let success = acquired.P.value in
  let t = acquired.P.state in
  if success then begin
    let cell = a in
    let before = ghost_ (P.own (borrow_ t)) in
    ghost_ (L.owned_def cell before);
    let t : {t : L.contents P.token | match P.Heap.at (P.own t) (L.location cell) with
      | Some (Some _) -> true | _ -> false} = t in
    let r = C.take cell t in
    let value = r.C.value in
    let value = value + 1 in
    let t = r.C.state in
    let t : {t : L.contents P.token | P.Heap.at (P.own t) (L.location cell) === Some None} = t in
    let written = ghost_ (Some (Data.snapshot (borrow_ value))) in
    let t = C.put cell value t in
    let h = ghost_ (P.own (borrow_ t)) in
    let p = ghost_ (L.location cell) in
    let empty = ghost_ (P.Heap.empty ()) in
    let old = ghost_ (match P.Heap.at before p with Some v -> v | None -> None) in
    let vacant = ghost_ None in
    ghost_ (P.Heap.put_law empty p old vacant);
    ghost_ (P.Heap.put_law empty p vacant written);
    ghost_ (L.owned_def cell h);
    let t : {t : L.contents P.token | L.owned (a) (P.own t)} = t in
    let _ = L.release a t in true
  end else false

let take_final (a : L.t) =
  let acquired = L.try_acquire a in
  let success = acquired.P.value in
  assert success;
  if success then begin
    let cell = a in
    let t = acquired.P.state in
    let h = ghost_ (P.own (borrow_ t)) in
    ghost_ (L.owned_def cell h);
    let t : {t : L.contents P.token | match P.Heap.at (P.own t) (L.location cell) with
      | Some (Some _) -> true | _ -> false} = t in
    let r = C.take cell t in
    r.C.value
  end else -1

let () =
  let initial = 0 in
  let a = L.make initial in
  for _i = 1 to 10 do assert (try_increment a) done;
  assert (take_final a = 10)
