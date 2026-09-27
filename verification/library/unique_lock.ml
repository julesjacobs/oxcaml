module Make (V : Unique_cell.Payload) = struct
module Cell = Unique_cell.Make(V)
module P = Ghost_pref

(* The guarded cell is a unique cell, full when it holds a payload. *)
module Spec = struct
  type payload = Cell.contents
  type cell = Cell.t
  let[@def] (location @ total) (c : Cell.t @ immutable) =
    ghost_ (Cell.location c)
  let[@def] (full @ total) (v : Cell.contents @ immutable) =
    ghost_ (match v with Some _ -> true | None -> false)
end
module L = Spin_lock.Make (Spec)
type t = L.t
type contents = Cell.contents
(* [owned], [try_acquire] and [release] are the functor's. The last two are
   wrapped, restating their contracts, because signature matching compares
   refinements syntactically and the functor's mention [L.owned], not
   [owned]. [owned_def] states the functor's in this module's terms and is
   proved from it. *)
let owned = L.owned
let[@def] (location @ total) (a : t @ local immutable) =
  ghost_ (Cell.location (L.cell a))
let (owned_def @ total) (a : t @ immutable)
    (h : Cell.contents P.heap @ immutable) :
    {u : unit | owned a h === (ghost_ (
      let p = location a in match P.Heap.at h p with
      | Some (Some x) -> h === P.Heap.put (P.Heap.empty ()) p (Some x)
      | _ -> false))} =
  ghost_ (L.owned_def a h; location_def a; Spec.location_def (L.cell a));
  ghost_ (match P.Heap.at h (location a) with
    | None -> ()
    | Some v -> Spec.full_def v);
  ()

let try_acquire (a : t) :
    {r : (bool, Cell.contents) P.step | if r.P.value then owned a (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique = L.try_acquire a
let release : (a : t) ->
    {t : Cell.contents P.token | owned a (P.own t)} @ unique ghost ->
    {t : Cell.contents P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t -> L.release a t

let make (x : V.t @ unique) : t =
  let e = P.empty () in
  let allocation = Cell.create x e in
  let c = allocation.Cell.value in
  let t = allocation.Cell.state in
  let h = ghost_ (P.own (borrow_ t)) in
  ghost_ (Spec.location_def c);
  ghost_ (match P.Heap.at h (Spec.location c) with
    | None -> ()
    | Some v -> Spec.full_def v);
  L.create c t

type 'a step = 'a Cell.step = { value : 'a; state : Cell.contents P.token @@ ghost }
let take : (a : t) ->
    (token : {t : contents Ghost_pref.token |
      match Ghost_pref.Heap.at (Ghost_pref.own t) (location a) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | Ghost_pref.Heap.at (Ghost_pref.own token) (location a)
        === Some (Some (V.snapshot r.value)) &&
      Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) None} @ unique = fun a t ->
  ghost_ (location_def a);
  Cell.take (L.cell a) t
let put : (a : t) -> (value : V.t) @ unique ->
    (token : {t : contents Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location a) === Some None}) @ unique ghost ->
    {t : contents Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) (Some (V.snapshot value))} @ unique ghost = fun a value t ->
  ghost_ (location_def a); Cell.put (L.cell a) value t

end
