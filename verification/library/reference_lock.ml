module P = Ghost_pref

(* The guarded cell is an integer reference whose value is nonnegative. *)
module Cell = struct
  type payload = int
  type cell = int P.t
  let[@def] (location @ total) (p : cell @ immutable) = ghost_ p
  let[@def] (full @ total) (x : int @ immutable) = ghost_ (0 <= x)
end
module L = Spin_lock.Make (Cell)
type t = L.t
let[@def] (location @ total) (a : t @ immutable) = L.cell a
(* [owned], [try_acquire] and [release] are the functor's. The last two are
   wrapped, restating their contracts, because signature matching compares
   refinements syntactically and the functor's mention [L.owned], not
   [owned]. [owned_def] states the functor's in this module's terms and is
   proved from it. *)
let owned = L.owned
let (owned_def @ total) (a : t @ immutable) (h : int P.heap @ immutable) :
    {u : unit | owned a h === (ghost_ (
      match P.Heap.at h (location a) with
      | None -> false
      | Some x -> 0 <= x && h === P.Heap.put (P.Heap.empty ()) (location a) x))} =
  ghost_ (L.owned_def a h; location_def a; Cell.location_def (L.cell a));
  ghost_ (match P.Heap.at h (location a) with
    | None -> ()
    | Some x -> Cell.full_def x);
  ()

let try_acquire (a : t) :
    {r : (bool, int) P.step | if r.P.value then owned a (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique = L.try_acquire a
let release : (a : t) ->
    {t : int P.token | owned a (P.own t)} @ unique ghost ->
    {t : int P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t -> L.release a t

let make (x : {n : int | 0 <= n}) : t =
  let e = P.empty () in
  let allocation = P.alloc x e in
  let p = allocation.P.value in
  let t = allocation.P.state in
  ghost_ (Cell.location_def p; Cell.full_def x);
  L.create p t

let read_owned : (a : t) ->
    (t : {t : int P.token | owned a (P.own t)}) @ local read ghost ->
    {n : int | 0 <= n && P.Heap.at (P.own t) (location a) === Some n} =
  fun a t ->
    let p = L.cell a in
    ghost_ (location_def a; owned_def a (P.own t));
    let v = P.read p t in v

let try_increment (a : t) =
  let r = try_acquire a in
  let success = r.P.value in
  let t = r.P.state in
  if success then begin
    let p = L.cell a in
    let h = ghost_ (P.own (borrow_ t)) in
    ghost_ (location_def a; owned_def a h);
    let x : {v : int | 0 <= v && h === P.Heap.put (P.Heap.empty ()) p v} =
      let b = borrow_ t in
      let v = P.read p b in
      v in
    let next = x + 1 in
    let y = if next < 0 then x else next in
    let t = P.write p y t in
    let h = ghost_ (P.own (borrow_ t)) in
    let empty = ghost_ (P.Heap.empty ()) in
    ghost_ (P.Heap.put_law empty p x y);
    ghost_ (owned_def a h);
    let _ = release a t in
    true
  end else false
