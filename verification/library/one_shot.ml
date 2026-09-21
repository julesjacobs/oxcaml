module P = Ghost_pref
module Slot = Unique_cell.Slot

module Invariant = struct
  type key = { location : bool P.t @@ ghost }
  let[@def] (full @ total) (k : key @ immutable) (h : P.heap @ immutable) =
    ghost_ (h === P.Heap.put (P.Heap.empty ()) k.location true)
  (* Zero leaves the slot with an endpoint; one owns the published payload. *)
  let[@def] (holds @ total) (k : key @ immutable)
      (flag : int @ immutable) (h : P.heap @ immutable) =
    ghost_ ((flag = 0 && h === P.Heap.empty ()) || (flag = 1 && full k h))
end
module A = Verified_atomic.Make (Invariant)

type ('a : value mod portable contended) sender = {
  cell : 'a Slot.t @@ aliased;
  atomic : A.t @@ aliased;
  permission : P.token @@ ghost;
}
type ('a : value mod portable contended) receiver = {
  cell : 'a Slot.t @@ aliased;
  atomic : A.t @@ aliased;
}
type ('a : value mod portable contended) send = {s : 'a sender |
  Slot.location s.cell === (A.key s.atomic).Invariant.location &&
  P.own s.permission ===
    P.Heap.put (P.Heap.empty ()) (Slot.location s.cell) false}
type ('a : value mod portable contended) recv = {r : 'a receiver |
  Slot.location r.cell === (A.key r.atomic).Invariant.location}

let create (type a : value mod portable contended) () :
    (a send * a recv) @ unique =
  let r = Slot.empty () (P.empty ()) in
  let cell = r.Slot.value in
  let k = ghost_ { Invariant.location = Slot.location cell } in
  let empty = P.empty () in
  ghost_ (Invariant.holds_def k 0 (P.own (borrow_ empty)));
  let atomic = A.create k 0 empty in
  let s : a send = { cell; atomic; permission = r.Slot.state } in
  let r : a recv = { cell; atomic } in
  s, r

let send (type a : value mod portable contended)
    (s : a send @ unique) (value : a @ unique) =
  let { cell; atomic; permission } = s in
  let k = ghost_ (A.key atomic) in
  let permission = Slot.put cell value permission in
  let h = ghost_ (P.own (borrow_ permission)) in
  ghost_ (P.Heap.put_law (P.Heap.empty ()) (Slot.location cell) false true);
  ghost_ (Invariant.full_def k h);
  let[@def] (post @ total) (success : bool @ immutable)
      (h : P.heap @ immutable) =
    ghost_ (success && h === P.Heap.empty ()) in
  let _ = A.compare_and_set atomic 0 1 (ghost_ post) permission
    (ghost_ (fun before inside outside ->
      let hi = ghost_ (P.own (borrow_ inside)) in
      let ho = ghost_ (P.own (borrow_ outside)) in
      ghost_ (Invariant.holds_def k before hi);
      ghost_ (Invariant.full_def k hi);
      ghost_ (Invariant.full_def k ho);
      ghost_ (Invariant.holds_def k 1 ho);
      ghost_ (post_def (before = 0) hi);
      { A.restored = outside; outgoing = inside })) in
  ()

let rec await : ('a : value mod portable contended).
    (cell : 'a Slot.t) ->
    {a : A.t | Slot.location cell === (A.key a).Invariant.location} ->
    'a @ unique = fun cell atomic ->
  let k = ghost_ (A.key atomic) in
  let[@def] (post @ total) (success : bool @ immutable)
      (h : P.heap @ immutable) =
    ghost_ (if success then Invariant.full k h else h === P.Heap.empty ()) in
  let result = A.compare_and_set atomic 1 0 (ghost_ post) (P.empty ())
    (ghost_ (fun before inside outside ->
      let hi = ghost_ (P.own (borrow_ inside)) in
      let ho = ghost_ (P.own (borrow_ outside)) in
      ghost_ (Invariant.holds_def k before hi);
      ghost_ (Invariant.holds_def k (if before = 1 then 0 else before) ho);
      ghost_ (post_def (before = 1) hi);
      { A.restored = outside; outgoing = inside })) in
  let success = result.#value in
  let permission = result.#state in
  let h = ghost_ (P.own (borrow_ permission)) in
  ghost_ (post_def success h);
  if success then begin
    ghost_ (Invariant.full_def k h);
    let result = Slot.take cell permission in
    result.Slot.value
  end else begin
    Domain.cpu_relax ();
    await cell atomic
  end

let recv (r : 'a recv @ unique) : 'a @ unique =
  let { cell; atomic } = r in
  await cell atomic
