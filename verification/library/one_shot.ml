module P = Ghost_pref
module Slot = Unique_cell.Slot

module Invariant = struct
  type payload = bool
  type key = { location : bool P.t @@ ghost }
  let[@def] (full @ total) (k : key @ immutable) (h : bool P.heap @ immutable) =
    ghost_ (h === P.Heap.put (P.Heap.empty ()) k.location true)
  (* Zero leaves the slot with an endpoint; one owns the published payload. *)
  let[@def] (holds @ total) (k : key @ immutable)
      (flag : int @ immutable) (h : bool P.heap @ immutable) =
    ghost_ ((flag = 0 && h === P.Heap.empty ()) || (flag = 1 && full k h))
end
module A = Verified_atomic.Make (Invariant)

let[@def] (publication_post @ total) (success : bool @ immutable)
    (h : bool P.heap @ immutable) =
  ghost_ (success && h === P.Heap.empty ())
let[@def] (reception_post @ total) (k : Invariant.key @ immutable)
    (success : bool @ immutable) (h : bool P.heap @ immutable) =
  ghost_ (if success then Invariant.full k h else h === P.Heap.empty ())

let (publication_transfer @ total) :
    (k : Invariant.key) @ immutable ghost ->
    (caller : {h : bool P.heap | Invariant.full k h}) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : bool P.token | Invariant.holds k before (P.own g) &&
      P.Heap.disjoint (P.own g) caller}) @ unique ghost ->
    (outside : {g : bool P.token | P.own g === caller}) @ unique ghost ->
    {r : A.transfer |
      Invariant.holds k (if before = 0 then 1 else before) (P.own r.restored) &&
      publication_post (before = 0) (P.own r.outgoing)} @ unique =
  fun k caller before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def k before hi);
  ghost_ (Invariant.full_def k hi; Invariant.full_def k ho);
  ghost_ (Invariant.holds_def k (if before = 0 then 1 else before) ho);
  ghost_ (publication_post_def (before = 0) hi);
  { A.restored = outside; outgoing = inside }

let (reception_transfer @ total) :
    (k : Invariant.key) @ immutable ghost -> (before : int) @ immutable ghost ->
    (inside : {g : bool P.token | Invariant.holds k before (P.own g) &&
      P.Heap.disjoint (P.own g) (P.Heap.empty ())}) @ unique ghost ->
    (outside : {g : bool P.token | P.own g === P.Heap.empty ()}) @ unique ghost ->
    {r : A.transfer |
      Invariant.holds k (if before = 1 then 0 else before) (P.own r.restored) &&
      reception_post k (before = 1) (P.own r.outgoing)} @ unique =
  fun k before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def k before hi);
  ghost_ (Invariant.holds_def k (if before = 1 then 0 else before) ho);
  ghost_ (reception_post_def k (before = 1) hi);
  { A.restored = outside; outgoing = inside }

type ('a : value mod portable contended) sender = {
  cell : 'a Slot.t @@ aliased;
  atomic : A.t @@ aliased;
  permission : bool P.token @@ ghost;
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
  let _ = A.compare_and_set atomic 0 1
    (ghost_ (fun success h -> publication_post success h)) permission
    (ghost_ (fun before inside outside ->
      publication_transfer k h before inside outside)) in
  ()

let rec await : ('a : value mod portable contended).
    (cell : 'a Slot.t) ->
    {a : A.t | Slot.location cell === (A.key a).Invariant.location} ->
    'a @ unique = fun cell atomic ->
  let k = ghost_ (A.key atomic) in
  let result = A.compare_and_set atomic 1 0
    (ghost_ (fun success h -> reception_post k success h)) (P.empty ())
    (ghost_ (fun before inside outside ->
      reception_transfer k before inside outside)) in
  let success = result.#value in
  let permission = result.#state in
  let h = ghost_ (P.own (borrow_ permission)) in
  ghost_ (reception_post_def k success h);
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
