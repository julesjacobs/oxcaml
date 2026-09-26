module P = Ghost_pref
let[@def] (good @ total) (p : int P.t @ immutable)
    (h : int P.heap @ immutable) =
  ghost_ (match P.Heap.at h p with
    | None -> false
    | Some x -> 0 <= x && h === P.Heap.put (P.Heap.empty ()) p x)
module Invariant = struct
  type payload = int
  type key = { location : int P.t @@ ghost }
  let[@def] (holds @ total) (p : key @ immutable)
      (flag : int @ immutable) (h : int P.heap @ immutable) =
    ghost_ ((flag = 0 && good p.location h) ||
      (flag = 1 && h === P.Heap.empty ()))
end
module A = Verified_atomic.Make (Invariant)

let[@def] (acquire_post @ total) (p : int P.t @ immutable)
    (success : bool @ immutable) (h : int P.heap @ immutable) =
  ghost_ (if success then good p h else h === P.Heap.empty ())
let[@def] (release_post @ total) (success : bool @ immutable)
    (h : int P.heap @ immutable) =
  ghost_ (success && h === P.Heap.empty ())

let (acquire_transfer @ total) :
    (p : int P.t) @ immutable ghost -> (before : int) @ immutable ghost ->
    (inside : {g : int P.token |
      Invariant.holds { location = p } before (P.own g) &&
      P.Heap.disjoint (P.own g) (P.Heap.empty ())}) @ unique ghost ->
    (outside : {g : int P.token | P.own g === P.Heap.empty ()})
      @ unique ghost ->
    {r : A.transfer |
      Invariant.holds { location = p } (if before = 0 then 1 else before)
        (P.own r.restored) && acquire_post p (before = 0) (P.own r.outgoing)}
      @ unique = fun p before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def { location = p } before hi);
  ghost_ (Invariant.holds_def { location = p } (if before = 0 then 1 else before) ho);
  ghost_ (acquire_post_def p (before = 0) hi);
  { A.restored = outside; outgoing = inside }

let (release_transfer @ total) :
    (p : int P.t) @ immutable ghost ->
    (caller : {h : int P.heap | good p h}) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : int P.token |
      Invariant.holds { location = p } before (P.own g) &&
      P.Heap.disjoint (P.own g) caller}) @ unique ghost ->
    (outside : {g : int P.token | P.own g === caller}) @ unique ghost ->
    {r : A.transfer |
      Invariant.holds { location = p } (if before = 1 then 0 else before)
        (P.own r.restored) && release_post (before = 1) (P.own r.outgoing)}
      @ unique = fun p caller before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def { location = p } before hi);
  ghost_ (good_def p hi; good_def p ho);
  ghost_ (Invariant.holds_def { location = p } (if before = 1 then 0 else before) ho);
  ghost_ (release_post_def (before = 1) hi);
  { A.restored = outside; outgoing = inside }

type storage = { location : int P.t; atomic : A.t }
type t = {a : storage | (A.key a.atomic).Invariant.location === a.location}
let[@def] (location @ total) (a : t @ immutable) = a.location
let[@def] (owned @ total) (a : t @ immutable) (h : int P.heap @ immutable) =
  ghost_ (match P.Heap.at h (location a) with
    | None -> false
    | Some x -> 0 <= x && h === P.Heap.put (P.Heap.empty ()) (location a) x)

let make (x : {n : int | 0 <= n}) : t =
  let e = P.empty () in
  let allocation = P.alloc x e in
  let p = allocation.P.value in
  let t = allocation.P.state in
  let h = ghost_ (P.own (borrow_ t)) in
  let zero = 0 in
  ghost_ (good_def p h);
  ghost_ (Invariant.holds_def { location = p } zero h);
  let g : {g : int P.token | Invariant.holds { location = p } zero (P.own g)} = t in
  let a = A.create (ghost_ { location = p }) zero g in
  let result : t = { location = p; atomic = a } in result
let try_acquire (a : t) :
    {r : (bool, int) P.step | if r.P.value then owned a (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique =
  let p = a.location in
  let e = P.empty () in
  let r = A.compare_and_set a.atomic 0 1
    (ghost_ (fun success h -> acquire_post p success h)) e
    (ghost_ (fun before inside outside ->
      acquire_transfer p before inside outside)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (acquire_post_def p success h; location_def a;
    good_def a.location h; owned_def a h);
  let result : (bool, int) P.step = { value = success; state = r.#state } in
  result
let release : (a : t) ->
    {t : int P.token | owned a (P.own t)} @ unique ghost ->
    {t : int P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t ->
  let p = a.location in
  let ht = ghost_ (P.own (borrow_ t)) in
  ghost_ (location_def a; owned_def a ht; good_def p ht);
  let r = A.compare_and_set a.atomic 1 0
    (ghost_ (fun success h -> release_post success h)) t
    (ghost_ (fun before inside outside ->
      release_transfer p ht before inside outside)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (release_post_def success h);
  let t = r.#state in
  t
let try_increment (a : t) =
  let r = try_acquire a in
  let success = r.P.value in
  let t = r.P.state in
  if success then begin
    let p = a.location in
    let h = ghost_ (P.own (borrow_ t)) in
    ghost_ (location_def a; owned_def a h; good_def p h);
    let x : {v : int | 0 <= v && h === P.Heap.put (P.Heap.empty ()) p v} =
      let b = borrow_ t in
      let b : {b : int P.token | P.Heap.mem (P.own b) p} = b in
      let v = P.read p b in
      v in
    let next = x + 1 in
    let y = if next < 0 then x else next in
    let t : {t : int P.token | P.Heap.mem (P.own t) p} = t in
    let t = P.write p y t in
    let h = ghost_ (P.own (borrow_ t)) in
    let empty = ghost_ (P.Heap.empty ()) in
    ghost_ (P.Heap.put_law empty p x y);
    ghost_ (location_def a; owned_def a h; good_def p h);
    let t : {t : int P.token | owned a (P.own t)} = t in
    let _ = release a t in
    true
  end else false

let read_owned : (a : t) ->
    (t : {t : int P.token | owned a (P.own t)}) @ local read ghost ->
    {n : int | 0 <= n && P.Heap.at (P.own t) (location a) === Some n} =
  fun a t ->
    let p = a.location in
    let h = ghost_ (P.own t) in
    ghost_ (location_def a; owned_def a h; good_def p h);
    let t : {t : int P.token | P.Heap.mem (P.own t) p} = t in
    let v = P.read p t in v
