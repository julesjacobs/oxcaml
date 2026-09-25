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
type storage = { location : int P.t; atomic : A.t }
type t = {a : storage | (A.key a.atomic).Invariant.location === a.location}
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
    {r : (bool, int) P.step | if r.P.value then good (a.location) (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique =
  let p = a.location in
  let zero = 0 in
  let one = 1 in
  let e = P.empty () in
  let[@def] (post @ total) (success : bool @ immutable)
      (h : int P.heap @ immutable) =
    ghost_ (if success then good p h else h === P.Heap.empty ()) in
  let erased_post = ghost_ post in
  let r = A.compare_and_set a.atomic zero one erased_post e
    (ghost_ (fun before inside outside ->
      let hi = ghost_ (P.own (borrow_ inside)) in
      let ho = ghost_ (P.own (borrow_ outside)) in
      let after = ghost_ (if before = zero then one else before) in
      let success = ghost_ (before = zero) in
      ghost_ (Invariant.holds_def { location = p } before hi);
      ghost_ (Invariant.holds_def { location = p } after ho);
      ghost_ (post_def success hi);
      let r : A.transfer = { restored = outside; outgoing = inside } in
      r)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (post_def success h);
  let result : (bool, int) P.step = { value = success; state = r.#state } in
  result
let release : (a : t) ->
    {t : int P.token | good (a.location) (P.own t)} @ unique ghost ->
    {t : int P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t ->
  let p = a.location in
  let zero = 0 in
  let one = 1 in
  let ht = ghost_ (P.own (borrow_ t)) in
  ghost_ (good_def p ht);
  let[@def] (post @ total) (success : bool @ immutable)
      (h : int P.heap @ immutable) =
    ghost_ (success && h === P.Heap.empty ()) in
  let erased_post = ghost_ post in
  let r = A.compare_and_set a.atomic one zero erased_post t
    (ghost_ (fun before inside outside ->
      let hi = ghost_ (P.own (borrow_ inside)) in
      let ho = ghost_ (P.own (borrow_ outside)) in
      let after = ghost_ (if before = one then zero else before) in
      let success = ghost_ (before = one) in
      ghost_ (Invariant.holds_def { location = p } before hi);
      ghost_ (good_def p hi);
      ghost_ (good_def p ho);
      ghost_ (Invariant.holds_def { location = p } after ho);
      ghost_ (post_def success hi);
      let r : A.transfer = { restored = outside; outgoing = inside } in
      r)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (post_def success h);
  let t = r.#state in
  t
let try_increment (a : t) =
  let r = try_acquire a in
  let success = r.P.value in
  let t = r.P.state in
  if success then begin
    let p = a.location in
    let h = ghost_ (P.own (borrow_ t)) in
    ghost_ (good_def p h);
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
    ghost_ (good_def p h);
    let t : {t : int P.token | good (a.location) (P.own t)} = t in
    let _ = release a t in
    true
  end else false

let read_owned : (a : t) ->
    (t : {t : int P.token | good (a.location) (P.own t)}) @ local read ghost ->
    {n : int | 0 <= n && P.Heap.at (P.own t) a.location === Some n} =
  fun a t ->
    let p = a.location in
    let h = ghost_ (P.own t) in
    ghost_ (good_def p h);
    let t : {t : int P.token | P.Heap.mem (P.own t) p} = t in
    let v = P.read p t in v


let[@def] (location @ total) (a : t @ immutable) = a.location
let[@def] (owned @ total) (a : t @ immutable) (h : int P.heap @ immutable) =
  ghost_ (match P.Heap.at h (location a) with
    | None -> false
    | Some x -> 0 <= x && h === P.Heap.put (P.Heap.empty ()) (location a) x)
let try_acquire_internal = try_acquire
let try_acquire : (a : t) ->
  {r : (bool, int) Ghost_pref.step |
    if r.value then owned a (Ghost_pref.own r.state)
    else Ghost_pref.own r.state === Ghost_pref.Heap.empty ()} @ unique = fun a ->
  ghost_ (location_def a);
  let r = try_acquire_internal a in
  let h = ghost_ (P.own (borrow_ r.P.state)) in
  ghost_ (good_def a.location h; owned_def a h);
  r
let release_internal = release
let release : (a : t) ->
  {t : int Ghost_pref.token | owned a (Ghost_pref.own t)} @ unique ghost ->
  {t : int Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.empty ()} @ unique ghost = fun a t ->
  ghost_ (location_def a);
  let h = ghost_ (P.own (borrow_ t)) in
  ghost_ (owned_def a h; good_def a.location h);
  release_internal a t
let read_owned_internal = read_owned
let read_owned : (a : t) ->
  (t : {t : int Ghost_pref.token | owned a (Ghost_pref.own t)}) @ local read ghost ->
  {n : int | 0 <= n && Ghost_pref.Heap.at (Ghost_pref.own t) (location a) === Some n} = fun a t ->
  ghost_ (location_def a);
  let h = ghost_ (P.own t) in
  ghost_ (owned_def a h; good_def a.location h);
  read_owned_internal a t
