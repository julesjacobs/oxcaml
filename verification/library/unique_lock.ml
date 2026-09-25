module Make (V : Unique_cell.Payload) = struct
module Cell = Unique_cell.Make(V)
module P = Ghost_pref
let[@def] (good @ total) (c : Cell.t @ immutable)
    (h : Cell.contents P.heap @ immutable) =
  ghost_ (let p = Cell.location c in match P.Heap.at h p with
    | Some (Some x) -> h === P.Heap.put (P.Heap.empty ()) p (Some x)
    | _ -> false)
module Invariant = struct
  type payload = Cell.contents
  type key = { cell : Cell.t @@ ghost }
  let[@def] (holds @ total) (p : key @ immutable)
      (flag : int @ immutable) (h : Cell.contents P.heap @ immutable) =
    ghost_ ((flag = 0 && good p.cell h) ||
      (flag = 1 && h === P.Heap.empty ()))
end
module A = Verified_atomic.Make (Invariant)
type storage = { cell : Cell.t; atomic : A.t }
type t = {a : storage | (A.key a.atomic).Invariant.cell === a.cell}
type contents = Cell.contents
let make (x : V.t @ unique) : t =
  let refine_ e = P.empty () in
  let refine_ allocation = Cell.create x e in
  let p = allocation.Cell.value in
  let t = allocation.Cell.state in
  let h = ghost_ (P.own (borrow_ t)) in
  let zero = 0 in
  ghost_ (good_def p h);
  ghost_ (Invariant.holds_def { cell = p } zero h);
  let g : {g : Cell.contents P.token | Invariant.holds { cell = p } zero (P.own g)} = refine_ t in
  let refine_ a = A.create (ghost_ { cell = p }) zero g in
  let result : t = { cell = p; atomic = a } in result
let try_acquire (a : t) :
    {r : (bool, Cell.contents) P.step | if r.P.value then good (a.cell) (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique =
  let p = a.cell in
  let zero = 0 in
  let one = 1 in
  let refine_ e = P.empty () in
  let[@def] (post @ total) (success : bool @ immutable)
      (h : Cell.contents P.heap @ immutable) =
    ghost_ (if success then good p h else h === P.Heap.empty ()) in
  let erased_post = ghost_ post in
  let refine_ r = A.compare_and_set a.atomic zero one erased_post e
    (ghost_ (fun before inside outside ->
      let refine_ inside = inside in
      let refine_ outside = outside in
      let hi = ghost_ (P.own (borrow_ inside)) in
      let ho = ghost_ (P.own (borrow_ outside)) in
      let after = ghost_ (if before = zero then one else before) in
      let success = ghost_ (before = zero) in
      ghost_ (Invariant.holds_def { cell = p } before hi);
      ghost_ (Invariant.holds_def { cell = p } after ho);
      ghost_ (post_def success hi);
      let r : A.transfer = { restored = outside; outgoing = inside } in
      refine_ r)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (post_def success h);
  let result : (bool, Cell.contents) P.step = { value = success; state = r.#state } in
  refine_ result
let release : (a : t) ->
    {t : Cell.contents P.token | good (a.cell) (P.own t)} @ unique ghost ->
    {t : Cell.contents P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t ->
  let p = a.cell in
  let zero = 0 in
  let one = 1 in
  let refine_ t = t in
  let ht = ghost_ (P.own (borrow_ t)) in
  ghost_ (good_def p ht);
  let[@def] (post @ total) (success : bool @ immutable)
      (h : Cell.contents P.heap @ immutable) =
    ghost_ (success && h === P.Heap.empty ()) in
  let erased_post = ghost_ post in
  let refine_ r = A.compare_and_set a.atomic one zero erased_post t
    (ghost_ (fun before inside outside ->
      let refine_ inside = inside in
      let refine_ outside = outside in
      let hi = ghost_ (P.own (borrow_ inside)) in
      let ho = ghost_ (P.own (borrow_ outside)) in
      let after = ghost_ (if before = one then zero else before) in
      let success = ghost_ (before = one) in
      ghost_ (Invariant.holds_def { cell = p } before hi);
      ghost_ (good_def p hi);
      ghost_ (good_def p ho);
      ghost_ (Invariant.holds_def { cell = p } after ho);
      ghost_ (post_def success hi);
      let r : A.transfer = { restored = outside; outgoing = inside } in
      refine_ r)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (post_def success h);
  let t = r.#state in
  refine_ t

let[@def] (location @ total) (a : t @ local immutable) = ghost_ (Cell.location a.cell)
let[@def] (owned @ total) (a : t @ immutable)
    (h : Cell.contents P.heap @ immutable) =
  ghost_ (let p = location a in match P.Heap.at h p with
    | Some (Some x) -> h === P.Heap.put (P.Heap.empty ()) p (Some x)
    | _ -> false)
let try_acquire_internal = try_acquire
let try_acquire : (a : t) ->
    {r : (bool, contents) Ghost_pref.step |
      if r.value then owned a (Ghost_pref.own r.state)
      else Ghost_pref.own r.state === Ghost_pref.Heap.empty ()} @ unique = fun a ->
  ghost_ (location_def a);
  let r = try_acquire_internal a in
  let h = ghost_ (P.own (borrow_ r.P.state)) in
  ghost_ (good_def a.cell h; owned_def a h);
  r
let release_internal = release
let release : (a : t) ->
    {t : contents Ghost_pref.token | owned a (Ghost_pref.own t)} @ unique ghost ->
    {t : contents Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.empty ()}
      @ unique ghost = fun a t ->
  ghost_ (location_def a);
  let h = ghost_ (P.own (borrow_ t)) in
  ghost_ (owned_def a h; good_def a.cell h);
  release_internal a t

type 'a step = { value : 'a; state : Cell.contents P.token @@ ghost }
let take : (a : t) ->
    (token : {t : contents Ghost_pref.token |
      match Ghost_pref.Heap.at (Ghost_pref.own t) (location a) with
      | Some (Some _) -> true | _ -> false}) @ unique ghost ->
    {r : V.t step | let refine_ token = token in
      Ghost_pref.Heap.at (Ghost_pref.own token) (location a)
        === Some (Some (V.snapshot r.value)) &&
      Ghost_pref.own r.state === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) None} @ unique = fun a t ->
  ghost_ (location_def a);
  let r = Cell.take a.cell t in
  { value = r.Cell.value; state = r.Cell.state }
let put : (a : t) -> (value : V.t) @ unique ->
    (token : {t : contents Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location a) === Some None}) @ unique ghost ->
    {t : contents Ghost_pref.token | let refine_ token = token in
      Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) (Some (V.snapshot value))} @ unique ghost = fun a value t ->
  ghost_ (location_def a); Cell.put a.cell value t

end
