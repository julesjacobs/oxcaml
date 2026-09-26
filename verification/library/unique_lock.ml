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

let[@def] (acquire_post @ total) (p : Cell.t @ immutable)
    (success : bool @ immutable) (h : Cell.contents P.heap @ immutable) =
  ghost_ (if success then good p h else h === P.Heap.empty ())
let[@def] (release_post @ total) (success : bool @ immutable)
    (h : Cell.contents P.heap @ immutable) =
  ghost_ (success && h === P.Heap.empty ())

let (acquire_transfer @ total) :
    (p : Cell.t) @ immutable ghost -> (before : int) @ immutable ghost ->
    (inside : {g : Cell.contents P.token |
      Invariant.holds { cell = p } before (P.own g) &&
      P.Heap.disjoint (P.own g) (P.Heap.empty ())}) @ unique ghost ->
    (outside : {g : Cell.contents P.token | P.own g === P.Heap.empty ()})
      @ unique ghost ->
    {r : A.transfer |
      Invariant.holds { cell = p } (if before = 0 then 1 else before)
        (P.own r.restored) && acquire_post p (before = 0) (P.own r.outgoing)}
      @ unique = fun p before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def { cell = p } before hi);
  ghost_ (Invariant.holds_def { cell = p } (if before = 0 then 1 else before) ho);
  ghost_ (acquire_post_def p (before = 0) hi);
  { A.restored = outside; outgoing = inside }

let (release_transfer @ total) :
    (p : Cell.t) @ immutable ghost ->
    (caller : {h : Cell.contents P.heap | good p h}) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : Cell.contents P.token |
      Invariant.holds { cell = p } before (P.own g) &&
      P.Heap.disjoint (P.own g) caller}) @ unique ghost ->
    (outside : {g : Cell.contents P.token | P.own g === caller}) @ unique ghost ->
    {r : A.transfer |
      Invariant.holds { cell = p } (if before = 1 then 0 else before)
        (P.own r.restored) && release_post (before = 1) (P.own r.outgoing)}
      @ unique = fun p caller before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def { cell = p } before hi);
  ghost_ (good_def p hi; good_def p ho);
  ghost_ (Invariant.holds_def { cell = p } (if before = 1 then 0 else before) ho);
  ghost_ (release_post_def (before = 1) hi);
  { A.restored = outside; outgoing = inside }

type storage = { cell : Cell.t; atomic : A.t }
type t = {a : storage | (A.key a.atomic).Invariant.cell === a.cell}
type contents = Cell.contents
let[@def] (location @ total) (a : t @ local immutable) = ghost_ (Cell.location a.cell)
let[@def] (owned @ total) (a : t @ immutable)
    (h : Cell.contents P.heap @ immutable) =
  ghost_ (let p = location a in match P.Heap.at h p with
    | Some (Some x) -> h === P.Heap.put (P.Heap.empty ()) p (Some x)
    | _ -> false)

let make (x : V.t @ unique) : t =
  let e = P.empty () in
  let allocation = Cell.create x e in
  let p = allocation.Cell.value in
  let t = allocation.Cell.state in
  let h = ghost_ (P.own (borrow_ t)) in
  let zero = 0 in
  ghost_ (good_def p h);
  ghost_ (Invariant.holds_def { cell = p } zero h);
  let g : {g : Cell.contents P.token | Invariant.holds { cell = p } zero (P.own g)} = t in
  let a = A.create (ghost_ { cell = p }) zero g in
  let result : t = { cell = p; atomic = a } in result
let try_acquire (a : t) :
    {r : (bool, Cell.contents) P.step | if r.P.value then owned a (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique =
  let p = a.cell in
  let e = P.empty () in
  let r = A.compare_and_set a.atomic 0 1
    (ghost_ (fun success h -> acquire_post p success h)) e
    (ghost_ (fun before inside outside ->
      acquire_transfer p before inside outside)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (acquire_post_def p success h; location_def a;
    good_def a.cell h; owned_def a h);
  let result : (bool, Cell.contents) P.step = { value = success; state = r.#state } in
  result
let release : (a : t) ->
    {t : Cell.contents P.token | owned a (P.own t)} @ unique ghost ->
    {t : Cell.contents P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t ->
  let p = a.cell in
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
  Cell.take a.cell t
let put : (a : t) -> (value : V.t) @ unique ->
    (token : {t : contents Ghost_pref.token |
      Ghost_pref.Heap.at (Ghost_pref.own t) (location a) === Some None}) @ unique ghost ->
    {t : contents Ghost_pref.token | Ghost_pref.own t === Ghost_pref.Heap.put (Ghost_pref.own token)
        (location a) (Some (V.snapshot value))} @ unique ghost = fun a value t ->
  ghost_ (location_def a); Cell.put a.cell value t

end
