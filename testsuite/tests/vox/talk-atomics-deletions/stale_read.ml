(* Read after release. The release compare-and-set hands the caller's token
   to the lock's invariant; the real interface consumes it ([caller] is
   [@ unique]), so the stale read below is a uniqueness error. *)
module P0 = Ghost_pref
module C = struct
  type payload = int
  type cell = int P0.t
  let[@def] (location @ total) (p : cell @ immutable) = ghost_ p
  let[@def] (full @ total) (x : int @ immutable) = ghost_ (0 <= x)
end
module L = struct
module P = Ghost_pref

(* [good c h]: [h] owns exactly the cell [c], and its payload is full. *)
let[@def] (good @ total) (c : C.cell @ immutable)
    (h : C.payload P.heap @ immutable) =
  ghost_ (let p = C.location c in match P.Heap.at h p with
    | None -> false
    | Some x -> C.full x && h === P.Heap.put (P.Heap.empty ()) p x)

(* The lock's invariant: flag 0 with the full cell, or flag 1 with nothing. *)
module Invariant = struct
  type payload = C.payload
  type key = { cell : C.cell @@ ghost }
  let[@def] (holds @ total) (k : key @ immutable)
      (flag : int @ immutable) (h : C.payload P.heap @ immutable) =
    ghost_ ((flag = 0 && good k.cell h) ||
      (flag = 1 && h === P.Heap.empty ()))
end
module A = Verified_atomic.Make (Invariant)

(* What each compare-and-set hands to its caller. *)
let[@def] (acquire_post @ total) (c : C.cell @ immutable)
    (success : bool @ immutable) (h : C.payload P.heap @ immutable) =
  ghost_ (if success then good c h else h === P.Heap.empty ())
let[@def] (release_post @ total) (success : bool @ immutable)
    (h : C.payload P.heap @ immutable) =
  ghost_ (success && h === P.Heap.empty ())

(* The ghost steps at each compare-and-set: swap the invariant's resource
   with the caller's. *)
let (acquire_transfer @ total) :
    (c : C.cell) @ immutable ghost -> (before : int) @ immutable ghost ->
    (inside : {g : C.payload P.token |
      Invariant.holds { cell = c } before (P.own g) &&
      P.Heap.disjoint (P.own g) (P.Heap.empty ())}) @ unique ghost ->
    (outside : {g : C.payload P.token | P.own g === P.Heap.empty ()})
      @ unique ghost ->
    {r : A.transfer |
      Invariant.holds { cell = c } (if before = 0 then 1 else before)
        (P.own r.restored) && acquire_post c (before = 0) (P.own r.outgoing)}
      @ unique = fun c before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def { cell = c } before hi);
  ghost_ (Invariant.holds_def { cell = c } (if before = 0 then 1 else before) ho);
  ghost_ (acquire_post_def c (before = 0) hi);
  { A.restored = outside; outgoing = inside }

let (release_transfer @ total) :
    (c : C.cell) @ immutable ghost ->
    (caller : {h : C.payload P.heap | good c h}) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : C.payload P.token |
      Invariant.holds { cell = c } before (P.own g) &&
      P.Heap.disjoint (P.own g) caller}) @ unique ghost ->
    (outside : {g : C.payload P.token | P.own g === caller}) @ unique ghost ->
    {r : A.transfer |
      Invariant.holds { cell = c } (if before = 1 then 0 else before)
        (P.own r.restored) && release_post (before = 1) (P.own r.outgoing)}
      @ unique = fun c caller before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def { cell = c } before hi);
  ghost_ (good_def c hi; good_def c ho);
  ghost_ (Invariant.holds_def { cell = c } (if before = 1 then 0 else before) ho);
  ghost_ (release_post_def (before = 1) hi);
  { A.restored = outside; outgoing = inside }

type storage = { cell : C.cell @@ global; atomic : A.t }
type t = {a : storage | (A.key a.atomic).Invariant.cell === a.cell}
let[@def] (cell @ total) (a : t @ local immutable) = a.cell
let[@def] (owned @ total) (a : t @ immutable)
    (h : C.payload P.heap @ immutable) =
  ghost_ (let p = C.location (cell a) in match P.Heap.at h p with
    | None -> false
    | Some x -> C.full x && h === P.Heap.put (P.Heap.empty ()) p x)

let create (c : C.cell)
    (t : {t : C.payload P.token |
      let p = C.location c in
      match P.Heap.at (P.own t) p with
      | None -> false
      | Some x -> C.full x && P.own t === P.Heap.put (P.Heap.empty ()) p x}
      @ unique ghost) : {a : t | cell a === c} =
  let zero = 0 in
  let h = ghost_ (P.own (borrow_ t)) in
  ghost_ (good_def c h; Invariant.holds_def { cell = c } zero h);
  let a = A.create (ghost_ { cell = c }) zero t in
  let result : t = { cell = c; atomic = a } in
  ghost_ (cell_def result);
  result

let try_acquire (a : t) :
    {r : (bool, C.payload) P.step | if r.P.value then owned a (P.own r.P.state)
      else P.own r.P.state === P.Heap.empty ()} @ unique =
  let c = a.cell in
  let e = P.empty () in
  let r = A.compare_and_set a.atomic 0 1
    (ghost_ (fun success h -> acquire_post c success h)) e
    (ghost_ (fun before inside outside ->
      acquire_transfer c before inside outside)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (acquire_post_def c success h; good_def c h; cell_def a;
    owned_def a h);
  let result : (bool, C.payload) P.step = { value = success; state = r.#state } in
  result

let release : (a : t) ->
    {t : C.payload P.token | owned a (P.own t)} @ unique ghost ->
    {t : C.payload P.token | P.own t === P.Heap.empty ()} @ unique ghost =
  fun a t ->
  let c = a.cell in
  let ht = ghost_ (P.own (borrow_ t)) in
  ghost_ (cell_def a; owned_def a ht; good_def c ht);
  let r = A.compare_and_set a.atomic 1 0
    release_post t
    (ghost_ (fun before inside outside ->
      release_transfer c ht before inside outside)) in
  let success = r.#value in
  let h = ghost_ (P.own (borrow_ r.#state)) in
  ghost_ (release_post_def success h);
  let t = r.#state in
  t

(* Acquire, write [value], hand the token to the invariant with the release
   CAS, run [between] (another holder's critical section), then keep reading
   through the (now aliased) token. The read's type says it returns
   [value]. *)
let write_release_read (a : t) (value : {v : int | 0 <= v})
    (between : unit -> unit) =
  let r = try_acquire a in
  if r.P.value then begin
    let t = r.P.state in
    let p = a.cell in
    let h = ghost_ (P.own (borrow_ t)) in
    ghost_ (cell_def a; owned_def a h; good_def p h; C.location_def p);
    let t = P.write p value t in
    let h2 = ghost_ (P.own (borrow_ t)) in
    ghost_ (match P.Heap.at h p with
      | Some x -> P.Heap.put_law (P.Heap.empty ()) p x value
      | None -> ());
    ghost_ (owned_def a h2; good_def p h2; C.full_def value);
    let _ = A.compare_and_set a.atomic 1 0 release_post t
      (ghost_ (fun before inside outside ->
        release_transfer p h2 before inside outside)) in
    between ();
    let v : {v : int | v = value} = P.read p t in
    if v <> value then
      Printf.printf "refinement {v | v = %d} violated: read %d\n%!" value v;
    1
  end else 0

let make () : t =
  let e = P.empty () in
  let allocation = P.alloc 0 e in
  let p = allocation.P.value in
  let t = allocation.P.state in
  ghost_ (C.location_def p; C.full_def 0);
  let a = create p t in a
end

(* One domain, one fixed interleaving: after the first holder's release,
   a second holder acquires the lock, writes 6 and releases it; then the
   first holder reads through its stale token. With two domains the same
   interleaving happens by chance. *)
let () =
  let a = L.make () in
  let second_holder () = ignore (L.write_release_read a 6 (fun () -> ())) in
  ignore (L.write_release_read a 5 second_holder);
  print_endline "done"
