(* The invariant always owns the cell. A transition that hands the caller a
   copy of the invariant's token would give every caller a "unique" write
   permission for the same cell. *)
module P = Ghost_pref
let[@def] (good @ total) (p : int P.t @ immutable) (h : int P.heap @ immutable) =
  ghost_ (match P.Heap.at h p with
    | None -> false
    | Some x -> h === P.Heap.put (P.Heap.empty ()) p x)
module Invariant = struct
  type payload = int
  type key = { cell : int P.t @@ ghost }
  let[@def] (holds @ total) (k : key @ immutable) (_flag : int @ immutable)
      (h : int P.heap @ immutable) = ghost_ (good k.cell h)
end
module A = Verified_atomic.Make (Invariant)

let[@def] (got_cell @ total) (k : Invariant.key @ immutable)
    (_success : bool @ immutable) (h : int P.heap @ immutable) =
  ghost_ (good k.cell h)

let steal (a : A.t) (p : {p : int P.t | (A.key a).Invariant.cell === p}) =
  let k = ghost_ (A.key a) in
  let r = A.compare_and_set a 0 0
    (ghost_ (fun success h -> got_cell k success h)) (P.empty ())
    (ghost_ (fun before inside outside ->
      let hi = P.own (borrow_ inside) in
      Invariant.holds_def k before hi;
      Invariant.holds_def k (if before = 0 then 0 else before) hi;
      got_cell_def k (before = 0) hi;
      { A.restored = inside; outgoing = inside })) in
  let t = r.#state in
  let h = ghost_ (P.own (borrow_ t)) in
  ghost_ (got_cell_def k r.#value h; good_def p h);
  (* a unique write permission for a cell the invariant still owns *)
  P.write p 7 t
