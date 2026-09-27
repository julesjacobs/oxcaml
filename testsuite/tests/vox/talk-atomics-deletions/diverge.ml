(* A lock whose acquire always "succeeds" in the logic: the transition never
   returns, so its result type may claim anything. The real interface
   rejects the transition because [loop] is partial. *)
module P = Ghost_pref
let[@def] (good @ total) (p : int P.t @ immutable) (h : int P.heap @ immutable) =
  ghost_ (match P.Heap.at h p with
    | None -> false
    | Some x -> h === P.Heap.put (P.Heap.empty ()) p x)
module Invariant = struct
  type payload = int
  type key = { cell : int P.t @@ ghost }
  let[@def] (holds @ total) (k : key @ immutable) (flag : int @ immutable)
      (h : int P.heap @ immutable) =
    ghost_ ((flag = 0 && good k.cell h) || (flag = 1 && h === P.Heap.empty ()))
end
module A = Verified_atomic.Make (Invariant)
let[@def] (got_cell @ total) (k : Invariant.key @ immutable)
    (_success : bool @ immutable) (h : int P.heap @ immutable) =
  ghost_ (good k.cell h)

let rec loop : unit -> {r : A.transfer | false} @ unique = fun () -> loop ()

let always_acquire (a : A.t) (p : {p : int P.t | (A.key a).Invariant.cell === p}) =
  let k = ghost_ (A.key a) in
  let r = A.compare_and_set a 0 1
    (ghost_ (fun success h -> got_cell k success h)) (P.empty ())
    (fun before inside outside -> loop ()) in
  let t = r.#state in
  let h = ghost_ (P.own (borrow_ t)) in
  ghost_ (got_cell_def k r.#value h; good_def p h);
  (* write permission even when the CAS failed *)
  let t = P.write p 7 t in
  Printf.printf "compare_and_set 0 -> 1 on a held lock returned %b; \
                 the cell was written anyway: %d\n"
    r.#value (P.read p (borrow_ t))

(* The lock starts held (flag 1), so the acquiring compare-and-set fails. *)
let () =
  let allocation = P.alloc 0 (P.empty ()) in
  let p = allocation.P.value in
  let k = ghost_ { Invariant.cell = p } in
  let e = P.empty () in
  ghost_ (Invariant.holds_def k 1 (P.own (borrow_ e)));
  let a = A.create k 1 e in
  always_acquire a p
