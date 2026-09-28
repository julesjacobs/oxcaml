(* Two domains bump one atomic counter; the invariant owns no memory. *)
module P = Ghost_pref
module Invariant = struct
  type payload = int
  type key = { unused : unit @@ ghost }
  let[@def] (holds @ total) (_k : key @ immutable) (_n : int @ immutable)
      (h : int P.heap @ immutable) = ghost_ (h === P.Heap.empty ())
end
module A = Verified_atomic.Make (Invariant)
let[@def] (nothing @ total) (_b : int @ immutable) (h : int P.heap @ immutable) =
  ghost_ (h === P.Heap.empty ())

let bump (a : A.t) =
  let k = ghost_ (A.key a) in
  let r = A.fetch_and_add a 1 (ghost_ (fun b h -> nothing b h)) (P.empty ())
    (ghost_ (fun before inside outside ->
      let hi = P.own (borrow_ inside) in
      let ho = P.own (borrow_ outside) in
      Invariant.holds_def k before hi;
      Invariant.holds_def k (before + 1) hi;
      nothing_def before ho;
      { A.restored = inside; outgoing = outside })) in
  r.#value

let make () =
  let k = ghost_ { Invariant.unused = () } in
  let e = P.empty () in
  ghost_ (Invariant.holds_def k 0 (P.own (borrow_ e)));
  A.create k 0 e

let () =
  let a = make () in
  let d = Domain.Safe.spawn (fun () -> for _ = 1 to 100_000 do ignore (bump a) done) in
  for _ = 1 to 100_000 do ignore (bump a) done;
  Domain.join d;
  Printf.printf "counter = %d\n" (bump a)
