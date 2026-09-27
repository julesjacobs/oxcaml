(* TEST
 has-z3;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml";
 { bytecode; }
 { native; }
*)

(* A counter that stays within [0, 1000], updated by exchange, set and
   fetch_and_add. The invariant owns no heap cells. *)
module P = Ghost_pref
module Invariant = struct
  type payload = int
  type key = { unused : unit @@ ghost }
  let[@def] (holds @ total) (_k : key @ immutable) (n : int @ immutable)
      (h : int P.heap @ immutable) =
    ghost_ (0 <= n && n <= 1000 && h === P.Heap.empty ())
end
module A = Verified_atomic.Make (Invariant)

let[@def] (empty_after @ total) (_before : int @ immutable)
    (h : int P.heap @ immutable) = ghost_ (h === P.Heap.empty ())
let[@def] (empty_heap @ total) (h : int P.heap @ immutable) =
  ghost_ (h === P.Heap.empty ())

(* Stores [desired]; the caller's resources come back unchanged. *)
let (store_transfer @ total) :
    (k : Invariant.key) @ immutable ghost ->
    (desired : {n : int | 0 <= n && n <= 1000}) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : int P.token | Invariant.holds k before (P.own g)})
      @ unique ghost ->
    (outside : {g : int P.token | P.own g === P.Heap.empty ()})
      @ unique ghost ->
    {r : A.transfer | Invariant.holds k desired (P.own r.restored)
      && empty_after before (P.own r.outgoing)} @ unique =
  fun k desired before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def k before hi);
  ghost_ (Invariant.holds_def k desired hi);
  ghost_ (empty_after_def before ho);
  { A.restored = inside; outgoing = outside }

let (set_transfer @ total) :
    (k : Invariant.key) @ immutable ghost ->
    (desired : {n : int | 0 <= n && n <= 1000}) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : int P.token | Invariant.holds k before (P.own g)})
      @ unique ghost ->
    (outside : {g : int P.token | P.own g === P.Heap.empty ()})
      @ unique ghost ->
    {r : A.transfer | Invariant.holds k desired (P.own r.restored)
      && empty_heap (P.own r.outgoing)} @ unique =
  fun k desired before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def k before hi);
  ghost_ (Invariant.holds_def k desired hi);
  ghost_ (empty_heap_def ho);
  { A.restored = inside; outgoing = outside }

(* Adding 0 keeps the invariant whatever the value was. *)
let (add_zero_transfer @ total) :
    (k : Invariant.key) @ immutable ghost ->
    (before : int) @ immutable ghost ->
    (inside : {g : int P.token | Invariant.holds k before (P.own g)})
      @ unique ghost ->
    (outside : {g : int P.token | P.own g === P.Heap.empty ()})
      @ unique ghost ->
    {r : A.transfer | Invariant.holds k (before + 0) (P.own r.restored)
      && empty_after before (P.own r.outgoing)} @ unique =
  fun k before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def k before hi);
  ghost_ (Invariant.holds_def k (before + 0) hi);
  ghost_ (empty_after_def before ho);
  { A.restored = inside; outgoing = outside }

let () =
  let k = ghost_ { Invariant.unused = () } in
  let e = P.empty () in
  let h = ghost_ (P.own (borrow_ e)) in
  let initial = 5 in
  ghost_ (Invariant.holds_def k initial h);
  let e : {g : int P.token | Invariant.holds k initial (P.own g)} = e in
  let a = A.create k initial e in
  let caller = P.empty () in
  let seven = 7 in
  let r = A.exchange a seven (ghost_ (fun before h -> empty_after before h))
    caller (ghost_ (fun before inside outside ->
      store_transfer (A.key a) seven before inside outside)) in
  let previous = r.#value in
  let caller = r.#state in
  let h = ghost_ (P.own (borrow_ caller)) in
  ghost_ (empty_after_def previous h);
  let caller : {g : int P.token | P.own g === P.Heap.empty ()} = caller in
  let nine = 9 in
  let r = A.set a nine (ghost_ (fun h -> empty_heap h)) caller
    (ghost_ (fun before inside outside ->
      set_transfer (A.key a) nine before inside outside)) in
  let caller = r.#state in
  let h = ghost_ (P.own (borrow_ caller)) in
  ghost_ (empty_heap_def h);
  let caller : {g : int P.token | P.own g === P.Heap.empty ()} = caller in
  let zero = 0 in
  let r = A.fetch_and_add a zero
    (ghost_ (fun before h -> empty_after before h)) caller
    (ghost_ (fun before inside outside ->
      add_zero_transfer (A.key a) before inside outside)) in
  let current = r.#value in
  Printf.printf "exchange returned %d, fetch_and_add returned %d\n"
    previous current
