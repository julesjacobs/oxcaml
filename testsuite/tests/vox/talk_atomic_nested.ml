(* TEST
 has-z3;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml";
 readonly_files = "talk_atomic_nested.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* Talk, scene 26 ("concurrency"): an invariant cannot be opened inside
   another. An atomic operation opens its atomic's invariant for the
   duration of the transition, the ghost function that re-splits the
   resources. The transition must be total, and atomic operations are
   partial, so no transition can perform an atomic operation on any atomic.
   This takes the place of Iris's invariant masks. The lock below is the one
   of talk_atomic_erasure.ml. The control, [try_acquire], is accepted; the
   same compare-and-set whose transition calls [try_acquire] on the same
   atomic is rejected because [try_acquire] is partial. From the
   investigation's concurrency/va/nested.ml. *)

#load "pref.cmo";;
#load "ghost_pref.cmo";;
#load "verified_atomic.cmo";;

(* A one-cell lock written directly against Verified_atomic: flag 0 means the
   invariant owns the cell [p]; flag 1 means some holder owns it. *)
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

let[@def] (acquire_post @ total) (k : Invariant.key @ immutable)
    (success : bool @ immutable) (h : int P.heap @ immutable) =
  ghost_ (if success then good k.cell h else h === P.Heap.empty ())

let (acquire_transfer @ total) :
    (k : Invariant.key) @ immutable ghost -> (before : int) @ immutable ghost ->
    (inside : {g : int P.token | Invariant.holds k before (P.own g) &&
      P.Heap.disjoint (P.own g) (P.Heap.empty ())}) @ unique ghost ->
    (outside : {g : int P.token | P.own g === P.Heap.empty ()}) @ unique ghost ->
    {r : A.transfer |
      Invariant.holds k (if before = 0 then 1 else before) (P.own r.restored) &&
      acquire_post k (before = 0) (P.own r.outgoing)} @ unique =
  fun k before inside outside ->
  let hi = ghost_ (P.own (borrow_ inside)) in
  let ho = ghost_ (P.own (borrow_ outside)) in
  ghost_ (Invariant.holds_def k before hi);
  ghost_ (Invariant.holds_def k (if before = 0 then 1 else before) ho);
  ghost_ (acquire_post_def k (before = 0) hi);
  { A.restored = outside; outgoing = inside };;
[%%expect{|
module P = Ghost_pref
val good : int P.t @ immutable -> int P.heap @ immutable -> bool @ ghost =
  <fun>
val good_def :
  (p : int P.t) @ immutable ->
  (h : int P.heap) @ immutable ->
  {u : unit
    | (good p h) ===
        (ghost_
           (match P.Heap.at h p with
            | None -> false
            | Some x -> h === (P.Heap.put (P.Heap.empty ()) p x)))} =
  <fun>
module Invariant :
  sig
    type payload = int
    type key = { cell : int P.t @@ ghost; }
    val holds :
      key @ immutable ->
      int @ immutable -> int P.heap @ immutable -> bool @ ghost
    val holds_def :
      (k : key) @ immutable ->
      (flag : int) @ immutable ->
      (h : int P.heap) @ immutable ->
      {u : unit
        | (holds k flag h) ===
            (ghost_
               (((flag = 0) && (good k.cell h)) ||
                  ((flag = 1) && (h === (P.Heap.empty ())))))}
  end
module A :
  sig
    type t = Verified_atomic.Make(Invariant).t
    external key : t @ local immutable -> Invariant.key @ immutable ghost @@
      total = "caml_vox_atomic_key_bytecode" "caml_vox_atomic_key"
    type ('a : immediate) result =
      'a Verified_atomic.Make(Invariant).result = #{
      value : 'a;
      state : Invariant.payload Ghost_pref.token @@ ghost;
    }
    type transfer =
      Verified_atomic.Make(Invariant).transfer = {
      restored : Invariant.payload Ghost_pref.token @@ ghost;
      outgoing : Invariant.payload Ghost_pref.token @@ ghost;
    }
    external create :
      (k : Invariant.key) @ immutable ghost ->
      (initial : int) ->
      {g : Invariant.payload Ghost_pref.token
        | Invariant.holds k initial (Ghost_pref.own g)} @ unique
      ghost -> {a : t | (key a) === k} @@ portable
      = "caml_vox_atomic_create_bytecode" "caml_vox_atomic_create"
    external load :
      (a : t) @ local contended ->
      (post : (int @ immutable ->
               Invariant.payload Ghost_pref.heap @ immutable -> bool @ ghost)) @ immutable
      ghost ->
      (caller : Invariant.payload Ghost_pref.token) @ unique ghost ->
      ((before : int) @ immutable ghost ->
       {g : Invariant.payload Ghost_pref.token
         | (Invariant.holds (key a) before (Ghost_pref.own g)) &&
             (Ghost_pref.Heap.disjoint (Ghost_pref.own g)
                (Ghost_pref.own caller))} @ unique
       ghost ->
       {c : Invariant.payload Ghost_pref.token
         | (Ghost_pref.own c) === (Ghost_pref.own caller)} @ unique
       ghost ->
       {r : transfer
         | (Invariant.holds (key a) before (Ghost_pref.own r.restored)) &&
             (post before (Ghost_pref.own r.outgoing))} @ unique) @ total
      immutable ghost ->
      {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique @@
      portable = "caml_vox_atomic_load_bytecode" "caml_vox_atomic_load"
      [@@noalloc]
    external compare_and_set :
      (a : t) @ local contended ->
      (expected : int) ->
      (desired : int) ->
      (post : (bool @ immutable ->
               Invariant.payload Ghost_pref.heap @ immutable -> bool @ ghost)) @ immutable
      ghost ->
      (caller : Invariant.payload Ghost_pref.token) @ unique ghost ->
      ((before : int) @ immutable ghost ->
       {g : Invariant.payload Ghost_pref.token
         | (Invariant.holds (key a) before (Ghost_pref.own g)) &&
             (Ghost_pref.Heap.disjoint (Ghost_pref.own g)
                (Ghost_pref.own caller))} @ unique
       ghost ->
       {c : Invariant.payload Ghost_pref.token
         | (Ghost_pref.own c) === (Ghost_pref.own caller)} @ unique
       ghost ->
       {r : transfer
         | (Invariant.holds (key a)
              (if before = expected then desired else before)
              (Ghost_pref.own r.restored))
             && (post (before = expected) (Ghost_pref.own r.outgoing))} @ unique) @ total
      immutable ghost ->
      {r : bool result | post r.#value (Ghost_pref.own r.#state)} @ unique @@
      portable = "caml_vox_atomic_cas_bytecode" "caml_vox_atomic_cas"
      [@@noalloc]
    external exchange :
      (a : t) @ local contended ->
      (desired : int) ->
      (post : (int @ immutable ->
               Invariant.payload Ghost_pref.heap @ immutable -> bool @ ghost)) @ immutable
      ghost ->
      (caller : Invariant.payload Ghost_pref.token) @ unique ghost ->
      ((before : int) @ immutable ghost ->
       {g : Invariant.payload Ghost_pref.token
         | (Invariant.holds (key a) before (Ghost_pref.own g)) &&
             (Ghost_pref.Heap.disjoint (Ghost_pref.own g)
                (Ghost_pref.own caller))} @ unique
       ghost ->
       {c : Invariant.payload Ghost_pref.token
         | (Ghost_pref.own c) === (Ghost_pref.own caller)} @ unique
       ghost ->
       {r : transfer
         | (Invariant.holds (key a) desired (Ghost_pref.own r.restored)) &&
             (post before (Ghost_pref.own r.outgoing))} @ unique) @ total
      immutable ghost ->
      {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique @@
      portable = "caml_vox_atomic_exchange_bytecode"
      "caml_vox_atomic_exchange" [@@noalloc]
    external set :
      (a : t) @ local contended ->
      (desired : int) ->
      (post : (Invariant.payload Ghost_pref.heap @ immutable -> bool @ ghost)) @ immutable
      ghost ->
      (caller : Invariant.payload Ghost_pref.token) @ unique ghost ->
      ((before : int) @ immutable ghost ->
       {g : Invariant.payload Ghost_pref.token
         | (Invariant.holds (key a) before (Ghost_pref.own g)) &&
             (Ghost_pref.Heap.disjoint (Ghost_pref.own g)
                (Ghost_pref.own caller))} @ unique
       ghost ->
       {c : Invariant.payload Ghost_pref.token
         | (Ghost_pref.own c) === (Ghost_pref.own caller)} @ unique
       ghost ->
       {r : transfer
         | (Invariant.holds (key a) desired (Ghost_pref.own r.restored)) &&
             (post (Ghost_pref.own r.outgoing))} @ unique) @ total
      immutable ghost ->
      {r : unit result | post (Ghost_pref.own r.#state)} @ unique @@ portable
      = "caml_vox_atomic_set_bytecode" "caml_vox_atomic_set" [@@noalloc]
    external fetch_and_add :
      (a : t) @ local contended ->
      (n : int) ->
      (post : (int @ immutable ->
               Invariant.payload Ghost_pref.heap @ immutable -> bool @ ghost)) @ immutable
      ghost ->
      (caller : Invariant.payload Ghost_pref.token) @ unique ghost ->
      ((before : int) @ immutable ghost ->
       {g : Invariant.payload Ghost_pref.token
         | (Invariant.holds (key a) before (Ghost_pref.own g)) &&
             (Ghost_pref.Heap.disjoint (Ghost_pref.own g)
                (Ghost_pref.own caller))} @ unique
       ghost ->
       {c : Invariant.payload Ghost_pref.token
         | (Ghost_pref.own c) === (Ghost_pref.own caller)} @ unique
       ghost ->
       {r : transfer
         | (Invariant.holds (key a) (before + n) (Ghost_pref.own r.restored))
             && (post before (Ghost_pref.own r.outgoing))} @ unique) @ total
      immutable ghost ->
      {r : int result | post r.#value (Ghost_pref.own r.#state)} @ unique @@
      portable = "caml_vox_atomic_fetch_add_bytecode"
      "caml_vox_atomic_fetch_add" [@@noalloc]
  end
val acquire_post :
  Invariant.key @ immutable ->
  bool @ immutable -> int P.heap @ immutable -> bool @ ghost = <fun>
val acquire_post_def :
  (k : Invariant.key) @ immutable ->
  (success : bool) @ immutable ->
  (h : int P.heap) @ immutable ->
  {u : unit
    | (acquire_post k success h) ===
        (ghost_
           (if success
            then good k.Invariant.cell h
            else h === (P.Heap.empty ())))} =
  <fun>
val acquire_transfer :
  (k : Invariant.key) @ immutable ghost ->
  ((before : int) @ immutable ghost ->
   {g : int P.token
     | (Invariant.holds k before (P.own g)) &&
         (P.Heap.disjoint (P.own g) (P.Heap.empty ()))} @ unique
   ghost ->
   {g : int P.token | (P.own g) === (P.Heap.empty ())} @ unique ghost ->
   {r : A.transfer
     | (Invariant.holds k (if before = 0 then 1 else before)
          (P.own r.A.restored))
         && (acquire_post k (before = 0) (P.own r.A.outgoing))} @ unique) @ total =
  <fun>
|}]

(* Control: the transition only re-splits the tokens. Accepted. *)
let try_acquire (a : A.t) =
  let k = ghost_ (A.key a) in
  A.compare_and_set a 0 1
    (ghost_ (fun success h -> acquire_post k success h)) (P.empty ())
    (ghost_ (fun before inside outside ->
       acquire_transfer k before inside outside));;
[%%expect{|
val try_acquire : A.t -> bool A.result = <fun>
|}]

(* Rejected: the transition performs a compare-and-set on the atomic whose
   invariant it holds open. *)
let nested (a : A.t) =
  let k = ghost_ (A.key a) in
  A.compare_and_set a 0 1
    (ghost_ (fun success h -> acquire_post k success h)) (P.empty ())
    (fun before inside outside ->
       let _ = try_acquire a in
       acquire_transfer k before inside outside);;
[%%expect{|
Line 6, characters 15-26:
6 |        let _ = try_acquire a in
                   ^^^^^^^^^^^
Error: The value "try_acquire" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 5-7, characters 4-48
         which is expected to be "total".
|}]
