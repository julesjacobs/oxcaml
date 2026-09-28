(* TEST
 has-z3;
 native-compiler;
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-extension refinement_types -dcmm -dno-unique-ids";
 compile_only = "true";
 module = "talk_atomic_erasure.ml";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* Talk, section 4c ("atomic invariants"), the "Say" line: "The transition
   is never built or run. Natively the CAS is
   caml_vox_atomic_cas a 1 3 48059 48059: the proof is the constant
   0xBBBB." talk_atomic_erasure.compilers.reference is the native Cmm of
   [try_acquire] below: the post and the transition, both [ghost_], are
   passed as the placeholder 48059 (0xBBBB) and never allocated. From the
   investigation's concurrency/va/base.ml. *)

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
  { A.restored = outside; outgoing = inside }

let try_acquire (a : A.t) =
  let k = ghost_ (A.key a) in
  A.compare_and_set a 0 1
    (ghost_ (fun success h -> acquire_post k success h)) (P.empty ())
    (ghost_ (fun before inside outside -> acquire_transfer k before inside outside))
