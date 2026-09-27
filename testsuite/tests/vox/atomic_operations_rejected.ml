(* TEST
 has-z3;
 flags = "-extension refinement_types -alert -do_not_spawn_domains";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml verified_atomic.mli verified_atomic.ml";
 readonly_files = "atomic_operations_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

(* Load the implementation so that the accepted phrases below can be
   evaluated. *)
#load "pref.cmo";;
#load "ghost_pref.cmo";;
#load "verified_atomic.cmo";;

module P = Ghost_pref;;
[%%expect{|
module P = Ghost_pref
|}]

(* fetch_and_add must restore the invariant for [before + n]. *)
module Increment = struct
  module Invariant = struct
    type payload = int
    type key = { unused : unit @@ ghost }
    let[@def] (holds @ total) (_k : key @ immutable) (n : int @ immutable)
        (h : int P.heap @ immutable) =
      ghost_ (0 <= n && n <= 1000 && h === P.Heap.empty ())
  end
  module A = Verified_atomic.Make (Invariant)

  let (increment_transfer @ total) :
      (k : Invariant.key) @ immutable ghost ->
      (before : int) @ immutable ghost ->
      (inside : {g : int P.token | Invariant.holds k before (P.own g)})
        @ unique ghost ->
      (outside : {g : int P.token | P.own g === P.Heap.empty ()})
        @ unique ghost ->
      {r : A.transfer | Invariant.holds k (before + 1) (P.own r.restored)
        && P.own r.outgoing === P.Heap.empty ()} @ unique =
    fun k before inside outside ->
    let hi = ghost_ (P.own (borrow_ inside)) in
    ghost_ (Invariant.holds_def k before hi);
    ghost_ (Invariant.holds_def k (before + 1) hi);
    { A.restored = inside; outgoing = outside }
end;;
[%%expect{|
Line 24, characters 4-47:
24 |     { A.restored = inside; outgoing = outside }
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 18-19, characters 24-47:
18 | ........................Invariant.holds k (before + 1) (P.own r.restored)
19 |         && P.own r.outgoing === P.Heap.empty ()............
  The refinement is stated here.
|}]

(* exchange must restore the invariant for the stored value. *)
module Store = struct
  module Invariant = struct
    type payload = int
    type key = { unused : unit @@ ghost }
    let[@def] (holds @ total) (_k : key @ immutable) (n : int @ immutable)
        (h : int P.heap @ immutable) =
      ghost_ (0 <= n && n <= 1000 && h === P.Heap.empty ())
  end
  module A = Verified_atomic.Make (Invariant)

  let store (a : A.t) (desired : int) =
    let caller = P.empty () in
    A.exchange a desired
      (ghost_ (fun before h -> h === P.Heap.empty ())) caller
      (ghost_ (fun before inside outside ->
        { A.restored = inside; outgoing = outside }))
end;;
[%%expect{|
Line 16, characters 8-51:
16 |         { A.restored = inside; outgoing = outside }))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
File "verified_atomic.mli", lines 86-87, characters 21-46:
  The refinement is stated here.
|}]
