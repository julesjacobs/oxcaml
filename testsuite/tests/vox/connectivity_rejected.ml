(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find_events.mli vox_union_find_events.ml vox_union_find.mli vox_union_find.ml vox_partition.ml vox_partition_classes.ml vox_partition_classes_proof.ml vox_partition_classes_bridge.ml vox_partition_transport_proof.ml vox_partition_classes_group.ml vox_union_find_partition_proof.ml vox_union_find_online.mli vox_union_find_online.ml vox_connectivity.mli vox_connectivity.ml";
 readonly_files = "connectivity_rejected.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

module Underpay_insertion = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  let bad : (state : {s : U.t | P.size (U.model s) < Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 10Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 8, characters 60-63:
8 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                                ^^^
Error: Refinement could not be proved (counterexample)
File "vox_connectivity.mli", line 65, characters 26-43:
  The refinement is stated here.
|}]

module Overpay_insertion = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  let bad : (state : {s : U.t | P.size (U.model s) < Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 12Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 8, characters 60-63:
8 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                                ^^^
Error: Refinement could not be proved (counterexample)
File "vox_connectivity.mli", line 65, characters 26-43:
  The refinement is stated here.
|}]

module Reuse_state = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  module Cost = U.Cost
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | P.contains (U.model s) x}) @ unique read_write total ->
      (fee1 : {b : C.token | let state = state in C.credits b = Cost.find_fee state})
        @ unique total ghost ->
      (fee2 : {b : C.token | let state = state in C.credits b = Cost.find_fee state})
        @ unique total ghost -> U.result @ unique = fun x state fee1 fee2 ->
    let _ = U.find x state fee1 in
    U.find x state fee2
end;;
[%%expect{|
Line 13, characters 13-18:
13 |     U.find x state fee2
                  ^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 12, characters 21-26:
12 |     let _ = U.find x state fee1 in
                          ^^^^^

|}]

module Hidden_savings = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad (s : U.t) = s.#savings
end;;
[%%expect{|
Line 4, characters 25-32:
4 |   let bad (s : U.t) = s.#savings
                             ^^^^^^^
Error: Unbound unboxed record field "savings"
|}]

module Nonmember = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  let bad (x : U.elem @ immutable)
      (state : U.t @ unique read_write total) =
    let state = state in
    (state : {s : U.t | P.contains (U.model s) x})
end;;
[%%expect{|
Line 8, characters 5-10:
8 |     (state : {s : U.t | P.contains (U.model s) x})
         ^^^^^
Error: Refinement could not be proved (counterexample)
Line 8, characters 24-48:
8 |     (state : {s : U.t | P.contains (U.model s) x})
                            ^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Machine_limit = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  let bad : (state : {s : U.t | P.size (U.model s) = Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 8, characters 54-59:
8 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                          ^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_connectivity.mli", line 63, characters 22-70:
  The refinement is stated here.
|}]

module Hidden_heap = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad = U.heap
end;;
[%%expect{|
Line 4, characters 12-18:
4 |   let bad = U.heap
                ^^^^^^
Error: Unbound value "U.heap"
|}]

module Forged_model = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  let bad (state : U.t @ local immutable total ghost forkable unyielding) :
      {u : unit | P.empty (U.model state)} @ ghost = ghost_ ()
end;;
[%%expect{|
Line 6, characters 60-62:
6 |       {u : unit | P.empty (U.model state)} @ ghost = ghost_ ()
                                                                ^^
Error: Refinement could not be proved (counterexample)
Line 6, characters 18-41:
6 |       {u : unit | P.empty (U.model state)} @ ghost = ghost_ ()
                      ^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Unrecorded_find = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  module Cost = U.Cost
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | P.contains (U.model s) x}) @ unique read_write total ->
      {s : U.t | let state = state in
        Cost.events s === Vox_union_find_events.Find (Cost.depth (Cost.snapshot state) x) ::
          Cost.events state} @ unique = fun x state -> state
end;;
[%%expect{|
Line 10, characters 55-60:
10 |           Cost.events state} @ unique = fun x state -> state
                                                            ^^^^^
Error: Refinement could not be proved (counterexample)
Lines 8-10, characters 17-27:
 8 | .................let state = state in
 9 |         Cost.events s === Vox_union_find_events.Find (Cost.depth (Cost.snapshot state) x) ::
10 |           Cost.events state.................................
  The refinement is stated here.
|}]

module Find_costs_two = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module P = Vox_connectivity.Partition
  module Cost = U.Cost
  module E = Vox_union_find_events
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | P.contains (U.model s) x}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = Cost.find_fee state})
        @ unique total ghost ->
      {r : U.result | let state = state in
        Cost.ticks r.#state = Bigint.add (Cost.ticks state) 2Z} @ unique =
    fun x state fee ->
    ghost_ (Cost.event_cost (borrow_ state));
    let history = ghost_ (Cost.events (borrow_ state)) in
    let depth = ghost_ (Cost.depth (Cost.snapshot (borrow_ state)) x) in
    let r = U.find x state fee in
    let #{U.value; state} = r in
    ghost_ (Cost.event_cost (borrow_ state);
      E.total_def (E.Find depth :: history); E.weight_def (E.Find depth));
    #{U.value; state}
end;;
[%%expect{|
Line 21, characters 4-21:
21 |     #{U.value; state}
         ^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 11-12, characters 22-62:
11 | ......................let state = state in
12 |         Cost.ticks r.#state = Bigint.add (Cost.ticks state) 2Z............
  The refinement is stated here.
|}]

module Insufficient_wallet = struct
  module C = Vox_big_credits.Make ()
  let bad () =
    let wallet = C.Budget.create 10Z in
    C.split 11Z wallet
end;;
[%%expect{|
Line 5, characters 16-22:
5 |     C.split 11Z wallet
                    ^^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_big_credits.mli", line 21, characters 42-61:
  The refinement is stated here.
|}]

module Prescribed_winner = struct
  module P = Vox_connectivity.Partition
  let bad (before : int P.t @ immutable) (after : int P.t @ immutable)
      (x : int) (y : int) (r : int)
      (_ : {u : unit | P.joined before after x y r}) :
      {u : unit | r === P.representative before x ||
        r === P.representative before y} @ ghost = ghost_ (
    Vox_partition_classes_proof.joined_law before after x y r r;
    P.connected_def before x r; P.connected_def before y r;
    ())
end;;
[%%expect{|
Line 10, characters 4-6:
10 |     ())
         ^^
Error: Refinement could not be proved (counterexample: x = 0, y = 0, r = 0)
Lines 6-7, characters 18-39:
6 | ..................r === P.representative before x ||
7 |         r === P.representative before y....................
  The refinement is stated here.
|}]
