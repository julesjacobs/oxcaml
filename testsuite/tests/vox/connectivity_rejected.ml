(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find_events.mli vox_union_find_events.ml vox_union_find.mli vox_union_find.ml vox_union_find_online.mli vox_union_find_online.ml vox_connectivity.mli vox_connectivity.ml";
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
  let bad : (state : {s : U.t | U.size s < Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 10Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 7, characters 60-63:
7 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                                ^^^
Error: Refinement could not be proved (counterexample)
File "vox_connectivity.mli", line 131, characters 28-45:
  The refinement is stated here.
|}]

module Overpay_insertion = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (state : {s : U.t | U.size s < Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 12Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 7, characters 60-63:
7 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                                ^^^
Error: Refinement could not be proved (counterexample)
File "vox_connectivity.mli", line 131, characters 28-45:
  The refinement is stated here.
|}]

module Reuse_state = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | U.contains (U.snapshot s) x}) @ unique read_write total ->
      (fee1 : {b : C.token | let state = state in C.credits b = U.find_fee state})
        @ unique total ghost ->
      (fee2 : {b : C.token | let state = state in C.credits b = U.find_fee state})
        @ unique total ghost -> U.result @ unique = fun x state fee1 fee2 ->
    let _ = U.find x state fee1 in
    U.find x state fee2
end;;
[%%expect{|
Line 11, characters 13-18:
11 |     U.find x state fee2
                  ^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 10, characters 21-26:
10 |     let _ = U.find x state fee1 in
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
  let bad (x : U.elem @ immutable)
      (state : U.t @ unique read_write total) =
    let state = state in
    (state : {s : U.t | U.contains (U.snapshot s) x})
end;;
[%%expect{|
Line 7, characters 5-10:
7 |     (state : {s : U.t | U.contains (U.snapshot s) x})
         ^^^^^
Error: Refinement could not be proved (counterexample)
Line 7, characters 24-51:
7 |     (state : {s : U.t | U.contains (U.snapshot s) x})
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Machine_limit = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (state : {s : U.t | U.size s = Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 7, characters 54-59:
7 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                          ^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_connectivity.mli", line 129, characters 24-54:
  The refinement is stated here.
|}]

module Hidden_model = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad = U.contents
end;;
[%%expect{|
Line 4, characters 12-22:
4 |   let bad = U.contents
                ^^^^^^^^^^
Error: Unbound value "U.contents"
Hint:   Did you mean "U.contains"?
|}]

module Forged_transition = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad (before : U.snapshot @ immutable)
      (after : U.snapshot @ immutable) (x : U.elem @ immutable) :
      {u : unit | U.found before after x} @ ghost = ghost_ ()
end;;
[%%expect{|
Line 6, characters 59-61:
6 |       {u : unit | U.found before after x} @ ghost = ghost_ ()
                                                               ^^
Error: Refinement could not be proved (counterexample)
Line 6, characters 18-40:
6 |       {u : unit | U.found before after x} @ ghost = ghost_ ()
                      ^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

module Unrecorded_find = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  (* A find that skips its work cannot claim the event a find records. *)
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | U.contains (U.snapshot s) x}) @ unique read_write total ->
      {s : U.t | let state = state in
        U.events s === Vox_union_find_events.Find (U.depth (U.snapshot state) x) ::
          U.events state} @ unique = fun x state -> state
end;;
[%%expect{|
Line 9, characters 52-57:
9 |           U.events state} @ unique = fun x state -> state
                                                        ^^^^^
Error: Refinement could not be proved (counterexample)
Lines 7-9, characters 17-24:
7 | .................let state = state in
8 |         U.events s === Vox_union_find_events.Find (U.depth (U.snapshot state) x) ::
9 |           U.events state.................................
  The refinement is stated here.
|}]

module Find_costs_two = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  module E = Vox_union_find_events
  (* A find costs 4 * depth + 2 ticks, so two ticks holds only at a root. *)
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | U.contains (U.snapshot s) x}) @ unique read_write total ->
      (fee : {b : C.token | let state = state in C.credits b = U.find_fee state})
        @ unique total ghost ->
      {r : U.result | let state = state in
        U.ticks r.#state = Bigint.add (U.ticks state) 2Z} @ unique =
    fun x state fee ->
    ghost_ (U.event_cost (borrow_ state));
    let history = ghost_ (U.events (borrow_ state)) in
    let depth = ghost_ (U.depth (U.snapshot (borrow_ state)) x) in
    let r = U.find x state fee in
    let #{U.value; state} = r in
    ghost_ (U.event_cost (borrow_ state);
      E.total_def (E.Find depth :: history); E.weight_def (E.Find depth));
    #{U.value; state}
end;;
[%%expect{|
Line 20, characters 4-21:
20 |     #{U.value; state}
         ^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 10-11, characters 22-56:
10 | ......................let state = state in
11 |         U.ticks r.#state = Bigint.add (U.ticks state) 2Z............
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
File "vox_big_credits.mli", line 21, characters 26-61:
  The refinement is stated here.
|}]
