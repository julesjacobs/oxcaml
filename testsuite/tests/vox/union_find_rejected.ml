(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find_events.mli vox_union_find_events.ml vox_union_find.mli vox_union_find.ml vox_union_find_complexity.ml";
 readonly_files = "union_find_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)

module Private_state = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad (s : U.t @ unique) =
    #{U.raw = s.#U.raw; paths = s.#U.paths; capacity = s.#U.capacity;
      alpha = s.#U.alpha; spent = s.#U.spent}
end;;
[%%expect{|
Lines 5-6, characters 4-45:
5 | ....#{U.raw = s.#U.raw; paths = s.#U.paths; capacity = s.#U.capacity;
6 |       alpha = s.#U.alpha; spent = s.#U.spent}
Error: Cannot create values of the private type "U.t"
|}]

module No_membership = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad (x : Vox_union_find_model.elem @ immutable)
      (state : {s : U.t | U.valid s} @ unique read_write total) =
    let state = state in
    (state : {s : U.t | U.valid s && U.member x s})
end;;
[%%expect{|
Line 7, characters 5-10:
7 |     (state : {s : U.t | U.valid s && U.member x s})
         ^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module No_fee = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad : (x : Vox_union_find_model.elem) @ immutable ->
      (state : {s : U.t | U.valid s && U.member x s}) @ unique read_write total ->
      U.result @ unique = fun x state ->
    let zero = C.empty () in
    let result = U.find x state (zero) in
    result
end;;
[%%expect{|
Line 8, characters 32-38:
8 |     let result = U.find x state (zero) in
                                    ^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Reuse_state = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad : (x : Vox_union_find_model.elem) @ immutable ->
      (state : {s : U.t | U.valid s && U.member x s}) @ unique read_write total ->
      (fee1 : {b : C.token | let state = state in
        C.credits b >= Vox_union_find_amortized.find_fee state.#U.alpha})
        @ unique total ghost ->
      (fee2 : {b : C.token | let state = state in
        C.credits b >= Vox_union_find_amortized.find_fee state.#U.alpha})
        @ unique total ghost -> U.result @ unique = fun x state fee1 fee2 ->
    let _ = U.find x state fee1 in
    let result = U.find x state fee2 in result
end;;
[%%expect{|
Line 13, characters 26-31:
13 |     let result = U.find x state fee2 in result
                               ^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 12, characters 21-26:
12 |     let _ = U.find x state fee1 in
                          ^^^^^

|}]

module Zero_capacity = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad () =
    let capacity = 0Z in let amount = 1Z in
    let input : {n : Bigint.t | n >= 0Z} = amount in
    let fee = C.Budget.create input in
    U.create (capacity) (fee)
end;;
[%%expect{|
Line 8, characters 13-23:
8 |     U.create (capacity) (fee)
                 ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
