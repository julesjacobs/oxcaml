(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find.mli vox_union_find.ml vox_union_find_complexity.ml";
 readonly_files = "union_find_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
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
    let refine_ state = state in
    (refine_ state : {s : U.t | U.valid s && U.member x s})
end;;
[%%expect{|
Line 7, characters 5-18:
7 |     (refine_ state : {s : U.t | U.valid s && U.member x s})
         ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module No_fee = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad : (x : Vox_union_find_model.elem) @ immutable ->
      (state : {s : U.t | U.valid s && U.member x s}) @ unique read_write total ->
      U.result @ unique = fun x state ->
    let refine_ zero = C.empty () in
    let refine_ result = U.find x state (refine_ zero) in
    result
end;;
[%%expect{|
Line 8, characters 40-54:
8 |     let refine_ result = U.find x state (refine_ zero) in
                                            ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Reuse_state = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad : (x : Vox_union_find_model.elem) @ immutable ->
      (state : {s : U.t | U.valid s && U.member x s}) @ unique read_write total ->
      (fee : {b : C.token | let refine_ state = state in
        C.credits b >= Vox_union_find_amortized.find_fee state.#U.alpha})
        @ unique total ghost -> U.result @ unique = fun x state fee ->
    let _ = U.find x state fee in
    let refine_ result = U.find x state fee in result
end;;
[%%expect{|
Line 10, characters 40-43:
10 |     let refine_ result = U.find x state fee in result
                                             ^^^
Error: This value is used here, but it has already been used as unique at:
Line 9, characters 27-30:
9 |     let _ = U.find x state fee in
                               ^^^

|}]

module Zero_capacity = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_union_find.Make (C)
  let bad () =
    let capacity = 0Z in let amount = 1Z in
    let input : {n : Bigint.t | n >= 0Z} = refine_ amount in
    let refine_ fee = C.Budget.create input in
    U.create (refine_ capacity) (refine_ fee)
end;;
[%%expect{|
Line 8, characters 13-31:
8 |     U.create (refine_ capacity) (refine_ fee)
                 ^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
