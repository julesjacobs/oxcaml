(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml ghost_pref.mli ghost_pref.ml vox_big_credits.mli vox_big_credits.ml vox_ackermann.ml vox_union_find_potential.ml vox_union_find_levels.ml vox_union_find_path_cost.ml vox_union_find_model.ml vox_union_find_forest.ml vox_union_find_rank.ml vox_union_find_mass.ml vox_union_find_link.ml vox_union_find_worker.ml vox_union_find_amortized.ml vox_union_find_bank.ml vox_union_find_spec.ml vox_union_find.mli vox_union_find.ml vox_union_find_online.mli vox_union_find_online.ml vox_connectivity.mli vox_connectivity.ml";
 readonly_files = "connectivity_rejected.ml";
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

module Underpay_insertion = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (state : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 10Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 7, characters 60-63:
7 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                                ^^^
Error: Refinement could not be proved (counterexample)
|}]

module Overpay_insertion = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (state : {s : U.t | U.valid s && U.size s < Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 12Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 7, characters 60-63:
7 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                                ^^^
Error: Refinement could not be proved (counterexample)
|}]

module Reuse_state = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (x : U.elem) @ immutable ->
      (state : {s : U.t | U.valid s && U.member x s}) @ unique read_write total ->
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

module Machine_limit = struct
  module C = Vox_big_credits.Make ()
  module U = Vox_connectivity.Make (C)
  let bad : (state : {s : U.t | U.valid s && U.size s = Bigint.of_int max_int})
      @ unique read_write total ->
      (fee : {b : C.token | C.credits b = 11Z}) @ unique total ghost ->
      U.result @ unique = fun state fee -> U.make_set state fee
end;;
[%%expect{|
Line 7, characters 54-59:
7 |       U.result @ unique = fun state fee -> U.make_set state fee
                                                          ^^^^^
Error: Refinement could not be proved (counterexample)
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
|}]
