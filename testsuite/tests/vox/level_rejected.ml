(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml level_spec.ml lower_locality_spec.ml level_unifier_spec.ml";
 readonly_files = "level_rejected.ml";
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
*)

open Copy_spec;;
open Level_spec;;
open Level_unifier_spec;;
[%%expect{|
|}]

module Raise_level = struct
  let bad : unit -> {u : unit | decreases (Finite 1) (Finite 2)} @ ghost = fun () -> ghost_ (
    let a = Finite 1 in let b = Finite 2 in decreases_def a b; let u = () in refine_ u)
end;;
[%%expect{|
Line 3, characters 77-86:
3 |     let a = Finite 1 in let b = Finite 2 in decreases_def a b; let u = () in refine_ u)
                                                                                 ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Generic_is_active = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.at h p === Some {desc = Var; level = Generic; memo = Empty_memo}} ->
      {u : unit | active h p} @ ghost = fun h p premise -> ghost_ (
    let refine_ premise = premise in active_def h p; at_level_def h p; let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 85-94:
5 |     let refine_ premise = premise in active_def h p; at_level_def h p; let u = () in refine_ u)
                                                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Skip_child = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.at h q === Some (cell Var 5)} ->
      {u : unit | bounded h 0 (Fork (p, Tip q, Tip q))} @ ghost = fun h p q premise -> ghost_ (
    let refine_ premise = premise in let desc = Var in cell_def desc 5;
    let child = Tip q in let tree = Fork (p, child, child) in
    bound_root_def child; bounded_def h 0 tree; bounded_def h 0 child; below_def h q 0; at_level_def h q;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Link_without_lowering = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (search : search) @ immutable ->
      {u : unit | H.at h p === Some (cell Var 0) && H.at h q === Some (cell Bool 3)} ->
      {u : unit | unified h p q true (H.put h p (redirect h p q)) (Bind_left search)} @ ghost = fun h p q search premise -> ghost_ (
    let refine_ premise = premise in let v = Var in cell_def v 0; let b = Bool in cell_def b 3;
    let d = Bind_left search in let after = H.put h p (redirect h p q) in let ok = true in
    unified_def h p q ok after d; at_level_def h p; at_level_def h q; below_def h q 0;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 9, characters 18-27:
9 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Equal_level_not_occurs_free = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | searched h p p false Leaf} @ ghost = fun h p -> ghost_ (
    let d = Leaf in let found = false in searched_def h p p found d; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 83-92:
4 |     let d = Leaf in let found = false in searched_def h p p found d; let u = () in refine_ u)
                                                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Memo_is_not_a_level = struct
  let bad : (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      (epoch : node Pref.t) @ immutable ->
      {u : unit | H.at h p === Some (cell Var 1)} ->
      {u : unit | lower_frame h (H.put h p {desc = Var; level = Finite 0; memo = Memo (epoch, p)}) p} @ ghost = fun h p epoch premise -> ghost_ (
    let refine_ premise = premise in let desc = Var in cell_def desc 1;
    let after = H.put h p {desc = Var; level = Finite 0; memo = Memo (epoch, p)} in
    lower_frame_def h after p; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 45-54:
8 |     lower_frame_def h after p; let u = () in refine_ u)
                                                 ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
