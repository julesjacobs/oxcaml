(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml";
 readonly_files = "copy_rejected.ml";
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
[%%expect{|
|}]

module Invent_mapping = struct
  let bad : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | mapping Start p === Some q} @ ghost = fun p q -> ghost_ (
    let d = Start in mapping_def d p; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 52-61:
4 |     let d = Start in mapping_def d p; let u = () in refine_ u)
                                                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Copy_boundary = struct
  let bad : (h : Pref.heap) @ immutable -> (d : history) @ immutable ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.at h p === Some (cell Var 0) && H.mem h p && not (p === q)} ->
      {u : unit | target_for h d p q} @ ghost = fun h d p q premise -> ghost_ (
    let refine_ premise = premise in let desc = Var in cell_def desc 0;
    target_for_def h d p q; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 42-51:
7 |     target_for_def h d p q; let u = () in refine_ u)
                                              ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Reuse_old_target = struct
  let bad : (h : Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      (old : node) @ immutable -> {u : unit | H.mem h q} ->
      {u : unit | valid h epoch 0 (Fresh (Start, p, q, old, Var))} @ ghost =
    fun h epoch p q old premise -> ghost_ (
      let refine_ premise = premise in let d = Start in let desc = Var in
      let event = Fresh (d, p, q, old, desc) in
      heap_def h epoch 0 d; valid_def h epoch 0 event; let u = () in refine_ u)
end;;
[%%expect{|
Line 9, characters 69-78:
9 |       heap_def h epoch 0 d; valid_def h epoch 0 event; let u = () in refine_ u)
                                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Drop_arrow_child = struct
  let bad : (h : Pref.heap) @ immutable -> (d : history) @ immutable ->
      (a : node Pref.t) @ immutable -> (b : node Pref.t) @ immutable ->
      {u : unit | ready h d (Arrow (a, b)) Var} @ ghost = fun h d a b -> ghost_ (
    let src = Arrow (a, b) in let dst = Var in ready_def h d src dst;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 6, characters 18-27:
6 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Split_parameter = struct
  let bad : (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (p : node Pref.t) @ immutable -> (a : node Pref.t) @ immutable ->
      {u : unit | interpret rho choices (Product (p, Parameter a, Parameter a))
        === Function (Boolean, Function (Boolean, Boolean))} @ ghost = fun rho choices p a -> ghost_ (
    let t = Parameter a in let tree = Product (p, t, t) in
    interpret_def rho choices t; interpret_def rho choices tree; let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 79-88:
8 |     interpret_def rho choices t; interpret_def rho choices tree; let u = () in refine_ u)
                                                                                   ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Wrong_epoch = struct
  let bad : (old : node) @ immutable -> (epoch : node Pref.t) @ immutable ->
      (stale : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | not (epoch === stale)} ->
      {u : unit | (mark old stale q).memo === Memo (epoch, q)} @ ghost =
    fun old epoch stale q premise -> ghost_ (
      let refine_ premise = premise in mark_def old stale q; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 75-84:
7 |       let refine_ premise = premise in mark_def old stale q; let u = () in refine_ u)
                                                                               ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
