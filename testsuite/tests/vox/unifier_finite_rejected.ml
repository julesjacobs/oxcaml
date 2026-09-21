(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml unifier_spec.ml unifier_proofs.ml unifier.ml unifier_finite_spec.ml unifier_finite_proofs.ml";
 readonly_files = "unifier_finite_rejected.ml";
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

open Unifier_spec;;
open Unifier_finite_spec;;
open Unifier_finite_proofs;;
[%%expect{|
|}]

module Self_link = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some (Link p)} ->
      {t : tree | root t === p && finite h t} @ immutable ghost =
    fun h p premise -> ghost_ (
      let refine_ premise = premise in
      let leaf = Free p in root_def leaf; finite_def h leaf;
      let t = Alias (p, leaf) in root_def t; finite_def h t; refine_ t)
end;;
[%%expect{|
Line 8, characters 61-70:
8 |       let t = Alias (p, leaf) in root_def t; finite_def h t; refine_ t)
                                                                 ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Stale_witness = struct
  let bad : (h : node Pref.heap) @ immutable ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some Var && H.mem h q} ->
      {t : tree | root t === p && finite (H.put h p (Link q)) t} @ immutable ghost =
    fun h p q premise -> ghost_ (
      let refine_ premise = premise in
      let after = H.put h p (Link q) in
      let t = Free p in root_def t; finite_def h t; finite_def after t; refine_ t)
end;;
[%%expect{|
Line 9, characters 72-81:
9 |       let t = Free p in root_def t; finite_def h t; finite_def after t; refine_ t)
                                                                            ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Omitted_arrow_child = struct
  let bad : (h : node Pref.heap) @ immutable ->
      (p : node Pref.t) @ immutable -> (a : node Pref.t) @ immutable ->
      (b : node Pref.t) @ immutable -> (left : tree) @ immutable ->
      {u : unit | H.mem h p && H.at h p === Some (Arrow (a, b))
        && root left === a && finite h left} ->
      {t : tree | root t === p && finite h t} @ immutable ghost =
    fun h p a b left premise -> ghost_ (
      let refine_ premise = premise in
      let t = Branch (p, left, left) in root_def t; finite_def h t; refine_ t)
end;;
[%%expect{|
Line 10, characters 68-77:
10 |       let t = Branch (p, left, left) in root_def t; finite_def h t; refine_ t)
                                                                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unchecked_binding = struct
  let bad : (h : node Pref.heap) @ immutable ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.mem h q && H.at h p === Some Var
        && H.at h q === Some (Arrow (p, p)) && not (p === q)} ->
      {t : tree | root t === q && finite (H.put h p (Link q)) t} @ immutable ghost =
    fun h p q premise -> ghost_ (
      let refine_ premise = premise in let u = () in
      let trace = Leaf in searched_def h p q false trace;
      search_finite h p q q trace (refine_ u))
end;;
[%%expect{|
Line 10, characters 34-45:
10 |       search_finite h p q q trace (refine_ u))
                                       ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
