(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml unifier_spec.ml unifier_proofs.ml unifier.ml unifier_finite_spec.ml unifier_finite_proofs.ml unifier_mgu_spec.ml unifier_mgu_proofs.ml stlc_spec.ml stlc_graph_proofs.ml stlc_model_proofs.ml stlc_complete_proofs.ml stlc_solve_proofs.ml stlc_inference_proofs.ml stlc_generate.ml stlc_solve.ml stlc_infer.ml";
 readonly_files = "stlc_rejected.ml";
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
open Stlc_spec;;
[%%expect{|
|}]

module Wrong_variable = struct
  let bad : (h : node Pref.heap) @ immutable ->
      (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | H.mem h p && H.mem h q && not (p === q)} ->
      {g : graph | built h (Bind (p, Bind (q, Empty))) g h && source g === Bound (S Z)}
      @ immutable ghost = fun h p q premise -> ghost_ (
    let refine_ premise = premise in let env = Bind (p, Bind (q, Empty)) in
    let rest = Bind (q, Empty) in let zero = Z in let i = S zero in
    let g = GVar (i, p) in lookup_def env i; lookup_def rest zero;
    built_def h env g h; source_def g; refine_ g)
end;;
[%%expect{|
Line 10, characters 39-48:
10 |     built_def h env g h; source_def g; refine_ g)
                                            ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Missing_application_constraint = struct
  let bad : (f : graph) @ immutable -> (a : graph) @ immutable ->
      (p : node Pref.t) @ immutable -> (arrow : node Pref.t) @ immutable ->
      (h1 : node Pref.heap) @ immutable -> (h2 : node Pref.heap) @ immutable ->
      {cs : equations | cs === constraints (GApp (f, a, p, arrow, h1, h2))} @ immutable ghost =
    fun f a p arrow h1 h2 -> ghost_ (
      let g = GApp (f, a, p, arrow, h1, h2) in constraints_def g;
      let cs = Nothing in refine_ cs)
end;;
[%%expect{|
Line 8, characters 26-36:
8 |       let cs = Nothing in refine_ cs)
                              ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Boolean_function = struct
  let bad : unit -> {d : typing | typed No_types (Apply (Boolean, Boolean)) TBool d} @ immutable ghost = fun () -> ghost_ (
    let ctx = No_types in let e = Boolean in let result = TBool in
    let arrow = TArrow (TBool, TBool) in let constant = Constant in
    let app = Apply (Boolean, Boolean) in let d = Application (TBool, Constant, Constant) in
    typed_def ctx e arrow constant; typed_def ctx e result constant; typed_def ctx app result d;
    refine_ d)
end;;
[%%expect{|
Line 7, characters 4-13:
7 |     refine_ d)
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Over_general_identity = struct
  let bad : unit -> {d : typing | typed No_types (Lambda (Bound Z)) (TArrow (TBool, TArrow (TBool, TBool))) d}
      @ immutable ghost = fun () -> ghost_ (
    let ctx = No_types in let env = Type (TBool, No_types) in let zero = Z in
    let body = Bound Z in let out = TArrow (TBool, TBool) in let v = Variable in
    let e = Lambda body in let t = TArrow (TBool, out) in let d = Abstraction (TBool, Variable) in
    lookup_type_def env zero; typed_def env body out v; typed_def ctx e t d;
    refine_ d)
end;;
[%%expect{|
Line 8, characters 4-13:
8 |     refine_ d)
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Skip_solver = struct
  let bad : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
      {u : unit | solved h (Equal (p, q)) true h Done} @ ghost = fun h p q -> ghost_ (
    let cs = Equal (p, q) in let d = Done in solved_def h cs true h d;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 18-27:
5 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unbound_variable = struct
  let bad () =
    let e = Bound Z in let zero = Z in
    ghost_ (scoped_term_def zero e; present_def zero zero);
    let e : {e : term | scoped_term Z e} = refine_ e in
    let refine_ r = Stlc_infer.infer e in r.#ok
end;;
[%%expect{|
Line 5, characters 43-52:
5 |     let e : {e : term | scoped_term Z e} = refine_ e in
                                               ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
