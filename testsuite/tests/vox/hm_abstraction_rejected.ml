(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_substitution.ml hm_abstraction.ml";
 readonly_files = "hm_abstraction_rejected.ml";
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
open Hm_declarative;;
open Hm_abstraction;;
[%%expect{|
|}]

module Generalize_environment_name = struct
  let bad : (p : Copy_spec.node Pref.t) @ immutable ->
    {u : unit | context_avoids (Name (p, No_names))
      (Binding (Forall (Z, Free p), Empty_context))} @ ghost = fun p -> ghost_ (
    let names = Name (p, No_names) in let t = Free p in let s = Forall (Z, t) in
    let g = Binding (s, Empty_context) in context_avoids_def names g;
    scheme_avoids_def names s; avoids_def names t; position_def names p;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Split_repeated_parameter = struct
  let bad : (p : Copy_spec.node Pref.t) @ immutable ->
    {u : unit | abstract_type (Name (p, No_names)) Z (Function (Free p, Free p))
      === Function (Parameter Z, Parameter (S Z))} @ ghost = fun p -> ghost_ (
    let names = Name (p, No_names) in let z = Z in let a = Free p in let t = Function (a, a) in
    abstract_type_def names z t; abstract_type_def names z a;
    abstract_free_def names z p; position_def names p; add_def z z;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Substitute_before_abstraction = struct
  let (bad @ total) : (p : Copy_spec.node Pref.t) @ immutable ->
      (q : Copy_spec.node Pref.t) @ immutable -> {u : unit | not (p === q)} ->
      {u : unit | true} @ ghost = fun p q distinct -> ghost_ (
    let refine_ distinct = distinct in let names = Name (q, No_names) in
    let t = Free p in avoids_def names t; position_def names p;
    let none = No_names in position_def none p;
    let[@def] rho : Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total =
      fun _x -> Copy_spec.Variable q in
    let substituted = Hm_substitution.substitute_type rho t in
    Hm_substitution.substitute_type_def rho t; rho_def p;
    let value = Copy_spec.Variable q in Hm_declarative.embed_def value;
    avoids_def names substituted; position_def names q;
    let u = () in let _fresh : {u : unit | avoids names substituted} = refine_ u in refine_ u)
end;;
[%%expect{|
Line 14, characters 71-80:
14 |     let u = () in let _fresh : {u : unit | avoids names substituted} = refine_ u in refine_ u)
                                                                            ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
