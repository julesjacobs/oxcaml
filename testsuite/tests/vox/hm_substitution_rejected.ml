(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hm_declarative.ml hm_type_proofs.ml hm_substitution.ml";
 readonly_files = "hm_substitution_rejected.ml";
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
open Hm_substitution;;
[%%expect{|
|}]

module Substitute_bound_parameter = struct
  let bad : (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total)) @ total ->
    {u : unit | substitute_type rho (Parameter Z) === Boolean} @ ghost = fun rho -> ghost_ (
    let t = Parameter Z in substitute_type_def rho t;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 5, characters 18-27:
5 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Forget_context_substitution = struct
  let bad : (p : Copy_spec.node Pref.t) @ immutable ->
      {d : typing | typed Z (Binding (Forall (Z, Free p), Empty_context))
        (Bound Z) Boolean d} @ immutable ghost = fun p -> ghost_ (
    let z = Z in let t = Free p in let s = Forall (z, t) in
    let g = Binding (s, Empty_context) in let args = No_arguments in
    let d = Variable args in let e = Bound z in let b = Boolean in
    typed_def z g e b d; lookup_def g z; open_scheme_def s args;
    open_type_def args t; refine_ d)
end;;
[%%expect{|
Line 9, characters 26-35:
9 |     open_type_def args t; refine_ d)
                              ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
