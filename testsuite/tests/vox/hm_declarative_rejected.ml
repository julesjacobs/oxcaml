(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hm_declarative.ml";
 readonly_files = "hm_declarative_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run-expect;
   check-program-output;
 }
*)
open Hm_declarative;;
[%%expect{|
|}]

module Missing_argument = struct
  let bad : unit -> {d : typing | typed Z
    (Binding (Forall (S Z, Parameter Z), Empty_context))
    (Bound Z) Boolean d} @ immutable ghost = fun () -> ghost_ (
    let z = Z in let one = S z in let p = Parameter z in
    let s = Forall (one, p) in let g = Binding (s, Empty_context) in
    let args = No_arguments in let d = Variable args in
    let e = Bound z in let t = Boolean in
    typed_def z g e t d; lookup_def g z; length_def args; arity_def s;
    refine_ d)
end;;
[%%expect{|
Line 10, characters 4-13:
10 |     refine_ d)
         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Capture_scheme_parameter = struct
  let bad : unit -> {u : unit | weaken_scheme (S Z) (Forall (S Z, Parameter Z))
    === Forall (S Z, Parameter (S Z))} @ ghost = fun () -> ghost_ (
    let z = Z in let one = S z in let p = Parameter z in
    let s = Forall (one, p) in weaken_scheme_def one s;
    shift_def one one p; shift_index_def one one z;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 18-27:
7 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Polymorphic_recursive_self = struct
  let bad : unit -> {d : typing | typed Z Empty_context
    (Recursive (Apply (Bound (S Z), Truth))) (Function (Boolean, Boolean)) d} @ immutable ghost = fun () -> ghost_ (
    let z = Z in let one = S z in let b = Boolean in let f = Function (b, b) in
    let self_scheme = Forall (z, f) in let arg_scheme = Forall (z, b) in
    let empty = Empty_context in let self = Binding (self_scheme, empty) in
    let env = Binding (arg_scheme, self) in
    let no_args = No_arguments in let args = Argument (b, no_args) in
    let variable = Variable args in let constant = Constant in
    let body = Application (b, variable, constant) in let d = Recursion (b, b, body) in
    let e = Bound one in let application = Apply (e, Truth) in
    let recursive = Recursive application in
    typed_def z empty recursive f d; typed_def z env application b body;
    typed_def z env e f variable; lookup_def env one; lookup_def self z;
    length_def args; length_def no_args; arity_def self_scheme;
    refine_ d)
end;;
[%%expect{|
Line 16, characters 4-13:
16 |     refine_ d)
         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
