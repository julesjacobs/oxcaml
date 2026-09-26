(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_elaboration_check.ml hm_elaboration_check_demo.ml";
 { bytecode; }
*)
open Hm_declarative

let accept e t d =
  if not (Hm_elaboration_check.check Z Empty_context e t d) then
    failwith "rejected valid elaboration"

let reject e t d =
  if Hm_elaboration_check.check Z Empty_context e t d then
    failwith "accepted invalid elaboration"

let () =
  let b = Boolean in
  let a = Parameter Z in
  let id = Lambda (Bound Z) in
  let variable = Variable No_arguments in
  accept Truth b Constant;
  accept id (Function (b, b)) (Abstraction (b, variable));
  accept (Recursive (Bound Z)) (Function (b, b))
    (Recursion (b, b, variable));
  let scheme = Forall (S Z, Function (a, a)) in
  let rhs = Abstraction (a, variable) in
  let use = Variable (Argument (b, No_arguments)) in
  accept (Let (id, Apply (Bound Z, Truth))) b
    (Let_binding (scheme, rhs, Application (b, use, Constant)));
  let bb = Function (b, b) in
  let outer = Bound (S Z) in
  let at_bool = Variable (Argument (b, No_arguments)) in
  let at_function = Variable (Argument (bb, No_arguments)) in
  accept (Let (id, Let (Apply (Bound Z, Truth), Apply (outer, outer))))
    bb (Let_binding (scheme, rhs,
      Let_binding (Forall (Z, b), Application (b, at_bool, Constant),
        Application (bb, at_function, at_bool))));
  reject (Let (id, Bound Z)) bb
    (Let_binding (scheme, rhs,
      Variable (Argument (b, Argument (b, No_arguments)))));
  reject (Bound Z) b variable;
  reject id b (Abstraction (b, variable));
  reject Truth b (Abstraction (b, variable));
  reject (Let (id, Apply (Bound Z, Truth))) b
    (Let_binding (scheme, rhs, Application (b, variable, Constant)));
  reject (Lambda Truth) (Function (b, b)) (Abstraction (b, variable))
