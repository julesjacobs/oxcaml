(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_declarative_demo.ml";
 { bytecode; }
 { native; }
*)
open Hm_declarative
open Hm_type_proofs

let (id_id @ total) : (a : mono) @ immutable ->
    {u : unit | mono_wf Z a} ->
    {d : typing | typed Z Empty_context
      (Let (Lambda (Bound Z), Apply (Bound Z, Bound Z))) (Function (a, a)) d}
    @ immutable ghost = fun a premise -> ghost_ (
  let refine_ premise = premise in
  let z = Z in let one = S z in let parameter = Parameter z in
  let identity_type = Function (parameter, parameter) in
  let identity = Forall (one, identity_type) in
  let no_args = No_arguments in let variable = Variable no_args in
  let rhs = Abstraction (parameter, variable) in
  let aa = Function (a, a) in
  let left_args = Argument (aa, no_args) in
  let right_args = Argument (a, no_args) in
  let left = Variable left_args in let right = Variable right_args in
  let app = Application (aa, left, right) in
  let d = Let_binding (identity, rhs, app) in
  let empty = Empty_context in let body_env = Binding (identity, empty) in
  let arg_scheme = Forall (z, parameter) in
  let rhs_env = Binding (arg_scheme, empty) in
  let bound = Bound z in let lam = Lambda bound in
  let application = Apply (bound, bound) in let source = Let (lam, application) in
  add_def one z; add_def z z; add_def z one;
  present_def one z; mono_wf_def one parameter;
  mono_wf_def one identity_type; mono_wf_def z aa;
  scheme_wf_def z identity; scheme_wf_def one arg_scheme;
  context_wf_def z empty; context_wf_def one empty;
  context_wf_def z body_env; context_wf_def one rhs_env;
  weaken_context_def one empty;
  length_def no_args; length_def left_args; length_def right_args;
  arity_def arg_scheme; arity_def identity;
  arguments_wf_def one no_args; arguments_wf_def z no_args;
  arguments_wf_def z left_args; arguments_wf_def z right_args;
  lookup_def rhs_env z; lookup_def body_env z;
  open_scheme_def arg_scheme no_args; open_type_def no_args parameter;
  open_index_def no_args z;
  open_scheme_def identity left_args; open_scheme_def identity right_args;
  open_type_def left_args identity_type; open_type_def right_args identity_type;
  open_type_def left_args parameter; open_type_def right_args parameter;
  open_index_def left_args z; open_index_def right_args z;
  typed_def one rhs_env bound parameter variable;
  typed_def one empty lam identity_type rhs;
  let left_type = Function (aa, aa) in mono_wf_def z left_type;
  typed_def z body_env bound left_type left;
  typed_def z body_env bound aa right;
  typed_def z body_env application aa app;
  typed_def z empty source aa d; refine_ d)

let () =
  ghost_ (
    let a = Boolean in let z = Z in mono_wf_def z a;
    let u = () in let _ = id_id a (refine_ u) in ());
  let zero = Z in let one = S zero in let two = S one in
  let s = Forall (one, Function (Parameter zero, Parameter one)) in
  assert (weaken_scheme one s = Forall (one, Function (Parameter zero, Parameter two)));
  assert (open_scheme s (Argument (Boolean, No_arguments))
    = Function (Boolean, Parameter zero))

let (recursive_call @ total) : (a : mono) @ immutable -> (b : mono) @ immutable ->
    {u : unit | mono_wf Z a && mono_wf Z b} ->
    {d : typing | typed Z Empty_context
      (Recursive (Apply (Bound (S Z), Bound Z))) (Function (a, b)) d}
    @ immutable ghost = fun a b premise -> ghost_ (
  let refine_ premise = premise in
  let z = Z in let one = S z in let t = Function (a, b) in
  let arg_scheme = Forall (z, a) in let self_scheme = Forall (z, t) in
  let empty = Empty_context in let self = Binding (self_scheme, empty) in
  let env = Binding (arg_scheme, self) in
  let args = No_arguments in let variable = Variable args in
  let body = Application (a, variable, variable) in let d = Recursion (a, b, body) in
  let f = Bound one in let x = Bound z in let app = Apply (f, x) in
  let source = Recursive app in
  add_def z z; mono_wf_def z t; scheme_wf_def z arg_scheme;
  scheme_wf_def z self_scheme; context_wf_def z empty;
  context_wf_def z self; context_wf_def z env;
  length_def args; arguments_wf_def z args;
  arity_def self_scheme; arity_def arg_scheme;
  lookup_def env one; lookup_def self z; lookup_def env z;
  open_scheme_def self_scheme args; open_scheme_def arg_scheme args;
  open_empty t; open_empty a;
  typed_def z env f t variable; typed_def z env x a variable;
  typed_def z env app b body; typed_def z empty source t d; refine_ d)
