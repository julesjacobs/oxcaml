open Hm_declarative
open Hm_type_proofs

let (nested_alias_typing @ total) : (a : mono) @ immutable ->
    {u : unit | mono_wf Z a} ->
    {d : typing | typed Z Empty_context
      (Let (Lambda (Bound Z), Let (Bound Z, Apply (Bound Z, Bound Z)))) (Function (a, a)) d}
    @ immutable ghost = fun a premise -> ghost_ (
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
  let alias_args = Argument (parameter, no_args) in let alias = Variable alias_args in
  let inner_d = Let_binding (identity, alias, app) in
  let d = Let_binding (identity, rhs, inner_d) in
  let empty = Empty_context in let outer_env = Binding (identity, empty) in
  let body_env = Binding (identity, outer_env) in
  let arg_scheme = Forall (z, parameter) in
  let rhs_env = Binding (arg_scheme, empty) in
  let bound = Bound z in let lam = Lambda bound in
  let application = Apply (bound, bound) in let inner = Let (bound, application) in let source = Let (lam, inner) in
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
  let two = S one in add_def one one; present_def two z;
  mono_wf_def two parameter; mono_wf_def two identity_type;
  scheme_wf_def one identity; context_wf_def one outer_env;
  context_wf_def z outer_env;
  weaken_context_def one outer_env; weaken_scheme_def one identity;
  shift_def one one identity_type; shift_def one one parameter; shift_index_def one one z;
  length_def alias_args; arguments_wf_def one alias_args;
  lookup_def outer_env z; open_scheme_def identity alias_args;
  open_type_def alias_args identity_type; open_type_def alias_args parameter; open_index_def alias_args z;
  typed_def one outer_env bound identity_type alias;
  typed_def z outer_env inner aa inner_d;
  typed_def z empty source aa d; d)


let (mixed_typing @ total) : (b : Copy_spec.ty) @ immutable ->
    {d : typing | typed Z Empty_context
      (Lambda (Let (Lambda (Bound (S Z)), Apply (Bound Z, Truth))))
      (embed (Copy_spec.Function (b, b))) d} @ immutable ghost = fun b -> ghost_ (
    let z = Z in let one = S z in let a = embed b in let parameter = Parameter z in let boolean = Boolean in
    let outer_sigma = Forall (z, a) in let arg_sigma = Forall (z, parameter) in
    let function_type = Function (parameter, a) in let scheme = Forall (one, function_type) in
    let empty = Empty_context in let outer_env = Binding (outer_sigma, empty) in
    let rhs_env = Binding (arg_sigma, outer_env) in let body_env = Binding (scheme, outer_env) in
    let no = No_arguments in let variable = Variable no in let rhs_d = Abstraction (parameter, variable) in
    let bool_args = Argument (boolean, no) in let function_d = Variable bool_args in
    let constant = Constant in let body_d = Application (boolean, function_d, constant) in
    let let_d = Let_binding (scheme, rhs_d, body_d) in let d = Abstraction (a, let_d) in
    let outer_var = Bound one in let rhs = Lambda outer_var in let function_var = Bound z in let truth = Truth in
    let body = Apply (function_var, truth) in let inside = Let (rhs, body) in let e = Lambda inside in
    let result_type = Copy_spec.Function (b, b) in embed_def result_type; let target = embed result_type in
    add_def z z; add_def z one; add_def one z;
    embed_wf z b; embed_wf one b; embed_wf z result_type;
    mono_wf_def one parameter; present_def one z;
    mono_wf_def one function_type; mono_wf_def z boolean; mono_wf_def one boolean;
    scheme_wf_def z outer_sigma; scheme_wf_def one outer_sigma; scheme_wf_def one arg_sigma; scheme_wf_def z scheme;
    context_wf_def z empty; context_wf_def one empty;
    context_wf_def z outer_env; context_wf_def one outer_env;
    context_wf_def one rhs_env; context_wf_def z body_env;
    weaken_context_def one outer_env; weaken_context_def one empty; weaken_scheme_def one outer_sigma;
    Hm_substitution_proofs.shift_embed z one b;
    length_def no; length_def bool_args; arity_def outer_sigma; arity_def scheme;
    arguments_wf_def one no; arguments_wf_def z no; arguments_wf_def z bool_args;
    lookup_def rhs_env one; lookup_def outer_env z; lookup_def body_env z;
    open_scheme_def outer_sigma no; open_empty a;
    typed_def one rhs_env outer_var a variable;
    typed_def one outer_env rhs function_type rhs_d;
    open_scheme_def scheme bool_args; open_type_def bool_args function_type;
    open_type_def bool_args parameter; open_index_def bool_args z;
    Hm_substitution_proofs.open_embed bool_args b;
    let function_result = Function (boolean, a) in mono_wf_def z function_result;
    typed_def z body_env function_var function_result function_d;
    typed_def z body_env truth boolean constant;
    typed_def z body_env body a body_d;
    typed_def z outer_env inside a let_d;
    typed_def z empty e target d; d)
