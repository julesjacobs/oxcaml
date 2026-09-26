open Hm_declarative
open Hm_type_proofs

let[@def] rec (substitute_type @ total)
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (t : mono @ immutable) = match t with
  | Parameter _ | Boolean | Word64 -> t | Free p -> embed (rho p)
  | List_type a -> List_type (substitute_type rho a)
  | Function (a, b) -> Function (substitute_type rho a, substitute_type rho b)
let[@def] (substitute_scheme @ total)
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (s : scheme @ immutable) = match s with Forall (k, t) -> Forall (k, substitute_type rho t)
let[@def] rec (substitute_arguments @ total)
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (args : arguments @ immutable) = match args with
  | No_arguments -> No_arguments
  | Argument (t, rest) -> Argument (substitute_type rho t, substitute_arguments rho rest)
let[@def] rec (substitute_context @ total)
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (g : context @ immutable) = match g with
  | Empty_context -> Empty_context
  | Binding (s, rest) -> Binding (substitute_scheme rho s, substitute_context rho rest)
let[@def] rec (substitute_typing @ total)
    (rho : (Copy_spec.node Pref.t @ immutable total -> Copy_spec.ty @ immutable total) @ total)
    (d : typing @ immutable) = match d with
  | Variable args -> Variable (substitute_arguments rho args)
  | Constant -> Constant
  | Word_constant -> Word_constant
  | Empty_list a -> Empty_list (substitute_type rho a)
  | List_cons (a, h, t) -> List_cons (substitute_type rho a, substitute_typing rho h, substitute_typing rho t)
  | List_case (a, s, l, r) -> List_case (substitute_type rho a, substitute_typing rho s, substitute_typing rho l, substitute_typing rho r)
  | Conditional (c, a, b) -> Conditional (substitute_typing rho c, substitute_typing rho a, substitute_typing rho b)
  | Word_primitive (a, b) -> Word_primitive (substitute_typing rho a, substitute_typing rho b)
  | Abstraction (a, body) -> Abstraction (substitute_type rho a, substitute_typing rho body)
  | Application (a, left, right) -> Application (substitute_type rho a,
      substitute_typing rho left, substitute_typing rho right)
  | Recursion (a, b, body) -> Recursion (substitute_type rho a,
      substitute_type rho b, substitute_typing rho body)
  | Let_binding (s, rhs, body) -> Let_binding (substitute_scheme rho s,
      substitute_typing rho rhs, substitute_typing rho body)
