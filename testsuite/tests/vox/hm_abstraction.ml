open Hm_declarative

type names = No_names | Name of Copy_spec.node Pref.t * names [@@inductive]
let[@def] rec (count @ total) (ps : names @ immutable) =
  match ps with No_names -> Z | Name (_, rest) -> S (count rest)
let[@def] rec (position @ total) (ps : names @ immutable) (p : Copy_spec.node Pref.t @ immutable) =
  ghost_ (match ps with No_names -> None | Name (q, rest) ->
    if p === q then Some Z else match position rest p with None -> None | Some i -> Some (S i))
let[@def] (abstract_free @ total) (ps : names @ immutable) (cut : index @ immutable)
    (p : Copy_spec.node Pref.t @ immutable) = ghost_ (match position ps p with
  | None -> Free p | Some i -> Parameter (add cut i))
let[@def] rec (abstract_type @ total) (ps : names @ immutable) (cut : index @ immutable)
    (t : mono @ immutable) = ghost_ (match t with
  | Parameter i -> Parameter (shift_index cut (count ps) i)
  | Free p -> abstract_free ps cut p | Boolean -> Boolean
  | Function (a, b) -> Function (abstract_type ps cut a, abstract_type ps cut b))
let[@def] (abstract_scheme @ total) (ps : names @ immutable) (cut : index @ immutable)
    (s : scheme @ immutable) = ghost_ (match s with
  | Forall (k, t) -> Forall (k, abstract_type ps (add k cut) t))
let[@def] rec (abstract_arguments @ total) (ps : names @ immutable) (cut : index @ immutable)
    (args : arguments @ immutable) = ghost_ (match args with
  | No_arguments -> No_arguments
  | Argument (t, rest) -> Argument (abstract_type ps cut t, abstract_arguments ps cut rest))
let[@def] rec (abstract_context @ total) (ps : names @ immutable) (cut : index @ immutable)
    (g : context @ immutable) = ghost_ (match g with
  | Empty_context -> Empty_context
  | Binding (s, rest) -> Binding (abstract_scheme ps cut s, abstract_context ps cut rest))
let[@def] rec (abstract_typing @ total) (ps : names @ immutable) (cut : index @ immutable)
    (d : typing @ immutable) = ghost_ (match d with
  | Variable args -> Variable (abstract_arguments ps cut args)
  | Constant -> Constant
  | Abstraction (a, body) -> Abstraction (abstract_type ps cut a, abstract_typing ps cut body)
  | Application (a, left, right) -> Application (abstract_type ps cut a,
      abstract_typing ps cut left, abstract_typing ps cut right)
  | Recursion (a, b, body) -> Recursion (abstract_type ps cut a,
      abstract_type ps cut b, abstract_typing ps cut body)
  | Let_binding (s, rhs, body) -> match s with Forall (k, _) ->
    Let_binding (abstract_scheme ps cut s, abstract_typing ps (add k cut) rhs,
      abstract_typing ps cut body))

let[@def] rec (avoids @ total) (ps : names @ immutable) (t : mono @ immutable) = ghost_ (
  match t with Parameter _ | Boolean -> true | Free p -> position ps p === None
  | Function (a, b) -> avoids ps a && avoids ps b)
let[@def] (scheme_avoids @ total) (ps : names @ immutable) (s : scheme @ immutable) = ghost_ (
  match s with Forall (_, t) -> avoids ps t)
let[@def] rec (context_avoids @ total) (ps : names @ immutable) (g : context @ immutable) = ghost_ (
  match g with Empty_context -> true
  | Binding (s, rest) -> scheme_avoids ps s && context_avoids ps rest)
