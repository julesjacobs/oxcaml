open Hm_declarative

type substitution = {front : arguments; tail : index}

let[@def] rec (at @ total) (front : arguments @ immutable)
    (tail : index @ immutable) (i : index @ immutable) =
  match front with
  | No_arguments -> Parameter (add tail i)
  | Argument (t, rest) -> match i with Z -> t | S i -> at rest tail i

let[@def] rec (act @ total) (s : substitution @ immutable)
    (t : mono @ immutable) = match t with
  | Parameter i -> at s.front s.tail i
  | Free _ | Boolean -> t
  | Function (a, b) -> Function (act s a, act s b)

let[@def] rec (shift_arguments @ total) (k : index @ immutable)
    (a : arguments @ immutable) = match a with
  | No_arguments -> No_arguments
  | Argument (t, rest) -> Argument (shift Z k t, shift_arguments k rest)

let[@def] (bump @ total) (s : substitution @ immutable) =
  {front = Argument (Parameter Z, shift_arguments (S Z) s.front);
   tail = S s.tail}

let[@def] rec (lift @ total) (k : index @ immutable)
    (s : substitution @ immutable) = match k with
  | Z -> s | S k -> bump (lift k s)

let[@def] (act_scheme @ total) (s : substitution @ immutable)
    (scheme : scheme @ immutable) = match scheme with
  | Forall (k, t) -> Forall (k, act (lift k s) t)

let[@def] rec (act_context @ total) (s : substitution @ immutable)
    (g : context @ immutable) = match g with
  | Empty_context -> Empty_context
  | Binding (scheme, rest) -> Binding (act_scheme s scheme, act_context s rest)

let[@def] rec (act_arguments @ total) (s : substitution @ immutable)
    (args : arguments @ immutable) = match args with
  | No_arguments -> No_arguments
  | Argument (t, rest) -> Argument (act s t, act_arguments s rest)

let[@def] rec (act_typing @ total) (s : substitution @ immutable)
    (d : typing @ immutable) = match d with
  | Variable args -> Variable (act_arguments s args)
  | Constant -> Constant
  | Abstraction (a, d) -> Abstraction (act s a, act_typing s d)
  | Application (a, f, x) -> Application (act s a, act_typing s f, act_typing s x)
  | Recursion (a, b, d) -> Recursion (act s a, act s b, act_typing s d)
  | Let_binding (scheme, rhs, body) -> match scheme with Forall (k, _) ->
    Let_binding (act_scheme s scheme, act_typing (lift k s) rhs, act_typing s body)
