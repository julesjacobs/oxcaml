type index = Z | S of index [@@inductive]
type mono = Parameter of index | Free of Copy_spec.node Pref.t
  | Boolean | Word64 | List_type of mono | Function of mono * mono [@@inductive]
type scheme = Forall of index * mono [@@inductive]
type arguments = No_arguments | Argument of mono * arguments [@@inductive]
type context = Empty_context | Binding of scheme * context [@@inductive]
type word_operation = Add | Subtract | Equal_word | Unsigned_less [@@inductive]
type term = Bound of index | Truth | False | Word of Hmc_word64.t
  | Nil | Cons of term * term | CaseList of term * term * term
  | If of term * term * term | Primitive of word_operation * term * term
  | Lambda of term | Recursive of term
  | Apply of term * term | Let of term * term [@@inductive]
type typing = Variable of arguments | Constant | Abstraction of mono * typing
  | Application of mono * typing * typing | Recursion of mono * mono * typing
  | Let_binding of scheme * typing * typing
  | Word_constant | Empty_list of mono | List_cons of mono * typing * typing
  | List_case of mono * typing * typing * typing
  | Conditional of typing * typing * typing
  | Word_primitive of typing * typing [@@inductive]

let[@def] rec (embed @ total) (t : Copy_spec.ty @ immutable) = match t with
  | Copy_spec.Variable p -> Free p | Copy_spec.Boolean -> Boolean
  | Copy_spec.Word64 -> Word64 | Copy_spec.List_type a -> List_type (embed a)
  | Copy_spec.Function (a, b) -> Function (embed a, embed b)

let[@def] rec (add @ total) (a : index @ immutable) (b : index @ immutable) =
  match a with Z -> b | S a -> S (add a b)
let[@def] rec (present @ total) (n : index @ immutable) (i : index @ immutable) =
  match n with Z -> false | S n -> match i with Z -> true | S i -> present n i
let[@def] rec (mono_wf @ total) (n : index @ immutable) (t : mono @ immutable) =
  match t with
  | Parameter i -> present n i | Free _ | Boolean | Word64 -> true
  | List_type a -> mono_wf n a
  | Function (a, b) -> mono_wf n a && mono_wf n b
let[@def] (scheme_wf @ total) (n : index @ immutable) (s : scheme @ immutable) =
  match s with Forall (k, t) -> mono_wf (add k n) t
let[@def] rec (context_wf @ total) (n : index @ immutable) (g : context @ immutable) =
  match g with Empty_context -> true
  | Binding (s, rest) -> scheme_wf n s && context_wf n rest
let[@def] rec (arguments_wf @ total) (n : index @ immutable) (a : arguments @ immutable) =
  match a with No_arguments -> true
  | Argument (t, rest) -> mono_wf n t && arguments_wf n rest
let[@def] rec (length @ total) (a : arguments @ immutable) =
  match a with No_arguments -> Z | Argument (_, rest) -> S (length rest)
let[@def] (arity @ total) (s : scheme @ immutable) = match s with Forall (k, _) -> k
let[@def] rec (lookup @ total) (g : context @ immutable) (i : index @ immutable) =
  match g with Empty_context -> None | Binding (s, rest) ->
  match i with Z -> Some s | S i -> lookup rest i
let[@def] rec (depth @ total) (g : context @ immutable) =
  match g with Empty_context -> Z | Binding (_, rest) -> S (depth rest)
let[@def] rec (scoped_term @ total) (n : index @ immutable) (e : term @ immutable) =
  match e with
  | Bound i -> present n i | Truth | False | Word _ | Nil -> true
  | Cons (a, b) | Primitive (_, a, b) -> scoped_term n a && scoped_term n b
  | If (c, a, b) -> scoped_term n c && scoped_term n a && scoped_term n b
  | CaseList (s, a, b) -> scoped_term n s && scoped_term n a && scoped_term (S (S n)) b
  | Lambda b -> scoped_term (S n) b
  | Recursive b -> scoped_term (S (S n)) b
  | Apply (a, b) -> scoped_term n a && scoped_term n b
  | Let (a, b) -> scoped_term n a && scoped_term (S n) b

let[@def] rec (shift_index @ total) (cut : index @ immutable)
    (k : index @ immutable) (i : index @ immutable) =
  match cut with Z -> add k i | S cut ->
  match i with Z -> Z | S i -> S (shift_index cut k i)
let[@def] rec (shift @ total) (cut : index @ immutable)
    (k : index @ immutable) (t : mono @ immutable) = match t with
  | Parameter i -> Parameter (shift_index cut k i)
  | Free _ | Boolean | Word64 -> t
  | List_type a -> List_type (shift cut k a)
  | Function (a, b) -> Function (shift cut k a, shift cut k b)
let[@def] (weaken_scheme @ total) (k : index @ immutable) (s : scheme @ immutable) =
  match s with Forall (m, t) -> Forall (m, shift m k t)
let[@def] rec (weaken_context @ total) (k : index @ immutable) (g : context @ immutable) =
  match g with Empty_context -> Empty_context
  | Binding (s, rest) -> Binding (weaken_scheme k s, weaken_context k rest)
let[@def] rec (open_index @ total) (a : arguments @ immutable) (i : index @ immutable) =
  match a with No_arguments -> Parameter i | Argument (t, rest) ->
  match i with Z -> t | S i -> open_index rest i
let[@def] rec (open_type @ total) (a : arguments @ immutable) (t : mono @ immutable) =
  match t with Parameter i -> open_index a i | Free _ | Boolean | Word64 -> t
  | List_type t -> List_type (open_type a t)
  | Function (x, y) -> Function (open_type a x, open_type a y)
let[@def] (open_scheme @ total) (s : scheme @ immutable) (a : arguments @ immutable) =
  match s with Forall (_, t) -> open_type a t

let[@def] (operation_type @ total) (op : word_operation @ immutable) =
  match op with Add | Subtract -> Word64 | Equal_word | Unsigned_less -> Boolean

let[@def] rec (typed @ total) (n : index @ immutable) (g : context @ immutable)
    (e : term @ immutable) (t : mono @ immutable) (d : typing @ immutable) = ghost_ (
  context_wf n g && mono_wf n t && match d with
  | Variable args -> (match e with Bound i -> (match lookup g i with
    | None -> false | Some s -> length args === arity s
      && arguments_wf n args && t === open_scheme s args) | _ -> false)
  | Constant -> (e === Truth || e === False) && t === Boolean
  | Word_constant -> (match e with Word _ -> t === Word64 | _ -> false)
  | Empty_list a -> e === Nil && t === List_type a && mono_wf n a
  | List_cons (a, head, tail) -> (match e with
    | Cons (h, r) -> t === List_type a && typed n g h a head && typed n g r t tail
    | _ -> false)
  | List_case (a, scrutinee, empty, nonempty) -> (match e with
    | CaseList (s, l, r) -> mono_wf n a && typed n g s (List_type a) scrutinee
      && typed n g l t empty
      && typed n (Binding (Forall (Z, a), Binding (Forall (Z, List_type a), g))) r t nonempty
    | _ -> false)
  | Conditional (condition, yes, no) -> (match e with
    | If (c, a, b) -> typed n g c Boolean condition && typed n g a t yes && typed n g b t no
    | _ -> false)
  | Word_primitive (left, right) -> (match e with
    | Primitive (op, a, b) -> t === operation_type op && typed n g a Word64 left && typed n g b Word64 right
    | _ -> false)
  | Abstraction (a, body) -> (match e, t with
    | Lambda e, Function (x, b) -> a === x
      && typed n (Binding (Forall (Z, a), g)) e b body
    | _ -> false)
  | Application (a, left, right) -> (match e with
    | Apply (f, x) -> typed n g f (Function (a, t)) left && typed n g x a right
    | _ -> false)
  | Recursion (a, b, body) -> (match e with
    | Recursive e -> t === Function (a, b)
      && typed n (Binding (Forall (Z, a), Binding (Forall (Z, t), g))) e b body
    | _ -> false)
  | Let_binding (s, rhs, body) -> (match e, s with
    | Let (r, b), Forall (k, a) -> scheme_wf n s
      && typed (add k n) (weaken_context k g) r a rhs
      && typed n (Binding (s, g)) b t body
    | _ -> false))
