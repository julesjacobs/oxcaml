open Hm_declarative

let rec (index_equal @ total) :
    (a : index) @ immutable -> (b : index) @ immutable ->
    {r : bool | r = (a === b)} = fun a b ->
  match a, b with
  | Z, Z -> true
  | S a, S b -> index_equal a b
  | _ -> false

let rec (mono_equal @ total) :
    (a : mono) @ immutable -> (b : mono) @ immutable ->
    {r : bool | r = (a === b)} = fun a b ->
  match a, b with
  | Parameter a, Parameter b -> index_equal a b
  | Free a, Free b -> Pref.equal a b
  | Boolean, Boolean | Word64, Word64 -> true
  | List_type a, List_type b -> mono_equal a b
  | Function (a, b), Function (x, y) ->
    mono_equal a x && mono_equal b y
  | _ -> false

let rec (check @ total) :
    (n : index) @ immutable -> (g : context) @ immutable ->
    (e : term) @ immutable -> (t : mono) @ immutable ->
    (d : typing) @ immutable ->
    {r : bool | r = typed n g e t d} = fun n g e t d ->
  ghost_ (typed_def n g e t d);
  context_wf n g && mono_wf n t &&
  match d with
  | Variable args ->
    (match e with
     | Bound i ->
       (match lookup g i with
        | None -> false
        | Some s -> index_equal (length args) (arity s)
          && arguments_wf n args && mono_equal t (open_scheme s args))
     | _ -> false)
  | Constant ->
    (match e, t with (Truth | False), Boolean -> true | _ -> false)
  | Word_constant -> (match e, t with Word _, Word64 -> true | _ -> false)
  | Empty_list a -> (match e with Nil -> mono_equal t (List_type a) && mono_wf n a | _ -> false)
  | List_cons (a, head, tail) -> (match e with
    | Cons (h, r) -> mono_equal t (List_type a) && check n g h a head && check n g r t tail
    | _ -> false)
  | List_case (a, scrutinee, empty, nonempty) -> (match e with
    | CaseList (s, l, r) -> mono_wf n a && check n g s (List_type a) scrutinee
      && check n g l t empty
      && check n (Binding (Forall (Z, a), Binding (Forall (Z, List_type a), g))) r t nonempty
    | _ -> false)
  | Conditional (condition, yes, no) -> (match e with
    | If (c, a, b) -> check n g c Boolean condition && check n g a t yes && check n g b t no
    | _ -> false)
  | Word_primitive (left, right) -> (match e with
    | Primitive (op, a, b) -> mono_equal t (operation_type op)
      && check n g a Word64 left && check n g b Word64 right
    | _ -> false)
  | Abstraction (a, body) ->
    (match e, t with
     | Lambda e, Function (x, b) -> mono_equal a x
       && check n (Binding (Forall (Z, a), g)) e b body
     | _ -> false)
  | Application (a, left, right) ->
    (match e with
     | Apply (f, x) -> check n g f (Function (a, t)) left
       && check n g x a right
     | _ -> false)
  | Recursion (a, b, body) ->
    (match e with
     | Recursive e -> mono_equal t (Function (a, b))
       && check n (Binding (Forall (Z, a), Binding (Forall (Z, t), g)))
            e b body
     | _ -> false)
  | Let_binding (s, rhs, body) ->
    (match e, s with
     | Let (r, b), Forall (k, a) -> scheme_wf n s
       && check (add k n) (weaken_context k g) r a rhs
       && check n (Binding (s, g)) b t body
     | _ -> false)
