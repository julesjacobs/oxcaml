module D = Hm_declarative

type value =
  | True | False
  | Word of Hmc_word64.t
  | Nil | Cons of value * value
  | Closure of D.term * value
  | Recursive_closure of D.term * value
  | Empty | Bind of value * value
  [@@inductive]

(* The operational proof needs the typing rules and scheme arities, but not
   bounds on type-parameter indices. [erase_typing] connects this to D.typed. *)
let[@def] rec (typed @ total) (g : D.context @ immutable)
    (e : D.term @ immutable) (t : D.mono @ immutable)
    (d : D.typing @ immutable) = ghost_ (
  match d with
  | D.Variable args -> (match e with D.Bound i -> (match D.lookup g i with
    | None -> false | Some s -> D.length args === D.arity s
      && t === D.open_scheme s args) | _ -> false)
  | D.Constant -> (e === D.Truth || e === D.False) && t === D.Boolean
  | D.Word_constant -> (match e with D.Word _ -> t === D.Word64 | _ -> false)
  | D.Empty_list a -> e === D.Nil && t === D.List_type a
  | D.List_cons (a, head, tail) -> (match e with
    | D.Cons (h, r) -> t === D.List_type a && typed g h a head && typed g r t tail
    | _ -> false)
  | D.List_case (a, scrutinee, empty, nonempty) -> (match e with
    | D.CaseList (s, l, r) -> typed g s (D.List_type a) scrutinee
      && typed g l t empty
      && typed (D.Binding (D.Forall (D.Z, a),
        D.Binding (D.Forall (D.Z, D.List_type a), g))) r t nonempty
    | _ -> false)
  | D.Conditional (condition, yes, no) -> (match e with
    | D.If (c, a, b) -> typed g c D.Boolean condition && typed g a t yes && typed g b t no
    | _ -> false)
  | D.Word_primitive (left, right) -> (match e with
    | D.Primitive (op, a, b) -> t === D.operation_type op
      && typed g a D.Word64 left && typed g b D.Word64 right
    | _ -> false)
  | D.Abstraction (a, body) -> (match e, t with
    | D.Lambda e, D.Function (x, b) -> a === x
      && typed (D.Binding (D.Forall (D.Z, a), g)) e b body
    | _ -> false)
  | D.Application (a, left, right) -> (match e with
    | D.Apply (f, x) -> typed g f (D.Function (a, t)) left && typed g x a right
    | _ -> false)
  | D.Recursion (a, b, body) -> (match e with
    | D.Recursive e -> t === D.Function (a, b)
      && typed
        (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, t), g)))
        e b body
    | _ -> false)
  | D.Let_binding (s, rhs, body) -> (match e, s with
    | D.Let (r, b), D.Forall (k, a) ->
      typed (D.weaken_context k g) r a rhs
      && typed (D.Binding (s, g)) b t body
    | _ -> false))

type judgement = Value of value * D.mono | Environment of value * D.context

type evidence =
  | Leaf
  | Capture of D.context * D.typing * evidence
  | Extend of evidence * evidence
  | Elements of evidence * evidence
  [@@inductive]

(* Recursive closures store no self certificate; application constructs it. *)
let[@def] rec (valid @ total) (p : evidence @ immutable)
    (j : judgement @ immutable) = ghost_ (
  match p, j with
  | Leaf, Value ((True | False), D.Boolean) -> true
  | Leaf, Value (Word _, D.Word64) -> true
  | Leaf, Value (Nil, D.List_type _) -> true
  | Elements (head, tail), Value (Cons (h, r), D.List_type a) ->
    valid head (Value (h, a)) && valid tail (Value (r, D.List_type a))
  | Leaf, Environment (Empty, D.Empty_context) -> true
  | Capture (g, d, captured_proof), Value (Closure (body, env), D.Function (a, b)) ->
    valid captured_proof (Environment (env, g))
    && typed (D.Binding (D.Forall (D.Z, a), g)) body b d
  | Capture (g, d, captured_proof),
      Value (Recursive_closure (body, env), D.Function (a, b)) ->
    valid captured_proof (Environment (env, g))
    && typed (D.Binding (D.Forall (D.Z, a),
      D.Binding (D.Forall (D.Z, D.Function (a, b)), g))) body b d
  | Extend (head, rest), Environment (Bind (v, env), D.Binding (D.Forall (_, a), g)) ->
    valid head (Value (v, a)) && valid rest (Environment (env, g))
  | _ -> false)
