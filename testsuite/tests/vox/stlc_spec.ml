open Unifier_spec

type index = Z | S of index [@@inductive]
type term = Bound of index | Boolean | Lambda of term | Recursive of term
  | Apply of term * term [@@inductive]
type env = Empty | Bind of node Pref.t * env [@@inductive]
type context = No_types | Type of ty * context [@@inductive]

let[@def] rec (depth @ total) (env : env @ immutable) = match env with
  | Empty -> Z | Bind (_, rest) -> S (depth rest)
let[@def] rec (present @ total) (n : index @ immutable) (i : index @ immutable) =
  match n with Z -> false | S n -> match i with Z -> true | S i -> present n i
let[@def] rec (scoped_term @ total) (n : index @ immutable) (e : term @ immutable) =
  match e with
  | Bound i -> present n i
  | Boolean -> true
  | Lambda body -> scoped_term (S n) body
  | Recursive body -> scoped_term (S (S n)) body
  | Apply (f, a) -> scoped_term n f && scoped_term n a
let[@def] rec (lookup @ total) (env : env @ immutable) (i : index @ immutable) =
  match env with Empty -> None | Bind (p, rest) -> match i with
  | Z -> Some p | S i -> lookup rest i
let[@def] rec (lookup_type @ total) (ctx : context @ immutable) (i : index @ immutable) =
  match ctx with No_types -> None | Type (t, rest) -> match i with
  | Z -> Some t | S i -> lookup_type rest i
let[@def] rec (context_of @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (env : env @ immutable) = match env with
  | Empty -> No_types | Bind (p, rest) -> Type (rho p, context_of rho rest)
let[@def] rec (env_allocated @ total) (h : node Pref.heap @ immutable) (env : env @ immutable) =
  ghost_ (match env with Empty -> true | Bind (p, rest) -> H.mem h p && env_allocated h rest)

type typing = Variable | Constant | Abstraction of ty * typing
  | Application of ty * typing * typing | Recursion of ty * ty * typing
  [@@inductive]
let[@def] rec (typed @ total) (ctx : context @ immutable) (e : term @ immutable)
    (t : ty @ immutable) (d : typing @ immutable) = ghost_ (match d with
  | Variable -> (match e with Bound i -> lookup_type ctx i === Some t | _ -> false)
  | Constant -> e === Boolean && t === TBool
  | Abstraction (a, body) -> (match e, t with
    | Lambda e, TArrow (x, b) -> a === x && typed (Type (a, ctx)) e b body
    | _ -> false)
  | Recursion (a, b, body) -> (match e with
    | Recursive e -> t === TArrow (a, b) && typed (Type (a, Type (t, ctx))) e b body
    | _ -> false)
  | Application (a, left, right) -> (match e with
    | Apply (f, x) -> typed ctx f (TArrow (a, t)) left && typed ctx x a right
    | _ -> false))

type equations = Nothing | Equal of node Pref.t * node Pref.t | And of equations * equations
  [@@inductive]
let[@def] rec (satisfies @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (eqs : equations @ immutable) = ghost_ (match eqs with
  | Nothing -> true | Equal (p, q) -> rho p === rho q
  | And (a, b) -> satisfies rho a && satisfies rho b)
let[@def] rec (equations_allocated @ total) (h : node Pref.heap @ immutable)
    (eqs : equations @ immutable) = ghost_ (match eqs with
  | Nothing -> true | Equal (p, q) -> H.mem h p && H.mem h q
  | And (a, b) -> equations_allocated h a && equations_allocated h b)

type graph =
  | GVar of index * node Pref.t
  | GBool of node Pref.t
  | GLam of node Pref.t * graph * node Pref.t * node Pref.heap
  | GRec of node Pref.t * node Pref.t * node Pref.t * graph
  | GApp of graph * graph * node Pref.t * node Pref.t * node Pref.heap * node Pref.heap
  [@@inductive]
let[@def] (root @ total) (g : graph @ immutable) = match g with
  | GVar (_, p) | GBool p | GLam (_, _, p, _)
  | GApp (_, _, p, _, _, _) | GRec (_, _, p, _) -> p
let[@def] rec (source @ total) (g : graph @ immutable) = match g with
  | GVar (i, _) -> Bound i | GBool _ -> Boolean
  | GLam (_, body, _, _) -> Lambda (source body)
  | GRec (_, _, _, body) -> Recursive (source body)
  | GApp (f, a, _, _, _, _) -> Apply (source f, source a)
let[@def] rec (constraints @ total) (g : graph @ immutable) = match g with
  | GVar _ | GBool _ -> Nothing
  | GLam (_, body, _, _) -> constraints body
  | GRec (_, result, _, body) -> And (constraints body, Equal (root body, result))
  | GApp (f, a, _, arrow, _, _) -> And (And (constraints f, constraints a), Equal (root f, arrow))
let[@def] rec (built @ total) (h : node Pref.heap @ immutable) (env : env @ immutable)
    (g : graph @ immutable) (after : node Pref.heap @ immutable) = ghost_ (match g with
  | GVar (i, p) -> lookup env i === Some p && H.mem h p && after === h
  | GBool p -> not (H.mem h p) && after === H.put h p Bool
  | GLam (arg, body, p, middle) -> not (H.mem h arg)
    && built (H.put h arg Var) (Bind (arg, env)) body middle
    && not (H.mem middle p) && after === H.put middle p (Arrow (arg, root body))
  | GRec (arg, result, p, body) -> not (H.mem h arg)
    && not (H.mem (H.put h arg Var) result)
    && not (H.mem (H.put (H.put h arg Var) result Var) p)
    && built (H.put (H.put (H.put h arg Var) result Var) p (Arrow (arg, result)))
      (Bind (arg, Bind (p, env))) body after
  | GApp (f, a, p, arrow, h1, h2) -> built h env f h1 && built h1 env a h2
    && not (H.mem h2 p) && not (H.mem (H.put h2 p Var) arrow)
    && after === H.put (H.put h2 p Var) arrow (Arrow (root a, p)))

type generated = #{ value : node Pref.t @@ aliased; equations : equations @@ aliased; state : node Pref.token; graph : graph @@ ghost }
type solving = Done | Unified of derivation | Sequence of node Pref.heap * bool * solving * solving [@@inductive]
let[@def] rec (solved @ total) (h : node Pref.heap @ immutable) (eqs : equations @ immutable)
    (ok : bool) (after : node Pref.heap @ immutable) (d : solving @ immutable) = ghost_ (
  match d with
  | Done -> eqs === Nothing && ok && after === h
  | Unified d -> (match eqs with Equal (p, q) -> unified h p q ok after d | _ -> false)
  | Sequence (middle, left_ok, left, right) -> (match eqs with
    | And (a, b) -> solved h a left_ok middle left
      && (if left_ok then solved middle b ok after right else not ok && after === middle)
    | _ -> false))
type solved_result = #{ ok : bool; state : node Pref.token; solving : solving @@ ghost }

let[@def] (describes @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (v : node @ immutable) (t : ty @ immutable) = ghost_ (match v with
  | Var -> true | Bool -> t === TBool | Link p -> t === rho p
  | Arrow (a, b) -> t === TArrow (rho a, rho b))
let[@def] rec (derive @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (g : graph @ immutable) = match g with
  | GVar _ -> Variable | GBool _ -> Constant
  | GLam (arg, body, _, _) -> Abstraction (rho arg, derive rho body)
  | GRec (arg, result, _, body) -> Recursion (rho arg, rho result, derive rho body)
  | GApp (f, a, _, _, _, _) -> Application (rho (root a), derive rho f, derive rho a)

let[@def] (inferred @ total) (e : term @ immutable) (middle : node Pref.heap @ immutable)
    (g : graph @ immutable) (ok : bool) (after : node Pref.heap @ immutable) (d : solving @ immutable) = ghost_ (
  source g === e && built (H.empty ()) Empty g middle && solved middle (constraints g) ok after d)
type inference = #{ value : node Pref.t @@ aliased; ok : bool; state : node Pref.token;
  graph : graph @@ ghost; generated_heap : node Pref.heap @@ ghost; solving : solving @@ ghost;
  tree : Unifier_finite_spec.tree @@ ghost }
