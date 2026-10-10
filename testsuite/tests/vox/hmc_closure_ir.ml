module D = Hm_declarative
module C = Hmc_monomorphic
module M = Hmc_manifest
module H = Hmc_monomorphic_typing
module I = Hmc_instance
module G = Hmc_ground_type
module A = Hmc_ground_annotations

type term = Local of D.index | Global of D.index | Closure of D.index
  | Truth | False | Word of Hmc_word64.t | Nil
  | Apply of term * term | Cons of term * term
  | Primitive of D.word_operation * term * term
  | If of term * term * term | CaseList of term * term * term
  | Let of term * term [@@inductive]
type entry = {recursive : bool; captured : D.context; argument : D.mono; result : D.mono;
  source : C.term; derivation : D.typing; body : term}
type table = Empty | Add of entry * table [@@inductive]
let[@def] rec (size @ total) (table : table @ immutable) = match table with
  | Empty -> D.Z | Add (_, rest) -> D.S (size rest)
let[@def] rec (lookup @ total) (table : table @ immutable) (id : D.index @ immutable) = match table with
  | Empty -> None | Add (entry, rest) ->
    if Hm_elaboration_check.index_equal id (size rest) then Some entry else lookup rest id
let[@def] rec (extends @ total) (larger : table @ immutable) (smaller : table @ immutable) =
  ghost_ (larger === smaller || match larger with Empty -> false | Add (_, rest) -> extends rest smaller)
let[@def] (context @ total) (entry : entry @ immutable) =
  let captured = if entry.recursive then D.Binding (D.Forall (D.Z, D.Function (entry.argument, entry.result)), entry.captured)
    else entry.captured in
  D.Binding (D.Forall (D.Z, entry.argument), captured)
let[@def] rec (ground_context @ total) (g : D.context @ immutable) = match g with
  | D.Empty_context -> true
  | D.Binding (D.Forall (D.Z, ty), rest) -> G.ground ty && ground_context rest
  | _ -> false
let[@def] rec (related @ total) (table : table @ immutable) (source : C.term @ immutable) (code : term @ immutable) =
  ghost_ (match source, code with
  | C.Local i, Local j -> i === j
  | C.Global (_, _, i), Global j -> i === j
  | C.Truth, Truth | C.False, False | C.Nil, Nil -> true
  | C.Word a, Word b -> a === b
  | C.Lambda source, Closure id -> (match lookup table id with
    | None -> false | Some entry -> not entry.recursive && entry.source === source)
  | C.Recursive source, Closure id -> (match lookup table id with
    | None -> false | Some entry -> entry.recursive && entry.source === source)
  | C.Apply (a, b), Apply (x, y) | C.Cons (a, b), Cons (x, y) | C.Let (a, b), Let (x, y) ->
    related table a x && related table b y
  | C.Primitive (op, a, b), Primitive (other, x, y) -> op === other && related table a x && related table b y
  | C.If (a, b, c), If (x, y, z) | C.CaseList (a, b, c), CaseList (x, y, z) ->
    related table a x && related table b y && related table c z
  | _ -> false)

let[@def] rec (typed @ total) (globals : M.table @ immutable) (table : table @ immutable) (g : D.context @ immutable)
    (e : term @ immutable) (t : D.mono @ immutable) (d : D.typing @ immutable) = ghost_ (
  match d with
  | D.Variable args -> (match e with Local i -> (match D.lookup g i with
    | None -> false | Some s -> D.length args === D.arity s
      && t === D.open_scheme s args)
    | Global id -> (match M.lookup globals id with
      | None -> false | Some entry -> t === G.mono entry.M.body.Hmc_specialized_body.origin.I.ty)
    | _ -> false)
  | D.Constant -> (e === Truth || e === False) && t === D.Boolean
  | D.Word_constant -> (match e with Word _ -> t === D.Word64 | _ -> false)
  | D.Empty_list a -> e === Nil && t === D.List_type a
  | D.List_cons (a, head, tail) -> (match e with
    | Cons (h, r) -> t === D.List_type a && typed globals table g h a head && typed globals table g r t tail
    | _ -> false)
  | D.List_case (a, scrutinee, empty, nonempty) -> (match e with
    | CaseList (s, l, r) -> typed globals table g s (D.List_type a) scrutinee
      && typed globals table g l t empty
      && typed globals table (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, D.List_type a), g))) r t nonempty
    | _ -> false)
  | D.Conditional (condition, yes, no) -> (match e with
    | If (c, a, b) -> typed globals table g c D.Boolean condition && typed globals table g a t yes && typed globals table g b t no
    | _ -> false)
  | D.Word_primitive (left, right) -> (match e with
    | Primitive (op, a, b) -> t === D.operation_type op && typed globals table g a D.Word64 left && typed globals table g b D.Word64 right
    | _ -> false)
  | D.Abstraction (a, _) -> (match e with Closure id -> (match lookup table id with
    | None -> false | Some entry -> not entry.recursive && entry.captured === g
      && entry.argument === a && t === D.Function (entry.argument, entry.result)) | _ -> false)
  | D.Application (a, left, right) -> (match e with
    | Apply (f, x) -> typed globals table g f (D.Function (a, t)) left && typed globals table g x a right
    | _ -> false)
  | D.Recursion (a, b, _) -> (match e with Closure id -> (match lookup table id with
    | None -> false | Some entry -> entry.recursive && entry.captured === g
      && entry.argument === a && entry.result === b && t === D.Function (a, b)) | _ -> false)
  | D.Let_binding (s, rhs, body) -> (match e, s with
    | Let (r, b), D.Forall (k, a) -> k === D.Z
      && typed globals table g r a rhs
      && typed globals table (D.Binding (s, g)) b t body
    | _ -> false))

let[@def] rec (valid @ total) (globals : M.table @ immutable) (table : table @ immutable) = ghost_ (match table with
  | Empty -> true
  | Add (entry, rest) -> valid globals rest && ground_context entry.captured
    && G.ground entry.argument && G.ground entry.result && A.typing entry.derivation
    && H.typed globals (context entry) entry.source entry.result entry.derivation
    && typed globals rest (context entry) entry.body entry.result entry.derivation
    && related rest entry.source entry.body)
