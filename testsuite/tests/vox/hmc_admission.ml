module D = Hm_declarative
module G = Hmc_grounding

type error = Unsupported_polymorphic_local_let | Non_callable_outer_binding
  | Non_callable_entry | Invalid_annotation [@@inductive]

let[@def] (callable @ total) (term : D.term @ immutable) = match term with
  | D.Lambda _ | D.Recursive _ -> true | _ -> false

let[@def] rec (local @ total) (term : D.term @ immutable) (d : D.typing @ immutable) =
  match term, d with
  | D.Bound _, D.Variable _ | (D.Truth | D.False), D.Constant
  | D.Word _, D.Word_constant | D.Nil, D.Empty_list _ -> true
  | D.Lambda body, D.Abstraction (_, db)
  | D.Recursive body, D.Recursion (_, _, db) -> local body db
  | D.Apply (a, b), D.Application (_, da, db)
  | D.Cons (a, b), D.List_cons (_, da, db)
  | D.Primitive (_, a, b), D.Word_primitive (da, db) -> local a da && local b db
  | D.If (a, b, c), D.Conditional (da, db, dc)
  | D.CaseList (a, b, c), D.List_case (_, da, db, dc) -> local a da && local b db && local c dc
  | D.Let (a, b), D.Let_binding (D.Forall (D.Z, _), da, db) -> local a da && local b db
  | _ -> false

let[@def] rec (outer @ total) (term : D.term @ immutable) (d : D.typing @ immutable) =
  match term, d with
  | D.Let (rhs, rest), D.Let_binding (_, dr, db) -> callable rhs && local rhs dr && outer rest db
  | _ -> callable term && local term d

let (first @ total) : (a : error option) @ immutable -> (b : error option) @ immutable ->
    {r : error option | (r === None) = (a === None && b === None)} @ immutable =
  fun a b -> match a with None -> b | Some _ -> a

let rec (check_local @ total) : (term : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {r : error option | (r === None) = local term d} @ immutable = fun term d ->
  ghost_ (local_def term d);
  match term, d with
  | D.Bound _, D.Variable _ | (D.Truth | D.False), D.Constant
  | D.Word _, D.Word_constant | D.Nil, D.Empty_list _ -> None
  | D.Lambda body, D.Abstraction (_, db)
  | D.Recursive body, D.Recursion (_, _, db) -> check_local body db
  | D.Apply (a, b), D.Application (_, da, db)
  | D.Cons (a, b), D.List_cons (_, da, db)
  | D.Primitive (_, a, b), D.Word_primitive (da, db) -> first (check_local a da) (check_local b db)
  | D.If (a, b, c), D.Conditional (da, db, dc)
  | D.CaseList (a, b, c), D.List_case (_, da, db, dc) ->
    first (check_local a da) (first (check_local b db) (check_local c dc))
  | D.Let (a, b), D.Let_binding (D.Forall (arity, _), da, db) ->
    (match arity with D.Z -> first (check_local a da) (check_local b db)
    | D.S _ -> Some Unsupported_polymorphic_local_let)
  | _ -> Some Invalid_annotation

let rec (check_outer @ total) : (term : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {r : error option | (r === None) = outer term d} @ immutable = fun term d ->
  ghost_ (outer_def term d);
  match term, d with
  | D.Let (rhs, rest), D.Let_binding (_, dr, db) ->
    if callable rhs then first (check_local rhs dr) (check_outer rest db)
    else Some Non_callable_outer_binding
  | _ -> if callable term then check_local term d else Some Non_callable_entry

type admitted = {p : G.grounded | outer p.G.term p.G.proof}
type result = Rejected of error | Admitted of admitted [@@inductive]

let (admit @ total) : (grounded : G.grounded) @ immutable ->
    {r : result | match r with Rejected _ -> not (outer grounded.G.term grounded.G.proof)
      | Admitted p -> p === grounded} @ immutable = fun grounded ->
  match check_outer grounded.G.term grounded.G.proof with
  | Some error -> Rejected error
  | None -> let admitted : admitted = refine_ grounded in Admitted admitted
