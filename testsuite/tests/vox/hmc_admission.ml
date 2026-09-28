(* Admission: the fragment the rest of the compiler accepts, checked on the
   grounded derivation.

   [outer term d] holds when [term] is a sequence of top-level [let]s whose
   right-hand sides are functions ([callable]) and that ends in a function,
   and every [let] inside them is monomorphic in [d] ([local]: its scheme
   has no parameters). [admit] decides it, or returns an error.

   An error is found on the derivation, but the interface states it about
   the term alone: [meaning] is the fact about the term that each error
   implies. [Invalid_annotation] means that the derivation does not have
   the shape of the term; [typed_matches] shows that a typing derivation
   always has it, so that error cannot occur. *)
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

(* Source-level meaning of each error. [matches] is [local] without the
   arity condition: the typing has the shape of the term. *)
let[@def] rec (let_free @ total) (term : D.term @ immutable) = match term with
  | D.Bound _ | D.Truth | D.False | D.Word _ | D.Nil -> true
  | D.Lambda body | D.Recursive body -> let_free body
  | D.Apply (a, b) | D.Cons (a, b) | D.Primitive (_, a, b) -> let_free a && let_free b
  | D.If (a, b, c) | D.CaseList (a, b, c) -> let_free a && let_free b && let_free c
  | D.Let _ -> false

let[@def] rec (outer_callable @ total) (term : D.term @ immutable) = match term with
  | D.Let (rhs, rest) -> callable rhs && outer_callable rest | _ -> true
let[@def] rec (entry_callable @ total) (term : D.term @ immutable) = match term with
  | D.Let (_, rest) -> entry_callable rest | _ -> callable term
let[@def] rec (no_local_let @ total) (term : D.term @ immutable) = match term with
  | D.Let (rhs, rest) -> let_free rhs && no_local_let rest | _ -> let_free term

let[@def] (meaning @ total) (term : D.term @ immutable) (error : error @ immutable) =
  match error with
  | Unsupported_polymorphic_local_let -> not (no_local_let term)
  | Non_callable_outer_binding -> not (outer_callable term)
  | Non_callable_entry -> not (entry_callable term)
  | Invalid_annotation -> false

let[@def] rec (matches @ total) (term : D.term @ immutable) (d : D.typing @ immutable) =
  match term, d with
  | D.Bound _, D.Variable _ | (D.Truth | D.False), D.Constant
  | D.Word _, D.Word_constant | D.Nil, D.Empty_list _ -> true
  | D.Lambda body, D.Abstraction (_, db)
  | D.Recursive body, D.Recursion (_, _, db) -> matches body db
  | D.Apply (a, b), D.Application (_, da, db)
  | D.Cons (a, b), D.List_cons (_, da, db)
  | D.Primitive (_, a, b), D.Word_primitive (da, db)
  | D.Let (a, b), D.Let_binding (_, da, db) -> matches a da && matches b db
  | D.If (a, b, c), D.Conditional (da, db, dc)
  | D.CaseList (a, b, c), D.List_case (_, da, db, dc) -> matches a da && matches b db && matches c dc
  | _ -> false

let rec (typed_matches @ total) : (n : D.index) @ immutable -> (g : D.context) @ immutable ->
    (term : D.term) @ immutable -> (t : D.mono) @ immutable -> (d : D.typing) @ immutable ->
    {u : unit | D.typed n g term t d} -> {u : unit | matches term d} @ ghost =
  fun n g term t d premise -> ghost_ (
    D.typed_def n g term t d; matches_def term d;
    match term, d with
    | D.Lambda body, D.Abstraction (a, db) -> (match t with
      | D.Function (_, b) -> typed_matches n (D.Binding (D.Forall (D.Z, a), g)) body b db ()
      | _ -> ())
    | D.Recursive body, D.Recursion (a, b, db) ->
      typed_matches n (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, t), g))) body b db ()
    | D.Apply (f, x), D.Application (a, df, dx) ->
      typed_matches n g f (D.Function (a, t)) df (); typed_matches n g x a dx ()
    | D.Cons (h, r), D.List_cons (a, dh, dr) -> typed_matches n g h a dh (); typed_matches n g r t dr ()
    | D.Primitive (_, a, b), D.Word_primitive (da, db) ->
      typed_matches n g a D.Word64 da (); typed_matches n g b D.Word64 db ()
    | D.If (c, a, b), D.Conditional (dc, da, db) ->
      typed_matches n g c D.Boolean dc (); typed_matches n g a t da (); typed_matches n g b t db ()
    | D.CaseList (s, l, r), D.List_case (a, ds, dl, dr) ->
      typed_matches n g s (D.List_type a) ds (); typed_matches n g l t dl ();
      typed_matches n (D.Binding (D.Forall (D.Z, a), D.Binding (D.Forall (D.Z, D.List_type a), g))) r t dr ()
    | D.Let (r, b), D.Let_binding (s, dr, db) -> (match s with D.Forall (k, a) ->
      typed_matches (D.add k n) (D.weaken_context k g) r a dr ();
      typed_matches n (D.Binding (s, g)) b t db ())
    | _ -> ())

let (first @ total) : (a : error option) @ immutable -> (b : error option) @ immutable ->
    {r : error option | (r === None) = (a === None && b === None) && (r === a || r === b)} @ immutable =
  fun a b -> match a with None -> b | Some _ -> a

let rec (check_local @ total) : (term : D.term) @ immutable -> (d : D.typing) @ immutable ->
    {r : error option | (r === None) = local term d && match r with
      | None -> true
      | Some Invalid_annotation -> not (matches term d)
      | Some Unsupported_polymorphic_local_let -> not (let_free term)
      | Some _ -> false} @ immutable = fun term d ->
  ghost_ (local_def term d; matches_def term d; let_free_def term);
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
    {r : error option | (r === None) = outer term d && match r with
      | None -> true
      | Some Invalid_annotation -> not (matches term d)
      | Some error -> not (matches term d) || meaning term error} @ immutable = fun term d ->
  ghost_ (outer_def term d; matches_def term d; outer_callable_def term; entry_callable_def term;
    no_local_let_def term; meaning_def term Unsupported_polymorphic_local_let;
    meaning_def term Non_callable_outer_binding; meaning_def term Non_callable_entry;
    match term with D.Let (rhs, rest) -> callable_def rhs; let_free_def rhs | _ -> callable_def term);
  match term, d with
  | D.Let (rhs, rest), D.Let_binding (_, dr, db) ->
    ghost_ (meaning_def rest Unsupported_polymorphic_local_let;
      meaning_def rest Non_callable_outer_binding; meaning_def rest Non_callable_entry);
    if callable rhs then first (check_local rhs dr) (check_outer rest db)
    else Some Non_callable_outer_binding
  | _ -> if callable term then check_local term d else Some Non_callable_entry

type admitted = {p : G.grounded | outer p.G.term p.G.proof}
type result = Rejected of error | Admitted of admitted [@@inductive]

let (admit @ total) : (grounded : G.grounded) @ immutable ->
    {r : result | match r with
      | Rejected error -> not (outer grounded.G.term grounded.G.proof) && meaning grounded.G.term error
      | Admitted p -> p === grounded} @ immutable = fun grounded ->
  match check_outer grounded.G.term grounded.G.proof with
  | Some error ->
    ghost_ (typed_matches D.Z D.Empty_context grounded.G.term (D.Function (D.Word64, D.Word64))
      grounded.G.proof (); meaning_def grounded.G.term error);
    Rejected error
  | None -> let admitted : admitted = refine_ grounded in Admitted admitted
