type elt : immutable_data = Global | Regional | Local

let[@def] (rank @ total) x = match x with
  | Global -> 0
  | Regional -> 1
  | Local -> 2

let[@def] (le @ total) a b = rank a <= rank b

let[@def] (join @ total) a b = if le a b then b else a

let[@def] (meet @ total) a b = if le a b then a else b

let[@def] (regional_to_global @ total) x = match x with
  | Global | Regional -> Global
  | Local -> Local

let[@def] (regional_to_local @ total) x = match x with
  | Global -> Global
  | Regional | Local -> Local

type term : immutable_data =
  | Const of elt
  | Var of int
  | Join of term * term
  | Meet of term * term
  | Regional_to_global of term
  | Table of elt * elt * elt * term
[@@inductive]

let[@def] (apply_table @ total) (global : elt) (regional : elt) (local : elt) value =
  match value with
  | Global -> global
  | Regional -> regional
  | Local -> local

type qf : immutable_data =
  | Le of term * term
  | And of qf * qf
  | Or of qf * qf
  | Not of qf
[@@inductive]

type formula : immutable_data =
  | Plain of qf
  | Conj of formula * formula
  | Disj of formula * formula
  | Neg of formula
  | Exists of formula
  | Forall of formula
[@@inductive]

let[@def] rec (lookup @ total) env i =
  if i < 0 then Global
  else match env with
  | [] -> Global
  | b :: rest -> if i = 0 then b else lookup rest (i - 1)

let[@def] rec (eval_term @ total) env t =
  match t with
  | Const b -> b
  | Var i -> lookup env i
  | Join (a, b) -> join (eval_term env a) (eval_term env b)
  | Meet (a, b) -> meet (eval_term env a) (eval_term env b)
  | Regional_to_global a -> regional_to_global (eval_term env a)
  | Table (global, regional, local, a) ->
    apply_table global regional local (eval_term env a)

let[@def] rec (eval_qf @ total) env q =
  match q with
  | Le (a, b) -> le (eval_term env a) (eval_term env b)
  | And (a, b) -> eval_qf env a && eval_qf env b
  | Or (a, b) -> eval_qf env a || eval_qf env b
  | Not a -> not (eval_qf env a)

let[@def] rec (eval @ total) env f =
  match f with
  | Plain q -> eval_qf env q
  | Conj (a, b) -> eval env a && eval env b
  | Disj (a, b) -> eval env a || eval env b
  | Neg a -> not (eval env a)
  | Exists a ->
    eval (Global :: env) a || eval (Regional :: env) a
    || eval (Local :: env) a
  | Forall a ->
    eval (Global :: env) a && eval (Regional :: env) a
    && eval (Local :: env) a

let[@def] rec (scoped_term @ total) depth t =
  match t with
  | Const _ -> true
  | Var i -> 0 <= i && i < depth
  | Join (a, b) | Meet (a, b) ->
    scoped_term depth a && scoped_term depth b
  | Regional_to_global a | Table (_, _, _, a) -> scoped_term depth a

let[@def] rec (scoped_qf @ total) depth q =
  match q with
  | Le (a, b) -> scoped_term depth a && scoped_term depth b
  | And (a, b) | Or (a, b) ->
    scoped_qf depth a && scoped_qf depth b
  | Not a -> scoped_qf depth a

let[@def] rec (scoped @ total) depth f =
  match f with
  | Plain q -> scoped_qf depth q
  | Conj (a, b) | Disj (a, b) ->
    scoped depth a && scoped depth b
  | Neg a -> scoped depth a
  | Exists a | Forall a -> scoped (depth + 1) a

let[@def] (subsumption_formula @ total) right obligation =
  Forall
    (Disj
       (Neg (Plain right),
        Exists (Plain obligation)))
