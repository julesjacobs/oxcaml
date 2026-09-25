type elt : immutable_data = Global | Regional | Local
val rank : elt -> int @@ total

val rank_def :
  (x : elt) ->
  {u : unit
    | (rank x) === (match x with | Global -> 0 | Regional -> 1 | Local -> 2)} @@ total

val le : elt -> elt -> bool @@ total

val le_def :
  (a : elt) -> (b : elt) -> {u : unit | (le a b) === ((rank a) <= (rank b))} @@ total

val join : elt -> elt -> elt @@ total

val join_def :
  (a : elt) ->
  (b : elt) -> {u : unit | (join a b) === (if le a b then b else a)} @@ total

val meet : elt -> elt -> elt @@ total

val meet_def :
  (a : elt) ->
  (b : elt) -> {u : unit | (meet a b) === (if le a b then a else b)} @@ total

val regional_to_global : elt -> elt @@ total

val regional_to_global_def :
  (x : elt) ->
  {u : unit
    | (regional_to_global x) ===
        (match x with | Global | Regional -> Global | Local -> Local)} @@ total

val regional_to_local : elt -> elt @@ total

val regional_to_local_def :
  (x : elt) ->
  {u : unit
    | (regional_to_local x) ===
        (match x with | Global -> Global | Regional | Local -> Local)} @@ total

type term : immutable_data =
    Const of elt
  | Var of int
  | Join of term * term
  | Meet of term * term
  | Regional_to_global of term
  | Table of elt * elt * elt * term
[@@inductive]
val apply_table : elt -> elt -> elt -> elt -> elt @@ total

val apply_table_def :
  (global : elt) ->
  (regional : elt) ->
  (local : elt) ->
  (value : elt) ->
  {u : unit
    | (apply_table global regional local value) ===
        (match value with
         | Global -> global
         | Regional -> regional
         | Local -> local)} @@ total

type qf : immutable_data = Le of term * term | And of qf * qf | Or of qf * qf | Not of qf
[@@inductive]
type formula : immutable_data =
    Plain of qf
  | Conj of formula * formula
  | Disj of formula * formula
  | Neg of formula
  | Exists of formula
  | Forall of formula
[@@inductive]
val lookup : elt list -> int -> elt @@ total

val lookup_def :
  (env : elt list) ->
  (i : int) ->
  {u : unit
    | (lookup env i) ===
        (if i < 0
         then Global
         else
           (match env with
            | [] -> Global
            | b::rest -> if i = 0 then b else lookup rest (i - 1)))} @@ total

val eval_term : elt list -> term -> elt @@ total

val eval_term_def :
  (env : elt list) ->
  (t : term) ->
  {u : unit
    | (eval_term env t) ===
        (match t with
         | Const b' -> b'
         | Var i -> lookup env i
         | Join (a', b'') -> join (eval_term env a') (eval_term env b'')
         | Meet (a'', b) -> meet (eval_term env a'') (eval_term env b)
         | Regional_to_global a''' -> regional_to_global (eval_term env a''')
         | Table (global, regional, local, a) ->
             apply_table global regional local (eval_term env a))} @@ total

val eval_qf : elt list -> qf -> bool @@ total

val eval_qf_def :
  (env : elt list) ->
  (q : qf) ->
  {u : unit
    | (eval_qf env q) ===
        (match q with
         | Le (a', b') -> le (eval_term env a') (eval_term env b')
         | And (a'', b'') -> (eval_qf env a'') && (eval_qf env b'')
         | Or (a''', b) -> (eval_qf env a''') || (eval_qf env b)
         | Not a -> not (eval_qf env a))} @@ total

val eval : elt list -> formula -> bool @@ total

val eval_def :
  (env : elt list) ->
  (f : formula) ->
  {u : unit
    | (eval env f) ===
        (match f with
         | Plain q -> eval_qf env q
         | Conj (a', b') -> (eval env a') && (eval env b')
         | Disj (a'', b) -> (eval env a'') || (eval env b)
         | Neg a''' -> not (eval env a''')
         | Exists a'''' ->
             (eval (Global :: env) a'''') ||
               ((eval (Regional :: env) a'''') || (eval (Local :: env) a''''))
         | Forall a ->
             (eval (Global :: env) a) &&
               ((eval (Regional :: env) a) && (eval (Local :: env) a)))} @@ total

val scoped_term : int -> term -> bool @@ total

val scoped_term_def :
  (depth : int) ->
  (t : term) ->
  {u : unit
    | (scoped_term depth t) ===
        (match t with
         | Const _ -> true
         | Var i -> (0 <= i) && (i < depth)
         | Join (a', b) | Meet (a', b) ->
             (scoped_term depth a') && (scoped_term depth b)
         | Regional_to_global a | Table (_, _, _, a) -> scoped_term depth a)} @@ total

val scoped_qf : int -> qf -> bool @@ total

val scoped_qf_def :
  (depth : int) ->
  (q : qf) ->
  {u : unit
    | (scoped_qf depth q) ===
        (match q with
         | Le (a', b') -> (scoped_term depth a') && (scoped_term depth b')
         | And (a'', b) | Or (a'', b) ->
             (scoped_qf depth a'') && (scoped_qf depth b)
         | Not a -> scoped_qf depth a)} @@ total

val scoped : int -> formula -> bool @@ total

val scoped_def :
  (depth : int) ->
  (f : formula) ->
  {u : unit
    | (scoped depth f) ===
        (match f with
         | Plain q -> scoped_qf depth q
         | Conj (a', b) | Disj (a', b) ->
             (scoped depth a') && (scoped depth b)
         | Neg a'' -> scoped depth a''
         | Exists a | Forall a -> scoped (depth + 1) a)} @@ total

val subsumption_formula : qf -> qf -> formula @@ total

val subsumption_formula_def :
  (right : qf) ->
  (obligation : qf) ->
  {u : unit
    | (subsumption_formula right obligation) ===
        (Forall (Disj ((Neg (Plain right)), (Exists (Plain obligation)))))} @@ total
