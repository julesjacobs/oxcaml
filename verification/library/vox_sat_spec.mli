type literal : immutable_data mod total = Positive of int | Negative of int
[@@inductive]
type formula : immutable_data mod total = literal list list
val lookup : bool list -> int -> bool @@ total

val lookup_def :
  (assignment : bool list) ->
  (index : int) ->
  {u : unit
    | (lookup assignment index) ===
        (match assignment with
         | [] -> false
         | value::rest ->
             if index = 0 then value else lookup rest (index - 1))} @@ total

val eval_literal : bool list -> literal -> bool @@ total

val eval_literal_def :
  (assignment : bool list) ->
  (literal : literal) ->
  {u : unit
    | (eval_literal assignment literal) ===
        (match literal with
         | Positive index' -> lookup assignment index'
         | Negative index -> not (lookup assignment index))} @@ total

val eval_clause : bool list -> literal list -> bool @@ total

val eval_clause_def :
  (assignment : bool list) ->
  (clause : literal list) ->
  {u : unit
    | (eval_clause assignment clause) ===
        (match clause with
         | [] -> false
         | literal::rest ->
             (eval_literal assignment literal) ||
               (eval_clause assignment rest))} @@ total

val eval_formula : bool list -> literal list list -> bool @@ total

val eval_formula_def :
  (assignment : bool list) ->
  (formula : literal list list) ->
  {u : unit
    | (eval_formula assignment formula) ===
        (match formula with
         | [] -> true
         | clause::rest ->
             (eval_clause assignment clause) &&
               (eval_formula assignment rest))} @@ total

val well_sized : int -> bool list -> bool @@ total

val well_sized_def :
  (n : int) ->
  (values : bool list) ->
  {u : unit
    | (well_sized n values) ===
        (if n <= 0
         then match values with | [] -> true | _::_ -> false
         else
           (match values with
            | [] -> false
            | _::rest -> well_sized (n - 1) rest))} @@ total

val valid_literal : int -> literal -> bool @@ total

val valid_literal_def :
  (n : int) ->
  (literal : literal) ->
  {u : unit
    | (valid_literal n literal) ===
        (match literal with
         | Positive index | Negative index -> (0 <= index) && (index < n))} @@
           total

val valid_clause : int -> literal list -> bool @@ total

val valid_clause_def :
  (n : int) ->
  (clause : literal list) ->
  {u : unit
    | (valid_clause n clause) ===
        (match clause with
         | [] -> true
         | literal::rest ->
             (valid_literal n literal) && (valid_clause n rest))} @@ total

val valid_formula : int -> literal list list -> bool @@ total

val valid_formula_def :
  (n : int) ->
  (formula : literal list list) ->
  {u : unit
    | (valid_formula n formula) ===
        (match formula with
         | [] -> true
         | clause::rest -> (valid_clause n clause) && (valid_formula n rest))}
           @@ total

val clauses_fit : int -> literal list list -> bool @@ total

val clauses_fit_def :
  (remaining : int) ->
  (formula : literal list list) ->
  {u : unit
    | (clauses_fit remaining formula) ===
        (match formula with
         | [] -> true
         | _::rest -> (remaining > 0) && (clauses_fit (remaining - 1) rest))}
           @@ total

val consume_literals : int -> literal list -> int option @@ total

val consume_literals_def :
  (remaining : int) ->
  (clause : literal list) ->
  {u : unit
    | (consume_literals remaining clause) ===
        (match clause with
         | [] -> Some remaining
         | _::rest ->
             if remaining <= 0
             then None
             else consume_literals (remaining - 1) rest)} @@ total

val literals_fit : int -> literal list list -> bool @@ total

val literals_fit_def :
  (remaining : int) ->
  (formula : literal list list) ->
  {u : unit
    | (literals_fit remaining formula) ===
        (match formula with
         | [] -> true
         | clause::rest ->
             (match consume_literals remaining clause with
              | None -> false
              | Some left -> literals_fit left rest))} @@ total

val check : int -> literal list list -> bool list -> bool @@ total

val check_def :
  (n : int) ->
  (formula : literal list list) ->
  (assignment : bool list) ->
  {u : unit
    | (check n formula assignment) ===
        ((0 <= n) &&
           ((valid_formula n formula) &&
              ((well_sized n assignment) && (eval_formula assignment
                formula))))} @@ total

val append_assignment : bool list -> bool list -> bool list @@ total
val append_assignment_def : (left : bool list) -> (right : bool list) ->
  {u : unit | append_assignment left right ===
    (match left with [] -> right
     | value :: rest -> value :: append_assignment rest right)} @@ total

val rejects_extensions : bool list -> int -> literal list list -> bool @@ total

val rejects_extensions_def :
  (prefix : bool list) ->
  (remaining : int) ->
  (formula : literal list list) ->
  {u : unit
    | (rejects_extensions prefix remaining formula) ===
        (if remaining <= 0
         then not (eval_formula prefix formula)
         else
           (rejects_extensions (append_assignment prefix [false]) (remaining -
             1) formula) &&
             (rejects_extensions (append_assignment prefix [true]) (remaining
               - 1) formula))} @@ total

val unsatisfiable : int -> literal list list -> bool @@ total

val unsatisfiable_def :
  (n : int) ->
  (formula : literal list list) ->
  {u : unit
    | (unsatisfiable n formula) ===
        ((0 <= n) &&
           ((valid_formula n formula) && (rejects_extensions [] n formula)))}
             @@ total

type input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals
