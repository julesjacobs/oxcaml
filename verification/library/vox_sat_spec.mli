(** The specification of the SAT solvers: CNF formulas, their evaluation,
    satisfiability, unsatisfiability and the inputs the solvers accept.

    Variables are the integers [0] to [n - 1] and an assignment is a list of
    booleans, the value of variable [i] at position [i]. Each definition is
    [total], so contracts can apply it, and comes with an equation [f_def]
    that unfolds it once; the checker does not unfold definitions on its
    own. The definitions themselves are in [vox_sat_spec.ml]. *)

(** A literal is a variable or its negation. *)
type literal : immutable_data mod total = Positive of int | Negative of int
[@@inductive]

(** A conjunction of clauses, each a disjunction of literals. *)
type formula : immutable_data mod total = literal list list

(** {2 Assignments} *)

(** The value of a variable; [false] beyond the end of the assignment. *)
val lookup : bool list -> int -> bool @@ total
val lookup_def : (assignment : bool list) -> (index : int) ->
  {u : unit | lookup assignment index ===
    (match assignment with
     | [] -> false
     | value :: rest -> if index = 0 then value else lookup rest (index - 1))}
  @@ total

(** [well_sized n values]: [values] has length [n], or is empty if [n] is
    negative. *)
val well_sized : int -> bool list -> bool @@ total
val well_sized_def : (n : int) -> (values : bool list) ->
  {u : unit | well_sized n values ===
    (if n <= 0 then (match values with [] -> true | _ :: _ -> false)
     else (match values with
       | [] -> false
       | _ :: rest -> well_sized (n - 1) rest))} @@ total

val append_assignment : bool list -> bool list -> bool list @@ total
val append_assignment_def : (left : bool list) -> (right : bool list) ->
  {u : unit | append_assignment left right ===
    (match left with
     | [] -> right
     | value :: rest -> value :: append_assignment rest right)} @@ total

(** {2 Evaluation} *)

val eval_literal : bool list -> literal -> bool @@ total
val eval_literal_def : (assignment : bool list) -> (literal : literal) ->
  {u : unit | eval_literal assignment literal ===
    (match literal with
     | Positive index -> lookup assignment index
     | Negative index -> not (lookup assignment index))} @@ total

(** A clause holds if one of its literals does; the empty clause is
    false. *)
val eval_clause : bool list -> literal list -> bool @@ total
val eval_clause_def : (assignment : bool list) -> (clause : literal list) ->
  {u : unit | eval_clause assignment clause ===
    (match clause with
     | [] -> false
     | literal :: rest ->
       eval_literal assignment literal || eval_clause assignment rest)}
  @@ total

(** A formula holds if all of its clauses do; the empty formula is true. *)
val eval_formula : bool list -> formula -> bool @@ total
val eval_formula_def : (assignment : bool list) -> (formula : formula) ->
  {u : unit | eval_formula assignment formula ===
    (match formula with
     | [] -> true
     | clause :: rest ->
       eval_clause assignment clause && eval_formula assignment rest)}
  @@ total

(** {2 Formulas over [n] variables}

    [valid_formula n formula]: every variable of [formula] is at least [0]
    and below [n]. *)

val valid_literal : int -> literal -> bool @@ total
val valid_literal_def : (n : int) -> (literal : literal) ->
  {u : unit | valid_literal n literal ===
    (match literal with
     | Positive index | Negative index -> 0 <= index && index < n)} @@ total

val valid_clause : int -> literal list -> bool @@ total
val valid_clause_def : (n : int) -> (clause : literal list) ->
  {u : unit | valid_clause n clause ===
    (match clause with
     | [] -> true
     | literal :: rest -> valid_literal n literal && valid_clause n rest)}
  @@ total

val valid_formula : int -> formula -> bool @@ total
val valid_formula_def : (n : int) -> (formula : formula) ->
  {u : unit | valid_formula n formula ===
    (match formula with
     | [] -> true
     | clause :: rest -> valid_clause n clause && valid_formula n rest)}
  @@ total

(** {2 Size limits} *)

(** [clauses_fit remaining formula]: [formula] has at most [remaining]
    clauses. *)
val clauses_fit : int -> formula -> bool @@ total
val clauses_fit_def : (remaining : int) -> (formula : formula) ->
  {u : unit | clauses_fit remaining formula ===
    (match formula with
     | [] -> true
     | _ :: rest -> remaining > 0 && clauses_fit (remaining - 1) rest)}
  @@ total

(** [Some (remaining - length clause)] if that is not negative, else
    [None]. *)
val consume_literals : int -> literal list -> int option @@ total
val consume_literals_def : (remaining : int) -> (clause : literal list) ->
  {u : unit | consume_literals remaining clause ===
    (match clause with
     | [] -> Some remaining
     | _ :: rest ->
       if remaining <= 0 then None
       else consume_literals (remaining - 1) rest)} @@ total

(** [literals_fit remaining formula]: [formula] has at most [remaining]
    literal occurrences in all. *)
val literals_fit : int -> formula -> bool @@ total
val literals_fit_def : (remaining : int) -> (formula : formula) ->
  {u : unit | literals_fit remaining formula ===
    (match formula with
     | [] -> true
     | clause :: rest ->
       (match consume_literals remaining clause with
        | None -> false
        | Some left -> literals_fit left rest))} @@ total

(** {2 Satisfiability and unsatisfiability} *)

(** [check n formula assignment]: [assignment] has length [n] and satisfies
    [formula], a formula over [n] variables. *)
val check : int -> formula -> bool list -> bool @@ total
val check_def : (n : int) -> (formula : formula) ->
  (assignment : bool list) ->
  {u : unit | check n formula assignment ===
    (0 <= n && valid_formula n formula
     && well_sized n assignment && eval_formula assignment formula)}
  @@ total

(** [rejects_extensions prefix remaining formula]: every extension of
    [prefix] by [remaining] more values falsifies [formula]. *)
val rejects_extensions : bool list -> int -> formula -> bool @@ total
val rejects_extensions_def : (prefix : bool list) -> (remaining : int) ->
  (formula : formula) ->
  {u : unit | rejects_extensions prefix remaining formula ===
    (if remaining <= 0 then not (eval_formula prefix formula)
     else
       rejects_extensions (append_assignment prefix [false]) (remaining - 1)
         formula
       && rejects_extensions (append_assignment prefix [true])
            (remaining - 1) formula)} @@ total

(** [unsatisfiable n formula]: [formula] is a formula over [n] variables
    and all [2{^n}] assignments of length [n] falsify it. *)
val unsatisfiable : int -> formula -> bool @@ total
val unsatisfiable_def : (n : int) -> (formula : formula) ->
  {u : unit | unsatisfiable n formula ===
    (0 <= n && valid_formula n formula && rejects_extensions [] n formula)}
  @@ total

(** {2 Accepted inputs} *)

type input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals

(** [None] for the inputs the solvers accept: [0 <= n <= 256], at most 4,096
    clauses and 65,536 literal occurrences, and every variable below [n].
    Otherwise the first of these conditions that fails, in that order. *)
val classify_input : int -> formula -> input_error option @@ total
val classify_input_def : (n : int) -> (formula : formula) ->
  {u : unit | classify_input n formula ===
    (if n < 0 || n > 256 then Some Unsupported_variable_count
     else if not (clauses_fit 4096 formula) then Some Too_many_clauses
     else if not (literals_fit 65536 formula) then Some Too_many_literals
     else if not (valid_formula n formula) then Some Invalid_formula
     else None)} @@ total
