type literal : immutable_data mod total =
  | Positive of int
  | Negative of int
[@@inductive]
type formula : immutable_data mod total = literal list list

let[@def] rec lookup assignment index =
  match assignment with
  | [] -> false
  | value :: rest -> if index = 0 then value else lookup rest (index - 1)

let[@def] eval_literal assignment literal =
  match literal with
  | Positive index -> lookup assignment index
  | Negative index -> not (lookup assignment index)

let[@def] rec eval_clause assignment clause =
  match clause with
  | [] -> false
  | literal :: rest ->
    eval_literal assignment literal || eval_clause assignment rest

let[@def] rec eval_formula assignment formula =
  match formula with
  | [] -> true
  | clause :: rest ->
    eval_clause assignment clause && eval_formula assignment rest

let[@def] rec well_sized n values =
  if n <= 0 then (match values with [] -> true | _ :: _ -> false)
  else match values with [] -> false | _ :: rest -> well_sized (n - 1) rest
[@@decreases n]

let[@def] valid_literal n literal =
  match literal with
  | Positive index | Negative index -> 0 <= index && index < n

let[@def] rec valid_clause n clause =
  match clause with
  | [] -> true
  | literal :: rest -> valid_literal n literal && valid_clause n rest

let[@def] rec valid_formula n formula =
  match formula with
  | [] -> true
  | clause :: rest -> valid_clause n clause && valid_formula n rest

let[@def] rec clauses_fit remaining formula =
  match formula with
  | [] -> true
  | _ :: rest -> remaining > 0 && clauses_fit (remaining - 1) rest

let[@def] rec consume_literals remaining clause =
  match clause with
  | [] -> Some remaining
  | _ :: rest ->
    if remaining <= 0 then None
    else consume_literals (remaining - 1) rest

let[@def] rec literals_fit remaining formula =
  match formula with
  | [] -> true
  | clause :: rest ->
    match consume_literals remaining clause with
    | None -> false
    | Some left -> literals_fit left rest

let[@def] check n formula assignment =
  0 <= n && valid_formula n formula
  && well_sized n assignment && eval_formula assignment formula

let[@def] rec append_assignment left right =
  match left with
  | [] -> right
  | value :: rest -> value :: append_assignment rest right

let[@def] rec rejects_extensions prefix remaining formula =
  if remaining <= 0 then not (eval_formula prefix formula)
  else rejects_extensions (append_assignment prefix [false]) (remaining - 1)
    formula
    && rejects_extensions (append_assignment prefix [true]) (remaining - 1)
      formula
[@@decreases remaining]

let[@def] unsatisfiable n formula =
  0 <= n && valid_formula n formula && rejects_extensions [] n formula


type input_error =
  | Invalid_formula
  | Unsupported_variable_count
  | Too_many_clauses
  | Too_many_literals

let[@def] classify_input n formula =
  if n < 0 || n > 256 then Some Unsupported_variable_count
  else if not (clauses_fit 4096 formula) then Some Too_many_clauses
  else if not (literals_fit 65536 formula) then Some Too_many_literals
  else if not (valid_formula n formula) then Some Invalid_formula
  else None
