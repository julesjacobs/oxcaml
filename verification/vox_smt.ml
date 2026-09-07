type sort =
  | Bool
  | Int63

module Symbol = struct
  type t =
    { id : int;
      label : string;
      sort : sort
    }

  let next = ref 0

  let create ~label sort =
    if !next = max_int then invalid_arg "Vox_smt.Symbol.create: exhausted";
    let id = !next in
    incr next;
    { id; label; sort }

  let label t = t.label

  let sort t = t.sort
end

type op =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Not
  | And
  | Or
  | Implies
  | Ite

type term =
  | Boolean of bool
  | Integer of int64
  | Var of Symbol.t
  | App of op * term list

type labelled_term =
  { label : string;
    term : term
  }

type query =
  { symbols : Symbol.t list;
    facts : labelled_term list;
    goal : labelled_term
  }

exception Sort_error of string

exception Unsupported_target of int

let operator = function
  | Add -> "int63_add"
  | Sub -> "int63_sub"
  | Mul -> "int63_mul"
  | Div -> "int63_div"
  | Rem -> "int63_rem"
  | Neg -> "int63_neg"
  | Eq -> "="
  | Ne -> "distinct"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Not -> "not"
  | And -> "and"
  | Or -> "or"
  | Implies -> "=>"
  | Ite -> "ite"

let sort_name = function Bool -> "Bool" | Int63 -> "Int"

let error fmt = Printf.ksprintf (fun s -> raise (Sort_error s)) fmt

type operator_signature =
  | Fixed of sort list * sort
  | Equality
  | Conditional

let operator_signature = function
  | Add | Sub | Mul | Div | Rem -> Fixed ([Int63; Int63], Int63)
  | Neg -> Fixed ([Int63], Int63)
  | Lt | Le | Gt | Ge -> Fixed ([Int63; Int63], Bool)
  | Not -> Fixed ([Bool], Bool)
  | And | Or | Implies -> Fixed ([Bool; Bool], Bool)
  | Eq | Ne -> Equality
  | Ite -> Conditional

let rec term_sort = function
  | Boolean _ -> Bool
  | Integer _ -> Int63
  | Var s -> Symbol.sort s
  | App (op, args) -> (
    match operator_signature op, args with
    | Fixed (_, result), _ -> result
    | Equality, _ -> Bool
    | Conditional, [_; t; _] -> term_sort t
    | Conditional, _ -> error "ite expects 3 operands")

let check ~int_width q =
  if int_width <> 63 then raise (Unsupported_target int_width);
  let declared = Hashtbl.create 16 in
  List.iter
    (fun s ->
      if Hashtbl.mem declared s.Symbol.id
      then error "Duplicate SMT symbol %S" (Symbol.label s);
      Hashtbl.add declared s.Symbol.id ())
    q.symbols;
  let require expected actual =
    if actual <> expected
    then error "Expected %s, got %s" (sort_name expected) (sort_name actual)
  in
  let rec infer = function
    | Boolean _ -> Bool
    | Integer n ->
      if n < -4611686018427387904L || n > 4611686018427387903L
      then error "Integer %Ld is outside the signed 63-bit range" n;
      Int63
    | Var s ->
      if not (Hashtbl.mem declared s.Symbol.id)
      then error "Undeclared SMT symbol %S" (Symbol.label s);
      Symbol.sort s
    | App (op, args) -> (
      let signature = operator_signature op in
      let arity =
        match signature with
        | Fixed (arguments, _) -> List.length arguments
        | Equality -> 2
        | Conditional -> 3
      in
      if List.length args <> arity
      then error "%s expects %d operands" (operator op) arity;
      let sorts = List.map infer args in
      match signature, sorts with
      | Fixed (arguments, result), _ ->
        List.iter2 require arguments sorts;
        result
      | Equality, [a; b] ->
        require a b;
        Bool
      | Conditional, [c; a; b] ->
        require Bool c;
        require a b;
        a
      | _ -> error "Invalid operands for %s" (operator op))
  in
  let fact { label; term } =
    try require Bool (infer term)
    with Sort_error message -> error "%s: %s" label message
  in
  List.iter fact q.facts;
  fact q.goal

let to_smtlib ~int_width ~timeout_ms q =
  check ~int_width q;
  if timeout_ms <= 0 then invalid_arg "Vox_smt.to_smtlib: timeout_ms";
  let names = Hashtbl.create 16 in
  List.iteri
    (fun i s -> Hashtbl.add names s.Symbol.id ("v" ^ string_of_int i))
    q.symbols;
  let b = Buffer.create 256 in
  let add s = Buffer.add_string b s in
  let minimum = "(- 4611686018427387904)" in
  let maximum = "4611686018427387903" in
  let modulus = "9223372036854775808" in
  let rec iter f term =
    f term;
    match term with
    | Boolean _ | Integer _ | Var _ -> ()
    | App (_, arguments) -> List.iter (iter f) arguments
  in
  let roots = q.goal.term :: List.map (fun fact -> fact.term) q.facts in
  let uses operator =
    List.exists
      (fun root ->
        let found = ref false in
        iter
          (function App (op, _) when op = operator -> found := true | _ -> ())
          root;
        !found)
      roots
  in
  let uses_general operator =
    List.exists
      (fun root ->
        let found = ref false in
        iter
          (function
            | App (op, [_; Integer divisor]) when op = operator && divisor <> 0L
              ->
              ()
            | App (op, _) when op = operator -> found := true
            | _ -> ())
          root;
        !found)
      roots
  in
  let multiplications =
    let found = ref [] in
    List.iter
      (iter (function
        | App (Mul, _) as multiplication -> found := multiplication :: !found
        | _ -> ()))
      roots;
    List.sort_uniq Stdlib.compare !found
  in
  let integer value =
    if value < 0L
    then add (Printf.sprintf "(- %Ld)" (Int64.neg value))
    else add (Int64.to_string value)
  in
  let rec term = function
    | Boolean v -> add (string_of_bool v)
    | Integer value -> integer value
    | Var s -> add (Hashtbl.find names s.Symbol.id)
    | App (Div, [dividend; Integer divisor]) when divisor <> 0L ->
      add "(let ((x ";
      term dividend;
      add ")) (let ((q ";
      if divisor < 0L then add "(- ";
      add "(ite (< x 0) (- (div (- x) ";
      integer (Int64.abs divisor);
      add ")) (div x ";
      integer (Int64.abs divisor);
      add "))";
      if divisor < 0L then add ")";
      add ")) (ite (> q ";
      add maximum;
      add ") (- q ";
      add modulus;
      add ") q)))"
    | App (Rem, [dividend; Integer divisor]) when divisor <> 0L ->
      add "(let ((x ";
      term dividend;
      add ")) (let ((r (mod (ite (< x 0) (- x) x) ";
      integer (Int64.abs divisor);
      add "))) (ite (< x 0) (- r) r)))"
    | App (op, args) ->
      add "(";
      add (operator op);
      List.iter
        (fun arg ->
          add " ";
          term arg)
        args;
      add ")"
  in
  add "(set-option :print-success false)\n";
  add "(set-option :produce-models true)\n";
  add (Printf.sprintf "(set-option :timeout %d)\n" timeout_ms);
  add
    (if uses_general Div || uses_general Rem
     then "(set-logic ALL)\n"
     else if multiplications = []
     then "(set-logic QF_LIA)\n"
     else "(set-logic QF_UFLIA)\n");
  if uses Add
  then
    add
      (Printf.sprintf
         "(define-fun int63_add ((x Int) (y Int)) Int\n\
         \  (ite (> (+ x y) %s) (- (+ x y) %s)\n\
         \    (ite (< (+ x y) %s) (+ (+ x y) %s) (+ x y))))\n"
         maximum modulus minimum modulus);
  if uses Sub
  then
    add
      (Printf.sprintf
         "(define-fun int63_sub ((x Int) (y Int)) Int\n\
         \  (ite (> (- x y) %s) (- (- x y) %s)\n\
         \    (ite (< (- x y) %s) (+ (- x y) %s) (- x y))))\n"
         maximum modulus minimum modulus);
  if uses Neg
  then
    add
      (Printf.sprintf
         "(define-fun int63_neg ((x Int)) Int\n  (ite (= x %s) %s (- x)))\n"
         minimum minimum);
  if uses_general Div || uses_general Rem
  then add "(define-fun int63_abs ((x Int)) Int (ite (< x 0) (- x) x))\n";
  if uses_general Div
  then
    add
      (Printf.sprintf
         "(define-fun int63_div ((x Int) (y Int)) Int\n\
         \  (ite (= y 0) 0\n\
         \ (let ((q (ite (= (< x 0) (< y 0))\n\
         \                   (div (int63_abs x) (int63_abs y))\n\
         \                   (- (div (int63_abs x) (int63_abs y))))))\n\
         \ (ite (> q %s) (- q %s) q))))\n"
         maximum modulus);
  if uses_general Rem
  then
    add
      "(define-fun int63_rem ((x Int) (y Int)) Int\n\
      \  (ite (= y 0) 0\n\
      \    (let ((r (mod (int63_abs x) (int63_abs y))))\n\
      \      (ite (< x 0) (- r) r))))\n";
  if multiplications <> [] then add "(declare-fun int63_mul (Int Int) Int)\n";
  List.iter
    (fun s ->
      add
        (Printf.sprintf "(declare-fun %s () %s)\n"
           (Hashtbl.find names s.Symbol.id)
           (sort_name (Symbol.sort s))))
    q.symbols;
  let bounded value =
    add "(assert (and (<= ";
    add minimum;
    add " ";
    term value;
    add ") (<= ";
    term value;
    add " ";
    add maximum;
    add ")))\n"
  in
  List.iter
    (fun symbol ->
      match Symbol.sort symbol with Bool -> () | Int63 -> bounded (Var symbol))
    q.symbols;
  List.iter bounded multiplications;
  List.iter
    (fun f ->
      add "(assert ";
      term f.term;
      add ")\n")
    q.facts;
  add "(assert (not ";
  term q.goal.term;
  add "))\n(check-sat)\n";
  Buffer.contents b

type value =
  | Bool_value of bool
  | Int_value of int64

type validity =
  | Valid
  | Invalid of (Symbol.t * value) list option
  | Unknown of string option
  | Timeout
  | Failure of string
