type sort =
  | Bool
  | Bv63

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

module Function = struct
  type t =
    { id : int;
      label : string;
      arguments : sort list;
      result : sort
    }

  let next = ref 0

  let create ~label ~arguments ~result =
    if !next = max_int then invalid_arg "Vox_smt.Function.create: exhausted";
    let id = !next in
    incr next;
    { id; label; arguments; result }

  let label t = t.label

  let arguments t = t.arguments

  let result t = t.result
end

type op =
  | Add
  | Sub
  | Mul
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
  | Call of Function.t * term list

type labelled_term =
  { label : string;
    term : term
  }

type query =
  { symbols : Symbol.t list;
    functions : Function.t list;
    facts : labelled_term list;
    goal : labelled_term
  }

exception Sort_error of string

exception Unsupported_target of int

let operator = function
  | Add -> "bvadd"
  | Sub -> "bvsub"
  | Mul -> "bvmul"
  | Neg -> "bvneg"
  | Eq -> "="
  | Ne -> "distinct"
  | Lt -> "bvslt"
  | Le -> "bvsle"
  | Gt -> "bvsgt"
  | Ge -> "bvsge"
  | Not -> "not"
  | And -> "and"
  | Or -> "or"
  | Implies -> "=>"
  | Ite -> "ite"

let sort_name = function Bool -> "Bool" | Bv63 -> "(_ BitVec 63)"

let error fmt = Printf.ksprintf (fun s -> raise (Sort_error s)) fmt

type operator_signature =
  | Fixed of sort list * sort
  | Equality
  | Conditional

let operator_signature = function
  | Add | Sub | Mul -> Fixed ([Bv63; Bv63], Bv63)
  | Neg -> Fixed ([Bv63], Bv63)
  | Lt | Le | Gt | Ge -> Fixed ([Bv63; Bv63], Bool)
  | Not -> Fixed ([Bool], Bool)
  | And | Or | Implies -> Fixed ([Bool; Bool], Bool)
  | Eq | Ne -> Equality
  | Ite -> Conditional

let rec term_sort = function
  | Boolean _ -> Bool
  | Integer _ -> Bv63
  | Var s -> Symbol.sort s
  | Call (f, _) -> Function.result f
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
  let functions = Hashtbl.create 16 in
  List.iter
    (fun f ->
      if Hashtbl.mem functions f.Function.id
      then error "Duplicate SMT function %S" (Function.label f);
      Hashtbl.add functions f.Function.id ())
    q.functions;
  let require expected actual =
    if actual <> expected
    then error "Expected %s, got %s" (sort_name expected) (sort_name actual)
  in
  let rec infer = function
    | Boolean _ -> Bool
    | Integer n ->
      if n < -4611686018427387904L || n > 4611686018427387903L
      then error "Integer %Ld is outside the signed 63-bit range" n;
      Bv63
    | Var s ->
      if not (Hashtbl.mem declared s.Symbol.id)
      then error "Undeclared SMT symbol %S" (Symbol.label s);
      Symbol.sort s
    | Call (f, args) ->
      if not (Hashtbl.mem functions f.Function.id)
      then error "Undeclared SMT function %S" (Function.label f);
      if List.length args <> List.length (Function.arguments f)
      then error "Wrong arity for SMT function %S" (Function.label f);
      List.iter2 require (Function.arguments f) (List.map infer args);
      Function.result f
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
  let functions = Hashtbl.create 16 in
  List.iteri
    (fun i f -> Hashtbl.add functions f.Function.id ("f" ^ string_of_int i))
    q.functions;
  let add s = Buffer.add_string b s in
  let rec term = function
    | Boolean v -> add (string_of_bool v)
    | Integer v ->
      add (Printf.sprintf "(_ bv%Ld 63)" (Int64.logand v Int64.max_int))
    | Var s -> add (Hashtbl.find names s.Symbol.id)
    | Call (f, args) ->
      let name = Hashtbl.find functions f.Function.id in
      if args = []
      then add name
      else begin
        add "(";
        add name;
        List.iter
          (fun arg ->
            add " ";
            term arg)
          args;
        add ")"
      end
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
    (if q.functions = [] then "(set-logic QF_BV)\n" else "(set-logic QF_UFBV)\n");
  List.iter
    (fun s ->
      add
        (Printf.sprintf "(declare-fun %s () %s)\n"
           (Hashtbl.find names s.Symbol.id)
           (sort_name (Symbol.sort s))))
    q.symbols;
  List.iter
    (fun f ->
      add
        (Printf.sprintf "(declare-fun %s (%s) %s)\n"
           (Hashtbl.find functions f.Function.id)
           (String.concat " " (List.map sort_name (Function.arguments f)))
           (sort_name (Function.result f))))
    q.functions;
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
