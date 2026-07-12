open Oxsmt_core

type status =
  | Sat
  | Unsat
  | Unknown
  | No_status

exception Unsupported of string
exception Malformed of string

module Decls = struct
  type t =
    { consts : (string, Symbol.t * Sort.t) Hashtbl.t
    ; funs : (string, Symbol.t * Rank.t) Hashtbl.t
    ; sorts : (string, Sort.t) Hashtbl.t
    }

  let create () =
    { consts = Hashtbl.create 32; funs = Hashtbl.create 32; sorts = Hashtbl.create 16 }
  ;;

  let const_sort t name = Option.map snd (Hashtbl.find_opt t.consts name)
  let fun_rank t name = Option.map snd (Hashtbl.find_opt t.funs name)

  let sort_by_name t name =
    match name with
    | "Int" -> Some Sort.int
    | "Bool" -> Some Sort.bool
    | _ -> Hashtbl.find_opt t.sorts name
  ;;

  let fold_consts f t acc =
    Hashtbl.fold (fun name (_, sort) acc -> f name sort acc) t.consts acc
  ;;

  let fold_funs f t acc =
    Hashtbl.fold (fun name (_, rank) acc -> f name rank acc) t.funs acc
  ;;
end

(* Token that names a symbol (either bare or |quoted|). *)
let sym_name = function
  | Sexp.Atom s -> s
  | Sexp.Quoted s -> s
  | Sexp.List _ -> raise (Malformed "expected a symbol name, found a list")
;;

let is_numeral s =
  String.length s > 0
  && String.for_all
       (function
         | '0' .. '9' -> true
         | _ -> false)
       s
;;

let int_of_numeral s =
  match int_of_string_opt s with
  | Some n -> n
  | None -> raise (Unsupported ("numeral exceeds native int: " ^ s))
;;

let checked_mul a b =
  let p = a * b in
  if a <> 0 && p / a <> b then raise (Unsupported "constant multiplication overflow");
  p
;;

(* Resolve a sort s-expression (Atom name; compound sorts are unsupported). *)
let parse_sort decls = function
  | Sexp.Atom name | Sexp.Quoted name ->
    (match Decls.sort_by_name decls name with
     | Some s -> s
     | None -> raise (Malformed ("unknown sort: " ^ name)))
  | Sexp.List _ -> raise (Unsupported "compound sort")
;;

let is_operator = function
  | "and"
  | "or"
  | "not"
  | "=>"
  | "ite"
  | "="
  | "distinct"
  | "<="
  | "<"
  | ">="
  | ">"
  | "+"
  | "-"
  | "*"
  | "div"
  | "mod"
  | "abs"
  | "let"
  | "!" -> true
  | _ -> false
;;

(* Build a left/right comparison chain: (op a b c ...) = (op a b) /\ (op b c) /\ ... *)
let chain ctx pair args =
  match args with
  | [] | [ _ ] -> raise (Malformed "comparison needs at least two arguments")
  | first :: rest ->
    let rec go prev = function
      | [] -> []
      | x :: tl -> pair ctx prev x :: go x tl
    in
    Context.and_ ctx (go first rest)
;;

let rec parse_term ctx decls (env : (string * Term.t) list) (s : Sexp.t) : Term.t =
  match s with
  | Sexp.Atom name -> parse_atom ctx decls env name
  | Sexp.Quoted name -> parse_symbol_ref ctx decls name []
  | Sexp.List [] -> raise (Malformed "empty application ()")
  | Sexp.List (head :: args) -> parse_app ctx decls env head args

and parse_atom ctx decls env name =
  match List.assoc_opt name env with
  | Some t -> t
  | None ->
    (match name with
     | "true" -> Context.bool_const ctx true
     | "false" -> Context.bool_const ctx false
     | _ when is_numeral name -> Context.int_const ctx (int_of_numeral name)
     | _ -> parse_symbol_ref ctx decls name [])

and parse_symbol_ref ctx decls name args =
  (* A reference to a declared symbol, possibly applied. *)
  match Hashtbl.find_opt decls.Decls.consts name with
  | Some (sym, _) ->
    if args = []
    then Context.const ctx sym
    else raise (Malformed ("constant applied to arguments: " ^ name))
  | None ->
    (match Hashtbl.find_opt decls.Decls.funs name with
     | Some (sym, _) -> Context.app ctx sym args
     | None -> raise (Malformed ("undeclared symbol: " ^ name)))

and parse_app ctx decls env head args =
  match head with
  | Sexp.Quoted name ->
    parse_symbol_ref ctx decls name (List.map (parse_term ctx decls env) args)
  | Sexp.List _ -> raise (Malformed "application head is not a symbol")
  | Sexp.Atom op when is_operator op -> parse_operator ctx decls env op args
  | Sexp.Atom name ->
    parse_symbol_ref ctx decls name (List.map (parse_term ctx decls env) args)

and parse_operator ctx decls env op args =
  let p = parse_term ctx decls env in
  match op, args with
  | "let", [ bindings; body ] -> parse_let ctx decls env bindings body
  | "let", _ -> raise (Malformed "let expects (let (bindings) body)")
  | "!", t :: _ -> p t (* annotation: keep the term, drop attributes *)
  | "!", [] -> raise (Malformed "(! ...) with no term")
  | "not", [ a ] -> Context.not_ ctx (p a)
  | "not", _ -> raise (Malformed "not expects one argument")
  | "and", _ -> Context.and_ ctx (List.map p args)
  | "or", _ -> Context.or_ ctx (List.map p args)
  | "=>", _ :: _ :: _ ->
    let ts = List.map p args in
    let rec fold = function
      | [ x ] -> x
      | x :: tl -> Context.implies ctx x (fold tl)
      | [] -> assert false
    in
    fold ts
  | "=>", _ -> raise (Malformed "=> expects at least two arguments")
  | "=", first :: (_ :: _ as rest) ->
    let first = p first in
    let rec go prev = function
      | [] -> []
      | x :: tl ->
        let x = p x in
        Context.eq ctx prev x :: go x tl
    in
    Context.and_ ctx (go first rest)
  | "=", _ -> raise (Malformed "= expects at least two arguments")
  | "distinct", _ :: _ :: _ -> Context.distinct ctx (List.map p args)
  | "distinct", _ -> raise (Malformed "distinct expects at least two arguments")
  | "<=", _ -> chain ctx Context.le (List.map p args)
  | "<", _ -> chain ctx Context.lt (List.map p args)
  | ">=", _ -> chain ctx Context.ge (List.map p args)
  | ">", _ -> chain ctx Context.gt (List.map p args)
  | "+", [ a ] -> p a
  | "+", first :: rest ->
    List.fold_left (fun acc x -> Context.add ctx acc (p x)) (p first) rest
  | "+", [] -> raise (Malformed "+ expects at least one argument")
  | "-", [ Sexp.Atom s ] when is_numeral s ->
    (* Signed integer literal [(- n)]: parse ["-" ^ n] directly rather than negating a
       parsed-positive [n]. The magnitude of [min_int] is [max_int + 1] (unrepresentable
       as a positive native int), so [neg (int_const n)] cannot build [min_int]; the whole
       signed literal can. Behaviour-preserving for every other numeral (folds to the same
       [Int_const]); genuinely out-of-range magnitudes still raise [Unsupported]. *)
    (match int_of_string_opt ("-" ^ s) with
     | Some n -> Context.int_const ctx n
     | None -> raise (Unsupported ("numeral exceeds native int: -" ^ s)))
  | "-", [ a ] -> Context.neg ctx (p a)
  | "-", first :: (_ :: _ as rest) ->
    List.fold_left (fun acc x -> Context.sub ctx acc (p x)) (p first) rest
  | "-", [] -> raise (Malformed "- expects at least one argument")
  | "*", _ -> parse_mul ctx (List.map p args)
  | "div", [ a; b ] -> Context.div ctx (p a) (p b)
  | "div", _ -> raise (Malformed "div expects two arguments")
  | "mod", [ a; b ] -> Context.mod_ ctx (p a) (p b)
  | "mod", _ -> raise (Malformed "mod expects two arguments")
  | "abs", [ a ] -> Context.abs ctx (p a)
  | "abs", _ -> raise (Malformed "abs expects one argument")
  | "ite", [ c; a; b ] -> Context.ite ctx (p c) (p a) (p b)
  | "ite", _ -> raise (Malformed "ite expects three arguments")
  | _ -> raise (Malformed ("misapplied operator: " ^ op))

and parse_mul ctx factors =
  (* Linear multiplication: at most one non-constant factor (ADR-0003 Decision 1). *)
  let consts, nonconsts =
    List.partition_map
      (fun (t : Term.t) ->
         match t.node with
         | Term.Int_const k -> Left k
         | _ -> Right t)
      factors
  in
  let c = List.fold_left checked_mul 1 consts in
  match nonconsts with
  | [] -> Context.int_const ctx c
  | [ t ] -> Context.mul_const ctx c t
  | _ :: _ :: _ ->
    raise (Unsupported "nonlinear multiplication (>= 2 non-constant factors)")

and parse_let ctx decls env bindings body =
  let parse_binding = function
    | Sexp.List [ name_s; e ] ->
      let name = sym_name name_s in
      name, parse_term ctx decls env e
    | _ -> raise (Malformed "let binding must be (name expr)")
  in
  let binds =
    match bindings with
    | Sexp.List bs -> List.map parse_binding bs
    | _ -> raise (Malformed "let bindings must be a list")
  in
  (* Parallel binding: each RHS is parsed in the outer [env] (done above), then all names
     come into scope for the body. *)
  parse_term ctx decls (binds @ env) body
;;

type query =
  { assertions : Term.t list
  ; status : status
  ; decls : Decls.t
  ; context : Context.t
  }

let logic_ok = function
  | "QF_UF" | "QF_LIA" | "QF_UFLIA" | "QF_IDL" | "QF_RDL" -> true
  | _ -> false
;;

let status_of_string = function
  | "sat" -> Sat
  | "unsat" -> Unsat
  | "unknown" -> Unknown
  | other -> raise (Malformed ("unrecognized :status " ^ other))
;;

let read_string (src : string) : query =
  let sexps = Sexp.parse_all src in
  let env = Env.create () in
  let ctx = Context.create env in
  let decls = Decls.create () in
  let assertions = ref [] in
  let status = ref No_status in
  let stopped = ref false in
  let handle_command = function
    | Sexp.List (Sexp.Atom cmd :: rest) when not !stopped ->
      (match cmd, rest with
       | "set-logic", [ Sexp.Atom l ] ->
         if not (logic_ok l) then raise (Unsupported ("logic " ^ l))
       | "set-logic", _ -> raise (Malformed "set-logic expects one symbol")
       | "set-info", [ Sexp.Atom ":status"; Sexp.Atom v ] -> status := status_of_string v
       | "set-info", _ -> () (* other attributes ignored *)
       | "set-option", _ -> ()
       | "declare-sort", [ name_s; Sexp.Atom arity ] ->
         if arity <> "0" then raise (Unsupported "declare-sort with nonzero arity");
         let name = sym_name name_s in
         let sym = Env.declare_sort env name in
         Hashtbl.replace decls.Decls.sorts name (Sort.uninterpreted sym)
       | "declare-sort", _ -> raise (Malformed "declare-sort expects (name arity)")
       | "declare-const", [ name_s; sort_s ] ->
         let name = sym_name name_s in
         let sort = parse_sort decls sort_s in
         let sym = Env.declare_fun env name (Rank.create [] sort) in
         Hashtbl.replace decls.Decls.consts name (sym, sort)
       | "declare-const", _ -> raise (Malformed "declare-const expects (name sort)")
       | "declare-fun", [ name_s; Sexp.List dom_s; cod_s ] ->
         let name = sym_name name_s in
         let dom = List.map (parse_sort decls) dom_s in
         let cod = parse_sort decls cod_s in
         let rank = Rank.create dom cod in
         let sym = Env.declare_fun env name rank in
         if dom = []
         then Hashtbl.replace decls.Decls.consts name (sym, cod)
         else Hashtbl.replace decls.Decls.funs name (sym, rank)
       | "declare-fun", _ ->
         raise (Malformed "declare-fun expects (name (domain) codomain)")
       | "assert", [ t ] ->
         let term = parse_term ctx decls [] t in
         if not (Sort.equal term.sort Sort.bool)
         then raise (Malformed "asserted term is not Bool-sorted");
         assertions := term :: !assertions
       | "assert", _ -> raise (Malformed "assert expects one term")
       | "check-sat", [] -> ()
       | "check-sat", _ -> raise (Malformed "check-sat takes no arguments")
       | "exit", _ -> stopped := true
       | ("push" | "pop" | "define-fun" | "define-fun-rec" | "declare-datatypes"), _ ->
         raise (Unsupported cmd)
       | _ -> raise (Unsupported ("command " ^ cmd)))
    | Sexp.List _ -> () (* after (exit) *)
    | (Sexp.Atom _ | Sexp.Quoted _) as s ->
      if not !stopped
      then raise (Malformed ("top-level token, expected a command: " ^ sym_name s))
  in
  List.iter handle_command sexps;
  { assertions = List.rev !assertions; status = !status; decls; context = ctx }
;;

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
       let len = in_channel_length ic in
       let s = really_input_string ic len in
       read_string s)
;;
