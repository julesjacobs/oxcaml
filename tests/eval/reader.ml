open Oxsmt_core

type status =
  | Sat
  | Unsat
  | Unknown
  | No_status

exception Unsupported of string
exception Malformed of string

module Decls = struct
  (* A [define-fun] is a non-recursive MACRO (SMT-LIB 2.6 §4.2.2): parameters + result
     sort + an UNEXPANDED body s-expression, substituted at each use site. It is not a
     declared symbol (it never reaches the model), so it lives apart from [consts]/[funs]. *)
  type macro =
    { params : (string * Sort.t) list
    ; result_sort : Sort.t
    ; body : Sexp.t
    }

  type t =
    { consts : (string, Symbol.t * Sort.t) Hashtbl.t
    ; funs : (string, Symbol.t * Rank.t) Hashtbl.t
    ; sorts : (string, Sort.t) Hashtbl.t
    ; macros : (string, macro) Hashtbl.t
    ; bv_mint : Bv_term.minter
      (* [define-fun] names currently mid-expansion — the recursion guard: a macro
           re-entered while already on the expansion stack is a rejected recursive
           definition (define-fun is non-recursive). *)
    ; expanding : (string, unit) Hashtbl.t
    }

  let create bv_mint =
    { consts = Hashtbl.create 32
    ; funs = Hashtbl.create 32
    ; sorts = Hashtbl.create 16
    ; macros = Hashtbl.create 16
    ; bv_mint
    ; expanding = Hashtbl.create 8
    }
  ;;

  (* Any name already claimed by a declaration or a prior definition — [define-fun]
     requires a fresh name (no redefinition). *)
  let is_defined t name =
    Hashtbl.mem t.consts name || Hashtbl.mem t.funs name || Hashtbl.mem t.macros name
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

(* The independent evaluator deliberately bounds allocations before constructing a
   bit-vector term. This matches the shipped parser's default ceiling but is not shared
   with it: the N-version reader derives and enforces the bound itself. *)
let max_bv_width = 1 lsl 20

let check_bv_width what width =
  if width < 1 || width > max_bv_width
  then
    raise
      (Unsupported
         (Printf.sprintf
            "%s: bitvector width %d is outside [1,%d]"
            what
            width
            max_bv_width))
;;

let checked_bv_sum what a b =
  if a < 1 || b < 0 || b > max_bv_width || a > max_bv_width - b
  then raise (Unsupported (what ^ ": result width exceeds evaluator limit"));
  a + b
;;

let checked_bv_product what a b =
  if a < 1 || b < 1 || a > max_bv_width / b
  then raise (Unsupported (what ^ ": result width exceeds evaluator limit"));
  a * b
;;

let bv_width_of_term what (t : Term.t) =
  match t.sort with
  | Sort.BitVec width -> width
  | _ -> raise (Malformed (what ^ ": expected a bitvector operand"))
;;

let bigint_of_radix_digits ~radix ~digit s =
  let base = Bigint.of_int radix in
  String.fold_left
    (fun acc c -> Bigint.add (Bigint.mul acc base) (Bigint.of_int (digit c)))
    Bigint.zero
    s
;;

let bin_digit = function
  | '0' -> 0
  | '1' -> 1
  | c -> raise (Malformed (Printf.sprintf "invalid binary digit %c" c))
;;

let hex_digit = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | c -> raise (Malformed (Printf.sprintf "invalid hexadecimal digit %c" c))
;;

let parse_bv_atom ctx decls s =
  let length = String.length s in
  if length <= 2 then raise (Malformed "empty bitvector literal");
  let digits = String.sub s 2 (length - 2) in
  let bits_per_digit, radix, digit =
    match s.[1] with
    | 'b' -> 1, 2, bin_digit
    | 'x' -> 4, 16, hex_digit
    | _ -> raise (Malformed ("invalid bitvector literal " ^ s))
  in
  let width =
    checked_bv_product "bitvector literal" (String.length digits) bits_per_digit
  in
  let bits = bigint_of_radix_digits ~radix ~digit digits in
  Bv_term.const ctx decls.Decls.bv_mint ~bits ~width
;;

(* Build an integer-constant term from a decimal string: native fast path, then
   arbitrary-precision (core-bignum W2) so a >int63 numeral reads without loss. [s] may
   carry a leading '-'. *)
let const_of_decimal ctx s =
  match int_of_string_opt s with
  | Some n -> Context.int_const ctx n
  | None ->
    (match Bigint.of_string s with
     | b -> Context.int_const_big ctx b
     | exception Invalid_argument _ -> raise (Unsupported ("malformed numeral: " ^ s)))
;;

(* Resolve a sort s-expression. BitVec is the sole supported indexed sort. *)
let parse_sort decls = function
  | Sexp.Atom name | Sexp.Quoted name ->
    (match Decls.sort_by_name decls name with
     | Some s -> s
     | None -> raise (Malformed ("unknown sort: " ^ name)))
  | Sexp.List [ Sexp.Atom "_"; Sexp.Atom "BitVec"; Sexp.Atom width_s ] ->
    (match int_of_string_opt width_s with
     | Some width ->
       check_bv_width "(_ BitVec w)" width;
       Sort.bitvec width
     | None -> raise (Unsupported "bitvector width does not fit a native integer"))
  | Sexp.List _ -> raise (Unsupported "compound sort")
;;

let is_bv_operator = function
  | "bvnot"
  | "bvand"
  | "bvor"
  | "bvxor"
  | "bvnand"
  | "bvnor"
  | "bvxnor"
  | "bvneg"
  | "bvadd"
  | "bvsub"
  | "bvmul"
  | "bvudiv"
  | "bvurem"
  | "bvsdiv"
  | "bvsrem"
  | "bvsmod"
  | "bvshl"
  | "bvlshr"
  | "bvashr"
  | "bvult"
  | "bvule"
  | "bvugt"
  | "bvuge"
  | "bvslt"
  | "bvsle"
  | "bvsgt"
  | "bvsge"
  | "bvcomp"
  | "concat" -> true
  | _ -> false
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
  | Sexp.List [ Sexp.Atom "_"; Sexp.Atom name; Sexp.Atom width_s ]
    when String.length name > 2
         && String.sub name 0 2 = "bv"
         && is_numeral (String.sub name 2 (String.length name - 2)) ->
    let width =
      match int_of_string_opt width_s with
      | Some width ->
        check_bv_width "(_ bvN w)" width;
        width
      | None ->
        raise (Unsupported "bitvector literal width does not fit a native integer")
    in
    let bits =
      match Bigint.of_string (String.sub name 2 (String.length name - 2)) with
      | bits -> bits
      | exception Invalid_argument _ ->
        raise (Malformed "invalid decimal bitvector literal")
    in
    Bv_term.const ctx decls.Decls.bv_mint ~bits ~width
  | Sexp.List (head :: args) -> parse_app ctx decls env head args

and parse_atom ctx decls env name =
  match List.assoc_opt name env with
  | Some t -> t
  | None ->
    (match name with
     | "true" -> Context.bool_const ctx true
     | "false" -> Context.bool_const ctx false
     | _ when String.starts_with ~prefix:"#b" name || String.starts_with ~prefix:"#x" name
       -> parse_bv_atom ctx decls name
     | _ when is_numeral name -> const_of_decimal ctx name
     | _ -> parse_symbol_ref ctx decls name [])

and parse_symbol_ref ctx decls name args =
  (* A reference to a define-fun macro, a declared symbol, or (applied) function. Macros
     are checked first: a [define-fun] name expands to its substituted body. *)
  match Hashtbl.find_opt decls.Decls.macros name with
  | Some m -> expand_macro ctx decls name m args
  | None ->
    (match Hashtbl.find_opt decls.Decls.consts name with
     | Some (sym, _) ->
       if args = []
       then Context.const ctx sym
       else raise (Malformed ("constant applied to arguments: " ^ name))
     | None ->
       (match Hashtbl.find_opt decls.Decls.funs name with
        | Some (sym, _) -> Context.app ctx sym args
        | None -> raise (Malformed ("undeclared symbol: " ^ name))))

(* Expand a [define-fun] application by capture-free substitution (SMT-LIB 2.6 §4.2.2).
   The already-parsed argument terms are bound to the parameter names and the body is
   parsed in THAT environment ONLY — the caller's local [let] bindings deliberately do not
   leak into the body, and substitution is capture-free because the arguments are complete
   hash-consed terms (an inner [let] in the body freely shadows a parameter). Arity, each
   argument's sort against its parameter, and the body's sort against the declared result
   sort are all checked (else [Malformed]). A macro re-entered while already expanding is
   a recursive definition, which [define-fun] forbids — rejected [Unsupported]. *)
and expand_macro ctx decls name (m : Decls.macro) (args : Term.t list) =
  let np = List.length m.Decls.params
  and na = List.length args in
  if na <> np
  then
    raise
      (Malformed
         (Printf.sprintf
            "define-fun %s applied to %d argument(s), expected %d"
            name
            na
            np));
  if Hashtbl.mem decls.Decls.expanding name
  then
    raise
      (Unsupported
         (Printf.sprintf
            "recursive define-fun %s (define-fun is non-recursive; define-fun-rec is out \
             of scope)"
            name));
  let env =
    List.map2
      (fun (pname, psort) (arg : Term.t) ->
         if not (Sort.equal arg.sort psort)
         then
           raise
             (Malformed
                (Printf.sprintf
                   "define-fun %s: argument for parameter %s has the wrong sort"
                   name
                   pname));
         pname, arg)
      m.Decls.params
      args
  in
  Hashtbl.add decls.Decls.expanding name ();
  let body =
    Fun.protect
      ~finally:(fun () -> Hashtbl.remove decls.Decls.expanding name)
      (fun () -> parse_term ctx decls env m.Decls.body)
  in
  if not (Sort.equal body.sort m.Decls.result_sort)
  then
    raise
      (Malformed
         (Printf.sprintf
            "define-fun %s: body sort does not match the declared result sort"
            name));
  body

and parse_app ctx decls env head args =
  match head with
  | Sexp.Quoted name ->
    parse_symbol_ref ctx decls name (List.map (parse_term ctx decls env) args)
  | Sexp.List indexed -> parse_bv_indexed ctx decls env indexed args
  | Sexp.Atom op when is_bv_operator op -> parse_bv_operator ctx decls env op args
  | Sexp.Atom op when is_operator op -> parse_operator ctx decls env op args
  | Sexp.Atom name ->
    parse_symbol_ref ctx decls name (List.map (parse_term ctx decls env) args)

and parse_bv_indexed ctx decls env indexed args =
  let one_arg what f =
    match args with
    | [ arg ] -> f (parse_term ctx decls env arg)
    | _ -> raise (Malformed (what ^ " expects one argument"))
  in
  let nonnegative what s =
    match int_of_string_opt s with
    | Some n when n >= 0 -> n
    | Some _ -> raise (Malformed (what ^ " index must be non-negative"))
    | None -> raise (Unsupported (what ^ " index does not fit a native integer"))
  in
  match indexed with
  | [ Sexp.Atom "_"; Sexp.Atom "extract"; Sexp.Atom hi_s; Sexp.Atom lo_s ] ->
    let hi = nonnegative "extract" hi_s
    and lo = nonnegative "extract" lo_s in
    one_arg "extract" (Bv_term.extract ctx decls.Decls.bv_mint ~i:hi ~j:lo)
  | [ Sexp.Atom "_"; Sexp.Atom "zero_extend"; Sexp.Atom n_s ] ->
    let n = nonnegative "zero_extend" n_s in
    one_arg "zero_extend" (fun x ->
      let width = bv_width_of_term "zero_extend" x in
      ignore (checked_bv_sum "zero_extend" width n : int);
      Bv_term.zero_extend ctx decls.Decls.bv_mint ~n x)
  | [ Sexp.Atom "_"; Sexp.Atom "sign_extend"; Sexp.Atom n_s ] ->
    let n = nonnegative "sign_extend" n_s in
    one_arg "sign_extend" (fun x ->
      let width = bv_width_of_term "sign_extend" x in
      ignore (checked_bv_sum "sign_extend" width n : int);
      Bv_term.sign_extend ctx decls.Decls.bv_mint ~n x)
  | [ Sexp.Atom "_"; Sexp.Atom "rotate_left"; Sexp.Atom n_s ] ->
    let n = nonnegative "rotate_left" n_s in
    one_arg "rotate_left" (Bv_term.rotate_left ctx decls.Decls.bv_mint ~n)
  | [ Sexp.Atom "_"; Sexp.Atom "rotate_right"; Sexp.Atom n_s ] ->
    let n = nonnegative "rotate_right" n_s in
    one_arg "rotate_right" (Bv_term.rotate_right ctx decls.Decls.bv_mint ~n)
  | [ Sexp.Atom "_"; Sexp.Atom "repeat"; Sexp.Atom n_s ] ->
    let n = nonnegative "repeat" n_s in
    if n < 1 then raise (Malformed "repeat count must be at least one");
    one_arg "repeat" (fun x ->
      let width = bv_width_of_term "repeat" x in
      ignore (checked_bv_product "repeat" width n : int);
      Bv_term.repeat ctx decls.Decls.bv_mint ~n x)
  | _ -> raise (Unsupported "indexed operator is outside the evaluator's BV subset")

and parse_bv_operator ctx decls env op args =
  let parse = parse_term ctx decls env in
  let unary bvop =
    match args with
    | [ a ] -> Bv_term.unop ctx decls.Decls.bv_mint bvop (parse a)
    | _ -> raise (Malformed (op ^ " expects one argument"))
  in
  let binary bvop =
    match args with
    | [ a; b ] -> Bv_term.binop ctx decls.Decls.bv_mint bvop (parse a) (parse b)
    | _ -> raise (Malformed (op ^ " expects two arguments"))
  in
  let left_assoc bvop =
    match args with
    | a :: b :: rest ->
      List.fold_left
        (fun acc x -> Bv_term.binop ctx decls.Decls.bv_mint bvop acc (parse x))
        (Bv_term.binop ctx decls.Decls.bv_mint bvop (parse a) (parse b))
        rest
    | _ -> raise (Malformed (op ^ " expects at least two arguments"))
  in
  match op with
  | "bvnot" -> unary Bv_term.Not
  | "bvneg" -> unary Bv_term.Neg
  | "bvand" -> left_assoc Bv_term.And
  | "bvor" -> left_assoc Bv_term.Or
  | "bvxor" -> left_assoc Bv_term.Xor
  | "bvadd" -> left_assoc Bv_term.Add
  | "bvmul" -> left_assoc Bv_term.Mul
  | "bvsub" -> binary Bv_term.Sub
  | "bvudiv" -> binary Bv_term.Udiv
  | "bvurem" -> binary Bv_term.Urem
  | "bvsdiv" -> binary Bv_term.Sdiv
  | "bvsrem" -> binary Bv_term.Srem
  | "bvsmod" -> binary Bv_term.Smod
  | "bvshl" -> binary Bv_term.Shl
  | "bvlshr" -> binary Bv_term.Lshr
  | "bvashr" -> binary Bv_term.Ashr
  | "bvult" -> binary Bv_term.Ult
  | "bvule" -> binary Bv_term.Ule
  | "bvugt" -> binary Bv_term.Ugt
  | "bvuge" -> binary Bv_term.Uge
  | "bvslt" -> binary Bv_term.Slt
  | "bvsle" -> binary Bv_term.Sle
  | "bvsgt" -> binary Bv_term.Sgt
  | "bvsge" -> binary Bv_term.Sge
  | "bvcomp" -> binary Bv_term.Comp
  | "bvnand" -> binary Bv_term.Nand
  | "bvnor" -> binary Bv_term.Nor
  | "bvxnor" -> binary Bv_term.Xnor
  | "concat" ->
    (match args with
     | [ hi; lo ] ->
       let hi = parse hi
       and lo = parse lo in
       ignore
         (checked_bv_sum
            "concat"
            (bv_width_of_term "concat" hi)
            (bv_width_of_term "concat" lo)
          : int);
       Bv_term.concat ctx decls.Decls.bv_mint hi lo
     | _ -> raise (Malformed "concat expects two arguments"))
  | _ -> raise (Unsupported ("bitvector operator " ^ op))

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
    const_of_decimal ctx ("-" ^ s)
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
  (* Constant factors fold in arbitrary precision (core-bignum W2); never overflows. *)
  let c = List.fold_left Bigint.mul Bigint.one consts in
  match nonconsts with
  | [] -> Context.int_const_big ctx c
  | [ t ] -> Context.mul_const_big ctx c t
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
  | "QF_UF" | "QF_LIA" | "QF_UFLIA" | "QF_IDL" | "QF_RDL" | "QF_BV" | "QF_UFBV" -> true
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
  let env, cap = Env.create_with_cap () in
  let ctx = Context.create env in
  let bv_minter =
    Internal_minter.create ~admit:Bv_term.is_name cap env |> Internal_minter.mint
  in
  let decls = Decls.create bv_minter in
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
         (* No redeclaration (§4.2.1). Sorts have their own namespace, distinct from term
            symbols, so this guards [sorts] only — a sort and a function may legally share
            a name. *)
         if Hashtbl.mem decls.Decls.sorts name
         then raise (Malformed ("declare-sort redeclares an existing sort: " ^ name));
         let sym = Env.declare_sort env name in
         Hashtbl.replace decls.Decls.sorts name (Sort.uninterpreted sym)
       | "declare-sort", _ -> raise (Malformed "declare-sort expects (name arity)")
       | "declare-const", [ name_s; sort_s ] ->
         let name = sym_name name_s in
         (* No redeclaration/redefinition of a term symbol (§4.2.1): reject a name already
            a const, function, or define-fun macro (mirrors the [define-fun] guard, so
            define-then-declare rejects symmetrically). *)
         if Decls.is_defined decls name
         then raise (Malformed ("declare-const redeclares an existing symbol: " ^ name));
         let sort = parse_sort decls sort_s in
         let sym = Env.declare_fun env name (Rank.create [] sort) in
         Hashtbl.replace decls.Decls.consts name (sym, sort)
       | "declare-const", _ -> raise (Malformed "declare-const expects (name sort)")
       | "declare-fun", [ name_s; Sexp.List dom_s; cod_s ] ->
         let name = sym_name name_s in
         if Decls.is_defined decls name
         then raise (Malformed ("declare-fun redeclares an existing symbol: " ^ name));
         let dom = List.map (parse_sort decls) dom_s in
         let cod = parse_sort decls cod_s in
         let rank = Rank.create dom cod in
         let sym = Env.declare_fun env name rank in
         if dom = []
         then Hashtbl.replace decls.Decls.consts name (sym, cod)
         else Hashtbl.replace decls.Decls.funs name (sym, rank)
       | "declare-fun", _ ->
         raise (Malformed "declare-fun expects (name (domain) codomain)")
       | "define-fun", [ name_s; params_s; ret_s; body ] ->
         (* A non-recursive macro (SMT-LIB 2.6 §4.2.2): record signature + unexpanded
            body; expand at each use site (see [expand_macro]). Zero parameters = a named
            constant. The name must be fresh (no redefinition). *)
         let name = sym_name name_s in
         if Decls.is_defined decls name
         then raise (Malformed ("define-fun redefines an existing symbol: " ^ name));
         let params =
           match params_s with
           | Sexp.List ps ->
             List.map
               (function
                 | Sexp.List [ pn_s; psort_s ] -> sym_name pn_s, parse_sort decls psort_s
                 | _ -> raise (Malformed "define-fun parameter must be (name sort)"))
               ps
           | _ -> raise (Malformed "define-fun parameters must be a list")
         in
         let names = List.map fst params in
         if List.length (List.sort_uniq String.compare names) <> List.length names
         then raise (Malformed ("define-fun " ^ name ^ " has duplicate parameter names"));
         let result_sort = parse_sort decls ret_s in
         Hashtbl.replace decls.Decls.macros name { Decls.params; result_sort; body }
       | "define-fun", _ ->
         raise (Malformed "define-fun expects (name ((param sort)...) sort body)")
       | ("define-fun-rec" | "define-funs-rec"), _ ->
         raise (Unsupported "recursive definitions (define-fun-rec / define-funs-rec)")
       | "assert", [ t ] ->
         let term = parse_term ctx decls [] t in
         if not (Sort.equal term.sort Sort.bool)
         then raise (Malformed "asserted term is not Bool-sorted");
         assertions := term :: !assertions
       | "assert", _ -> raise (Malformed "assert expects one term")
       | "check-sat", [] -> ()
       | "check-sat", _ -> raise (Malformed "check-sat takes no arguments")
       | "exit", _ -> stopped := true
       | ("push" | "pop" | "declare-datatypes"), _ -> raise (Unsupported cmd)
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
