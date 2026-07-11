(* SMT-LIB2 -> frozen-API terms, test-only. See parser.mli. All term construction threads
   one Context, so the smart constructors do the sort-checking and normalization for us;
   we only translate syntax and manage declarations + let-scopes. *)

open Oxsmt_core

exception Malformed of string
exception Unsupported of string

let malformedf fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt
let unsupportedf fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type t =
  { env : Env.t
  ; ctx : Context.t
  ; logic : string option
  ; status : Oxsmt_smtlib.Status.t option
  ; assertions : Term.t list
  }

type fundecl =
  { sym : Symbol.t
  ; dom : Sort.t list
  }

(* A [define-fun] macro: parameters (name + sort), declared result sort, and the body as
   an unread s-expression. The body is expanded (capture-avoidingly) at each use site,
   never at definition time — see [expand]. *)
type definition =
  { params : (string * Sort.t) list
  ; ret : Sort.t
  ; body : Sexp.t
  }

type pstate =
  { ctx : Context.t
  ; env : Env.t
  ; sorts : (string, Symbol.t) Hashtbl.t
  ; funs : (string, fundecl) Hashtbl.t
  ; defines : (string, definition) Hashtbl.t
  ; expanding :
      (string, unit) Hashtbl.t (* define names currently mid-expansion (cycle guard) *)
  }

(* ---- sorts ---- *)

let sort_of_sexp st (s : Sexp.t) : Sort.t =
  match s with
  | Sexp.Atom "Bool" -> Sort.bool
  | Sexp.Atom "Int" -> Sort.int
  | Sexp.Atom name ->
    (match Hashtbl.find_opt st.sorts name with
     | Some sym -> Sort.uninterpreted sym
     | None -> malformedf "unknown sort: %s" name)
  | Sexp.List _ ->
    unsupportedf "parametric/compound sorts are not supported: %s" (Sexp.to_string s)
;;

(* ---- numerals ---- *)

let is_numeral s = String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s

let int_lit st a =
  match int_of_string_opt a with
  | Some k -> Context.int_const st.ctx k
  | None -> unsupportedf "integer literal exceeds native int range: %s" a
;;

(* ---- terms ---- *)

(* [scope] is the let-binding stack, innermost first. *)
let rec read_term st scope (s : Sexp.t) : Term.t =
  match s with
  | Sexp.Atom "true" -> Context.bool_const st.ctx true
  | Sexp.Atom "false" -> Context.bool_const st.ctx false
  | Sexp.Atom a when is_numeral a -> int_lit st a
  | Sexp.Atom a ->
    (match List.assoc_opt a scope with
     | Some t -> t
     | None ->
       (match Hashtbl.find_opt st.defines a with
        | Some def -> expand st scope a def []
        | None ->
          (match Hashtbl.find_opt st.funs a with
           | Some { sym; dom = []; _ } -> Context.const st.ctx sym
           | Some { dom = _ :: _; _ } -> malformedf "function %s used without arguments" a
           | None -> malformedf "undeclared symbol: %s" a)))
  | Sexp.List (Sexp.Atom "let" :: rest) -> read_let st scope rest
  (* [(! t :attr ...)] annotation: keep the term, drop the attributes (e.g. :named). *)
  | Sexp.List (Sexp.Atom "!" :: body :: _attrs) -> read_term st scope body
  | Sexp.List (Sexp.Atom op :: args) -> read_app st scope op args s
  | Sexp.List [] -> malformedf "empty application ()"
  | Sexp.List (hd :: _) ->
    unsupportedf "higher-order / non-symbol application head: %s" (Sexp.to_string hd)

and read_let st scope rest =
  match rest with
  | [ Sexp.List bindings; body ] ->
    (* parallel let: definitions see the outer scope, then all extend it at once *)
    let new_scope =
      List.map
        (fun b ->
           match b with
           | Sexp.List [ Sexp.Atom name; def ] -> name, read_term st scope def
           | _ -> malformedf "malformed let binding: %s" (Sexp.to_string b))
        bindings
    in
    read_term st (new_scope @ scope) body
  | _ -> malformedf "malformed let (expected (let (bindings) body))"

and read_app st scope op args orig =
  let rd = read_term st scope in
  let rds () = List.map rd args in
  match op, args with
  | "not", [ a ] -> Context.not_ st.ctx (rd a)
  | "not", _ -> malformedf "not expects 1 argument"
  | "and", _ -> Context.and_ st.ctx (rds ())
  | "or", _ -> Context.or_ st.ctx (rds ())
  | "=>", _ :: _ :: _ -> read_implies st scope args
  | "=>", _ -> malformedf "=> expects >= 2 arguments"
  | "ite", [ c; th; el ] -> Context.ite st.ctx (rd c) (rd th) (rd el)
  | "ite", _ -> malformedf "ite expects 3 arguments"
  | "=", _ :: _ :: _ -> chain st (fun a b -> Context.eq st.ctx a b) (rds ())
  | "=", _ -> malformedf "= expects >= 2 arguments"
  | "distinct", _ :: _ :: _ -> Context.distinct st.ctx (rds ())
  | "distinct", _ -> malformedf "distinct expects >= 2 arguments"
  | "<=", _ :: _ :: _ -> chain st (fun a b -> Context.le st.ctx a b) (rds ())
  | "<", _ :: _ :: _ -> chain st (fun a b -> Context.lt st.ctx a b) (rds ())
  | ">=", _ :: _ :: _ -> chain st (fun a b -> Context.ge st.ctx a b) (rds ())
  | ">", _ :: _ :: _ -> chain st (fun a b -> Context.gt st.ctx a b) (rds ())
  | ("<=" | "<" | ">=" | ">"), _ -> malformedf "%s expects >= 2 arguments" op
  (* Build sums in one [linear_combination] pass rather than left-folding [add]/[sub]: a
     left fold re-normalizes and hash-conses every partial sum (O(n^2) work AND O(n^2)
     interned intermediates — a real memory blowup on wide sums), while
     [linear_combination] merges once. The normalized result is identical. *)
  | "+", _ :: _ -> Context.linear_combination st.ctx (List.map (fun a -> 1, rd a) args) 0
  | "+", [] -> malformedf "+ expects >= 1 argument"
  | "-", [ a ] -> Context.neg st.ctx (rd a)
  | "-", x :: rest ->
    (* a - b - c ... = 1*a + (-1)*b + (-1)*c ... *)
    Context.linear_combination st.ctx ((1, rd x) :: List.map (fun a -> -1, rd a) rest) 0
  | "-", [] -> malformedf "- expects >= 1 argument"
  | "*", _ :: _ -> read_mul st scope args
  | "*", [] -> malformedf "* expects >= 1 argument"
  | "div", [ a; b ] -> Context.div st.ctx (rd a) (rd b)
  | "mod", [ a; b ] -> Context.mod_ st.ctx (rd a) (rd b)
  | ("div" | "mod"), _ -> malformedf "%s expects 2 arguments" op
  | "abs", [ a ] -> Context.abs st.ctx (rd a)
  | "abs", _ -> malformedf "abs expects 1 argument"
  | ("forall" | "exists"), _ -> unsupportedf "quantifiers are not supported (QF only)"
  | _ ->
    (match Hashtbl.find_opt st.defines op with
     | Some def -> expand st scope op def args
     | None ->
       (match Hashtbl.find_opt st.funs op with
        | Some { sym; dom; _ } ->
          let n_expect = List.length dom
          and n_got = List.length args in
          if n_expect <> n_got
          then malformedf "%s applied to %d args, expected %d" op n_got n_expect;
          Context.app st.ctx sym (rds ())
        | None ->
          malformedf "undeclared function or unknown operator: %s" (Sexp.to_string orig)))

(* Expand a [define-fun] use site by capture-avoiding substitution: the argument
   s-expressions are read in the CALLER's [scope] (so they may use the caller's
   let-bindings and globals), then the body is read in a FRESH scope containing ONLY the
   parameters — the caller's locals do not leak into the body, and a nested [let] in the
   body binds tighter than a parameter (both fall out of [read_term]'s innermost-first
   scope lookup). Argument values are already-built [Term.t]s, so substituting them can
   never capture. Recursion (direct or mutual) is rejected via the [expanding] cycle
   guard; SMT-LIB non-rec [define-fun] bodies reference only earlier definitions, so this
   is the only cycle possible. *)
and expand st scope name (def : definition) arg_sexps =
  if Hashtbl.mem st.expanding name
  then unsupportedf "recursive use of define-fun %s is not supported" name;
  let n_expect = List.length def.params
  and n_got = List.length arg_sexps in
  if n_expect <> n_got
  then malformedf "define-fun %s applied to %d args, expected %d" name n_got n_expect;
  let bindings =
    List.map2
      (fun (pname, psort) arg ->
         let t = read_term st scope arg in
         if not (Sort.equal t.Term.sort psort)
         then malformedf "define-fun %s: argument for %s has the wrong sort" name pname;
         pname, t)
      def.params
      arg_sexps
  in
  Hashtbl.replace st.expanding name ();
  let body = read_term st bindings def.body in
  Hashtbl.remove st.expanding name;
  if not (Sort.equal body.Term.sort def.ret)
  then malformedf "define-fun %s body sort differs from declared result sort" name;
  body

(* [(=> a b c)] is right-associative: [a => (b => c)]. *)
and read_implies st scope args =
  match List.rev_map (read_term st scope) args with
  | last :: rest -> List.fold_left (fun acc a -> Context.implies st.ctx a acc) last rest
  | [] -> malformedf "=> expects arguments"

(* Linear multiplication only: at most one non-constant factor (DESIGN §1). Constant
   factors fold into a coefficient via [mul_const]; two or more non-constants is nonlinear
   and unsupported. *)
and read_mul st scope args =
  let ts = List.map (read_term st scope) args in
  let consts, nonconsts =
    List.partition_map
      (fun (t : Term.t) ->
         match t.node with
         | Term.Int_const k -> Either.Left k
         | _ -> Either.Right t)
      ts
  in
  match nonconsts with
  | _ :: _ :: _ -> unsupportedf "nonlinear multiplication (>= 2 non-constant factors)"
  | _ ->
    let base =
      match nonconsts with
      | [ t ] -> t
      | _ -> Context.int_const st.ctx 1
    in
    List.fold_left (fun acc k -> Context.mul_const st.ctx k acc) base consts

(* [(rel a b c ...)] means the conjunction of consecutive pairs. *)
and chain st mk ts =
  match ts with
  | a :: (_ :: _ as rest) ->
    let rec loop prev = function
      | [] -> []
      | x :: tl -> mk prev x :: loop x tl
    in
    (match loop a rest with
     | [ one ] -> one
     | many -> Context.and_ st.ctx many)
  | _ -> malformedf "chained relation needs >= 2 arguments"
;;

(* ---- commands ---- *)

let declare_sort st name =
  if Hashtbl.mem st.sorts name then malformedf "redeclaration of sort %s" name;
  match Env.declare_sort st.env name with
  | sym -> Hashtbl.replace st.sorts name sym
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

let declare_fun st name dom cod =
  if Hashtbl.mem st.funs name || Hashtbl.mem st.defines name
  then malformedf "redeclaration of symbol %s" name;
  match Env.declare_fun st.env name (Rank.create dom cod) with
  | sym -> Hashtbl.replace st.funs name { sym; dom }
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

(* [(define-fun name ((p S)...) Ret body)]: a MACRO. We parse the parameter/result sorts
   now (so undeclared sorts fail here) but store the body unread — it is expanded at each
   use site (see [expand]). define-fun names share the function namespace, so they collide
   with declares and each other; [div]/[mod] stay reserved. *)
let define_fun st name params_sexp ret_sexp body =
  if Hashtbl.mem st.funs name || Hashtbl.mem st.defines name
  then malformedf "redeclaration of symbol %s" name;
  if String.equal name "div" || String.equal name "mod"
  then malformedf "cannot define reserved symbol %s" name;
  let params =
    List.map
      (fun p ->
         match p with
         | Sexp.List [ Sexp.Atom pn; psort ] -> pn, sort_of_sexp st psort
         | _ -> malformedf "malformed define-fun parameter: %s" (Sexp.to_string p))
      params_sexp
  in
  let ret = sort_of_sexp st ret_sexp in
  Hashtbl.replace st.defines name { params; ret; body }
;;

let read_signature st (params : Sexp.t) (ret : Sexp.t) =
  let dom =
    match params with
    | Sexp.List ps -> List.map (sort_of_sexp st) ps
    | _ -> malformedf "declare-fun parameter list must be a list"
  in
  dom, sort_of_sexp st ret
;;

let known_logic = function
  | "QF_UFLIA" | "QF_UF" | "QF_LIA" | "QF_IDL" | "QF_RDL" -> true
  | _ -> false
;;

let run st sexps =
  let logic = ref None in
  let status = ref None in
  let asserts = ref [] in
  List.iter
    (fun (cmd : Sexp.t) ->
       match cmd with
       | Sexp.List [ Sexp.Atom "set-logic"; Sexp.Atom l ] ->
         if known_logic l
         then logic := Some l
         else unsupportedf "unsupported logic: %s (need QF_UF/QF_LIA/QF_UFLIA)" l
       | Sexp.List (Sexp.Atom "set-info" :: rest) ->
         (match rest with
          | [ Sexp.Atom ":status"; Sexp.Atom v ] ->
            (match Oxsmt_smtlib.Status.of_string v with
             | Some s -> status := Some s
             | None -> malformedf "unknown :status value: %s" v)
          | _ -> () (* ignore other :info, incl. multi-line |...| :source *))
       | Sexp.List [ Sexp.Atom "declare-sort"; Sexp.Atom name; Sexp.Atom "0" ] ->
         declare_sort st name
       | Sexp.List [ Sexp.Atom "declare-sort"; Sexp.Atom name; _ ] ->
         unsupportedf "declare-sort %s with nonzero arity" name
       | Sexp.List [ Sexp.Atom "declare-const"; Sexp.Atom name; ret ] ->
         declare_fun st name [] (sort_of_sexp st ret)
       | Sexp.List [ Sexp.Atom "declare-fun"; Sexp.Atom name; params; ret ] ->
         let dom, cod = read_signature st params ret in
         declare_fun st name dom cod
       | Sexp.List [ Sexp.Atom "define-fun"; Sexp.Atom name; Sexp.List params; ret; body ]
         -> define_fun st name params ret body
       | Sexp.List (Sexp.Atom ("define-fun-rec" | "define-funs-rec") :: _) ->
         unsupportedf "recursive define-fun-rec / define-funs-rec is not supported"
       | Sexp.List [ Sexp.Atom "assert"; body ] ->
         let t = read_term st [] body in
         if not (Sort.equal t.Term.sort Sort.bool) then malformedf "assertion is not Bool";
         asserts := t :: !asserts
       | Sexp.List (Sexp.Atom "check-sat" :: _) -> ()
       | Sexp.List (Sexp.Atom "exit" :: _) -> ()
       | Sexp.List (Sexp.Atom ("push" | "pop") :: _) ->
         unsupportedf "incremental push/pop is not supported"
       | Sexp.List (Sexp.Atom "define-fun" :: _) ->
         malformedf "malformed define-fun: %s" (Sexp.to_string cmd)
       | Sexp.List (Sexp.Atom ("get-model" | "get-value" | "get-unsat-core") :: _) -> ()
       | Sexp.List (Sexp.Atom ("set-option" | "reset" | "reset-assertions") :: _) -> ()
       | Sexp.Atom a -> malformedf "unexpected top-level atom: %s" a
       | Sexp.List (Sexp.Atom other :: _) -> unsupportedf "unsupported command: %s" other
       | Sexp.List _ -> malformedf "malformed command: %s" (Sexp.to_string cmd))
    sexps;
  !logic, !status, List.rev !asserts
;;

let parse_into env ctx src =
  let st =
    { ctx
    ; env
    ; sorts = Hashtbl.create 16
    ; funs = Hashtbl.create 64
    ; defines = Hashtbl.create 16
    ; expanding = Hashtbl.create 8
    }
  in
  let sexps =
    try Sexp.parse_many src with
    | Sexp.Malformed m -> raise (Malformed ("s-expression: " ^ m))
  in
  let logic, status, assertions =
    try run st sexps with
    | Term.Sort_error m -> raise (Malformed ("sort error: " ^ m))
    | Term.Unsupported m -> raise (Unsupported m)
    | Term.Overflow -> raise (Unsupported "arithmetic exceeds native int range")
  in
  { env; ctx; logic; status; assertions }
;;

let parse src =
  let env = Env.create () in
  let ctx = Context.create env in
  parse_into env ctx src
;;
