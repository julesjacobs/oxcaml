(* SMT-LIB2 -> frozen-API terms, test-only. See parser.mli. All term construction threads
   one Context, so the smart constructors do the sort-checking and normalization for us;
   we only translate syntax and manage declarations + let-scopes. *)

open Oxsmt_core

exception Malformed of string
exception Unsupported of string

let malformedf fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt
let unsupportedf fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

let name_of s =
  match Sexp.symbol_name s with
  | Some n -> n
  | None -> malformedf "expected a symbol name, got %s" (Sexp.to_string s)
;;

(* The internal function-symbol name minted for a constructor's tester [(_ is C)].
   Readable and deterministic; a user symbol colliding with it is caught as a
   redeclaration by [declare_fun]. *)
let tester_name_of cname = "is-" ^ cname

(* A universally-quantified assertion, parsed from [(assert (forall (binders) body))]. The
   parser cannot BUILD the lemma itself: the bound variables must be minted as cap-gated
   placeholder qvars through the {!Oxsmt_interface.Session} (ADR-0012 §1.3
   mint-before-build), which lives in a library this test-only parser must not depend on.
   So instead the parser records the binders it read and a deferred [build] closure: the
   driver mints one qvar per binder, hands their {!Term.t} images to [build] in binder
   order, and [build] reads the body (and any [:pattern] triggers) with each binder name
   bound to its qvar image. The body is read lazily inside [build] rather than eagerly,
   because it is only well-sorted once the qvar images exist. *)
type lemma_src =
  { qvars :
      (string * Sort.t) list (* forall binders, outer-first then inner (flattened) *)
  ; build : Term.t array -> Term.t * Term.t list list
    (* [build qvar_images] is [(body, triggers)]: [qvar_images.(k)] is the term to
     substitute for the k-th binder. May raise {!Malformed}/{!Unsupported} (a body op
     outside the subset), which the driver maps to a sound [unknown]. *)
  }

type t =
  { env : Env.t
  ; ctx : Context.t
  ; logic : string option
  ; status : Oxsmt_smtlib.Status.t option
  ; assertions : Term.t list
  ; datatypes : Datatype_defs.t
    (* the algebraic-datatype shape declared by [declare-datatype(s)], for the datatype
         theory; empty when the query declares none *)
  ; lemmas : lemma_src list (* the [(assert (forall ...))] assertions, in file order *)
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
  ; memo : (string * int list, Term.t) Hashtbl.t
    (* expansion cache keyed by (define name, argument-term tags). A define with the
         same arguments always expands to the same hash-consed term, so this turns the
         exponential body re-read on nested chains (e.g. [f_{i+1}(x) = f_i(x) + f_i(x)])
         into linear work. Tags are the [Context] hash-cons identity, so the key is exact
         and cheap. *)
  ; dt_names : (string, unit) Hashtbl.t
    (* sort names introduced by [declare-datatype(s)]: [sort_of_sexp] resolves these to
         [Sort.datatype_] rather than [Sort.uninterpreted] *)
  ; mutable datatypes : Datatype_defs.t (* the accumulated datatype shape registry *)
  }

module Tok = Oxsmt_lexical.Lexer

(* ---- sorts ---- *)

let sort_of_sexp st (s : Sexp.t) : Sort.t =
  match Sexp.symbol_name s with
  (* [Bool]/[Int] are the builtin sorts regardless of quoting (quoting is lexical). *)
  | Some "Bool" -> Sort.bool
  | Some "Int" -> Sort.int
  | Some name ->
    (match Hashtbl.find_opt st.sorts name with
     | Some sym ->
       if Hashtbl.mem st.dt_names name then Sort.datatype_ sym else Sort.uninterpreted sym
     | None -> malformedf "unknown sort: %s" name)
  | None ->
    (match s with
     | Sexp.List _ ->
       unsupportedf "parametric/compound sorts are not supported: %s" (Sexp.to_string s)
     | _ -> malformedf "expected a sort, got %s" (Sexp.to_string s))
;;

(* ---- numerals ---- *)

let int_lit st a =
  match int_of_string_opt a with
  | Some k -> Context.int_const st.ctx k
  | None ->
    (* Exceeds int63 — build an arbitrary-precision constant (core-bignum W2). SMT-LIB
       numerals are canonical decimal (no sign, no leading zeros), which is exactly
       [Bigint.of_string]'s grammar; a genuinely malformed token is a parse error. *)
    (match Bigint.of_string a with
     | b -> Context.int_const_big st.ctx b
     | exception Invalid_argument _ -> malformedf "malformed integer literal: %s" a)
;;

(* ---- terms ---- *)

(* [scope] is the let-binding stack, innermost first. Matching is on the shared lexer's
   token KINDS, so a quoted [|0|]/[|let|] is a symbol looked up by name — never the
   numeral [0] or the [let] keyword (the ADR-0008 boundary invariant, enforced
   end-to-end). *)
(* F2 (codex): validate a [(! body attr...)] annotation TAIL is well-formed —
   [attr = :keyword | :keyword value]. The tail is otherwise discarded, so an unvalidated
   drop can silently delete content: [(! true (forall ((x Int)) false))] parses to [true],
   dropping the [forall] (a wrong [sat]). Rejecting a non-keyword in attribute-name
   position turns that malformed input into [Malformed] -> unknown. A well-formed tail
   ([:named foo], [:pattern (...)], bare [:foo]) validates and is still dropped. *)
let is_keyword_atom = function
  | Sexp.Atom (Tok.Keyword _) -> true
  | _ -> false
;;

let rec validate_bang_attrs = function
  | [] -> ()
  | Sexp.Atom (Tok.Keyword _) :: rest ->
    (* consume the optional single attribute value (any non-keyword s-expr) *)
    let rest =
      match rest with
      | v :: more when not (is_keyword_atom v) -> more
      | _ -> rest
    in
    validate_bang_attrs rest
  | other :: _ ->
    malformedf
      "malformed (! ...) annotation: expected a :keyword attribute, got %s"
      (Sexp.to_string other)
;;

let rec read_term st scope (s : Sexp.t) : Term.t =
  match s with
  | Sexp.Atom tok -> read_atom st scope tok
  | Sexp.List (Sexp.Atom (Tok.Reserved "let") :: rest) -> read_let st scope rest
  (* [(! t :attr ...)] annotation: validate the attribute tail (F2), then keep the term
     and drop the (well-formed) attributes (e.g. :named). *)
  | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: body :: attrs) ->
    validate_bang_attrs attrs;
    read_term st scope body
  | Sexp.List (head :: args) -> read_app st scope head args s
  | Sexp.List [] -> malformedf "empty application ()"

and read_atom st scope (tok : Tok.token) : Term.t =
  match tok with
  | Tok.Numeral n -> int_lit st n
  | Tok.Decimal d -> unsupportedf "decimal (real) literal is not in QF_UFLIA: %s" d
  | Tok.Hex h -> unsupportedf "bitvector literal #x%s is not supported" h
  | Tok.Binary b -> unsupportedf "bitvector literal #b%s is not supported" b
  | Tok.String s -> malformedf "unexpected string literal in term position: %S" s
  | Tok.Keyword k -> malformedf "unexpected keyword :%s in term position" k
  | Tok.Reserved r -> malformedf "unexpected reserved word %s in term position" r
  | Tok.Lparen | Tok.Rparen -> malformedf "internal: paren token as atom"
  (* [true]/[false] are the booleans only UNQUOTED; [|true|] is a symbol named "true". *)
  | Tok.Symbol { text = "true"; quoted = false } -> Context.bool_const st.ctx true
  | Tok.Symbol { text = "false"; quoted = false } -> Context.bool_const st.ctx false
  | Tok.Symbol { text = name; _ } ->
    (match List.assoc_opt name scope with
     | Some t -> t
     | None ->
       (match Hashtbl.find_opt st.defines name with
        | Some def -> expand st scope name def []
        | None ->
          (match Hashtbl.find_opt st.funs name with
           | Some { sym; dom = []; _ } -> Context.const st.ctx sym
           | Some { dom = _ :: _; _ } ->
             malformedf "function %s used without arguments" name
           | None -> malformedf "undeclared symbol: %s" name)))

and read_let st scope rest =
  match rest with
  | [ Sexp.List bindings; body ] ->
    (* parallel let: definitions see the outer scope, then all extend it at once *)
    let new_scope =
      List.map
        (fun b ->
           match b with
           | Sexp.List [ name; def ] ->
             (match Sexp.symbol_name name with
              | Some n -> n, read_term st scope def
              | None -> malformedf "malformed let binding name: %s" (Sexp.to_string name))
           | _ -> malformedf "malformed let binding: %s" (Sexp.to_string b))
        bindings
    in
    read_term st (new_scope @ scope) body
  | _ -> malformedf "malformed let (expected (let (bindings) body))"

(* The application head selects interpretation. Only an UNQUOTED symbol can be a builtin
   operator; a quoted [|+|] head (or a reserved word) is never an operator. *)
and read_app st scope head args orig =
  match head with
  (* F1 (codex): a let-/qvar-bound variable in HEAD position. [scope] must be consulted
     for the head, not just [read_atom] — SMT-LIB scoping shadows a global function of the
     same name, and a scalar-bound name is not applicable, so [(x args)] is ill-sorted.
     Resolving it to the shadowed global instead (the pre-fix behaviour) mis-parses the
     body and can yield a definite verdict on ill-typed input (a wrong [unsat] when a
     refuting lemma body is mis-built). Fail closed: reject -> Malformed -> unknown.
     Binder-agnostic (any scope entry: [let] or a lemma qvar). *)
  | Sexp.Atom (Tok.Symbol { text; _ }) when List.mem_assoc text scope ->
    malformedf "bound variable %s cannot head an application (ill-sorted)" text
  | Sexp.Atom (Tok.Symbol { text = op; quoted = false }) -> read_op st scope op args orig
  | Sexp.Atom (Tok.Symbol { text = op; quoted = true }) ->
    apply_named st scope op args orig
  (* [(as t Sort)] sort ascription (e.g. [(as nil nat)]): read the term and CHECK the
     ascription against its actual sort, rejecting a mismatch (codex) rather than dropping
     it — a silently-ignored ascription could let a wrong-sorted term through. A
     parametric / compound ascription [sort_of_sexp] cannot model raises [Unsupported] (->
     unknown), which is sound: we abstain rather than skip the check. *)
  | Sexp.Atom (Tok.Reserved "as") ->
    (match args with
     | [ t; sort_s ] ->
       let term = read_term st scope t in
       let ascribed = sort_of_sexp st sort_s in
       if not (Sort.equal term.Term.sort ascribed)
       then
         malformedf
           "(as ...) sort ascription %s does not match the term's sort"
           (Sexp.to_string sort_s);
       term
     | _ -> malformedf "malformed (as term sort): %s" (Sexp.to_string orig))
  | Sexp.Atom (Tok.Reserved ("forall" | "exists")) ->
    unsupportedf "quantifiers are not supported (QF only)"
  | Sexp.Atom (Tok.Reserved "match") -> unsupportedf "datatype match is not supported yet"
  | Sexp.Atom (Tok.Reserved r) ->
    malformedf "reserved word %s cannot head an application" r
  (* Tester [((_ is C) t)]: an indexed identifier heads the application. Resolve to the
     constructor [C]'s tester symbol (registered by [declare-datatype(s)]) and apply. *)
  | Sexp.List
      [ Sexp.Atom (Tok.Reserved "_"); Sexp.Atom (Tok.Symbol { text = "is"; _ }); cname_s ]
    -> read_tester st scope (name_of cname_s) args orig
  | _ ->
    unsupportedf "higher-order / non-symbol application head: %s" (Sexp.to_string head)

(* A tester application [((_ is C) t)]. The tester function symbol is minted as
   ["is-" ^ C] by [declare-datatype(s)]; look it up and apply it (it is a [(dt) -> Bool]
   predicate). *)
and read_tester st scope cname args orig =
  let tester_name = tester_name_of cname in
  match Hashtbl.find_opt st.funs tester_name with
  | None -> malformedf "tester for unknown constructor %s: %s" cname (Sexp.to_string orig)
  | Some { sym; dom; _ } ->
    if List.length args <> List.length dom
    then malformedf "tester (_ is %s) expects 1 argument" cname;
    Context.app st.ctx sym (List.map (read_term st scope) args)

(* Apply a user-declared function or expand a define-fun (no builtin-operator meaning). *)
and apply_named st scope op args orig =
  match Hashtbl.find_opt st.defines op with
  | Some def -> expand st scope op def args
  | None ->
    (match Hashtbl.find_opt st.funs op with
     | Some { sym; dom; _ } ->
       let n_expect = List.length dom
       and n_got = List.length args in
       if n_expect <> n_got
       then malformedf "%s applied to %d args, expected %d" op n_got n_expect;
       Context.app st.ctx sym (List.map (read_term st scope) args)
     | None ->
       malformedf "undeclared function or unknown operator: %s" (Sexp.to_string orig))

and read_op st scope op args orig =
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
  (* not a builtin operator — a user-declared function or a define-fun *)
  | _ -> apply_named st scope op args orig

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
  let key = name, List.map (fun (_, (t : Term.t)) -> t.tag) bindings in
  match Hashtbl.find_opt st.memo key with
  | Some cached -> cached
  | None ->
    (* Cycle guard stays live across the body read: recursion re-enters [expand] with the
       same [name] before this key is cached, so it is caught here, not memoized. *)
    Hashtbl.replace st.expanding name ();
    let body = read_term st bindings def.body in
    Hashtbl.remove st.expanding name;
    if not (Sort.equal body.Term.sort def.ret)
    then malformedf "define-fun %s body sort differs from declared result sort" name;
    Hashtbl.replace st.memo key body;
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
    (* [k] is a [Bigint.t] coefficient (possibly > int63); fold with the
       arbitrary-precision [mul_const_big]. *)
    List.fold_left (fun acc k -> Context.mul_const_big st.ctx k acc) base consts

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

(* ---- quantifiers (ADR-0012 lemma tier) ---- *)

(* A quantifier binder list [((x S) (y T) ...)] -> [(name, sort)] pairs. Sorts are read
   eagerly (so an out-of-subset binder sort — e.g. an array — fails at parse time, exactly
   as a top-level declaration would). *)
let read_binders st (binders : Sexp.t) : (string * Sort.t) list =
  match binders with
  | Sexp.List bs ->
    List.map
      (fun b ->
         match b with
         | Sexp.List [ nm; srt ] ->
           (match Sexp.symbol_name nm with
            | Some n -> n, sort_of_sexp st srt
            | None ->
              malformedf "malformed quantifier binder name: %s" (Sexp.to_string nm))
         | _ -> malformedf "malformed quantifier binder: %s" (Sexp.to_string b))
      bs
  | _ -> malformedf "quantifier binder list must be a list"
;;

(* Pull the [:pattern (t1 t2 ...)] annotations out of a [(! body :attr ... )] attribute
   tail. Each [:pattern] is one CONJUNCTIVE multi-trigger (its terms must all match under
   one substitution); several [:pattern]s are ALTERNATIVE triggers (any may fire). Other
   attributes (e.g. [:qid], [:named], [:weight]) are ignored. Returns the pattern groups
   as raw s-expressions, read (with the qvars in scope) only inside [build]. *)
let rec extract_patterns (attrs : Sexp.t list) : Sexp.t list list =
  match attrs with
  | Sexp.Atom (Tok.Keyword "pattern") :: Sexp.List pats :: rest ->
    pats :: extract_patterns rest
  | Sexp.Atom (Tok.Keyword _) :: _value :: rest -> extract_patterns rest
  | _ :: rest -> extract_patterns rest
  | [] -> []
;;

(* Peel nested [forall]s into one flat binder list (forall x. forall y. P == forall
   x y. P) and return the innermost body together with its trigger groups. Only universal
   quantifiers are flattened; an [exists] anywhere (including alternation under a forall)
   is out of the fragment. [(! body :pattern ...)] on the innermost body supplies the
   triggers. *)
let rec collect_forall st acc (tail : Sexp.t list) =
  match tail with
  | [ binders; body ] ->
    let acc = acc @ read_binders st binders in
    (match body with
     | Sexp.List (Sexp.Atom (Tok.Reserved "forall") :: inner) ->
       collect_forall st acc inner
     | Sexp.List (Sexp.Atom (Tok.Reserved "exists") :: _) ->
       unsupportedf "existential quantifier is not supported (universal lemmas only)"
     | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: core :: attrs) ->
       acc, core, extract_patterns attrs
     | _ -> acc, body, [])
  | _ -> malformedf "malformed forall (expected (forall (binders) body))"
;;

(* Parse [(assert (forall ...))] into a {!lemma_src}. The binders are read now; the body
   and triggers are read lazily by [build], with each binder name bound (innermost-first,
   so an inner binder shadows an outer one of the same name) to its minted qvar image. *)
let read_forall st (tail : Sexp.t list) : lemma_src =
  let qvars, body_sexp, trigger_sexps = collect_forall st [] tail in
  let build qvar_images =
    let scope =
      List.rev (List.mapi (fun i (name, _sort) -> name, qvar_images.(i)) qvars)
    in
    let body = read_term st scope body_sexp in
    let triggers = List.map (List.map (read_term st scope)) trigger_sexps in
    body, triggers
  in
  { qvars; build }
;;

(* ---- commands ---- *)

(* Reject user declarations in the reserved fresh-symbol namespace (board #48): a user
   symbol named ".oxsmt.*" would collide with a symbol preprocessing invents, which is
   unsound. Single source of truth = {!Oxsmt_core.Env} (ADR-0012 F1): this parser links
   [oxsmt_core], so it references [Env.is_reserved_name] directly rather than keeping a
   local copy of the prefix (retiring the two-copies drift the old note warned about). *)
let check_not_reserved name =
  if Env.is_reserved_name name
  then
    malformedf
      "declaration of reserved internal symbol %s (%s* is preprocessing-only)"
      name
      Env.reserved_prefix
;;

let declare_sort st name =
  check_not_reserved name;
  if Hashtbl.mem st.sorts name then malformedf "redeclaration of sort %s" name;
  match Env.declare_sort st.env name with
  | sym -> Hashtbl.replace st.sorts name sym
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

let declare_fun st name dom cod =
  check_not_reserved name;
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
         | Sexp.List [ pn; psort ] ->
           (match Sexp.symbol_name pn with
            | Some pn -> pn, sort_of_sexp st psort
            | None ->
              malformedf "malformed define-fun parameter name: %s" (Sexp.to_string pn))
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

(* ---- datatypes ---- *)

(* Declare (and store) a function symbol, returning its interned [Symbol.t]. *)
let declare_fun_sym st name dom cod =
  declare_fun st name dom cod;
  (Hashtbl.find st.funs name).sym
;;

(* Intern a datatype sort name (phase 1), marking it so [sort_of_sexp] renders it as
   [Sort.datatype_]. *)
let declare_datatype_sort st name =
  check_not_reserved name;
  if Hashtbl.mem st.sorts name then malformedf "redeclaration of sort %s" name;
  match Env.declare_sort st.env name with
  | sym ->
    Hashtbl.replace st.sorts name sym;
    Hashtbl.replace st.dt_names name ();
    sym
  | exception Env.Reserved_symbol _ -> malformedf "cannot declare reserved symbol %s" name
;;

(* Parse one constructor definition [(C (sel1 S1) ... (seln Sn))] (nullary: [(C)]).
   Declares the constructor [(S1..Sn) -> dt], each selector [(dt) -> Si], and the tester
   [(dt) -> Bool], and returns the {!Datatype_defs.constructor} shape. Field sorts resolve
   through [sort_of_sexp], so they may reference this datatype or a sibling already
   interned in phase 1 (mutual recursion). *)
let parse_constructor st dt_sort (cdef : Sexp.t) : Datatype_defs.constructor =
  match cdef with
  | Sexp.List (cname_s :: sel_sexps) ->
    let cname = name_of cname_s in
    let selectors =
      List.mapi
        (fun index sel ->
           match sel with
           | Sexp.List [ sname_s; ssort_s ] ->
             name_of sname_s, index, sort_of_sexp st ssort_s
           | _ ->
             malformedf
               "malformed selector in constructor %s: %s"
               cname
               (Sexp.to_string sel))
        sel_sexps
    in
    let dom = List.map (fun (_, _, fs) -> fs) selectors in
    let ctor_sym = declare_fun_sym st cname dom dt_sort in
    let sel_records =
      List.map
        (fun (sname, index, field_sort) ->
           let sel_sym = declare_fun_sym st sname [ dt_sort ] field_sort in
           { Datatype_defs.sym = sel_sym; index; field_sort })
        selectors
    in
    let tester_sym = declare_fun_sym st (tester_name_of cname) [ dt_sort ] Sort.bool in
    { Datatype_defs.sym = ctor_sym; selectors = sel_records; tester = tester_sym }
  | _ -> malformedf "malformed constructor definition: %s" (Sexp.to_string cdef)
;;

(* Shared core of [declare-datatype] (one datatype) and [declare-datatypes] (mutually
   recursive block). [sort_decls] are the [(name arity)] pairs, [ctor_lists] the parallel
   constructor-definition lists. Phase 1 interns every sort name first so phase 2's field
   sorts can reference any of them. *)
let process_datatypes st sort_decls ctor_lists =
  if List.length sort_decls <> List.length ctor_lists
  then
    malformedf
      "declare-datatypes: %d sort declarations but %d constructor lists"
      (List.length sort_decls)
      (List.length ctor_lists);
  let sort_syms =
    List.map
      (fun (name, arity) ->
         (match arity with
          | Sexp.Atom (Tok.Numeral "0") -> ()
          | _ ->
            unsupportedf "parametric datatype %s (nonzero arity) is not supported" name);
         name, declare_datatype_sort st name)
      sort_decls
  in
  List.iter2
    (fun (name, sort_sym) ctor_list ->
       let dt_sort = Sort.datatype_ sort_sym in
       let constructors =
         match ctor_list with
         (* A datatype with zero constructors is uninhabited and not well-formed SMT-LIB
           (SMT-LIB 2.6 requires >= 1 constructor); reject rather than register an empty,
           value-less datatype. *)
         | Sexp.List [] -> malformedf "datatype %s has no constructors" name
         | Sexp.List cs -> List.map (parse_constructor st dt_sort) cs
         | _ -> malformedf "malformed constructor list for datatype %s" name
       in
       match Datatype_defs.add st.datatypes { sort_sym; constructors } with
       | dts -> st.datatypes <- dts
       | exception Invalid_argument m -> malformedf "%s" m)
    sort_syms
    ctor_lists
;;

(* The sort-declaration list [((n0 a0) (n1 a1) ...)] of a declare-datatypes block. *)
let parse_sort_decls (s : Sexp.t) =
  match s with
  | Sexp.List decls ->
    List.map
      (fun d ->
         match d with
         | Sexp.List [ n; a ] -> name_of n, a
         | _ -> malformedf "malformed datatype sort declaration: %s" (Sexp.to_string d))
      decls
  | _ -> malformedf "declare-datatypes sort list must be a list: %s" (Sexp.to_string s)
;;

(* Logics we accept at [set-logic]. The quantifier-free family is fully modelled. The
   QUANTIFIED family (UF/LIA + the array/real/datatype supersets present in the public
   corpora) is accepted at the NAME level so the lemma pipeline (ADR-0012) is reached —
   but acceptance of the name is not a promise to model the theory: any construct outside
   the subset (an array/real/datatype sort, a decimal literal, a nonlinear product) still
   fails downstream (a parse-level [Unsupported], or a runtime degrade to [unknown] for a
   lemma body). This is fail-closed by CONSTRUCT, not by logic name — a UFLIA file using
   only UF + linear integers + universals solves; an AUFLIA file touching arrays degrades.
   Universals only: an [exists] in the body is rejected as {!Unsupported} regardless. *)
let known_logic = function
  | "QF_UFLIA" | "QF_UF" | "QF_LIA" | "QF_IDL" | "QF_RDL" | "QF_DT" | "QF_UFDT" -> true
  (* quantified UF/LIA family + array/real/datatype supersets seen in ../corpora *)
  | "UF"
  | "UFLIA"
  | "UFIDL"
  | "UFLRA"
  | "LIA"
  | "LRA"
  | "AUFLIA"
  | "AUFLIRA"
  | "ALIA"
  | "AUFDTLIA"
  | "UFDT"
  | "UFDTLIA" -> true
  | _ -> false
;;

let run st sexps =
  let logic = ref None in
  let status = ref None in
  let asserts = ref [] in
  let lemmas = ref [] in
  (* Classify an assertion body: a top-level [forall] (optionally wrapped in a [(! ...)]
     annotation, e.g. [:named]) becomes a lemma; anything else is a ground term. A
     [forall] nested INSIDE a term (not the assertion root) stays out of the fragment —
     [read_term] rejects it — so only assertion-root universals are lifted to lemmas. *)
  let rec classify (b : Sexp.t) =
    match b with
    | Sexp.List (Sexp.Atom (Tok.Reserved "forall") :: tail) ->
      `Lemma (read_forall st tail)
    | Sexp.List (Sexp.Atom (Tok.Reserved "!") :: inner :: attrs) ->
      (* F2: validate the annotation tail before dropping it — otherwise a malformed tail
         (e.g. a bare [forall] mistaken for an attribute) is silently discarded and can
         drop a live quantifier (wrong [sat]). *)
      validate_bang_attrs attrs;
      classify inner
    | _ ->
      let t = read_term st [] b in
      if not (Sort.equal t.Term.sort Sort.bool) then malformedf "assertion is not Bool";
      `Ground t
  in
  (* Extract a declared name (any symbol atom, quoted or not). *)
  let name_of s =
    match Sexp.symbol_name s with
    | Some n -> n
    | None -> malformedf "expected a symbol name, got %s" (Sexp.to_string s)
  in
  List.iter
    (fun (cmd : Sexp.t) ->
       (* Command keywords are UNQUOTED symbol heads; dispatch on that text. *)
       match cmd with
       | Sexp.Atom _ -> malformedf "unexpected top-level atom: %s" (Sexp.to_string cmd)
       | Sexp.List [] -> malformedf "malformed command: ()"
       | Sexp.List (head :: rest) ->
         (match Sexp.simple head, rest with
          | Some "set-logic", [ l ] ->
            (match Sexp.simple l with
             | Some l when known_logic l -> logic := Some l
             | Some l ->
               unsupportedf "unsupported logic: %s (need QF_UF/QF_LIA/QF_UFLIA)" l
             | None -> malformedf "malformed set-logic argument")
          | Some "set-info", _ ->
            (match rest with
             | [ Sexp.Atom (Tok.Keyword "status"); v ] ->
               (match Sexp.simple v with
                | Some v ->
                  (match Oxsmt_smtlib.Status.of_string v with
                   | Some s -> status := Some s
                   | None -> malformedf "unknown :status value: %s" v)
                | None -> malformedf "malformed :status value")
             | _ -> () (* ignore other :info, incl. multi-line |...| / string values *))
          | Some "declare-sort", [ n; arity ] ->
            (match arity with
             | Sexp.Atom (Tok.Numeral "0") -> declare_sort st (name_of n)
             | _ -> unsupportedf "declare-sort %s with nonzero arity" (name_of n))
          | Some "declare-const", [ n; ret ] ->
            declare_fun st (name_of n) [] (sort_of_sexp st ret)
          | Some "declare-fun", [ n; params; ret ] ->
            let dom, cod = read_signature st params ret in
            declare_fun st (name_of n) dom cod
          (* [(declare-datatypes ((T0 a0) ...) (ctor-list0 ...))] — mutually recursive. *)
          | Some "declare-datatypes", [ sort_decls; Sexp.List ctor_lists ] ->
            process_datatypes st (parse_sort_decls sort_decls) ctor_lists
          | Some "declare-datatypes", _ ->
            malformedf "malformed declare-datatypes: %s" (Sexp.to_string cmd)
          (* [(declare-datatype T (ctor ...))] — the single-datatype (arity 0) form. *)
          | Some "declare-datatype", [ n; Sexp.List ctors ] ->
            process_datatypes
              st
              [ name_of n, Sexp.Atom (Tok.Numeral "0") ]
              [ Sexp.List ctors ]
          | Some "declare-datatype", _ ->
            malformedf "malformed declare-datatype: %s" (Sexp.to_string cmd)
          | Some "define-fun", [ n; Sexp.List params; ret; body ] ->
            define_fun st (name_of n) params ret body
          | Some ("define-fun-rec" | "define-funs-rec"), _ ->
            unsupportedf "recursive define-fun-rec / define-funs-rec is not supported"
          | Some "define-fun", _ ->
            malformedf "malformed define-fun: %s" (Sexp.to_string cmd)
          | Some "assert", [ body ] ->
            (match classify body with
             | `Ground t -> asserts := t :: !asserts
             | `Lemma l -> lemmas := l :: !lemmas)
          | Some "check-sat", _ -> ()
          | Some "exit", _ -> ()
          | Some ("push" | "pop"), _ ->
            unsupportedf "incremental push/pop is not supported"
          | Some ("get-model" | "get-value" | "get-unsat-core"), _ -> ()
          (* Output-only / non-stateful directives: ignoring them cannot change the
            assertion set, hence cannot flip a verdict. *)
          | Some "set-option", _ -> ()
          | Some (("reset" | "reset-assertions") as c), _ ->
            (* Fail CLOSED — NOT a silent no-op. This reader folds every [assert] into ONE
              assertion set for a single [check-sat], so it cannot honour [reset] /
              [reset-assertions] clearing that set mid-script. Silently ignoring them left
              the pre-reset assertions live and FLIPPED the verdict (e.g.
              [(assert (= 0 1)) (reset-assertions) (check-sat)] is [sat] but came out
              [unsat]). Raising degrades the query to [unknown] (I8), never a wrong
              verdict; incremental support is a documented follow-up (see the CLI's
              push/pop degrade). *)
            unsupportedf "%s is not supported by the batch (single-check) reader" c
          | Some other, _ -> unsupportedf "unsupported command: %s" other
          | None, _ -> malformedf "malformed command: %s" (Sexp.to_string cmd)))
    sexps;
  !logic, !status, List.rev !asserts, List.rev !lemmas
;;

let parse_into env ctx src =
  let st =
    { ctx
    ; env
    ; sorts = Hashtbl.create 16
    ; funs = Hashtbl.create 64
    ; defines = Hashtbl.create 16
    ; expanding = Hashtbl.create 8
    ; memo = Hashtbl.create 64
    ; dt_names = Hashtbl.create 8
    ; datatypes = Datatype_defs.empty
    }
  in
  let sexps =
    try Sexp.parse_many src with
    | Sexp.Malformed m -> raise (Malformed ("s-expression: " ^ m))
    | Tok.Error m -> raise (Malformed ("lexical: " ^ m))
  in
  let logic, status, assertions, lemmas =
    try run st sexps with
    | Term.Sort_error m -> raise (Malformed ("sort error: " ^ m))
    | Term.Unsupported m -> raise (Unsupported m)
    | Term.Overflow -> raise (Unsupported "arithmetic exceeds native int range")
  in
  { env; ctx; logic; status; assertions; datatypes = st.datatypes; lemmas }
;;

let parse src =
  let env = Env.create () in
  let ctx = Context.create env in
  parse_into env ctx src
;;
