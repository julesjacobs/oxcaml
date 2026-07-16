(* CHC (Constrained Horn Clause) intermediate representation.

   A HORN problem is a set of clauses [forall vars. (constr /\ B_1 /\ ... /\ B_n) => H],
   where the [B_i] are applied uninterpreted predicates, [constr] is an interpreted LIA
   formula, and the head [H] is either an applied predicate or [false].

   We deliberately keep the body/constraint/head as our OWN expression AST ([expr]) rather
   than as fully-built {!Oxsmt_core.Term.t}s. The engine must re-express a clause over
   many different renamings of its bound variables (one fresh copy per BMC step / PDR
   frame), and oxsmt exposes no generic term substitution — the term type is hash-consed
   and [private]. Carrying an [expr] plus a builder that takes a [string -> Term.t]
   variable environment gives us free, capture-free renaming: the engine supplies the
   mapping and {!build_bool} constructs the term through the shared {!Oxsmt_core.Context}. *)

module Bigint = Oxsmt_core.Bigint
module Sort = Oxsmt_core.Sort
module Term = Oxsmt_core.Term
module Context = Oxsmt_core.Context

(* Interpreted / predicate expression over named variables. Sort-correctness is not
   enforced here — it is enforced by {!Oxsmt_core.Context}'s smart constructors when the
   builder runs, which is the single sanctioned construction path (I2). *)
type expr =
  | Var of string
  | Int_lit of Bigint.t
  | Bool_lit of bool
  | Neg of expr
  | Add of expr list
  | Sub of expr list (* n-ary; [Sub [a;b;c]] = a-b-c; [Sub [a]] = -a *)
  | Mul of expr * expr (* linear: at least one side must build to a constant *)
  | Div of expr * expr
  | Mod of expr * expr
  | Eq of expr * expr
  | Le of expr * expr
  | Lt of expr * expr
  | Ge of expr * expr
  | Gt of expr * expr
  | Not of expr
  | And of expr list
  | Or of expr list
  | Implies of expr * expr
  | Iff of expr * expr
  | Ite of expr * expr * expr
  | Distinct of expr list
  | Pred_app of string * expr list

(* An applied predicate. *)
type app =
  { pred : string
  ; args : expr list
  }

type head =
  | H_false
  | H_pred of app

(* A normalized Horn clause. *)
type clause =
  { vars : (string * Sort.t) list (* forall-bound variables and their sorts *)
  ; body_apps : app list (* uninterpreted predicate applications in the antecedent *)
  ; constr : expr list (* interpreted antecedent conjuncts (implicit conjunction) *)
  ; head : head
  }

type pred =
  { name : string
  ; arg_sorts : Sort.t list
  }

let arity p = List.length p.arg_sorts

type system =
  { env : Oxsmt_core.Env.t
  ; ctx : Context.t
  ; preds : pred list
  ; clauses : clause list
  }

let find_pred sys name = List.find_opt (fun p -> String.equal p.name name) sys.preds

(* ------------------------------------------------------------------ *)
(* Building oxsmt terms from an [expr] under a variable environment. *)
(* ------------------------------------------------------------------ *)

exception Build_error of string

(* [const_of_expr] recognizes an expression that is a ground integer constant, so [Mul]
   can route to the linear [mul_const] constructor (LIA admits only constant
   coefficients). Returns [None] for anything non-constant. *)
let rec const_of_expr (e : expr) : Bigint.t option =
  match e with
  | Int_lit n -> Some n
  | Neg a -> Option.map Bigint.neg (const_of_expr a)
  | Add es ->
    List.fold_left
      (fun acc e ->
        match acc, const_of_expr e with
        | Some s, Some n -> Some (Bigint.add s n)
        | _ -> None)
      (Some Bigint.zero)
      es
  | Sub (first :: rest) ->
    (match const_of_expr first with
     | None -> None
     | Some f ->
       List.fold_left
         (fun acc e ->
           match acc, const_of_expr e with
           | Some s, Some n -> Some (Bigint.sub s n)
           | _ -> None)
         (Some f)
         rest)
  | Sub [] -> None
  | Mul (a, b) ->
    (match const_of_expr a, const_of_expr b with
     | Some x, Some y -> Some (Bigint.mul x y)
     | _ -> None)
  | _ -> None
;;

let rec build (ctx : Context.t) (venv : string -> Term.t) (e : expr) : Term.t =
  let b = build ctx venv in
  match e with
  | Var x -> venv x
  | Int_lit n -> Context.int_const_big ctx n
  | Bool_lit v -> Context.bool_const ctx v
  | Neg a -> Context.neg ctx (b a)
  | Add [] -> Context.int_const ctx 0
  | Add (x :: rest) -> List.fold_left (fun acc e -> Context.add ctx acc (b e)) (b x) rest
  | Sub [ x ] -> Context.neg ctx (b x)
  | Sub (x :: rest) -> List.fold_left (fun acc e -> Context.sub ctx acc (b e)) (b x) rest
  | Sub [] -> raise (Build_error "empty (-)")
  | Mul (a, c) ->
    (match const_of_expr a with
     | Some k -> Context.mul_const_big ctx k (b c)
     | None ->
       (match const_of_expr c with
        | Some k -> Context.mul_const_big ctx k (b a)
        | None -> raise (Build_error "nonlinear multiplication")))
  | Div (a, c) -> Context.div ctx (b a) (b c)
  | Mod (a, c) -> Context.mod_ ctx (b a) (b c)
  | Eq (a, c) -> Context.eq ctx (b a) (b c)
  | Le (a, c) -> Context.le ctx (b a) (b c)
  | Lt (a, c) -> Context.lt ctx (b a) (b c)
  | Ge (a, c) -> Context.ge ctx (b a) (b c)
  | Gt (a, c) -> Context.gt ctx (b a) (b c)
  | Not a -> Context.not_ ctx (b a)
  | And [] -> Context.bool_const ctx true
  | And [ x ] -> b x
  | And es -> Context.and_ ctx (List.map b es)
  | Or [] -> Context.bool_const ctx false
  | Or [ x ] -> b x
  | Or es -> Context.or_ ctx (List.map b es)
  | Implies (a, c) -> Context.implies ctx (b a) (b c)
  | Iff (a, c) -> Context.iff ctx (b a) (b c)
  | Ite (c, t, e) -> Context.ite ctx (b c) (b t) (b e)
  | Distinct es -> Context.distinct ctx (List.map b es)
  | Pred_app (name, _) ->
    raise (Build_error ("predicate application in interpreted position: " ^ name))
;;

let build_bool ctx venv es =
  match es with
  | [] -> Context.bool_const ctx true
  | [ e ] -> build ctx venv e
  | _ -> Context.and_ ctx (List.map (build ctx venv) es)
;;

(* ------------------------------------------------------------------ *)
(* mod/div elimination (CHC front-end preprocessing). *)
(* ------------------------------------------------------------------ *)

(* Replace every [(mod a d)] / [(div a d)] with a constant divisor [d] by a fresh
   quotient/remainder variable [q]/[r] plus the Euclidean defining constraints
   [a = d*q + r], [0 <= r], [r < |d|] (SMT-LIB Ints semantics, matching z3). This keeps
   the clause purely linear (the coefficient [d] is a literal) and — crucially — routes
   the query AWAY from the reserved div/mod symbols, whose SAT-direction model self-check
   in the shipped Session degrades a satisfiable query to [unknown] (the LIA-oracle gap
   that made PDR's satisfiability-seeking predecessor/CTI queries bail). A non-constant
   divisor is genuinely nonlinear and left untouched (it will degrade downstream,
   soundly). *)
let elim_moddiv_clause (c : clause) : clause =
  let ctr = ref 0 in
  let fresh_vars = ref [] in
  let new_constrs = ref [] in
  let fresh prefix =
    (* A plain (non-reserved) identifier, prefixed to avoid colliding with user variables. *)
    let n = Printf.sprintf "chcmd_%s%d" prefix !ctr in
    incr ctr;
    fresh_vars := (n, Sort.int) :: !fresh_vars;
    n
  in
  let zero = Int_lit Bigint.zero in
  let rec go (e : expr) : expr =
    match e with
    | Var _ | Int_lit _ | Bool_lit _ -> e
    | Neg a -> Neg (go a)
    | Add es -> Add (List.map go es)
    | Sub es -> Sub (List.map go es)
    | Mul (a, b) -> Mul (go a, go b)
    | Mod (a, d) -> elim `Mod (go a) (go d)
    | Div (a, d) -> elim `Div (go a) (go d)
    | Eq (a, b) -> Eq (go a, go b)
    | Le (a, b) -> Le (go a, go b)
    | Lt (a, b) -> Lt (go a, go b)
    | Ge (a, b) -> Ge (go a, go b)
    | Gt (a, b) -> Gt (go a, go b)
    | Not a -> Not (go a)
    | And es -> And (List.map go es)
    | Or es -> Or (List.map go es)
    | Implies (a, b) -> Implies (go a, go b)
    | Iff (a, b) -> Iff (go a, go b)
    | Ite (a, b, c) -> Ite (go a, go b, go c)
    | Distinct es -> Distinct (List.map go es)
    | Pred_app (n, es) -> Pred_app (n, List.map go es)
  and elim which a d =
    match const_of_expr d with
    | Some k when not (Bigint.is_zero k) ->
      let q = fresh "q"
      and r = fresh "r" in
      let kd = Int_lit k in
      let absk = Int_lit (Bigint.abs k) in
      (* a = d*q + r ; 0 <= r ; r < |d| *)
      new_constrs
      := Eq (a, Add [ Mul (kd, Var q); Var r ])
         :: Ge (Var r, zero)
         :: Lt (Var r, absk)
         :: !new_constrs;
      (match which with
       | `Mod -> Var r
       | `Div -> Var q)
    | _ ->
      (match which with
       | `Mod -> Mod (a, d)
       | `Div -> Div (a, d))
  in
  let constr = List.map go c.constr in
  let body_apps = List.map (fun a -> { a with args = List.map go a.args }) c.body_apps in
  let head =
    match c.head with
    | H_false -> H_false
    | H_pred a -> H_pred { a with args = List.map go a.args }
  in
  { vars = c.vars @ !fresh_vars; body_apps; constr = !new_constrs @ constr; head }
;;
