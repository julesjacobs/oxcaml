(* Multi-predicate PDR for LINEAR Constrained Horn Clauses.

   Generalizes {!Chc_engine} (single-predicate transition system) to N predicates: the
   classic Spacer/GPDR structure of one over-approximating frame sequence PER predicate,
   with proof obligations [(predicate, cube, level)] pushed backward along clause edges.
   "Linear" = every clause has at most one body predicate (the CHC-COMP LIA-lin fragment),
   so the reachability structure is a multi-location transition system (predicates =
   locations, clauses = edges, plus a distinguished error sink).

   Same soundness discipline as {!Chc_engine} — neither verdict is trusted on the frame
   bookkeeping alone:
   - [Unsafe] is emitted only after the discovered counterexample PATH (the exact edge
     sequence recovered from the failed block recursion) is re-confirmed [sat] as one
     chained unrolling on a fresh Session;
   - [Safe] is emitted only after the per-predicate candidate invariants are re-verified
     independently: every fact edge's image lands in the head invariant, every transition
     edge maps its body invariant into its head invariant, and every bad edge is excluded.
     A PDR-bookkeeping bug can only cost [Unknown] / nontermination (budget-bounded),
     never a wrong verdict. *)

module Session = Oxsmt_interface.Session
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Bigint = Oxsmt_core.Bigint
open Chc_ast

type verdict =
  | Safe
  | Unsafe
  | Unknown of string

type result =
  { verdict : verdict
  ; detail : string
  }

(* Reuse the shared value/rel/cube vocabulary shapes locally. *)
type value =
  | VInt of Bigint.t
  | VBool of bool

type rel =
  | Req
  | Rle
  | Rge

(* A cube literal over a predicate's argument indices. [Lin (coeffs, rel, c)] is the
   general linear literal [Sum_i coeff_i * x_i  rel  c] (a single-variable bound and an
   octagon difference are the 1- and 2-term special cases); [Bl (i, b)] pins boolean arg
   [i] to [b]. The general-linear shape (vs the earlier octagon-only record) is what lets
   model-based projection emit multi-variable relational predecessors like [x + y = 10]. *)
type lit =
  | Lin of (int * Bigint.t) list * rel * Bigint.t
  | Bl of int * bool

type cube = lit list

(* ---- expr helpers (rename / free vars), independent of the single-pred engine ---- *)

let rec rename (f : string -> string) (e : expr) : expr =
  let r = rename f in
  match e with
  | Var x -> Var (f x)
  | Int_lit _ | Bool_lit _ -> e
  | Neg a -> Neg (r a)
  | Add es -> Add (List.map r es)
  | Sub es -> Sub (List.map r es)
  | Mul (a, b) -> Mul (r a, r b)
  | Div (a, b) -> Div (r a, r b)
  | Mod (a, b) -> Mod (r a, r b)
  | Eq (a, b) -> Eq (r a, r b)
  | Le (a, b) -> Le (r a, r b)
  | Lt (a, b) -> Lt (r a, r b)
  | Ge (a, b) -> Ge (r a, r b)
  | Gt (a, b) -> Gt (r a, r b)
  | Not a -> Not (r a)
  | And es -> And (List.map r es)
  | Or es -> Or (List.map r es)
  | Implies (a, b) -> Implies (r a, r b)
  | Iff (a, b) -> Iff (r a, r b)
  | Ite (a, b, c) -> Ite (r a, r b, r c)
  | Distinct es -> Distinct (List.map r es)
  | Pred_app (n, es) -> Pred_app (n, List.map r es)
;;

module SS = Set.Make (String)

let rec fv acc (e : expr) =
  match e with
  | Var x -> SS.add x acc
  | Int_lit _ | Bool_lit _ -> acc
  | Neg a | Not a -> fv acc a
  | Add es | Sub es | And es | Or es | Distinct es -> List.fold_left fv acc es
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Eq (a, b)
  | Le (a, b)
  | Lt (a, b)
  | Ge (a, b)
  | Gt (a, b)
  | Implies (a, b)
  | Iff (a, b) -> fv (fv acc a) b
  | Ite (a, b, c) -> fv (fv (fv acc a) b) c
  | Pred_app (_, es) -> List.fold_left fv acc es
;;

(* ---- system model: predicates as locations, clauses as edges ---- *)

(* Canonical state-variable names. [pre p i] is the i-th arg of predicate p in a body
   (pre-state) position, [post p i] in a head (post-state) position. *)
let pre p i = Printf.sprintf "s_%d_%d" p i
let post p i = Printf.sprintf "t_%d_%d" p i

(* A transition edge: from body predicate [src] to head predicate [dst] (both by index),
   the interpreted [guard] (a conjunction of expr conjuncts over [pre src] and [post dst]
   plus fresh clause-local aux vars). *)
type edge =
  { src : int
  ; dst : int
  ; guard : expr list
  }

type sys =
  { npreds : int
  ; arity : int array
  ; sorts : (string, Sort.t) Hashtbl.t
  ; init : expr array (* init.(p): the initial-state set of predicate p, over [pre p] *)
  ; edges : edge array
  ; bad : (int * expr list) array (* (src predicate, guard over [pre src]) *)
  ; trivially_unsafe : expr list
  (* every fact-free "constr => false" body; UNSAFE if ANY is satisfiable (accumulator,
     not a single slot — overwriting would mask an earlier genuine query) *)
  }

exception Not_linear of string

let build_sys (s : system) : sys =
  let n = List.length s.preds in
  let pid = Hashtbl.create 16 in
  List.iteri (fun i p -> Hashtbl.replace pid p.name i) s.preds;
  let arity = Array.of_list (List.map arity s.preds) in
  let sorts = Hashtbl.create 256 in
  List.iteri
    (fun i p ->
      List.iteri
        (fun j srt ->
          Hashtbl.replace sorts (pre i j) srt;
          Hashtbl.replace sorts (post i j) srt)
        p.arg_sorts)
    s.preds;
  let get_pid name =
    match Hashtbl.find_opt pid name with
    | Some i -> i
    | None -> raise (Not_linear ("undeclared predicate " ^ name))
  in
  let inits = Array.make n [] in
  let edges = ref [] in
  let bads = ref [] in
  let triv = ref [] in
  let ctr = ref 0 in
  let freshen (c : clause) =
    let cid = !ctr in
    incr ctr;
    let m = Hashtbl.create 16 in
    List.iter
      (fun (name, srt) ->
        let f = Printf.sprintf "a%d_%s" cid name in
        Hashtbl.replace m name f;
        Hashtbl.replace sorts f srt)
      c.vars;
    fun x ->
      match Hashtbl.find_opt m x with
      | Some f -> f
      | None -> x
  in
  let eqs ~name args = List.mapi (fun i a -> Eq (Var (name i), a)) args in
  List.iter
    (fun (c : clause) ->
      (match c.body_apps with
       | [] | [ _ ] -> ()
       | _ -> raise (Not_linear "clause with multiple body predicates"));
      let f = freshen c in
      let constr = List.map (rename f) c.constr in
      match c.body_apps, c.head with
      | [], H_pred { pred; args } ->
        let p = get_pid pred in
        let args = List.map (rename f) args in
        inits.(p) <- And (constr @ eqs ~name:(pre p) args) :: inits.(p)
      | [ { pred = bp; args = bargs } ], H_pred { pred = hp; args = hargs } ->
        let src = get_pid bp
        and dst = get_pid hp in
        let bargs = List.map (rename f) bargs
        and hargs = List.map (rename f) hargs in
        let guard = constr @ eqs ~name:(pre src) bargs @ eqs ~name:(post dst) hargs in
        edges := { src; dst; guard } :: !edges
      | [ { pred = bp; args = bargs } ], H_false ->
        let src = get_pid bp in
        let bargs = List.map (rename f) bargs in
        bads := (src, constr @ eqs ~name:(pre src) bargs) :: !bads
      | [], H_false -> triv := And constr :: !triv
      | _ -> raise (Not_linear "unexpected clause shape"))
    s.clauses;
  let init =
    Array.map
      (function
        | [] -> Bool_lit false
        | [ x ] -> x
        | xs -> Or xs)
      inits
  in
  { npreds = n
  ; arity
  ; sorts
  ; init
  ; edges = Array.of_list (List.rev !edges)
  ; bad = Array.of_list (List.rev !bads)
  ; trivially_unsafe = List.rev !triv
  }
;;

(* ---- SMT oracle ---- *)

type smt =
  | R_sat
  | R_unsat
  | R_unknown

let query_count = ref 0
let budget_ref = ref max_int

(* Per-[check_sat] effort cap threaded to every oracle Session (board #60 [max_effort]).
   Without it a Session is UNBOUNDED and a diverging LIA search hangs [check_sat] forever,
   uninterruptible by the between-queries query-count budget. A finite cap degrades such a
   query to [Unknown] (budget) so the PDR solve bails to unknown instead of hanging. *)
let effort_cap = ref 1_000_000

(* Model-based projection predecessor generalization (dark lever [OXSMT_CHC_MBP]). When
   on, the predecessor cube in [block] (and the initial bad cube in [solve]) is a
   model-based projection region instead of a model point + octagon differences. Set from
   the environment once per [solve]; sound in both states (the MBP result is
   under-approximating and every verdict is independently re-verified). *)
let use_mbp = ref false

exception Give_up of string

let check_budget () =
  if !query_count > !budget_ref then raise (Give_up "effort budget exhausted")
;;

let solve_exprs (sy : sys) (asserts : expr list) : smt * Session.t =
  incr query_count;
  check_budget ();
  let sess = Session.create ~max_effort:!effort_cap () in
  let ctx = Session.context sess in
  let fvs = List.fold_left fv SS.empty asserts in
  let vmap = Hashtbl.create 64 in
  SS.iter
    (fun name ->
      let srt =
        match Hashtbl.find_opt sy.sorts name with
        | Some s -> s
        | None -> Sort.int
      in
      Hashtbl.replace vmap name (Context.const ctx (Session.declare_const sess name srt)))
    fvs;
  let venv name =
    match Hashtbl.find_opt vmap name with
    | Some t -> t
    | None -> failwith ("unbound " ^ name)
  in
  (* Term construction runs OUTSIDE the Session firewall, so an ill-sorted / unsupported
     expr (a v1 encoding gap for some file shape) would crash the process. Degrade any
     construction fault on this query to [R_unknown] — sound (a query we cannot build
     cannot decide the problem, so PDR bails to Unknown), never a crash or wrong verdict. *)
  let r =
    match
      List.iter (fun e -> Session.assert_term sess (Chc_ast.build ctx venv e)) asserts
    with
    | () ->
      (match Session.check_sat sess with
       | Session.Sat -> R_sat
       | Session.Unsat -> R_unsat
       | Session.Unknown -> R_unknown)
    | exception Oxsmt_core.Term.Sort_error _ -> R_unknown
    | exception Oxsmt_core.Term.Unsupported _ -> R_unknown
    | exception Oxsmt_core.Term.Overflow -> R_unknown
    | exception Chc_ast.Build_error _ -> R_unknown
  in
  r, sess
;;

let check sy es = fst (solve_exprs sy es)

let smt_bool = function
  | R_sat -> true
  | R_unsat -> false
  | R_unknown -> raise (Give_up "LIA oracle unknown")
;;

(* Read predicate [p]'s pre-state values from the most recent Sat model. *)
let model_cube (sy : sys) (p : int) (sess : Session.t) : cube =
  match Session.get_model sess with
  | None -> []
  | Some (_, bindings) ->
    let tbl = Hashtbl.create 16 in
    List.iter
      (function
        | Session.Const (name, Session.VInt n) -> Hashtbl.replace tbl name (VInt n)
        | Session.Const (name, Session.VBool b) -> Hashtbl.replace tbl name (VBool b)
        | _ -> ())
      bindings;
    let ints = ref [] in
    let acc = ref [] in
    for i = sy.arity.(p) - 1 downto 0 do
      match Hashtbl.find_opt tbl (pre p i) with
      | Some (VInt n) ->
        acc := Lin ([ i, Bigint.one ], Req, n) :: !acc;
        ints := (i, n) :: !ints
      | Some (VBool b) -> acc := Bl (i, b) :: !acc
      | None -> ()
    done;
    (* Difference (octagon) literals x_i - x_j = v_i - v_j for each int pair: the
       relational-invariant template. *)
    let diffs = ref [] in
    List.iter
      (fun (i, vi) ->
        List.iter
          (fun (j, vj) ->
            if i < j
            then
              diffs
              := Lin ([ i, Bigint.one; j, Bigint.neg Bigint.one ], Req, Bigint.sub vi vj)
                 :: !diffs)
          !ints)
      !ints;
    !acc @ !diffs
;;

(* ---- cube / lemma expressions ---- *)

(* [name] maps a state-var index to its name in the chosen (pre/post) namespace. *)
let lit_expr (name : int -> string) (l : lit) : expr =
  match l with
  | Bl (i, true) -> Var (name i)
  | Bl (i, false) -> Not (Var (name i))
  | Lin (coeffs, rel, c) ->
    let lhs =
      match coeffs with
      | [] -> Int_lit Bigint.zero
      | _ ->
        Add
          (List.map
             (fun (i, k) ->
               if Bigint.equal k Bigint.one
               then Var (name i)
               else Mul (Int_lit k, Var (name i)))
             coeffs)
    in
    (match rel with
     | Req -> Eq (lhs, Int_lit c)
     | Rle -> Le (lhs, Int_lit c)
     | Rge -> Ge (lhs, Int_lit c))
;;

(* [cube_expr p ~post_ns s]: the cube over predicate p's state, rendered in the pre ([s_])
   or post ([t_]) namespace. *)
let cube_expr (p : int) ~post_ns (s : cube) : expr =
  let name i = if post_ns then post p i else pre p i in
  And (List.map (lit_expr name) s)
;;

let clause_expr (p : int) (s : cube) : expr = Not (cube_expr p ~post_ns:false s)

(* ---- frames ---- *)

type pdr =
  { sy : sys
  ; mutable frontier : int
  ; lemmas :
      cube list array array (* lemmas.(p).(i) : cubes blocked for pred p at level i *)
  }

let mk_pdr sy cap =
  { sy; frontier = 1; lemmas = Array.init sy.npreds (fun _ -> Array.make cap []) }
;;

let ensure (p : pdr) (n : int) =
  if n + 1 >= Array.length p.lemmas.(0)
  then
    for k = 0 to p.sy.npreds - 1 do
      let a = Array.make (n + 2) [] in
      Array.blit p.lemmas.(k) 0 a 0 (Array.length p.lemmas.(k));
      p.lemmas.(k) <- a
    done
;;

(* Frame formula for predicate [pi] at level [i], over its pre-state namespace. *)
let frame (p : pdr) (pi : int) (i : int) : expr list =
  if i = 0
  then [ p.sy.init.(pi) ]
  else (
    let acc = ref [] in
    for j = i to p.frontier do
      List.iter (fun s -> acc := clause_expr pi s :: !acc) p.lemmas.(pi).(j)
    done;
    !acc)
;;

let add_lemma (p : pdr) (pi : int) (i : int) (s : cube) =
  p.lemmas.(pi).(i) <- s :: p.lemmas.(pi).(i)
;;

(* Edge guard rewritten so the head (post) state uses the DESTINATION predicate's post
   namespace already (it does by construction); nothing to do — [e.guard] is over
   [pre e.src] and [post e.dst]. *)

(* Is predicate [pi]'s cube [s] intersecting init? *)
let hits_init (p : pdr) (pi : int) (s : cube) : bool =
  smt_bool (check p.sy [ p.sy.init.(pi); cube_expr pi ~post_ns:false s ])
;;

(* Admissible generalized cube for pred [pi] at level [i]: disjoint from init AND no edge
   carries a frame-[i-1] source state into it. *)
let admissible (p : pdr) (pi : int) (i : int) (cand : cube) : bool =
  if cand = []
  then false
  else if smt_bool (check p.sy [ p.sy.init.(pi); cube_expr pi ~post_ns:false cand ])
  then false
  else (
    (* for every edge into pi: frame(src,i-1) /\ guard /\ cand@post unsat *)
    let ok = ref true in
    Array.iter
      (fun e ->
        if !ok && e.dst = pi
        then (
          let q = frame p e.src (i - 1) @ e.guard @ [ cube_expr pi ~post_ns:true cand ] in
          if smt_bool (check p.sy q) then ok := false))
      p.sy.edges;
    !ok)
;;

let generalize (p : pdr) (pi : int) (i : int) (s : cube) : cube =
  let step cur (l : lit) =
    check_budget ();
    let without = List.filter (fun x -> x != l) cur in
    let replace nl = List.map (fun x -> if x == l then nl else x) cur in
    let cands =
      match l with
      | Lin (coeffs, Req, c) ->
        [ without; replace (Lin (coeffs, Rle, c)); replace (Lin (coeffs, Rge, c)) ]
      | _ -> [ without ]
    in
    match List.find_opt (admissible p pi i) cands with
    | Some better -> better
    | None -> cur
  in
  List.fold_left step s s
;;

(* ---- model-based projection (MBP) predecessor generalization ----

   Spacer's core lever. Instead of taking a single predecessor POINT from a [Sat] model
   and relaxing it by oracle-checked trial ([model_cube] + [generalize]), we project the
   transition formula [exists (aux, post-state). guard /\ pob@post] onto the source
   pre-state variables, using the model both to pick a model-consistent implicant (which
   disjunct / ite-branch is taken) and to select the active bound in Loos-Weispfenning
   variable elimination. The result is a generalized predecessor CUBE (a whole region),
   not a point — far fewer proof obligations to discharge.

   MBP contract: the returned cube [psi] satisfies [M |= psi] and
   [psi ==> exists elim. phi] (an UNDER-approximation of the projection: every state in
   psi genuinely reaches the POB). Any variable we cannot cleanly eliminate is pinned to
   its model value — always a sound under-approximation. And the whole PDR verdict is
   independently re-verified downstream ([verify] for Safe, [replay] for Unsafe), so even
   a mis-projection can only cost a solve to Unknown, never a wrong verdict. A cheap
   [M |= psi] self-check (below) guarantees the model lies in [psi], which is what makes
   the recursive block step strictly progress (blocking [psi] excludes the model from the
   caller's next predecessor query). *)

let model_tbl (sess : Session.t) : (string, value) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  (match Session.get_model sess with
   | Some (_, bindings) ->
     List.iter
       (function
         | Session.Const (n, Session.VInt v) -> Hashtbl.replace tbl n (VInt v)
         | Session.Const (n, Session.VBool b) -> Hashtbl.replace tbl n (VBool b)
         | _ -> ())
       bindings
   | None -> ());
  tbl
;;

(* A linear form: var -> coeff association (merged, no zero coeffs) plus a constant. *)
type linform = (string * Bigint.t) list * Bigint.t

let lf_merge (ts : (string * Bigint.t) list) : (string * Bigint.t) list =
  let tbl = Hashtbl.create 8 in
  let order = ref [] in
  List.iter
    (fun (x, k) ->
      match Hashtbl.find_opt tbl x with
      | Some k0 -> Hashtbl.replace tbl x (Bigint.add k0 k)
      | None ->
        Hashtbl.replace tbl x k;
        order := x :: !order)
    ts;
  List.filter_map
    (fun x ->
      let k = Hashtbl.find tbl x in
      if Bigint.is_zero k then None else Some (x, k))
    (List.rev !order)
;;

let lf_add ((ta, ca) : linform) ((tb, cb) : linform) : linform =
  lf_merge (ta @ tb), Bigint.add ca cb
;;

let lf_neg ((ts, c) : linform) : linform =
  List.map (fun (x, k) -> x, Bigint.neg k) ts, Bigint.neg c
;;

let lf_scale (k : Bigint.t) ((ts, c) : linform) : linform =
  if Bigint.is_zero k
  then [], Bigint.zero
  else List.map (fun (x, kk) -> x, Bigint.mul k kk) ts, Bigint.mul k c
;;

(* [lin_of_expr] renders an [expr] as a linear form, or [None] if nonlinear. Semantics
   MUST match {!Chc_ast.build} (the singleton [Sub [x]] is [-x], not [x]). *)
let rec lin_of_expr (e : expr) : linform option =
  match e with
  | Var x -> Some ([ x, Bigint.one ], Bigint.zero)
  | Int_lit n -> Some ([], n)
  | Neg a -> Option.map lf_neg (lin_of_expr a)
  | Add es ->
    List.fold_left
      (fun acc e ->
        match acc, lin_of_expr e with
        | Some s, Some l -> Some (lf_add s l)
        | _ -> None)
      (Some ([], Bigint.zero))
      es
  | Sub [ x ] -> Option.map lf_neg (lin_of_expr x)
  | Sub (first :: rest) ->
    (match lin_of_expr first with
     | None -> None
     | Some f ->
       List.fold_left
         (fun acc e ->
           match acc, lin_of_expr e with
           | Some s, Some l -> Some (lf_add s (lf_neg l))
           | _ -> None)
         (Some f)
         rest)
  | Sub [] -> None
  | Mul (a, b) ->
    (match Chc_ast.const_of_expr a, lin_of_expr b with
     | Some k, Some l -> Some (lf_scale k l)
     | _ ->
       (match Chc_ast.const_of_expr b, lin_of_expr a with
        | Some k, Some l -> Some (lf_scale k l)
        | _ -> None))
  | _ -> None
;;

(* Evaluate an expr under the model [m]; [None] if any needed value is
   missing/non-numeric. Used only to pick model-consistent implicant branches and active
   LW bounds — a wrong guess degrades to the pin fallback and is caught by the
   M-consistency self-check. *)
let rec eval_int (m : (string, value) Hashtbl.t) (e : expr) : Bigint.t option =
  let ( let* ) = Option.bind in
  match e with
  | Var x ->
    (match Hashtbl.find_opt m x with
     | Some (VInt n) -> Some n
     | _ -> None)
  | Int_lit n -> Some n
  | Neg a -> Option.map Bigint.neg (eval_int m a)
  | Add es ->
    List.fold_left
      (fun acc e ->
        match acc, eval_int m e with
        | Some s, Some n -> Some (Bigint.add s n)
        | _ -> None)
      (Some Bigint.zero)
      es
  | Sub [ x ] -> Option.map Bigint.neg (eval_int m x)
  | Sub (x :: rest) ->
    let* f = eval_int m x in
    List.fold_left
      (fun acc e ->
        match acc, eval_int m e with
        | Some s, Some n -> Some (Bigint.sub s n)
        | _ -> None)
      (Some f)
      rest
  | Sub [] -> None
  | Mul (a, b) ->
    let* x = eval_int m a in
    let* y = eval_int m b in
    Some (Bigint.mul x y)
  | Div (a, b) ->
    let* x = eval_int m a in
    let* y = eval_int m b in
    if Bigint.is_zero y then None else Some (fst (Bigint.divmod x y))
  | Mod (a, b) ->
    let* x = eval_int m a in
    let* y = eval_int m b in
    if Bigint.is_zero y then None else Some (snd (Bigint.divmod x y))
  | Ite (c, t, f) ->
    let* cb = eval_bool m c in
    eval_int m (if cb then t else f)
  | _ -> None

and eval_bool (m : (string, value) Hashtbl.t) (e : expr) : bool option =
  let ( let* ) = Option.bind in
  let cmp a b f =
    let* x = eval_int m a in
    let* y = eval_int m b in
    Some (f (Bigint.compare x y))
  in
  match e with
  | Bool_lit b -> Some b
  | Var x ->
    (match Hashtbl.find_opt m x with
     | Some (VBool b) -> Some b
     | _ -> None)
  | Not a -> Option.map not (eval_bool m a)
  | And es ->
    List.fold_left
      (fun acc e ->
        match acc, eval_bool m e with
        | Some s, Some b -> Some (s && b)
        | _ -> None)
      (Some true)
      es
  | Or es ->
    List.fold_left
      (fun acc e ->
        match acc, eval_bool m e with
        | Some s, Some b -> Some (s || b)
        | _ -> None)
      (Some false)
      es
  | Implies (a, b) ->
    let* x = eval_bool m a in
    let* y = eval_bool m b in
    Some ((not x) || y)
  | Iff (a, b) ->
    let* x = eval_bool m a in
    let* y = eval_bool m b in
    Some (Bool.equal x y)
  | Eq (a, b) ->
    (match eval_bool m a, eval_bool m b with
     | Some x, Some y -> Some (Bool.equal x y)
     | _ -> cmp a b (fun c -> c = 0))
  | Le (a, b) -> cmp a b (fun c -> c <= 0)
  | Lt (a, b) -> cmp a b (fun c -> c < 0)
  | Ge (a, b) -> cmp a b (fun c -> c >= 0)
  | Gt (a, b) -> cmp a b (fun c -> c > 0)
  | Ite (c, t, f) ->
    let* cb = eval_bool m c in
    eval_bool m (if cb then t else f)
  | Distinct es ->
    let vs = List.map (eval_int m) es in
    if List.exists Option.is_none vs
    then None
    else (
      let vs = List.map Option.get vs in
      let rec uniq = function
        | [] -> true
        | x :: r -> (not (List.exists (Bigint.equal x) r)) && uniq r
      in
      Some (uniq vs))
  | _ -> None
;;

(* Model-consistent implicant extraction: return a list of literal exprs whose conjunction
   is true under [m] and implies [(if pos then e else not e)]. Or/And/Ite/Implies/Iff are
   resolved by the model; an atom that cannot be evaluated is kept whole (opaque) and
   later pinned to model values. *)
let rec lits (m : (string, value) Hashtbl.t) (pos : bool) (e : expr) : expr list =
  match pos, e with
  | true, And es -> List.concat_map (lits m true) es
  | false, Or es -> List.concat_map (lits m false) es
  | true, Or es ->
    (match List.find_opt (fun d -> eval_bool m d = Some true) es with
     | Some d -> lits m true d
     | None -> [ e ])
  | false, And es ->
    (match List.find_opt (fun d -> eval_bool m d = Some false) es with
     | Some d -> lits m false d
     | None -> [ Not e ])
  | _, Not a -> lits m (not pos) a
  | true, Implies (a, b) ->
    if eval_bool m a = Some false
    then lits m false a
    else if eval_bool m b = Some true
    then lits m true b
    else [ e ]
  | false, Implies (a, b) -> lits m true a @ lits m false b
  | _, Iff (a, b) ->
    (match eval_bool m a, eval_bool m b with
     | Some va, Some vb ->
       (if va then lits m true a else lits m false a)
       @ if vb then lits m true b else lits m false b
     | _ -> [ (if pos then e else Not e) ])
  | _, Ite (c, t, f) ->
    (match eval_bool m c with
     | Some true -> lits m true c @ lits m pos t
     | Some false -> lits m false c @ lits m pos f
     | None -> [ (if pos then e else Not e) ])
  | false, Eq (a, b) ->
    (* Model-based disequality split: [a <> b] with a numeric model becomes the strict
       side the model takes ([a > b] or [a < b]). This keeps a relational literal (an
       inequality over a linear combination) instead of an opaque point — the key to
       generalizing a "safety = state <> bad-value" query into a relational half-space. *)
    (match eval_int m a, eval_int m b with
     | Some va, Some vb when Bigint.compare va vb > 0 -> [ Gt (a, b) ]
     | Some va, Some vb when Bigint.compare va vb < 0 -> [ Lt (a, b) ]
     | _ -> [ Not e ])
  | true, Distinct es | false, Distinct es ->
    (* Expand [distinct] to its pairwise (dis)equalities and recurse (each pair is then
       model-split by the [Eq] cases above). *)
    let pairs =
      let rec go = function
        | [] | [ _ ] -> []
        | x :: rest -> List.map (fun y -> x, y) rest @ go rest
      in
      go es
    in
    if pos
    then List.concat_map (fun (x, y) -> lits m false (Eq (x, y))) pairs
    else (
      match List.find_opt (fun (x, y) -> eval_bool m (Eq (x, y)) = Some true) pairs with
      | Some (x, y) -> lits m true (Eq (x, y))
      | None -> [ Not e ])
  | true, Bool_lit true -> []
  | false, Bool_lit false -> []
  | _, _ -> [ (if pos then e else Not e) ]
;;

type nrel =
  | NLe
  | NGe
  | NEq

(* A normalized atom: [NLin (lf, rel)] means [value(lf) rel 0] (constant folded into
   [lf]); [NOpaque e] is a literal we could not linearize (a boolean atom, disequality,
   nonlinear term, ...). *)
type natom =
  | NLin of linform * nrel
  | NOpaque of expr

(* Build an [NLin], dropping a trivially-true constant literal ([None]). *)
let mk_nlin (lf : linform) (rel : nrel) : natom option =
  let ts, c = lf in
  let ts = lf_merge ts in
  match ts with
  | [] ->
    let sat =
      match rel with
      | NLe -> Bigint.compare c Bigint.zero <= 0
      | NGe -> Bigint.compare c Bigint.zero >= 0
      | NEq -> Bigint.is_zero c
    in
    if sat then None else Some (NLin (([], c), rel))
  | _ -> Some (NLin ((ts, c), rel))
;;

(* Normalize a single literal expr (an atom or negated atom) into a [natom]; [None] drops
   a trivially-true one. *)
let rec natom_of_expr (e : expr) : natom option =
  let cmp a b rel =
    match lin_of_expr a, lin_of_expr b with
    | Some la, Some lb -> mk_nlin (lf_add la (lf_neg lb)) rel
    | _ -> Some (NOpaque e)
  in
  let one = Bigint.one in
  match e with
  | Eq (a, b) -> cmp a b NEq
  | Le (a, b) -> cmp a b NLe
  | Ge (a, b) -> cmp a b NGe
  | Lt (a, b) ->
    (match lin_of_expr a, lin_of_expr b with
     | Some la, Some lb -> mk_nlin (lf_add (lf_add la (lf_neg lb)) ([], one)) NLe
     | _ -> Some (NOpaque e))
  | Gt (a, b) ->
    (match lin_of_expr a, lin_of_expr b with
     | Some la, Some lb ->
       mk_nlin (lf_add (lf_add la (lf_neg lb)) ([], Bigint.neg one)) NGe
     | _ -> Some (NOpaque e))
  | Not a ->
    (match natom_of_expr a with
     | None -> None
     | Some (NLin ((ts, c), NLe)) -> Some (NLin ((ts, Bigint.sub c one), NGe))
     | Some (NLin ((ts, c), NGe)) -> Some (NLin ((ts, Bigint.add c one), NLe))
     | Some (NLin (_, NEq)) -> Some (NOpaque e)
     | Some (NOpaque _) -> Some (NOpaque e))
  | Bool_lit true -> None
  | _ -> Some (NOpaque e)
;;

(* [subst_var y repl e]: replace every [Var y] in [e] with [repl]. *)
let rec subst_var (y : string) (repl : expr) (e : expr) : expr =
  let r = subst_var y repl in
  match e with
  | Var x -> if String.equal x y then repl else e
  | Int_lit _ | Bool_lit _ -> e
  | Neg a -> Neg (r a)
  | Add es -> Add (List.map r es)
  | Sub es -> Sub (List.map r es)
  | Mul (a, b) -> Mul (r a, r b)
  | Div (a, b) -> Div (r a, r b)
  | Mod (a, b) -> Mod (r a, r b)
  | Eq (a, b) -> Eq (r a, r b)
  | Le (a, b) -> Le (r a, r b)
  | Lt (a, b) -> Lt (r a, r b)
  | Ge (a, b) -> Ge (r a, r b)
  | Gt (a, b) -> Gt (r a, r b)
  | Not a -> Not (r a)
  | And es -> And (List.map r es)
  | Or es -> Or (List.map r es)
  | Implies (a, b) -> Implies (r a, r b)
  | Iff (a, b) -> Iff (r a, r b)
  | Ite (a, b, c) -> Ite (r a, r b, r c)
  | Distinct es -> Distinct (List.map r es)
  | Pred_app (n, es) -> Pred_app (n, List.map r es)
;;

let atom_mentions (y : string) : natom -> bool = function
  | NLin ((ts, _), _) -> List.mem_assoc y ts
  | NOpaque e -> SS.mem y (fv SS.empty e)
;;

(* Pin [y] to its model value across all atoms (sound under-approximation of eliminating
   [y]). If unbound, drop atoms mentioning [y] (over-approximation; rare, firewalled). *)
let pin (m : (string, value) Hashtbl.t) (y : string) (atoms : natom list) : natom list =
  match Hashtbl.find_opt m y with
  | Some (VInt n) ->
    List.filter_map
      (function
        | NLin ((ts, c), rel) ->
          (match List.assoc_opt y ts with
           | None -> Some (NLin ((ts, c), rel))
           | Some k ->
             let ts' = List.filter (fun (x, _) -> not (String.equal x y)) ts in
             mk_nlin (ts', Bigint.add c (Bigint.mul k n)) rel)
        | NOpaque e -> natom_of_expr (subst_var y (Int_lit n) e))
      atoms
  | Some (VBool b) ->
    List.filter_map
      (function
        | NLin _ as a -> Some a
        | NOpaque e -> natom_of_expr (subst_var y (Bool_lit b) e))
      atoms
  | None -> List.filter (fun a -> not (atom_mentions y a)) atoms
;;

(* Eliminate [y] from [atoms]: equality substitution (unit coeff), else Loos-Weispfenning
   model-based bound selection (unit-coeff inequalities only), else pin to model. *)
let elim_one (m : (string, value) Hashtbl.t) (y : string) (atoms : natom list)
  : natom list
  =
  let ys, others = List.partition (atom_mentions y) atoms in
  if ys = []
  then atoms
  else (
    let has_opaque =
      List.exists
        (function
          | NOpaque _ -> true
          | _ -> false)
        ys
    in
    let is_unit k = Bigint.equal (Bigint.abs k) Bigint.one in
    let eq_unit =
      List.find_opt
        (function
          | NLin ((ts, _), NEq) ->
            (match List.assoc_opt y ts with
             | Some k -> is_unit k
             | None -> false)
          | _ -> false)
        ys
    in
    let all_unit_ineq =
      List.for_all
        (function
          | NLin ((ts, _), (NLe | NGe)) ->
            (match List.assoc_opt y ts with
             | Some k -> is_unit k
             | None -> true)
          | _ -> false)
        ys
    in
    if has_opaque
    then pin m y atoms
    else (
      match eq_unit with
      | Some (NLin ((ts, c), NEq)) ->
        let coeff = List.assoc y ts in
        let rest_ts = List.filter (fun (x, _) -> not (String.equal x y)) ts in
        (* coeff*y + value(rest) = 0. coeff=+1: y = -(rest); coeff=-1: y = rest. *)
        let yform =
          if Bigint.equal coeff Bigint.one then lf_neg (rest_ts, c) else rest_ts, c
        in
        let subst = function
          | NLin ((ts, c), rel) ->
            (match List.assoc_opt y ts with
             | None -> Some (NLin ((ts, c), rel))
             | Some k ->
               let ts' = List.filter (fun (x, _) -> not (String.equal x y)) ts in
               mk_nlin (lf_add (ts', c) (lf_scale k yform)) rel)
          | NOpaque _ as a -> Some a
        in
        let rest_ys =
          List.filter
            (fun a ->
              match a, eq_unit with
              | _, Some chosen -> a != chosen
              | _ -> true)
            ys
        in
        others @ List.filter_map subst rest_ys
      | _ ->
        if not all_unit_ineq
        then pin m y atoms
        else (
          (* Loos-Weispfenning: collect lower/upper bound forms on y. *)
          let lowers = ref []
          and uppers = ref [] in
          List.iter
            (function
              | NLin ((ts, c), rel) ->
                let k = List.assoc y ts in
                let rest = List.filter (fun (x, _) -> not (String.equal x y)) ts, c in
                (* k*y + value(rest) rel 0 *)
                let bound = if Bigint.equal k Bigint.one then lf_neg rest else rest in
                let is_upper =
                  (Bigint.equal k Bigint.one && rel = NLe)
                  || (Bigint.equal k (Bigint.neg Bigint.one) && rel = NGe)
                in
                if is_upper
                then uppers := bound :: !uppers
                else lowers := bound :: !lowers
              | NOpaque _ -> ())
            ys;
          let mval (ts, c) =
            List.fold_left
              (fun acc (x, k) ->
                match Hashtbl.find_opt m x with
                | Some (VInt n) -> Bigint.add acc (Bigint.mul k n)
                | _ -> raise Exit)
              c
              ts
          in
          let emit_le a b = mk_nlin (lf_add a (lf_neg b)) NLe in
          try
            let acc = ref others in
            let add o =
              match o with
              | Some a -> acc := a :: !acc
              | None -> ()
            in
            if !lowers <> []
            then (
              let lstar =
                List.fold_left
                  (fun best l ->
                    if Bigint.compare (mval l) (mval best) > 0 then l else best)
                  (List.hd !lowers)
                  (List.tl !lowers)
              in
              List.iter (fun u -> add (emit_le lstar u)) !uppers;
              List.iter (fun l -> if l != lstar then add (emit_le l lstar)) !lowers;
              !acc)
            else if !uppers <> []
            then (
              let ustar =
                List.fold_left
                  (fun best u ->
                    if Bigint.compare (mval u) (mval best) < 0 then u else best)
                  (List.hd !uppers)
                  (List.tl !uppers)
              in
              List.iter (fun l -> add (emit_le l ustar)) !lowers;
              List.iter (fun u -> if u != ustar then add (emit_le ustar u)) !uppers;
              !acc)
            else others
          with
          | Exit -> pin m y atoms)))
;;

(* Project [phi] onto predicate [keep_pred]'s pre-state ([s_keep_pred_*]), guided by the
   model in [sess]. Returns a cube over those indices, or falls back to [model_cube] (the
   exact point) if projection yields an empty or model-inconsistent cube. *)
let mbp_project (sy : sys) (sess : Session.t) ~(keep_pred : int) ~(phi : expr list) : cube
  =
  let m = model_tbl sess in
  let prefix = Printf.sprintf "s_%d_" keep_pred in
  let plen = String.length prefix in
  let idx_of x =
    if String.length x > plen && String.sub x 0 plen = prefix
    then int_of_string_opt (String.sub x plen (String.length x - plen))
    else None
  in
  let atoms = List.filter_map natom_of_expr (List.concat_map (lits m true) phi) in
  let all_vars =
    List.fold_left
      (fun acc a ->
        match a with
        | NLin ((ts, _), _) -> List.fold_left (fun s (x, _) -> SS.add x s) acc ts
        | NOpaque e -> fv acc e)
      SS.empty
      atoms
  in
  let elim_vars =
    SS.fold (fun x acc -> if idx_of x = None then x :: acc else acc) all_vars []
  in
  let atoms = List.fold_left (fun ats y -> elim_one m y ats) atoms elim_vars in
  let pin_expr_keep (e : expr) : lit list =
    SS.fold
      (fun x acc ->
        match idx_of x, Hashtbl.find_opt m x with
        | Some i, Some (VInt n) -> Lin ([ i, Bigint.one ], Req, n) :: acc
        | Some i, Some (VBool b) -> Bl (i, b) :: acc
        | _ -> acc)
      (fv SS.empty e)
      []
  in
  let to_lits (a : natom) : lit list =
    match a with
    | NLin ((ts, c), rel) ->
      let idxs = List.map (fun (x, k) -> idx_of x, k) ts in
      if ts <> [] && List.for_all (fun (o, _) -> o <> None) idxs
      then (
        let coeffs = List.map (fun (o, k) -> Option.get o, k) idxs in
        let r =
          match rel with
          | NLe -> Rle
          | NGe -> Rge
          | NEq -> Req
        in
        [ Lin (coeffs, r, Bigint.neg c) ])
      else
        (* residual non-keep var (should not occur post-elimination): pin present
           keep-vars *)
        List.filter_map
          (fun (x, _) ->
            match idx_of x, Hashtbl.find_opt m x with
            | Some i, Some (VInt n) -> Some (Lin ([ i, Bigint.one ], Req, n))
            | Some i, Some (VBool b) -> Some (Bl (i, b))
            | _ -> None)
          ts
    | NOpaque e -> pin_expr_keep e
  in
  let cube = List.concat_map to_lits atoms in
  let lit_true (l : lit) : bool =
    match l with
    | Bl (i, b) ->
      (match Hashtbl.find_opt m (Printf.sprintf "s_%d_%d" keep_pred i) with
       | Some (VBool b') -> Bool.equal b b'
       | _ -> false)
    | Lin (coeffs, rel, c) ->
      (try
         let v =
           List.fold_left
             (fun acc (i, k) ->
               match Hashtbl.find_opt m (Printf.sprintf "s_%d_%d" keep_pred i) with
               | Some (VInt n) -> Bigint.add acc (Bigint.mul k n)
               | _ -> raise Exit)
             Bigint.zero
             coeffs
         in
         match rel with
         | Req -> Bigint.equal v c
         | Rle -> Bigint.compare v c <= 0
         | Rge -> Bigint.compare v c >= 0
       with
       | Exit -> false)
  in
  if cube <> [] && List.for_all lit_true cube then cube else model_cube sy keep_pred sess
;;

(* A counterexample trace element: predicate + entry cube (values), plus the edge that led
   here (edge index into sy.edges, or -1 for the initial obligation). *)
type step =
  { spred : int
  ; scube : cube
  }

(* Recursively block cube [s] of predicate [pi] at level [i]. Returns [None] if blocked,
   [Some trace] (a counterexample chain, this obligation first) if [s] is reachable. *)
let rec block (p : pdr) (pi : int) (s : cube) (i : int) : step list option =
  check_budget ();
  if hits_init p pi s
  then Some [ { spred = pi; scube = s } ] (* reached an initial state *)
  else if i = 0
  then None (* level 0 and not in init => unreachable here *)
  else (
    let rec loop () =
      check_budget ();
      (* find some edge into pi with a predecessor in frame i-1 *)
      let found = ref None in
      Array.iter
        (fun e ->
          if !found = None && e.dst = pi
          then (
            let q = frame p e.src (i - 1) @ e.guard @ [ cube_expr pi ~post_ns:true s ] in
            let r, sess = solve_exprs p.sy q in
            match r with
            | R_unknown -> raise (Give_up "predecessor query unknown")
            | R_unsat -> ()
            | R_sat ->
              let pc =
                if !use_mbp
                then
                  mbp_project
                    p.sy
                    sess
                    ~keep_pred:e.src
                    ~phi:(e.guard @ [ cube_expr pi ~post_ns:true s ])
                else model_cube p.sy e.src sess
              in
              found := Some (e, pc)))
        p.sy.edges;
      match !found with
      | None ->
        (* no predecessor via any edge: block and generalize *)
        add_lemma p pi i (generalize p pi i s);
        None
      | Some (e, pcube) ->
        if pcube = [] then raise (Give_up "empty predecessor cube");
        (match block p e.src pcube (i - 1) with
         | None -> loop () (* predecessor blocked; retry for another *)
         | Some tr -> Some ({ spred = pi; scube = s } :: tr))
    in
    loop ())
;;

(* Propagate lemmas forward; return [Some i] if a global fixpoint level exists (for every
   predicate, no lemma sits at exactly level i). *)
let propagate (p : pdr) : int option =
  let result = ref None in
  let i = ref 1 in
  while !result = None && !i <= p.frontier do
    let level = !i in
    for pi = 0 to p.sy.npreds - 1 do
      let pushed = ref []
      and kept = ref [] in
      List.iter
        (fun s ->
          (* clause holds at level+1 iff no edge carries a frame-level source into s *)
          let holds = ref true in
          Array.iter
            (fun e ->
              if !holds && e.dst = pi
              then (
                let q =
                  frame p e.src level @ e.guard @ [ cube_expr pi ~post_ns:true s ]
                in
                if smt_bool (check p.sy q) then holds := false))
            p.sy.edges;
          if !holds then pushed := s :: !pushed else kept := s :: !kept)
        p.lemmas.(pi).(level);
      p.lemmas.(pi).(level) <- !kept;
      List.iter (fun s -> add_lemma p pi (level + 1) s) !pushed
    done;
    (* fixpoint: every predicate empty at this level (and level < frontier) *)
    let all_empty = ref true in
    for pi = 0 to p.sy.npreds - 1 do
      if p.lemmas.(pi).(level) <> [] then all_empty := false
    done;
    if level < p.frontier && !all_empty then result := Some level;
    incr i
  done;
  !result
;;

(* ---- independent verification ---- *)

let prime_to_post pi e =
  (* rewrite a pred-pi pre-state expr ([s_pi_*]) into its post namespace ([t_pi_*]) *)
  rename
    (fun x ->
      let pfx = Printf.sprintf "s_%d_" pi in
      if String.length x > String.length pfx && String.sub x 0 (String.length pfx) = pfx
      then "t_" ^ String.sub x 2 (String.length x - 2)
      else x)
    e
;;

(* Verify per-predicate invariants [inv.(p)] (each an expr list over [pre p]) are a
   genuine safe inductive assignment. *)
let verify (sy : sys) (inv : expr list array) : bool =
  let ok = ref true in
  (* init: init.(p) => inv.(p) *)
  for p = 0 to sy.npreds - 1 do
    List.iter
      (fun c -> if check sy [ sy.init.(p); Not c ] <> R_unsat then ok := false)
      inv.(p)
  done;
  (* edges: inv.(src) /\ guard => inv.(dst) in post namespace *)
  if !ok
  then
    Array.iter
      (fun e ->
        List.iter
          (fun c ->
            let c_post = prime_to_post e.dst c in
            if check sy (inv.(e.src) @ e.guard @ [ Not c_post ]) <> R_unsat
            then ok := false)
          inv.(e.dst))
      sy.edges;
  (* bad: inv.(src) /\ badguard unsat *)
  if !ok
  then
    Array.iter
      (fun (src, g) -> if check sy (inv.(src) @ g) <> R_unsat then ok := false)
      sy.bad;
  (* trivially-unsafe (fact-free [constr => false]) bodies: a SAFE verdict requires EVERY
     such body to be provably unsatisfiable. These constrain no predicate (no invariant
     can exclude them), so the frame/[verify] machinery above never touches them —
     omitting this check silently reports SAFE on a satisfiable (real counterexample) or
     oracle-undecidable ([R_unknown], e.g. a nonlinear body) constraint-only query.
     [<> R_unsat] fails closed on both [R_sat] and [R_unknown]: an undecidable body cannot
     certify SAFE (degrades to Unknown), never a wrong SAFE. *)
  if !ok
  then
    List.iter (fun c -> if check sy [ c ] <> R_unsat then ok := false) sy.trivially_unsafe;
  !ok
;;

(* ---- forward two-sided interval propagation (candidate-invariant generator) ---- *)

(* A cheap forward abstract interpretation on the integer-interval domain: propagate
   per-predicate, per-argument bounds [lo <= x <= hi] from the [init] sets along the edges
   to a fixpoint (Kleene iteration with interval widening for termination). The result is
   only ever a CANDIDATE invariant — it is handed to {!verify} (the same independent
   firewall that gates PDR's [Safe]), so a loose or wrong guess can only cost a
   fall-through to the full PDR search, never a wrong verdict. This is the "two-sided
   single-variable bound" template: it reaches invariants like [x = 0] (i.e.
   [0 <= x <= 0]) propagated down a predicate chain, which one-sided half-space PDR
   generalization diverges on. *)

(* Read the integer value bound to [vname] in the session's current model, if any. *)
let model_int (sess : Session.t) (vname : string) : Bigint.t option =
  match Session.get_model sess with
  | None -> None
  | Some (_, bindings) ->
    List.find_map
      (function
        | Session.Const (n, Session.VInt v) when String.equal n vname -> Some v
        | _ -> None)
      bindings
;;

type bound =
  | Empty (* the constraint set is unsat: no contribution *)
  | Bounded of Bigint.t
  | Unbounded

(* Guess the extremal (max if [want_max], else min) value of integer variable [vname] over
   [asserts], with value-level widening: after two model bumps in the extremal direction
   we give up to [Unbounded] (a sound over-approximation, since the guess is verified
   later). An [unknown] oracle also widens to [Unbounded]. *)
let guess_bound (sy : sys) (asserts : expr list) (vname : string) ~(want_max : bool)
  : bound
  =
  match solve_exprs sy asserts with
  | R_unsat, _ -> Empty
  | R_unknown, _ -> Empty
  | R_sat, sess0 ->
    (match model_int sess0 vname with
     | None -> Unbounded (* not pinned by the model: treat as unbounded *)
     | Some m0 ->
       let rec go count m =
         let more =
           if want_max then Gt (Var vname, Int_lit m) else Lt (Var vname, Int_lit m)
         in
         match solve_exprs sy (asserts @ [ more ]) with
         | R_unsat, _ -> Bounded m
         | R_unknown, _ -> Unbounded
         | R_sat, sess ->
           (match model_int sess vname with
            | None -> Unbounded
            | Some m' -> if count >= 2 then Unbounded else go (count + 1) m')
       in
       go 0 m0)
;;

(* Per-predicate interval state: [reach.(p)] tracks whether p is known reachable; when it
   is, [los.(p).(i)]/[his.(p).(i)] hold [Some bound] (finite) or [None] (unbounded). *)
type istate =
  { reach : bool array
  ; los : Bigint.t option array array
  ; his : Bigint.t option array array
  }

let interval_invariant (sy : sys) : expr list array option =
  (* A private query budget so the propagation cannot dominate the solve; on exhaustion or
     any give-up we simply produce no candidate (PDR then runs as before). *)
  let start = !query_count in
  let cap = min 3000 (max 200 ((!budget_ref - start) / 4)) in
  let over () = !query_count - start > cap in
  let st =
    { reach = Array.make sy.npreds false
    ; los = Array.init sy.npreds (fun p -> Array.make sy.arity.(p) None)
    ; his = Array.init sy.npreds (fun p -> Array.make sy.arity.(p) None)
    }
  in
  (* Widening join of a fresh [lo,hi] contribution into predicate [p]'s arg [i]. If [p]
     was not yet reachable, adopt the contribution verbatim (first observation); otherwise
     widen any side that would loosen straight to unbounded ([None]) so bounds change O(1)
     times and the iteration terminates. Returns [true] if anything changed. *)
  let join_lo p i (b : bound) : bool =
    let cur = st.los.(p).(i) in
    let nv =
      match b with
      | Empty -> cur
      | Unbounded -> None
      | Bounded m -> Some m
    in
    if not st.reach.(p)
    then (
      st.los.(p).(i) <- nv;
      false)
    else (
      let merged =
        match cur, nv with
        | None, _ | _, None -> None
        | Some a, Some c -> if Bigint.compare c a < 0 then None else Some a
      in
      if merged <> cur
      then (
        st.los.(p).(i) <- merged;
        true)
      else false)
  in
  let join_hi p i (b : bound) : bool =
    let cur = st.his.(p).(i) in
    let nv =
      match b with
      | Empty -> cur
      | Unbounded -> None
      | Bounded m -> Some m
    in
    if not st.reach.(p)
    then (
      st.his.(p).(i) <- nv;
      false)
    else (
      let merged =
        match cur, nv with
        | None, _ | _, None -> None
        | Some a, Some c -> if Bigint.compare c a > 0 then None else Some a
      in
      if merged <> cur
      then (
        st.his.(p).(i) <- merged;
        true)
      else false)
  in
  (* Current interval invariant of pred [p] as constraints over [namespace i]. *)
  let bounds_exprs (namespace : int -> string) (p : int) : expr list =
    let acc = ref [] in
    for i = sy.arity.(p) - 1 downto 0 do
      (match st.his.(p).(i) with
       | Some h -> acc := Le (Var (namespace i), Int_lit h) :: !acc
       | None -> ());
      match st.los.(p).(i) with
      | Some l -> acc := Ge (Var (namespace i), Int_lit l) :: !acc
      | None -> ()
    done;
    !acc
  in
  (* Contribute the bounds of [namespace 0..arity-1] under [asserts] into predicate [p].
     Marks [p] reachable iff [asserts] is satisfiable. Returns whether anything changed. *)
  let contribute (p : int) (namespace : int -> string) (asserts : expr list) : bool =
    if over ()
    then false
    else (
      match check sy asserts with
      | R_unsat | R_unknown -> false
      | R_sat ->
        let changed = ref false in
        let was = st.reach.(p) in
        for i = 0 to sy.arity.(p) - 1 do
          if not (over ())
          then (
            let lo = guess_bound sy asserts (namespace i) ~want_max:false in
            let hi = guess_bound sy asserts (namespace i) ~want_max:true in
            (* [join_*] must observe reachability BEFORE we flip it on for the first
               contribution, so the initial bounds are adopted verbatim. *)
            if join_lo p i lo then changed := true;
            if join_hi p i hi then changed := true)
        done;
        if not was
        then (
          st.reach.(p) <- true;
          changed := true);
        !changed)
  in
  try
    (* seed from init *)
    for p = 0 to sy.npreds - 1 do
      ignore (contribute p (pre p) [ sy.init.(p) ] : bool)
    done;
    (* propagate along edges to a fixpoint (widening guarantees termination) *)
    let changed = ref true in
    let rounds = ref 0 in
    while !changed && (not (over ())) && !rounds <= sy.npreds + 3 do
      changed := false;
      incr rounds;
      Array.iter
        (fun e ->
          if st.reach.(e.src) && not (over ())
          then (
            let asserts = bounds_exprs (pre e.src) e.src @ e.guard in
            if contribute e.dst (post e.dst) asserts then changed := true))
        sy.edges
    done;
    if over ()
    then None
    else (
      let inv =
        Array.init sy.npreds (fun p ->
          if not st.reach.(p) then [ Bool_lit false ] else bounds_exprs (pre p) p)
      in
      if verify sy inv then Some inv else None)
  with
  | Give_up _ -> None
;;

(* ---- counterexample path replay ---- *)

(* Confirm a counterexample soundly by replaying its full edge chain, independent of the
   PDR bookkeeping. From the trace we take only the PREDICATE SEQUENCE [p_0 .. p_L] ([p_0]
   = the bad-edge source, [p_L] hits init) and rebuild the path over fresh per- position
   state copies [c{k}_i]: [init(p_L)] at position L, for each hop the DISJUNCTION over all
   edges [src=p_k, dst=p_{k-1}] of that edge's guard (linking position k's pre- state to
   position k-1's post-state, with hop-private aux vars), and [bad_guard] at position 0.
   If that whole path formula is [sat], a concrete counterexample derivation of this shape
   genuinely exists — a sound witness. Fails closed to [false] on a spurious trace or an
   [unknown] oracle. *)
let replay (sy : sys) ~(bad_guard : expr list) (trace : step list) : bool =
  match trace with
  | [] -> false
  | _ ->
    let seq = Array.of_list (List.map (fun st -> st.spred) trace) in
    let len = Array.length seq in
    (* position-k state var name for arg i *)
    let cvar k i = Printf.sprintf "c%d_%d" k i in
    (* register sorts for the position copies *)
    for k = 0 to len - 1 do
      let pk = seq.(k) in
      for i = 0 to sy.arity.(pk) - 1 do
        match Hashtbl.find_opt sy.sorts (pre pk i) with
        | Some srt -> Hashtbl.replace sy.sorts (cvar k i) srt
        | None -> ()
      done
    done;
    (* rename an expr over [pre p]/[post q]/aux into the position copies for a given hop.
       [pre_pos]/[post_pos] are the copy positions the pre/post namespaces map to;
       [hoptag] prefixes aux vars so distinct hops never share a clause-local var. *)
    let reloc ~pre_p ~pre_pos ~post_q ~post_pos ~hoptag e =
      rename
        (fun x ->
          let spre = Printf.sprintf "s_%d_" pre_p in
          let spost = Printf.sprintf "t_%d_" post_q in
          if pre_pos >= 0
             && String.length x > String.length spre
             && String.sub x 0 (String.length spre) = spre
          then
            cvar
              pre_pos
              (int_of_string
                 (String.sub
                    x
                    (String.length spre)
                    (String.length x - String.length spre)))
          else if post_pos >= 0
                  && String.length x > String.length spost
                  && String.sub x 0 (String.length spost) = spost
          then
            cvar
              post_pos
              (int_of_string
                 (String.sub
                    x
                    (String.length spost)
                    (String.length x - String.length spost)))
          else Printf.sprintf "R%s_%s" hoptag x (* aux var, hop-private *))
        e
    in
    let asserts = ref [] in
    (* init at the terminal position L-1 *)
    let pl = seq.(len - 1) in
    asserts
    := reloc
         ~pre_p:pl
         ~pre_pos:(len - 1)
         ~post_q:(-1)
         ~post_pos:(-1)
         ~hoptag:"init"
         sy.init.(pl)
       :: !asserts;
    (* each hop k -> k-1 : OR over matching edges of the relocated guard *)
    let ok_hops = ref true in
    for k = len - 1 downto 1 do
      let dst = seq.(k - 1)
      and src = seq.(k) in
      let choices =
        Array.to_list sy.edges
        |> List.mapi (fun ei e -> ei, e)
        |> List.filter (fun (_, e) -> e.src = src && e.dst = dst)
        |> List.map (fun (ei, e) ->
          let tag = Printf.sprintf "h%d_e%d" k ei in
          And
            (List.map
               (reloc ~pre_p:src ~pre_pos:k ~post_q:dst ~post_pos:(k - 1) ~hoptag:tag)
               e.guard))
      in
      match choices with
      | [] -> ok_hops := false (* no edge realizes this hop: spurious trace *)
      | [ c ] -> asserts := c :: !asserts
      | cs -> asserts := Or cs :: !asserts
    done;
    (* bad guard at position 0 *)
    asserts
    := List.map
         (reloc ~pre_p:seq.(0) ~pre_pos:0 ~post_q:(-1) ~post_pos:(-1) ~hoptag:"bad")
         bad_guard
       @ !asserts;
    !ok_hops && check sy !asserts = R_sat
;;

(* ---- top-level solve ---- *)

let solve
  ?mbp
  ?(max_frames = 60)
  ?(budget = 200_000)
  ?(max_effort = 1_000_000)
  (s : system)
  : result
  =
  query_count := 0;
  budget_ref := budget;
  effort_cap := max_effort;
  (use_mbp
   := match mbp with
      | Some b -> b
      | None ->
        (match Sys.getenv_opt "OXSMT_CHC_MBP" with
         | Some ("0" | "false" | "") | None -> false
         | Some _ -> true));
  match build_sys s with
  | exception Not_linear r -> { verdict = Unknown ("not linear: " ^ r); detail = r }
  | sy ->
    (try
       (* UNSAFE if ANY fact-free "constr => false" body is satisfiable (accumulator). *)
       if List.exists (fun c -> check sy [ c ] = R_sat) sy.trivially_unsafe
       then raise (Give_up "__unsafe_trivial");
       (* depth-0: any bad guard satisfiable directly from init of its source *)
       Array.iter
         (fun (src, g) ->
           if check sy (sy.init.(src) :: g) = R_sat then raise (Give_up "__unsafe0"))
         sy.bad;
       (* Cheap forward two-sided interval propagation: if it produces a candidate that
          the independent {!verify} firewall certifies, we are done (this reaches
          chain-propagated bounds like [x = 0] that one-sided PDR generalization diverges
          on). Otherwise fall through to the full PDR search below. *)
       (match interval_invariant sy with
        | Some _ -> raise (Give_up "__safe_interval")
        | None -> ());
       let p = mk_pdr sy 4 in
       let verdict = ref None in
       while !verdict = None do
         check_budget ();
         if p.frontier > max_frames then raise (Give_up "frame limit");
         ensure p (p.frontier + 1);
         (* block all bad states at the frontier *)
         let cex = ref None in
         (try
            Array.iter
              (fun (src, g) ->
                if !cex = None
                then (
                  let continue = ref true in
                  while !continue do
                    let r, sess = solve_exprs sy (frame p src p.frontier @ g) in
                    match r with
                    | R_unknown -> raise (Give_up "frontier bad-query unknown")
                    | R_unsat -> continue := false
                    | R_sat ->
                      let c =
                        if !use_mbp
                        then mbp_project sy sess ~keep_pred:src ~phi:g
                        else model_cube sy src sess
                      in
                      if c = [] then raise (Give_up "empty bad cube");
                      (match block p src c p.frontier with
                       | None -> ()
                       | Some tr ->
                         cex := Some (src, g, tr);
                         continue := false)
                  done))
              sy.bad
          with
          | Give_up "__cexbail" -> ());
         match !cex with
         | Some (_src, g, tr) ->
           if replay sy ~bad_guard:g tr
           then verdict := Some Unsafe
           else raise (Give_up "cex not confirmed by replay")
         | None ->
           (match propagate p with
            | Some i ->
              let inv = Array.init sy.npreds (fun pi -> frame p pi i) in
              if verify sy inv
              then verdict := Some Safe
              else raise (Give_up "candidate invariant failed verification")
            | None -> p.frontier <- p.frontier + 1)
       done;
       match !verdict with
       | Some Safe -> { verdict = Safe; detail = "multi-pred PDR invariant, verified" }
       | Some Unsafe ->
         { verdict = Unsafe; detail = "multi-pred PDR cex, replay-confirmed" }
       | _ -> { verdict = Unknown "no result"; detail = "" }
     with
     | Give_up "__unsafe_trivial" ->
       { verdict = Unsafe; detail = "trivial constraint-only counterexample" }
     | Give_up "__unsafe0" -> { verdict = Unsafe; detail = "counterexample at depth 0" }
     | Give_up "__safe_interval" ->
       { verdict = Safe; detail = "forward interval invariant, verified" }
     | Give_up r -> { verdict = Unknown r; detail = r }
     | Chc_ast.Build_error m -> { verdict = Unknown ("build: " ^ m); detail = m })
;;
