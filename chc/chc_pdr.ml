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

(* [jdx = Some j] is a difference (octagon) literal [(x_idx - x_j) rel v]; [None] is the
   single-variable [x_idx rel v]. Difference literals are the relational-invariant
   template (interpolation proxy), same as the single-predicate engine. *)
type lit =
  { idx : int
  ; jdx : int option
  ; rel : rel
  ; v : value
  }

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
      | Some v ->
        acc := { idx = i; jdx = None; rel = Req; v } :: !acc;
        (match v with
         | VInt n -> ints := (i, n) :: !ints
         | VBool _ -> ())
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
              := { idx = i; jdx = Some j; rel = Req; v = VInt (Bigint.sub vi vj) }
                 :: !diffs)
          !ints)
      !ints;
    !acc @ !diffs
;;

(* ---- cube / lemma expressions ---- *)

(* [name] maps a state-var index to its name in the chosen (pre/post) namespace. *)
let lit_expr (name : int -> string) (l : lit) : expr =
  let lhs =
    match l.jdx with
    | None -> Var (name l.idx)
    | Some j -> Sub [ Var (name l.idx); Var (name j) ]
  in
  match l.v, l.rel with
  | VInt n, Req -> Eq (lhs, Int_lit n)
  | VInt n, Rle -> Le (lhs, Int_lit n)
  | VInt n, Rge -> Ge (lhs, Int_lit n)
  | VBool true, _ -> lhs
  | VBool false, _ -> Not lhs
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
      match l.v, l.rel with
      | VInt _, Req ->
        [ without; replace { l with rel = Rle }; replace { l with rel = Rge } ]
      | _ -> [ without ]
    in
    match List.find_opt (admissible p pi i) cands with
    | Some better -> better
    | None -> cur
  in
  List.fold_left step s s
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
            | R_sat -> found := Some (e, model_cube p.sy e.src sess)))
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

let solve ?(max_frames = 60) ?(budget = 200_000) ?(max_effort = 1_000_000) (s : system)
  : result
  =
  query_count := 0;
  budget_ref := budget;
  effort_cap := max_effort;
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
                      let c = model_cube sy src sess in
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
