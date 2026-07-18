(* Data-driven, syntax-guided invariant synthesis for LINEAR Constrained Horn Clauses — a
   cvc5-style (SyGuS/CEGIS-class) second engine, complementary to the backward Spacer/PDR
   search in {!Chc_pdr}.

   Where PDR searches BACKWARD from the error, generalizing counterexample-to-induction
   cubes (a shape that provably diverges on the sum/counter class — e.g. a [x + y = 10]
   invariant with point init, see the [safe_rel_sum] analysis in the chc-v2 report), this
   engine searches FORWARD in the space of INVARIANT SYNTAX:

   1. sample a set of concrete reachable states per predicate, by asking the SMT oracle
      for models along the clause edges seeded from [init];
   2. propose candidate atomic predicates from a small linear GRAMMAR (single-variable
      bounds, octagon [x_i +/- x_j], and the full sum [sum_i x_i]) whose CONSTANTS are
      LEARNED from the observed sample min/max — so [x + y = 10] is proposed directly when
      the sum is observed constant;
   3. prune the candidate conjunction to its greatest inductive sub-conjunction by
      monotone Houdini refinement (drop any atom a counterexample shows non-inductive);
   4. hand the surviving conjunction to the SAME independent firewall {!Chc_pdr.verify}
      that gates PDR's [Safe]. The synthesized invariant is CHECKED, never trusted: a
      wrong or too-weak guess can only ever cost [Unknown] / fall-through to PDR, never a
      wrong verdict. This engine emits only [Safe] or [Unknown]; UNSAFE is left to the PDR
      path (portfolio), except for the two instant syntactic checks it shares. *)

module Session = Oxsmt_interface.Session
module Bigint = Oxsmt_core.Bigint
open Chc_ast
module P = Chc_pdr

type verdict = P.verdict =
  | Safe
  | Unsafe
  | Unknown of string

type result = P.result =
  { verdict : verdict
  ; detail : string
  }

(* ------------------------------------------------------------------ *)
(* concrete samples *)
(* ------------------------------------------------------------------ *)

(* A concrete state of a predicate: one value per argument position. *)
type sample = P.value array

let sample_key (s : sample) : string =
  String.concat
    ","
    (Array.to_list
       (Array.map
          (function
            | P.VInt n -> Bigint.to_string n
            | P.VBool b -> string_of_bool b)
          s))
;;

(* Read predicate [p]'s argument values (over [ns], the pre/post namespace) from the
   current model. Returns [None] if any argument is not pinned by the model (an unpinned
   value would spuriously widen the learned bounds; we simply skip such an incomplete
   sample). *)
let model_sample (sy : P.sys) (p : int) (ns : int -> string) (sess : Session.t)
  : sample option
  =
  match Session.get_model sess with
  | None -> None
  | Some (_, bindings) ->
    let tbl = Hashtbl.create 16 in
    List.iter
      (function
        | Session.Const (name, Session.VInt n) -> Hashtbl.replace tbl name (P.VInt n)
        | Session.Const (name, Session.VBool b) -> Hashtbl.replace tbl name (P.VBool b)
        | _ -> ())
      bindings;
    let ar = sy.P.arity.(p) in
    let out = Array.make ar (P.VInt Bigint.zero) in
    let ok = ref true in
    for i = 0 to ar - 1 do
      match Hashtbl.find_opt tbl (ns i) with
      | Some v -> out.(i) <- v
      | None -> ok := false
    done;
    if !ok then Some out else None
;;

(* Fix a concrete sample of predicate [p] as equality constraints over namespace [ns]. *)
let sample_constraints (ns : int -> string) (s : sample) : expr list =
  Array.to_list
    (Array.mapi
       (fun i v ->
         match v with
         | P.VInt n -> Eq (Var (ns i), Int_lit n)
         | P.VBool true -> Var (ns i)
         | P.VBool false -> Not (Var (ns i)))
       s)
;;

(* Collect up to [cap] reachable samples per predicate. Seed each predicate from a few
   [init] models (diversified by blocking previously-seen states), then propagate along
   the edges by fixing a known source sample and reading the resulting successor. Every
   query is budget-guarded through the shared oracle, so sampling can never dominate the
   solve. *)
let collect_samples (sy : P.sys) ~(cap : int) ~(init_models : int) ~(rounds : int)
  : sample list array * bool array
  =
  let samples = Array.make sy.P.npreds [] in
  let seen = Array.init sy.P.npreds (fun _ -> Hashtbl.create 32) in
  let reach = Array.make sy.P.npreds false in
  let add p (s : sample) : bool =
    let k = sample_key s in
    if Hashtbl.mem seen.(p) k || List.length samples.(p) >= cap
    then false
    else (
      Hashtbl.replace seen.(p) k ();
      samples.(p) <- s :: samples.(p);
      true)
  in
  (* seed from init, diversified *)
  for p = 0 to sy.P.npreds - 1 do
    let blocks = ref [] in
    try
      for _ = 1 to init_models do
        match P.solve_exprs sy (sy.P.init.(p) :: !blocks) with
        | P.R_sat, sess ->
          reach.(p) <- true;
          (match model_sample sy p (P.pre p) sess with
           | Some s ->
             ignore (add p s : bool);
             blocks := Not (And (sample_constraints (P.pre p) s)) :: !blocks
           | None -> raise Exit)
        | _ -> raise Exit
      done
    with
    | Exit -> ()
    | P.Give_up _ -> ()
  done;
  (* propagate along edges *)
  (try
     for _ = 1 to rounds do
       let changed = ref false in
       Array.iter
         (fun (e : P.edge) ->
           List.iter
             (fun (s : sample) ->
               if List.length samples.(e.P.dst) < cap
               then (
                 let q = sample_constraints (P.pre e.P.src) s @ e.P.guard in
                 match P.solve_exprs sy q with
                 | P.R_sat, sess ->
                   reach.(e.P.dst) <- true;
                   (match model_sample sy e.P.dst (P.post e.P.dst) sess with
                    | Some s' -> if add e.P.dst s' then changed := true
                    | None -> ())
                 | _ -> ()))
             samples.(e.P.src))
         sy.P.edges;
       if not !changed then raise Exit
     done
   with
   | Exit -> ()
   | P.Give_up _ -> ());
  samples, reach
;;

(* ------------------------------------------------------------------ *)
(* candidate atoms (linear grammar with learned constants) *)
(* ------------------------------------------------------------------ *)

type crel =
  | Cle
  | Cge
  | Ceq

(* A linear atom [sum_i coeffs.(i) * x_i  rel  rhs] over a predicate's integer arguments. *)
type atom =
  { coeffs : Bigint.t array
  ; rel : crel
  ; rhs : Bigint.t
  }

let atom_expr (ns : int -> string) (a : atom) : expr =
  let terms = ref [] in
  Array.iteri
    (fun i c ->
      if not (Bigint.is_zero c) then terms := Mul (Int_lit c, Var (ns i)) :: !terms)
    a.coeffs;
  let lhs =
    match !terms with
    | [] -> Int_lit Bigint.zero
    | [ t ] -> t
    | ts -> Add ts
  in
  match a.rel with
  | Cle -> Le (lhs, Int_lit a.rhs)
  | Cge -> Ge (lhs, Int_lit a.rhs)
  | Ceq -> Eq (lhs, Int_lit a.rhs)
;;

(* Dot product of a coefficient vector with a concrete sample (ints only). *)
let dot (coeffs : Bigint.t array) (s : sample) : Bigint.t =
  let acc = ref Bigint.zero in
  Array.iteri
    (fun i c ->
      match s.(i) with
      | P.VInt n -> acc := Bigint.add !acc (Bigint.mul c n)
      | P.VBool _ -> ())
    coeffs;
  !acc
;;

(* The coefficient-vector grammar for a predicate with integer-argument indices [ints]:
   single-variable [x_i], octagon [x_i - x_j] and [x_i + x_j], and (when there are more
   than two integer arguments) the full sum [sum_i x_i]. *)
let coeff_vectors ~(arity : int) (ints : int list) : Bigint.t array list =
  let mk f = Array.init arity (fun i -> Bigint.of_int (f i)) in
  let units = List.map (fun i -> mk (fun k -> if k = i then 1 else 0)) ints in
  let pairs = ref [] in
  let rec go = function
    | [] -> ()
    | i :: rest ->
      List.iter
        (fun j ->
          pairs := mk (fun k -> if k = i then 1 else if k = j then -1 else 0) :: !pairs;
          pairs := mk (fun k -> if k = i then 1 else if k = j then 1 else 0) :: !pairs)
        rest;
      go rest
  in
  go ints;
  let allsum =
    if List.length ints > 2
    then [ mk (fun k -> if List.mem k ints then 1 else 0) ]
    else []
  in
  units @ !pairs @ allsum
;;

(* Integer value of sample position [i] (booleans map to 0/1). *)
let coord (s : sample) (i : int) : Bigint.t =
  match s.(i) with
  | P.VInt n -> n
  | P.VBool b -> if b then Bigint.one else Bigint.zero
;;

(* Data-driven affine-equality mining (env [OXSMT_CHC_CEGIS_AFFINE]): the fixed grammar in
   [coeff_vectors] only expresses unit and octagon (+/-1) coefficients, so exact relations
   with other coefficients — e.g. [2*i = j] — are invisible to it. For each pair of
   integer arguments whose sample projections are collinear, recover the exact integer
   normal [a*x_i + b*x_j = c] from the observed data (reduced by gcd, sign-normalized)
   and, when it is not already a unit/octagon form, propose it as an equality atom. Purely
   additive candidate generation; still Houdini-pruned and verify-gated downstream, so a
   spurious mined relation only ever costs a dropped atom, never a wrong verdict. *)
let mine_affine_atoms ~(arity : int) (ints : int list) (samps : sample list) : atom list =
  let mk_vec i a j b =
    Array.init arity (fun k -> if k = i then a else if k = j then b else Bigint.zero)
  in
  let small v =
    match Bigint.to_int_opt (Bigint.abs v) with
    | Some n -> n <= 1
    | None -> false
  in
  let out = ref [] in
  let consider i j =
    let s0 = List.hd samps in
    let x0i = coord s0 i
    and x0j = coord s0 j in
    let dir =
      List.find_map
        (fun s ->
          let di = Bigint.sub (coord s i) x0i
          and dj = Bigint.sub (coord s j) x0j in
          if Bigint.is_zero di && Bigint.is_zero dj then None else Some (di, dj))
        samps
    in
    match dir with
    | None -> ()
    | Some (di, dj) ->
      (* the exact affine relation orthogonal to the change direction *)
      let a0 = dj
      and b0 = Bigint.neg di in
      let g = Bigint.gcd a0 b0 in
      if not (Bigint.is_zero g)
      then (
        let a = fst (Bigint.divmod a0 g)
        and b = fst (Bigint.divmod b0 g) in
        let a, b =
          if Bigint.sign a < 0 || (Bigint.is_zero a && Bigint.sign b < 0)
          then Bigint.neg a, Bigint.neg b
          else a, b
        in
        if not (small a && small b)
        then (
          let c = Bigint.add (Bigint.mul a x0i) (Bigint.mul b x0j) in
          let holds s =
            Bigint.equal
              (Bigint.add (Bigint.mul a (coord s i)) (Bigint.mul b (coord s j)))
              c
          in
          if List.for_all holds samps
          then out := { coeffs = mk_vec i a j b; rel = Ceq; rhs = c } :: !out))
  in
  (match samps with
   | [] | [ _ ] -> ()
   | _ ->
     let rec go = function
       | [] -> ()
       | i :: rest ->
         List.iter (fun j -> consider i j) rest;
         go rest
     in
     go ints);
  !out
;;

(* Learn candidate atoms for predicate [p] from its samples: for each coefficient vector,
   observe [lo = min], [hi = max] of the linear form across the samples and propose the
   tightest sound-for-the-data atom(s) — an equality if the form is observed constant,
   else both bound directions. With [OXSMT_CHC_CEGIS_AFFINE], also append data-mined exact
   affine equalities with non-unit coefficients (see [mine_affine_atoms]). *)
let learn_atoms ~(affine : bool) (sy : P.sys) (p : int) (samps : sample list) : atom list =
  match samps with
  | [] -> []
  | _ ->
    let arity = sy.P.arity.(p) in
    let ints =
      List.filter
        (fun i ->
          match Hashtbl.find_opt sy.P.sorts (P.pre p i) with
          | Some s -> Oxsmt_core.Sort.equal s Oxsmt_core.Sort.int
          | None -> true)
        (List.init arity (fun i -> i))
    in
    let vecs = coeff_vectors ~arity ints in
    let grammar =
      List.concat_map
        (fun coeffs ->
          let vals = List.map (dot coeffs) samps in
          let lo =
            List.fold_left
              (fun a v -> if Bigint.compare v a < 0 then v else a)
              (List.hd vals)
              (List.tl vals)
          and hi =
            List.fold_left
              (fun a v -> if Bigint.compare v a > 0 then v else a)
              (List.hd vals)
              (List.tl vals)
          in
          if Bigint.compare lo hi = 0
          then [ { coeffs; rel = Ceq; rhs = lo } ]
          else [ { coeffs; rel = Cle; rhs = hi }; { coeffs; rel = Cge; rhs = lo } ])
        vecs
    in
    let mined = if affine then mine_affine_atoms ~arity ints samps else [] in
    grammar @ mined
;;

(* ------------------------------------------------------------------ *)
(* Houdini refinement *)
(* ------------------------------------------------------------------ *)

(* Prune each predicate's candidate atom set to the greatest inductive sub-conjunction:
   drop any atom violated by an init model, then repeatedly drop any atom whose head is
   not entailed across an edge, until a full pass removes nothing. Monotone
   (removal-only), so it terminates; every query is budget-guarded. [reach.(p) = false]
   predicates take the strongest invariant [false]. *)
let houdini (sy : P.sys) (reach : bool array) (cands : atom list array) : expr list array =
  let inv = Array.copy cands in
  (* invariant of pred p as expr list over [ns] (false when unreachable) *)
  let inv_exprs (ns : int -> string) (p : int) : expr list =
    if not reach.(p) then [ Bool_lit false ] else List.map (atom_expr ns) inv.(p)
  in
  (* init pruning (once — init is fixed) *)
  for p = 0 to sy.P.npreds - 1 do
    if reach.(p)
    then
      inv.(p)
      <- List.filter
           (fun a ->
             match P.check sy [ sy.P.init.(p); Not (atom_expr (P.pre p) a) ] with
             | P.R_unsat -> true
             | _ -> false)
           inv.(p)
  done;
  (* edge pruning to fixpoint *)
  let changed = ref true in
  while !changed do
    changed := false;
    Array.iter
      (fun (e : P.edge) ->
        if reach.(e.P.dst)
        then (
          let src_inv = inv_exprs (P.pre e.P.src) e.P.src in
          inv.(e.P.dst)
          <- List.filter
               (fun a ->
                 match
                   P.check
                     sy
                     (src_inv @ e.P.guard @ [ Not (atom_expr (P.post e.P.dst) a) ])
                 with
                 | P.R_unsat -> true
                 | _ ->
                   changed := true;
                   false)
               inv.(e.P.dst)))
      sy.P.edges
  done;
  Array.init sy.P.npreds (fun p -> inv_exprs (P.pre p) p)
;;

(* ------------------------------------------------------------------ *)
(* disjunctive (DNF) candidate synthesis *)
(* ------------------------------------------------------------------ *)

(* Does concrete sample [t] satisfy atom [a]? (namespace-independent: the atom is a linear
   form over argument indices, evaluated by [dot].) *)
let atom_holds (a : atom) (t : sample) : bool =
  let v = dot a.coeffs t in
  match a.rel with
  | Cle -> Bigint.compare v a.rhs <= 0
  | Cge -> Bigint.compare v a.rhs >= 0
  | Ceq -> Bigint.compare v a.rhs = 0
;;

(* Partition [samps] into at most [k] axis-aligned clusters by splitting on the integer
   coordinate of largest observed range: sort by that coordinate and cut into [k]
   contiguous, roughly equal slices. Deterministic. Returns the non-empty slices (so a
   degenerate split collapses to fewer clusters, and an unsplittable set to one). *)
let cluster_samples (ints : int list) (k : int) (samps : sample list) : sample list list =
  let n = List.length samps in
  if k <= 1 || n <= 1 || ints = []
  then [ samps ]
  else (
    (* pick the int coordinate with the widest [max - min] spread *)
    let bmin a b = if Bigint.compare a b <= 0 then a else b in
    let bmax a b = if Bigint.compare a b >= 0 then a else b in
    let spread i =
      let vs = List.map (fun s -> coord s i) samps in
      let lo = List.fold_left bmin (List.hd vs) (List.tl vs)
      and hi = List.fold_left bmax (List.hd vs) (List.tl vs) in
      Bigint.sub hi lo
    in
    let axis, _ =
      List.fold_left
        (fun (bi, bs) i ->
          let s = spread i in
          if Bigint.compare s bs > 0 then i, s else bi, bs)
        (List.hd ints, Bigint.of_int (-1))
        ints
    in
    let sorted =
      List.sort (fun a b -> Bigint.compare (coord a axis) (coord b axis)) samps
    in
    let arr = Array.of_list sorted in
    let per = (n + k - 1) / k in
    let out = ref [] in
    let i = ref 0 in
    while !i < n do
      let hi = min n (!i + per) in
      out := Array.to_list (Array.sub arr !i (hi - !i)) :: !out;
      i := hi
    done;
    List.rev !out)
;;

(* Build the DNF expr for predicate [p] over namespace [ns] from its per-disjunct atom
   lists: [OR_c (AND (atoms of disjunct c))]. Unreachable predicates take [false]; an
   empty disjunct is [true] (the weakest region). *)
let dnf_expr (reach : bool array) (ns : int -> string) (p : int) (disj : atom list array)
  : expr
  =
  if not reach.(p)
  then Bool_lit false
  else (
    let ds =
      Array.to_list disj
      |> List.map (fun atoms ->
        match atoms with
        | [] -> Bool_lit true
        | _ -> And (List.map (atom_expr ns) atoms))
    in
    match ds with
    | [] -> Bool_lit true
    | [ d ] -> d
    | ds -> Or ds)
;;

(* Sample concrete BAD states per predicate: states of predicate [src] that directly
   satisfy some bad guard. These are ICE negative examples — no safe invariant may contain
   them, so the disjunctive weakening below never drops atoms in a way that would admit
   one. Every query is budget-guarded; a small cap keeps this cheap. *)
let collect_bad_samples (sy : P.sys) ~(cap : int) : sample list array =
  let bad = Array.make sy.P.npreds [] in
  let seen = Array.init sy.P.npreds (fun _ -> Hashtbl.create 16) in
  let add p (s : sample) =
    let key = sample_key s in
    if (not (Hashtbl.mem seen.(p) key)) && List.length bad.(p) < cap
    then (
      Hashtbl.replace seen.(p) key ();
      bad.(p) <- s :: bad.(p))
  in
  (try
     Array.iter
       (fun (src, g) ->
         let blocks = ref [] in
         try
           for _ = 1 to cap do
             match P.solve_exprs sy (g @ !blocks) with
             | P.R_sat, sess ->
               (match model_sample sy src (P.pre src) sess with
                | Some s ->
                  add src s;
                  blocks := Not (And (sample_constraints (P.pre src) s)) :: !blocks
                | None -> raise Exit)
             | _ -> raise Exit
           done
         with
         | Exit -> ())
       sy.P.bad
   with
   | P.Give_up _ -> ());
  bad
;;

(* Greedy ICE-style disjunctive synthesis for the full system: cluster each reachable
   predicate's samples into [k] boxes, then weaken (monotone atom-dropping) until the DNF
   is inductive for init + edges or the repair budget is spent, and verify. A repair that
   would admit a sampled BAD state ([bad_samples]) is refused (ICE negative examples), so
   the weakening keeps the DNF safe-by-construction w.r.t. the observed error states —
   which is what lets the final firewall pass. Returns [Some inv] (verified-safe) or
   [None]. Every query is budget-guarded; weakening only ever enlarges a region (drops
   atoms), so it strictly decreases atom count and terminates. The final [P.verify] is the
   sole soundness gate — a wrong/weak guess yields [None]. *)
let disj_solve
  (sy : P.sys)
  (reach : bool array)
  (samples : sample list array)
  (bad_samples : sample list array)
  ~(k : int)
  ~(max_rounds : int)
  ~(query_cap : int)
  ~(affine : bool)
  : expr list array option
  =
  (* per-predicate integer argument indices (for clustering) *)
  let int_args p =
    let arity = sy.P.arity.(p) in
    List.filter
      (fun i ->
        match Hashtbl.find_opt sy.P.sorts (P.pre p i) with
        | Some s -> Oxsmt_core.Sort.equal s Oxsmt_core.Sort.int
        | None -> true)
      (List.init arity (fun i -> i))
  in
  (* initial per-predicate disjuncts: one learned box per cluster *)
  let disj : atom list array array =
    Array.init sy.P.npreds (fun p ->
      if not reach.(p)
      then [||]
      else (
        let clusters = cluster_samples (int_args p) k samples.(p) in
        Array.of_list (List.map (fun cl -> learn_atoms ~affine sy p cl) clusters)))
  in
  (* did any predicate actually split into >1 disjunct? if not, this is just the
     conjunction we already tried — skip. *)
  let any_split = Array.exists (fun d -> Array.length d > 1) disj in
  if not any_split
  then None
  else (
    let expr_of ns p = dnf_expr reach ns p disj.(p) in
    (* Enlarge predicate [p]'s DNF to admit counterexample state [t] by dropping the atoms
       [t] violates from ONE disjunct — but only a disjunct whose surviving atoms still
       EXCLUDE every sampled bad state (ICE negative-example guard), and among those the
       one needing the fewest drops. If no disjunct can admit [t] without also admitting a
       bad state (e.g. [t] itself is a bad witness), refuse — the DNF stays as-is and this
       edge stays violated (falls through to [None]), never a repair that would corrupt
       safety. *)
    let excludes_all_bad p survive =
      List.for_all
        (fun b -> List.exists (fun a -> not (atom_holds a b)) survive)
        bad_samples.(p)
    in
    let repair p (t : sample) : bool =
      let best = ref (-1)
      and best_drop = ref max_int
      and best_survive = ref [] in
      Array.iteri
        (fun c atoms ->
          let survive = List.filter (fun a -> atom_holds a t) atoms in
          let drop = List.length atoms - List.length survive in
          if drop > 0 && drop < !best_drop && excludes_all_bad p survive
          then (
            best_drop := drop;
            best := c;
            best_survive := survive))
        disj.(p);
      if !best >= 0
      then (
        disj.(p).(!best) <- !best_survive;
        true)
      else false
    in
    (* weakening sweeps, bounded by [max_rounds] and a disj-local query cap: successful
       repairs converge in a few cheap queries, so a tight cap captures every win while
       bailing fast on the many files where no disjunctive invariant exists (keeps the
       engine within the portfolio time-box instead of spinning to the global budget). *)
    let q0 = !P.query_count in
    let round = ref 0 in
    let changed = ref true in
    while !changed && !round < max_rounds && !P.query_count - q0 < query_cap do
      incr round;
      changed := false;
      (* init: init.(p) => dnf(pre p) *)
      for p = 0 to sy.P.npreds - 1 do
        if reach.(p)
        then (
          match P.solve_exprs sy [ sy.P.init.(p); Not (expr_of (P.pre p) p) ] with
          | P.R_sat, sess ->
            (match model_sample sy p (P.pre p) sess with
             | Some t -> if repair p t then changed := true
             | None -> ())
          | _ -> ())
      done;
      (* edges: dnf(pre src) /\ guard => dnf(post dst) *)
      Array.iter
        (fun (e : P.edge) ->
          if reach.(e.P.dst)
          then (
            let ante = expr_of (P.pre e.P.src) e.P.src in
            let conseq = Not (expr_of (P.post e.P.dst) e.P.dst) in
            match P.solve_exprs sy ((ante :: e.P.guard) @ [ conseq ]) with
            | P.R_sat, sess ->
              (match model_sample sy e.P.dst (P.post e.P.dst) sess with
               | Some t -> if repair e.P.dst t then changed := true
               | None -> ())
            | _ -> ()))
        sy.P.edges
    done;
    let inv = Array.init sy.P.npreds (fun p -> [ expr_of (P.pre p) p ]) in
    if P.verify sy inv then Some inv else None)
;;

(* ------------------------------------------------------------------ *)
(* top-level solve *)
(* ------------------------------------------------------------------ *)

(* Env-or-override boolean: explicit [arg] wins; else the env var (treating unset as
   [default]). Lets the CLI drive the two SAFE-frontier levers by env while the self-tests
   toggle them explicitly. *)
let flag_of arg env ~default =
  match arg with
  | Some b -> b
  | None ->
    (match Sys.getenv_opt env with
     | Some ("0" | "false" | "") -> false
     | Some _ -> true
     | None -> default)
;;

let solve
  ?(budget = 60_000)
  ?(max_effort = 1_000_000)
  ?(sample_cap = 40)
  ?(init_models = 4)
  ?disj
  ?affine
  (s : system)
  : result
  =
  P.query_count := 0;
  P.budget_ref := budget;
  P.effort_cap := max_effort;
  (* the disjunctive-DNF fallback defaults ON; the affine-equality mining defaults OFF *)
  let disj_on = flag_of disj "OXSMT_CHC_CEGIS_DISJ" ~default:true in
  let affine_on = flag_of affine "OXSMT_CHC_CEGIS_AFFINE" ~default:false in
  match P.build_sys s with
  | exception P.Not_linear r -> { verdict = Unknown ("not linear: " ^ r); detail = r }
  | sy ->
    (try
       (* the two instant syntactic UNSAFE checks the PDR path also does up front *)
       if List.exists (fun c -> P.check sy [ c ] = P.R_sat) sy.P.trivially_unsafe
       then raise (P.Give_up "__unsafe_trivial");
       Array.iter
         (fun (src, g) ->
           if P.check sy (sy.P.init.(src) :: g) = P.R_sat
           then raise (P.Give_up "__unsafe0"))
         sy.P.bad;
       let samples, reach =
         collect_samples sy ~cap:sample_cap ~init_models ~rounds:(sy.P.npreds + 6)
       in
       let cands =
         Array.init sy.P.npreds (fun p -> learn_atoms ~affine:affine_on sy p samples.(p))
       in
       let inv = houdini sy reach cands in
       if P.verify sy inv
       then { verdict = Safe; detail = "syntax-guided (Houdini) invariant, verified" }
       else (
         (* Fallback: disjunctive (DNF) candidate synthesis. The conjunctive hull was not
            a verifiable invariant; try a union of per-cluster boxes for the SAFE class
            whose reachable set is non-convex (hull admits a bad state, the union does
            not). Purely additive — reached only when the conjunction already failed — and
            gated by the same [P.verify] firewall, so it can only ever add Safe verdicts,
            never a wrong one. Env-gated ([OXSMT_CHC_CEGIS_DISJ], default on; [_DISJ_K]
            max disjuncts). *)
         let max_k =
           match Sys.getenv_opt "OXSMT_CHC_CEGIS_DISJ_K" with
           | Some s ->
             (try max 2 (int_of_string s) with
              | _ -> 3)
           | None -> 3
         in
         let bad_samples =
           if disj_on then collect_bad_samples sy ~cap:8 else Array.make sy.P.npreds []
         in
         let rec try_k k =
           if (not disj_on) || k > max_k
           then None
           else (
             match
               disj_solve
                 sy
                 reach
                 samples
                 bad_samples
                 ~k
                 ~max_rounds:6
                 ~query_cap:200
                 ~affine:affine_on
             with
             | Some inv -> Some inv
             | None -> try_k (k + 1))
         in
         match try_k 2 with
         | Some _ ->
           { verdict = Safe
           ; detail = "syntax-guided disjunctive (DNF) invariant, verified"
           }
         | None ->
           { verdict = Unknown "no verified invariant"; detail = "cegis: not proved" })
     with
     | P.Give_up "__unsafe_trivial" ->
       { verdict = Unsafe; detail = "trivial constraint-only counterexample" }
     | P.Give_up "__unsafe0" -> { verdict = Unsafe; detail = "counterexample at depth 0" }
     | P.Give_up r -> { verdict = Unknown r; detail = r }
     | Chc_ast.Build_error m -> { verdict = Unknown ("build: " ^ m); detail = m })
;;
