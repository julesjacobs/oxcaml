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

(* Learn candidate atoms for predicate [p] from its samples: for each coefficient vector,
   observe [lo = min], [hi = max] of the linear form across the samples and propose the
   tightest sound-for-the-data atom(s) — an equality if the form is observed constant,
   else both bound directions. *)
let learn_atoms (sy : P.sys) (p : int) (samps : sample list) : atom list =
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
(* top-level solve *)
(* ------------------------------------------------------------------ *)

let solve
  ?(budget = 60_000)
  ?(max_effort = 1_000_000)
  ?(sample_cap = 40)
  ?(init_models = 4)
  (s : system)
  : result
  =
  P.query_count := 0;
  P.budget_ref := budget;
  P.effort_cap := max_effort;
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
       let cands = Array.init sy.P.npreds (fun p -> learn_atoms sy p samples.(p)) in
       let inv = houdini sy reach cands in
       if P.verify sy inv
       then { verdict = Safe; detail = "syntax-guided (Houdini) invariant, verified" }
       else { verdict = Unknown "no verified invariant"; detail = "cegis: not proved" }
     with
     | P.Give_up "__unsafe_trivial" ->
       { verdict = Unsafe; detail = "trivial constraint-only counterexample" }
     | P.Give_up "__unsafe0" -> { verdict = Unsafe; detail = "counterexample at depth 0" }
     | P.Give_up r -> { verdict = Unknown r; detail = r }
     | Chc_ast.Build_error m -> { verdict = Unknown ("build: " ^ m); detail = m })
;;
