(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_residual_state.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

type state : immutable_data = { residual : qf }

let[@def] (models @ total) env state = eval_qf env state.residual

let empty =
  { residual = Le (Const Global, Const Global) }

let[@def] (assert_residual @ total) state clause =
  { residual = And (state.residual, clause) }

let (assert_residual_exact @ total) :
    (env : elt list) -> (state : state) -> (clause : qf) ->
    {u : unit |
      models env (assert_residual state clause) =
      (models env state && eval_qf env clause)} =
 fun env state clause ->
  ghost_ (models_def env (assert_residual state clause));
  ghost_ (assert_residual_def state clause);
  ghost_ (models_def env state);
  ghost_ (eval_qf_def env (And (state.residual, clause)));
  ()

let[@def] (project_first @ total) state =
  { residual = eliminate (Exists (Plain state.residual)) }

let (project_first_exact @ total) :
    (env : elt list) -> (state : state) ->
    {u : unit |
      models env (project_first state) =
      (models (Global :: env) state
       || models (Regional :: env) state
       || models (Local :: env) state)} =
 fun env state ->
  ghost_ (project_first_def state);
  ghost_ (models_def env (project_first state));
  ghost_ (eliminate_exact env (Exists (Plain state.residual)));
  ghost_ (eval_def env (Exists (Plain state.residual)));
  ghost_ (eval_def (Global :: env) (Plain state.residual));
  ghost_ (eval_def (Regional :: env) (Plain state.residual));
  ghost_ (eval_def (Local :: env) (Plain state.residual));
  ghost_ (models_def (Global :: env) state);
  ghost_ (models_def (Regional :: env) state);
  ghost_ (models_def (Local :: env) state);
  ()

let[@def] (restore @ total) before _after = before

let (restore_exact @ total) :
    (env : elt list) -> (before : state) -> (after : state) ->
    {u : unit |
      models env (restore before after) = models env before} =
 fun env before after ->
  ghost_ (restore_def before after);
  ()

let[@def] (query_at @ total) env state clause =
  not (models env state) || eval_qf env clause

let (query_at_exact @ total) :
    (env : elt list) -> (state : state) -> (clause : qf) ->
    {u : unit |
      query_at env state clause =
      (not (models env state)
       || models env (assert_residual state clause))} =
 fun env state clause ->
  ghost_ (query_at_def env state clause);
  ghost_ (assert_residual_exact env state clause);
  ()

let[@def] rec (forall_formula @ total) count body =
  match count with
  | [] -> Plain body
  | _ :: rest -> Forall (forall_formula rest body)

let[@def] rec (all_assignments @ total) count env state clause =
  match count with
  | [] -> query_at env state clause
  | _ :: rest ->
    all_assignments rest (Global :: env) state clause
    && all_assignments rest (Regional :: env) state clause
    && all_assignments rest (Local :: env) state clause

let rec (forall_formula_exact @ total) :
    (count : unit list) -> (env : elt list) ->
    (state : state) -> (clause : qf) ->
    {u : unit |
      eval env
        (forall_formula count (Or (Not state.residual, clause)))
      = all_assignments count env state clause} =
 fun count env state clause ->
  ghost_ (forall_formula_def count (Or (Not state.residual, clause)));
  ghost_ (all_assignments_def count env state clause);
  ghost_
    (eval_def env
       (forall_formula count (Or (Not state.residual, clause))));
  match count with
  | [] ->
    ghost_ (eval_def env (Plain (Or (Not state.residual, clause))));
    ghost_ (eval_qf_def env (Or (Not state.residual, clause)));
    ghost_ (eval_qf_def env (Not state.residual));
    ghost_ (query_at_def env state clause);
    ghost_ (models_def env state);
    ()
  | _ :: rest ->
    ghost_
      (forall_formula_exact rest (Global :: env) state clause);
    ghost_
      (forall_formula_exact rest (Regional :: env) state clause);
    ghost_
      (forall_formula_exact rest (Local :: env) state clause);
    ()

let[@def] (query_all @ total) count state clause =
  eval_qf []
    (eliminate
       (forall_formula count (Or (Not state.residual, clause))))

let (query_all_exact @ total) :
    (count : unit list) -> (state : state) -> (clause : qf) ->
    {u : unit |
      query_all count state clause =
      all_assignments count [] state clause} =
 fun count state clause ->
  ghost_ (query_all_def count state clause);
  ghost_
    (eliminate_exact []
       (forall_formula count (Or (Not state.residual, clause))));
  ghost_ (forall_formula_exact count [] state clause);
  ()

let (query_all_checked @ total) (count : unit list) (state : state)
    (clause : qf) :
    {answer : bool option |
      match answer with
      | None ->
        not (scoped_qf (List.length count) state.residual
             && scoped_qf (List.length count) clause)
      | Some value ->
        scoped_qf (List.length count) state.residual
        && scoped_qf (List.length count) clause
        && value = all_assignments count [] state clause} =
  if scoped_qf (List.length count) state.residual
     && scoped_qf (List.length count) clause
  then begin
    ghost_ (query_all_exact count state clause);
    Some (query_all count state clause)
  end else None

let[@def] (assert_subsumption_state @ total) state guard obligation =
  assert_residual state (subsumption_residual guard obligation)

let (assert_subsumption_state_exact @ total) :
    (env : elt list) -> (state : state) ->
    (guard : qf) -> (obligation : qf) ->
    {u : unit |
      models env (assert_subsumption_state state guard obligation) =
      (models env state
       && eval env (subsumption_formula guard obligation))} =
 fun env state guard obligation ->
  ghost_ (assert_subsumption_state_def state guard obligation);
  ghost_
    (assert_residual_exact env state
       (subsumption_residual guard obligation));
  ghost_ (subsumption_residual_exact env guard obligation);
  ()

let[@def] (query_subsumption @ total) outer_count state guard obligation =
  query_all outer_count state
    (subsumption_residual guard obligation)

let (query_subsumption_exact @ total) :
    (outer_count : unit list) -> (state : state) ->
    (guard : qf) -> (obligation : qf) ->
    {u : unit |
      query_subsumption outer_count state guard obligation =
      all_assignments outer_count [] state
        (subsumption_residual guard obligation)} =
 fun outer_count state guard obligation ->
  ghost_ (query_subsumption_def outer_count state guard obligation);
  ghost_
    (query_all_exact outer_count state
       (subsumption_residual guard obligation));
  ()

let[@def] rec (shift_term @ total) term =
  match term with
  | Const value -> Const value
  | Var i -> Var (if i < 0 then i else i + 1)
  | Join (a, b) -> Join (shift_term a, shift_term b)
  | Meet (a, b) -> Meet (shift_term a, shift_term b)
  | Regional_to_global a -> Regional_to_global (shift_term a)
  | Table (global, regional, local, a) ->
    Table (global, regional, local, shift_term a)

let[@def] rec (shift_qf @ total) formula =
  match formula with
  | Le (a, b) -> Le (shift_term a, shift_term b)
  | And (a, b) -> And (shift_qf a, shift_qf b)
  | Or (a, b) -> Or (shift_qf a, shift_qf b)
  | Not a -> Not (shift_qf a)

let[@def] rec (shiftable_term @ total) term =
  match term with
  | Const _ -> true
  | Var i -> i < 1_000_000
  | Join (a, b) | Meet (a, b) ->
    shiftable_term a && shiftable_term b
  | Regional_to_global a | Table (_, _, _, a) -> shiftable_term a

let[@def] rec (shiftable_qf @ total) formula =
  match formula with
  | Le (a, b) -> shiftable_term a && shiftable_term b
  | And (a, b) | Or (a, b) ->
    shiftable_qf a && shiftable_qf b
  | Not a -> shiftable_qf a

let rec (shift_term_exact @ total) :
    (env : elt list) -> (fresh : elt) ->
    (term : {term : term | shiftable_term term}) ->
    {u : unit |
      eval_term (fresh :: env) (shift_term term) ===
      eval_term env term} =
 fun env fresh term ->
  ghost_ (shiftable_term_def term);
  ghost_ (shift_term_def term);
  ghost_ (eval_term_def (fresh :: env) (shift_term term));
  ghost_ (eval_term_def env term);
  match term with
  | Const _ -> ()
  | Var i ->
    ghost_ (lookup_shift env fresh (if i < 0 then i else i + 1));
    ghost_ (lookup_def (fresh :: env) (if i < 0 then i else i + 1));
    ghost_ (lookup_def env i);
    ()
  | Join (a, b) | Meet (a, b) ->
    ghost_ (shift_term_exact env fresh a);
    ghost_ (shift_term_exact env fresh b);
    ()
  | Regional_to_global a ->
    ghost_ (shift_term_exact env fresh a);
    ()
  | Table (_, _, _, a) ->
    ghost_ (shift_term_exact env fresh a);
    ()

let rec (shift_qf_exact @ total) :
    (env : elt list) -> (fresh : elt) ->
    (formula : {formula : qf | shiftable_qf formula}) ->
    {u : unit |
      eval_qf (fresh :: env) (shift_qf formula) =
      eval_qf env formula} =
 fun env fresh formula ->
  ghost_ (shiftable_qf_def formula);
  ghost_ (shift_qf_def formula);
  ghost_ (eval_qf_def (fresh :: env) (shift_qf formula));
  ghost_ (eval_qf_def env formula);
  match formula with
  | Le (a, b) ->
    ghost_ (shift_term_exact env fresh a);
    ghost_ (shift_term_exact env fresh b);
    ()
  | And (a, b) | Or (a, b) ->
    ghost_ (shift_qf_exact env fresh a);
    ghost_ (shift_qf_exact env fresh b);
    ()
  | Not a ->
    ghost_ (shift_qf_exact env fresh a);
    ()

let[@def] (fresh_state @ total) state =
  { residual = shift_qf state.residual }

let (fresh_state_exact @ total) :
    (env : elt list) -> (value : elt) ->
    (state : {state : state | shiftable_qf state.residual}) ->
    {u : unit |
      models (value :: env) (fresh_state state) = models env state} =
 fun env value state ->
  ghost_ (fresh_state_def state);
  ghost_ (models_def (value :: env) (fresh_state state));
  ghost_ (models_def env state);
  ghost_ (shift_qf_exact env value state.residual);
  ()

let[@def] (copy_first @ total) state =
  { residual =
      And
        (shift_qf state.residual,
         And (Le (Var 0, Var 1), Le (Var 1, Var 0))) }

let (copy_first_exact @ total) :
    (env : elt list) -> (fresh : elt) ->
    (state : {state : state | shiftable_qf state.residual}) ->
    {u : unit |
      models (fresh :: env) (copy_first state) =
      (models env state && le fresh (lookup env 0)
       && le (lookup env 0) fresh)} =
 fun env fresh state ->
  ghost_ (copy_first_def state);
  ghost_ (models_def (fresh :: env) (copy_first state));
  ghost_ (models_def env state);
  ghost_ (shift_qf_exact env fresh state.residual);
  ghost_ (eval_qf_def (fresh :: env) (copy_first state).residual);
  ghost_ (eval_qf_def (fresh :: env)
            (And (Le (Var 0, Var 1), Le (Var 1, Var 0))));
  ghost_ (eval_qf_def (fresh :: env) (Le (Var 0, Var 1)));
  ghost_ (eval_qf_def (fresh :: env) (Le (Var 1, Var 0)));
  ghost_ (eval_term_def (fresh :: env) (Var 0));
  ghost_ (eval_term_def (fresh :: env) (Var 1));
  ghost_ (lookup_def (fresh :: env) 0);
  ghost_ (lookup_def (fresh :: env) 1);
  ()

let (copy_first_checked @ total) (state : state) :
    {result : state option |
      match result with
      | None -> not (shiftable_qf state.residual)
      | Some copied ->
        shiftable_qf state.residual && copied === copy_first state} =
  if shiftable_qf state.residual then Some (copy_first state) else None

let (project_copy_exact @ total) :
    (env : elt list) ->
    (state : {state : state | shiftable_qf state.residual}) ->
    {u : unit |
      models env (project_first (copy_first state)) = models env state} =
 fun env state ->
  ghost_ (project_first_exact env (copy_first state));
  ghost_ (copy_first_exact env Global state);
  ghost_ (copy_first_exact env Regional state);
  ghost_ (copy_first_exact env Local state);
  ghost_ (lookup_def env 0);
  ghost_ (le_def Global Global);
  ghost_ (le_def Regional Regional);
  ghost_ (le_def Local Local);
  (match lookup env 0 with Global | Regional | Local -> ());
  ghost_ (rank_def Global);
  ghost_ (rank_def Regional);
  ghost_ (rank_def Local);
  ()

let[@def] rec (swap_two_term @ total) term =
  match term with
  | Const value -> Const value
  | Var i -> Var (if i = 0 then 1 else if i = 1 then 0 else i)
  | Join (a, b) -> Join (swap_two_term a, swap_two_term b)
  | Meet (a, b) -> Meet (swap_two_term a, swap_two_term b)
  | Regional_to_global a -> Regional_to_global (swap_two_term a)
  | Table (global, regional, local, a) ->
    Table (global, regional, local, swap_two_term a)

let[@def] rec (swap_two_qf @ total) formula =
  match formula with
  | Le (a, b) -> Le (swap_two_term a, swap_two_term b)
  | And (a, b) -> And (swap_two_qf a, swap_two_qf b)
  | Or (a, b) -> Or (swap_two_qf a, swap_two_qf b)
  | Not a -> Not (swap_two_qf a)

let rec (swap_two_term_exact @ total) :
    (a : elt) -> (b : elt) ->
    (term : {term : term | scoped_term 2 term}) ->
    {u : unit |
      eval_term [b; a] (swap_two_term term) ===
      eval_term [a; b] term} =
 fun a b term ->
  ghost_ (scoped_term_def 2 term);
  ghost_ (swap_two_term_def term);
  ghost_ (eval_term_def [b; a] (swap_two_term term));
  ghost_ (eval_term_def [a; b] term);
  match term with
  | Const _ -> ()
  | Var i ->
    ghost_ (lookup_def [b; a] (if i = 0 then 1 else if i = 1 then 0 else i));
    ghost_ (lookup_def [a; b] i);
    ghost_ (lookup_def [a] 0);
    ghost_ (lookup_def [b] 0);
    ()
  | Join (x, y) | Meet (x, y) ->
    ghost_ (swap_two_term_exact a b x);
    ghost_ (swap_two_term_exact a b y);
    ()
  | Regional_to_global x | Table (_, _, _, x) ->
    ghost_ (swap_two_term_exact a b x);
    ()

let rec (swap_two_qf_exact @ total) :
    (a : elt) -> (b : elt) ->
    (formula : {formula : qf | scoped_qf 2 formula}) ->
    {u : unit |
      eval_qf [b; a] (swap_two_qf formula) =
      eval_qf [a; b] formula} =
 fun a b formula ->
  ghost_ (scoped_qf_def 2 formula);
  ghost_ (swap_two_qf_def formula);
  ghost_ (eval_qf_def [b; a] (swap_two_qf formula));
  ghost_ (eval_qf_def [a; b] formula);
  match formula with
  | Le (x, y) ->
    ghost_ (swap_two_term_exact a b x);
    ghost_ (swap_two_term_exact a b y);
    ()
  | And (x, y) | Or (x, y) ->
    ghost_ (swap_two_qf_exact a b x);
    ghost_ (swap_two_qf_exact a b y);
    ()
  | Not x ->
    ghost_ (swap_two_qf_exact a b x);
    ()

let[@def] (project_second @ total) state =
  project_first { residual = swap_two_qf state.residual }

let (project_second_exact @ total) :
    (a : elt) ->
    (state : {state : state | scoped_qf 2 state.residual}) ->
    {u : unit |
      models [a] (project_second state) =
      (models [a; Global] state
       || models [a; Regional] state
       || models [a; Local] state)} =
 fun a state ->
  ghost_ (project_second_def state);
  ghost_ (project_first_exact [a]
            { residual = swap_two_qf state.residual });
  ghost_ (swap_two_qf_exact a Global state.residual);
  ghost_ (swap_two_qf_exact a Regional state.residual);
  ghost_ (swap_two_qf_exact a Local state.residual);
  ghost_
    (models_def [Global; a]
       { residual = swap_two_qf state.residual });
  ghost_
    (models_def [Regional; a]
       { residual = swap_two_qf state.residual });
  ghost_
    (models_def [Local; a]
       { residual = swap_two_qf state.residual });
  ghost_ (models_def [a; Global] state);
  ghost_ (models_def [a; Regional] state);
  ghost_ (models_def [a; Local] state);
  ()

let disjunctive_residual =
  assert_residual empty
    (Or (Le (Var 0, Var 1), Le (Var 0, Var 2)))

let (disjunctive_residual_exact @ total) :
    (v : elt) -> (x : elt) -> (y : elt) ->
    {u : unit |
      models [v; x; y] disjunctive_residual =
      le v (join x y)} =
 fun v x y ->
  ghost_
    (assert_residual_exact [v; x; y] empty
       (Or (Le (Var 0, Var 1), Le (Var 0, Var 2))));
  ghost_ (models_def [v; x; y] disjunctive_residual);
  ghost_ (models_def [v; x; y] empty);
  ghost_ (assert_residual_def empty
            (Or (Le (Var 0, Var 1), Le (Var 0, Var 2))));
  ghost_ (eval_qf_def [v; x; y] disjunctive_residual.residual);
  ghost_ (eval_qf_def [v; x; y]
            (Or (Le (Var 0, Var 1), Le (Var 0, Var 2))));
  ghost_ (eval_qf_def [v; x; y] (Le (Var 0, Var 1)));
  ghost_ (eval_qf_def [v; x; y] (Le (Var 0, Var 2)));
  ghost_ (eval_qf_def [v; x; y]
            (Le (Const Global, Const Global)));
  ghost_ (eval_term_def [v; x; y] (Const Global));
  ghost_ (le_def Global Global);
  ghost_ (eval_term_def [v; x; y] (Var 0));
  ghost_ (eval_term_def [v; x; y] (Var 1));
  ghost_ (eval_term_def [v; x; y] (Var 2));
  ghost_ (lookup_def [v; x; y] 0);
  ghost_ (lookup_def [v; x; y] 1);
  ghost_ (lookup_def [v; x; y] 2);
  ghost_ (lookup_def [x; y] 0);
  ghost_ (lookup_def [x; y] 1);
  ghost_ (lookup_def [y] 0);
  ghost_ (join_def x y);
  ghost_ (le_def v x);
  ghost_ (le_def v y);
  ghost_ (le_def v (join x y));
  ghost_ (le_def x y);
  ghost_ (rank_def v);
  ghost_ (rank_def x);
  ghost_ (rank_def y);
  ghost_ (rank_def (join x y));
  (match v with Global | Regional | Local -> ());
  (match x with Global | Regional | Local -> ());
  (match y with Global | Regional | Local -> ());
  ()
