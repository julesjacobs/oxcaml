open Mode_solver_semantics

let (regionality_adjunction @ total) :
    (a : elt) -> (b : elt) ->
    {u : unit |
      le (regional_to_local a) b = le a (regional_to_global b)} =
 fun a b ->
  ghost_ (regional_to_local_def a);
  ghost_ (regional_to_global_def b);
  ghost_ (le_def (regional_to_local a) b);
  ghost_ (le_def a (regional_to_global b));
  (match a with Global | Regional | Local -> ());
  (match b with Global | Regional | Local -> ());
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def (regional_to_local a));
  ghost_ (rank_def (regional_to_global b));
  ()

let[@def] rec (subst_term @ total) value t =
  match t with
  | Const b -> Const b
  | Var i ->
    if i = 0 then Const value
    else if i > 0 then Var (i - 1)
    else Var i
  | Join (a, b) -> Join (subst_term value a, subst_term value b)
  | Meet (a, b) -> Meet (subst_term value a, subst_term value b)
  | Regional_to_global a -> Regional_to_global (subst_term value a)
  | Table (global, regional, local, a) ->
    Table (global, regional, local, subst_term value a)

let[@def] rec (subst_qf @ total) value q =
  match q with
  | Le (a, b) -> Le (subst_term value a, subst_term value b)
  | And (a, b) -> And (subst_qf value a, subst_qf value b)
  | Or (a, b) -> Or (subst_qf value a, subst_qf value b)
  | Not a -> Not (subst_qf value a)

let[@def] rec (eliminate @ total) f =
  match f with
  | Plain q -> q
  | Conj (a, b) -> And (eliminate a, eliminate b)
  | Disj (a, b) -> Or (eliminate a, eliminate b)
  | Neg a -> Not (eliminate a)
  | Exists a ->
    let q = eliminate a in
    Or
      (subst_qf Global q,
       Or (subst_qf Regional q, subst_qf Local q))
  | Forall a ->
    let q = eliminate a in
    And
      (subst_qf Global q,
       And (subst_qf Regional q, subst_qf Local q))

let (lookup_shift @ total) :
    (env : elt list) -> (value : elt) -> (i : int) ->
    {u : unit |
      lookup (value :: env) i ===
        if i = 0 then value
        else if i > 0 then lookup env (i - 1)
        else lookup env i} =
 fun env value i ->
  ghost_ (lookup_def (value :: env) i);
  if i < 0 then ghost_ (lookup_def env i);
  ()

let rec (subst_term_exact @ total) :
    (env : elt list) -> (value : elt) -> (t : term) ->
    {u : unit |
      eval_term env (subst_term value t) ===
        eval_term (value :: env) t} =
 fun env value t ->
  ghost_ (subst_term_def value t);
  ghost_ (eval_term_def env (subst_term value t));
  ghost_ (eval_term_def (value :: env) t);
  match t with
  | Const _ -> ()
  | Var i ->
    ghost_ (lookup_shift env value i);
    (if i = 0 then ()
     else if i > 0 then ghost_ (lookup_def env (i - 1))
     else ghost_ (lookup_def env i));
    ()
  | Join (a, b) | Meet (a, b) ->
    ghost_ (subst_term_exact env value a);
    ghost_ (subst_term_exact env value b);
    ghost_ (eval_term_def env (subst_term value a));
    ghost_ (eval_term_def env (subst_term value b));
    ghost_ (eval_term_def (value :: env) a);
    ghost_ (eval_term_def (value :: env) b);
    ()
  | Regional_to_global a ->
    ghost_ (subst_term_exact env value a);
    ()
  | Table (_, _, _, a) ->
    ghost_ (subst_term_exact env value a);
    ()

let rec (subst_qf_exact @ total) :
    (env : elt list) -> (value : elt) -> (q : qf) ->
    {u : unit |
      eval_qf env (subst_qf value q) =
        eval_qf (value :: env) q} =
 fun env value q ->
  ghost_ (subst_qf_def value q);
  ghost_ (eval_qf_def env (subst_qf value q));
  ghost_ (eval_qf_def (value :: env) q);
  match q with
  | Le (a, b) ->
    ghost_ (subst_term_exact env value a);
    ghost_ (subst_term_exact env value b);
    ()
  | And (a, b) | Or (a, b) ->
    ghost_ (subst_qf_exact env value a);
    ghost_ (subst_qf_exact env value b);
    ()
  | Not a ->
    ghost_ (subst_qf_exact env value a);
    ()

let rec (eliminate_exact @ total) :
    (env : elt list) -> (f : formula) ->
    {u : unit | eval_qf env (eliminate f) = eval env f} =
 fun env f ->
  ghost_ (eliminate_def f);
  ghost_ (eval_qf_def env (eliminate f));
  ghost_ (eval_def env f);
  match f with
  | Plain _ -> ()
  | Conj (a, b) | Disj (a, b) ->
    ghost_ (eliminate_exact env a);
    ghost_ (eliminate_exact env b);
    ()
  | Neg a ->
    ghost_ (eliminate_exact env a);
    ()
  | Exists a ->
    ghost_ (eliminate_exact (Global :: env) a);
    ghost_ (eliminate_exact (Regional :: env) a);
    ghost_ (eliminate_exact (Local :: env) a);
    ghost_ (subst_qf_exact env Global (eliminate a));
    ghost_ (subst_qf_exact env Regional (eliminate a));
    ghost_ (subst_qf_exact env Local (eliminate a));
    ghost_
      (eval_qf_def env
         (Or (subst_qf Regional (eliminate a),
              subst_qf Local (eliminate a))));
    ()
  | Forall a ->
    ghost_ (eliminate_exact (Global :: env) a);
    ghost_ (eliminate_exact (Regional :: env) a);
    ghost_ (eliminate_exact (Local :: env) a);
    ghost_ (subst_qf_exact env Global (eliminate a));
    ghost_ (subst_qf_exact env Regional (eliminate a));
    ghost_ (subst_qf_exact env Local (eliminate a));
    ghost_
      (eval_qf_def env
         (And (subst_qf Regional (eliminate a),
               subst_qf Local (eliminate a))));
    ()

let rec (subst_term_scoped @ total) :
    (depth : int) -> (value : elt) ->
    (t : {t : term | scoped_term (depth + 1) t}) ->
    {u : unit | scoped_term depth (subst_term value t)} =
 fun depth value t ->
  ghost_ (scoped_term_def (depth + 1) t);
  ghost_ (scoped_term_def depth (subst_term value t));
  ghost_ (subst_term_def value t);
  match t with
  | Const _ -> ()
  | Var _ -> ()
  | Join (a, b) | Meet (a, b) ->
    ghost_ (subst_term_scoped depth value a);
    ghost_ (subst_term_scoped depth value b);
    ()
  | Regional_to_global a | Table (_, _, _, a) ->
    ghost_ (subst_term_scoped depth value a);
    ()

let rec (subst_qf_scoped @ total) :
    (depth : int) -> (value : elt) ->
    (q : {q : qf | scoped_qf (depth + 1) q}) ->
    {u : unit | scoped_qf depth (subst_qf value q)} =
 fun depth value q ->
  ghost_ (scoped_qf_def (depth + 1) q);
  ghost_ (scoped_qf_def depth (subst_qf value q));
  ghost_ (subst_qf_def value q);
  match q with
  | Le (a, b) ->
    ghost_ (subst_term_scoped depth value a);
    ghost_ (subst_term_scoped depth value b);
    ()
  | And (a, b) | Or (a, b) ->
    ghost_ (subst_qf_scoped depth value a);
    ghost_ (subst_qf_scoped depth value b);
    ()
  | Not a ->
    ghost_ (subst_qf_scoped depth value a);
    ()

let rec (eliminate_scoped @ total) :
    (depth : int) ->
    (f : {f : formula | scoped depth f}) ->
    {u : unit | scoped_qf depth (eliminate f)} =
 fun depth f ->
  ghost_ (scoped_def depth f);
  ghost_ (scoped_qf_def depth (eliminate f));
  ghost_ (eliminate_def f);
  match f with
  | Plain _ -> ()
  | Conj (a, b) | Disj (a, b) ->
    ghost_ (eliminate_scoped depth a);
    ghost_ (eliminate_scoped depth b);
    ()
  | Neg a ->
    ghost_ (eliminate_scoped depth a);
    ()
  | Exists a ->
    ghost_ (eliminate_scoped (depth + 1) a);
    ghost_ (subst_qf_scoped depth Global (eliminate a));
    ghost_ (subst_qf_scoped depth Regional (eliminate a));
    ghost_ (subst_qf_scoped depth Local (eliminate a));
    ghost_
      (scoped_qf_def depth
         (Or (subst_qf Regional (eliminate a),
              subst_qf Local (eliminate a))));
    ()
  | Forall a ->
    ghost_ (eliminate_scoped (depth + 1) a);
    ghost_ (subst_qf_scoped depth Global (eliminate a));
    ghost_ (subst_qf_scoped depth Regional (eliminate a));
    ghost_ (subst_qf_scoped depth Local (eliminate a));
    ghost_
      (scoped_qf_def depth
         (And (subst_qf Regional (eliminate a),
               subst_qf Local (eliminate a))));
    ()

let[@def] (decide @ total) f = eval_qf [] (eliminate f)

let (decide_exact @ total) :
    (f : {f : formula | scoped 0 f}) ->
    {u : unit | decide f = eval [] f && scoped_qf 0 (eliminate f)} =
 fun f ->
  ghost_ (decide_def f);
  ghost_ (eliminate_exact [] f);
  ghost_ (eliminate_scoped 0 f);
  ()

let[@def] (subsumption_residual @ total) right obligation =
  eliminate (subsumption_formula right obligation)

let (subsumption_residual_exact @ total) :
    (env : elt list) -> (right : qf) -> (obligation : qf) ->
    {u : unit |
      eval_qf env (subsumption_residual right obligation) =
        eval env (subsumption_formula right obligation)} =
 fun env right obligation ->
  ghost_ (subsumption_residual_def right obligation);
  ghost_ (eliminate_exact env (subsumption_formula right obligation));
  ()

let (subsumption_residual_scoped @ total) :
    (depth : int) ->
    (right : {right : qf | scoped_qf (depth + 1) right}) ->
    (obligation : {obligation : qf |
      scoped_qf (depth + 2) obligation}) ->
    {u : unit |
      scoped_qf depth (subsumption_residual right obligation)} =
 fun depth right obligation ->
  ghost_ (subsumption_formula_def right obligation);
  ghost_ (scoped_def depth (subsumption_formula right obligation));
  ghost_ (scoped_def (depth + 1)
            (Disj (Neg (Plain right), Exists (Plain obligation))));
  ghost_ (scoped_def (depth + 1) (Neg (Plain right)));
  ghost_ (scoped_def (depth + 1) (Plain right));
  ghost_ (scoped_def (depth + 1) (Exists (Plain obligation)));
  ghost_ (scoped_def (depth + 2) (Plain obligation));
  ghost_ (subsumption_residual_def right obligation);
  ghost_ (eliminate_scoped depth (subsumption_formula right obligation));
  ()

let[@def] (assert_subsumption @ total) gamma right obligation =
  And (gamma, subsumption_residual right obligation)

let (assert_subsumption_exact @ total) :
    (env : elt list) -> (gamma : qf) ->
    (right : qf) -> (obligation : qf) ->
    {u : unit |
      eval_qf env (assert_subsumption gamma right obligation) =
        (eval_qf env gamma
         && eval env (subsumption_formula right obligation))} =
 fun env gamma right obligation ->
  ghost_ (assert_subsumption_def gamma right obligation);
  ghost_ (eval_qf_def env (assert_subsumption gamma right obligation));
  ghost_ (subsumption_residual_exact env right obligation);
  ()
