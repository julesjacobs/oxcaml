(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_conditional_cut.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

let conditional_query =
  Forall
    (Disj
       (Neg (Plain (Le (Var 1, Var 0))),
        Plain (Le (Var 2, Var 0))))

let (conditional_at_exact @ total) :
    (x : elt) -> (v : elt) -> (u : elt) ->
    {w : unit |
      eval (u :: [x; v])
        (Disj
           (Neg (Plain (Le (Var 1, Var 0))),
            Plain (Le (Var 2, Var 0)))) =
        (not (le x u) || le v u)} =
 fun x v u ->
  let env = u :: [x; v] in
  ghost_
    (eval_def env
       (Disj
          (Neg (Plain (Le (Var 1, Var 0))),
           Plain (Le (Var 2, Var 0)))));
  ghost_ (eval_def env (Neg (Plain (Le (Var 1, Var 0)))));
  ghost_ (eval_def env (Plain (Le (Var 1, Var 0))));
  ghost_ (eval_def env (Plain (Le (Var 2, Var 0))));
  ghost_ (eval_qf_def env (Le (Var 1, Var 0)));
  ghost_ (eval_qf_def env (Le (Var 2, Var 0)));
  ghost_ (eval_term_def env (Var 0));
  ghost_ (eval_term_def env (Var 1));
  ghost_ (eval_term_def env (Var 2));
  ghost_ (lookup_def env 0);
  ghost_ (lookup_def env 1);
  ghost_ (lookup_def env 2);
  ghost_ (lookup_def [x; v] 0);
  ghost_ (lookup_def [x; v] 1);
  ghost_ (lookup_def [v] 0);
  ()

let (conditional_projection_exact @ total) :
    (x : elt) -> (v : elt) ->
    {u : unit |
      eval_qf [x; v] (eliminate conditional_query) = le v x} =
 fun x v ->
  ghost_ (eliminate_exact [x; v] conditional_query);
  ghost_ (eval_def [x; v] conditional_query);
  ghost_ (conditional_at_exact x v Global);
  ghost_ (conditional_at_exact x v Regional);
  ghost_ (conditional_at_exact x v Local);
  ghost_ (le_def v x);
  ghost_ (le_def x Global);
  ghost_ (le_def x Regional);
  ghost_ (le_def x Local);
  ghost_ (le_def v Global);
  ghost_ (le_def v Regional);
  ghost_ (le_def v Local);
  ghost_ (rank_def Global);
  ghost_ (rank_def Regional);
  ghost_ (rank_def Local);
  ghost_ (rank_def x);
  ghost_ (rank_def v);
  (match x with Global | Regional | Local -> ());
  (match v with Global | Regional | Local -> ());
  ()

let () =
  assert (eval_qf [Regional; Regional] (eliminate conditional_query));
  assert (not (eval_qf [Regional; Local] (eliminate conditional_query)))

let two_lower_bounds =
  Forall
    (Disj
       (Neg
          (Plain
             (And
                (Le (Var 1, Var 0), Le (Var 2, Var 0)))),
        Plain (Le (Var 3, Var 0))))

let (two_lower_bounds_at_exact @ total) :
    (x : elt) -> (y : elt) -> (v : elt) -> (u : elt) ->
    {w : unit |
      eval (u :: [x; y; v])
        (Disj
           (Neg
              (Plain
                 (And
                    (Le (Var 1, Var 0), Le (Var 2, Var 0)))),
            Plain (Le (Var 3, Var 0)))) =
        (not (le x u && le y u) || le v u)} =
 fun x y v u ->
  let env = u :: [x; y; v] in
  let guard = And (Le (Var 1, Var 0), Le (Var 2, Var 0)) in
  ghost_
    (eval_def env
       (Disj (Neg (Plain guard), Plain (Le (Var 3, Var 0)))));
  ghost_ (eval_def env (Neg (Plain guard)));
  ghost_ (eval_def env (Plain guard));
  ghost_ (eval_def env (Plain (Le (Var 3, Var 0))));
  ghost_ (eval_qf_def env guard);
  ghost_ (eval_qf_def env (Le (Var 1, Var 0)));
  ghost_ (eval_qf_def env (Le (Var 2, Var 0)));
  ghost_ (eval_qf_def env (Le (Var 3, Var 0)));
  ghost_ (eval_term_def env (Var 0));
  ghost_ (eval_term_def env (Var 1));
  ghost_ (eval_term_def env (Var 2));
  ghost_ (eval_term_def env (Var 3));
  ghost_ (lookup_def env 0);
  ghost_ (lookup_def env 1);
  ghost_ (lookup_def env 2);
  ghost_ (lookup_def env 3);
  ghost_ (lookup_def [x; y; v] 0);
  ghost_ (lookup_def [x; y; v] 1);
  ghost_ (lookup_def [x; y; v] 2);
  ghost_ (lookup_def [y; v] 0);
  ghost_ (lookup_def [y; v] 1);
  ghost_ (lookup_def [v] 0);
  ()

let (two_lower_bounds_exact @ total) :
    (x : elt) -> (y : elt) -> (v : elt) ->
    {w : unit |
      eval_qf [x; y; v] (eliminate two_lower_bounds) =
        le v (join x y)} =
 fun x y v ->
  ghost_ (eliminate_exact [x; y; v] two_lower_bounds);
  ghost_ (eval_def [x; y; v] two_lower_bounds);
  ghost_ (two_lower_bounds_at_exact x y v Global);
  ghost_ (two_lower_bounds_at_exact x y v Regional);
  ghost_ (two_lower_bounds_at_exact x y v Local);
  ghost_ (join_def x y);
  ghost_ (le_def v (join x y));
  ghost_ (le_def x Global);
  ghost_ (le_def x Regional);
  ghost_ (le_def x Local);
  ghost_ (le_def y Global);
  ghost_ (le_def y Regional);
  ghost_ (le_def y Local);
  ghost_ (le_def v Global);
  ghost_ (le_def v Regional);
  ghost_ (le_def v Local);
  ghost_ (rank_def Global);
  ghost_ (rank_def Regional);
  ghost_ (rank_def Local);
  ghost_ (rank_def x);
  ghost_ (rank_def y);
  ghost_ (rank_def v);
  ghost_ (rank_def (join x y));
  (match x with Global | Regional | Local -> ());
  (match y with Global | Regional | Local -> ());
  (match v with Global | Regional | Local -> ());
  ()
