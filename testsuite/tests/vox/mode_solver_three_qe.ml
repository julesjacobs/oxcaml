(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_three_qe.ml";
 native;
*)

open Mode_solver_semantics
include Mode_solver_three_qe_proof

let equality a b =
  And (Le (a, b), Le (b, a))

let forall_exists_equal =
  Forall (Exists (Plain (equality (Var 1) (Var 0))))

let exists_forall_equal =
  Exists (Forall (Plain (equality (Var 1) (Var 0))))

let outer_example =
  Forall
    (Disj
       (Neg (Plain (Le (Var 0, Var 1))),
        Exists
          (Plain
             (And
                (Le (Var 0, Const Regional),
                 Le (Var 1, Var 0))))))

let outer_subsumption_residual =
  subsumption_residual
    (Le (Var 0, Var 1))
    (And (Le (Var 0, Const Regional), Le (Var 1, Var 0)))

let guarded_disjunction =
  Forall
    (Disj
       (Neg
          (Plain
             (And (Le (Var 1, Var 0), Le (Var 2, Var 0)))),
        Exists
          (Plain
             (And
                (Le (Const Local, Var 0), Le (Var 0, Var 1))))))

let rigid_morph =
  Forall (Plain (Le (Var 0, Regional_to_global (Var 0))))

let rigid_flexible_morph =
  Forall
    (Exists
       (Plain
          (And
             (Le (Var 1, Var 0),
              Le (Var 0, Regional_to_global (Var 1))))))

let () =
  assert (eval_qf [] (eliminate forall_exists_equal));
  assert (not (eval_qf [] (eliminate exists_forall_equal)));
  assert (eval_qf [Global] (eliminate outer_example));
  assert (eval_qf [Regional] (eliminate outer_example));
  assert (not (eval_qf [Local] (eliminate outer_example)));
  assert (eval_qf [Global] outer_subsumption_residual);
  assert (eval_qf [Regional] outer_subsumption_residual);
  assert (not (eval_qf [Local] outer_subsumption_residual));
  assert (not (eval_qf [Global; Global] (eliminate guarded_disjunction)));
  assert (eval_qf [Local; Global] (eliminate guarded_disjunction));
  assert (eval_qf [Global; Local] (eliminate guarded_disjunction));
  assert (eval_qf [Local; Local] (eliminate guarded_disjunction));
  assert (not (eval_qf [] (eliminate rigid_morph)));
  assert (not (eval_qf [] (eliminate rigid_flexible_morph)))
