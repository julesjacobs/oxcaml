(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_graph_semantics.mli mode_solver_graph_semantics.ml mode_solver_graph_qe_proof.ml mode_solver_graph_qe.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

open Mode_solver_graph_semantics
include Mode_solver_graph_qe_proof

let rigid_graph =
  [{left = Var 0; right = Regional_to_global (Var 0)}]

let outer_bound =
  assert_graph_subsumption
    (Le (Const Global, Const Global))
    []
    [{left = Var 2; right = Var 0};
     {left = Var 0; right = Const Regional}]

let () =
  assert
    (decide_graph_checked [(); ()]
       [{left = Var 0; right = Var 1}] = Some true);
  assert
    (decide_graph_checked [()] [{left = Var 1; right = Var 0}] = None);
  assert
    (decide_graph [(); ()] [{left = Var 0; right = Var 1}]);
  assert (eval_qf [] (project_many [(); ()] [{left = Var 0; right = Var 1}]));
  assert (eval_qf [Global] outer_bound);
  assert (eval_qf [Regional] outer_bound);
  assert (not (eval_qf [Local] outer_bound));
  assert (not (eval_qf [] (eliminate (Forall (Plain (compile rigid_graph))))));
  assert
    (not
       (eval_qf []
          (assert_graph_subsumption
             (Le (Const Global, Const Global))
             []
             [{left = Var 1; right = Var 0};
              {left = Var 0; right = Regional_to_global (Var 1)}])))
