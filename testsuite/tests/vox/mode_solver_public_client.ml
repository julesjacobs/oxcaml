(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_graph_semantics.mli mode_solver_graph_semantics.ml mode_solver_graph_qe_proof.ml mode_solver_guarded_semantics.mli mode_solver_guarded_semantics.ml mode_solver_retained_symbolic.ml mode_solver_public.mli mode_solver_public.ml mode_solver_public_client.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_guarded_semantics
open Mode_solver_public

let (residual_value @ total) env gamma guard obligation :
    {answer : bool | answer =
      (eval_qf env gamma && eval env (subsumption_formula guard obligation))} =
  ghost_ (assert_subsumption_exact env gamma guard obligation);
  eval_qf env (assert_subsumption gamma guard obligation)

let (guarded_value @ total) prefix env guard witness :
    {answer : bool | answer = normalized_game prefix env guard witness} =
  ghost_ (project_admissible_exact prefix env guard witness);
  eval_qf env (project_admissible prefix guard witness)

let (formula_value @ total) env formula :
    {answer : bool | answer = eval env formula} =
  ghost_ (eliminate_exact env formula);
  eval_qf env (eliminate formula)

let () =
  let equality = And (Le (Var 0, Var 1), Le (Var 1, Var 0)) in
  assert (decide_checked (Forall (Exists (Plain equality))) = Some true);
  assert (decide_checked (Exists (Forall (Plain equality))) = Some false);
  assert (decide_checked (Plain (Le (Var 0, Const Local))) = None);
  let truth = Le (Const Global, Const Local) in
  assert (guarded_value [Universal; Existential] [] truth equality);
  assert (not (guarded_value [Existential; Universal] [] truth equality))
