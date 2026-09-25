(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_fixed_projection.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

let[@def] (fixed @ total) env value relation =
  (not (eval_qf (Global :: env) relation) || (match value with Global -> true | _ -> false))
  && (not (eval_qf (Regional :: env) relation) || (match value with Regional -> true | _ -> false))
  && (not (eval_qf (Local :: env) relation) || (match value with Local -> true | _ -> false))

let (fixed_projection_exact @ total) :
    (env : elt list) -> (value : elt) -> (relation : qf) ->
    {u : unit | not (fixed env value relation) ||
      eval_qf env (eliminate (Exists (Plain relation))) =
        eval_qf env (subst_qf value relation)} =
 fun env value relation ->
  ghost_ (fixed_def env value relation);
  ghost_ (subst_qf_exact env value relation);
  ghost_ (eliminate_exact env (Exists (Plain relation)));
  ghost_ (eval_def env (Exists (Plain relation)));
  ghost_ (eval_def (Global :: env) (Plain relation));
  ghost_ (eval_def (Regional :: env) (Plain relation));
  ghost_ (eval_def (Local :: env) (Plain relation));
  (match value with Global | Regional | Local -> ());
  ()

let (fixed_conjunction_projection @ total) :
    (env : elt list) -> (value : elt) -> (left : qf) -> (right : qf) ->
    {u : unit |
      eval_qf env (subst_qf value (And (left, right))) =
        (eval_qf env (subst_qf value left) &&
         eval_qf env (subst_qf value right))} =
 fun env value left right ->
  ghost_ (subst_qf_exact env value (And (left, right)));
  ghost_ (subst_qf_exact env value left);
  ghost_ (subst_qf_exact env value right);
  ghost_ (eval_qf_def (value :: env) (And (left, right)));
  ()
