(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_guarded_semantics.mli mode_solver_guarded_semantics.ml mode_solver_retained_symbolic.ml mode_solver_quantifier_blocks.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof
open Mode_solver_guarded_semantics
open Mode_solver_retained_symbolic

let (coalesce_existential @ total) :
    (env : elt list) -> (guard : qf) -> (witness : qf) ->
    {u : unit | game [Existential; Existential] env guard witness =
      ((eval_qf (Global :: Global :: env) guard && eval_qf (Global :: Global :: env) witness) || (eval_qf (Regional :: Global :: env) guard && eval_qf (Regional :: Global :: env) witness) || (eval_qf (Local :: Global :: env) guard && eval_qf (Local :: Global :: env) witness) || (eval_qf (Global :: Regional :: env) guard && eval_qf (Global :: Regional :: env) witness) || (eval_qf (Regional :: Regional :: env) guard && eval_qf (Regional :: Regional :: env) witness) || (eval_qf (Local :: Regional :: env) guard && eval_qf (Local :: Regional :: env) witness) || (eval_qf (Global :: Local :: env) guard && eval_qf (Global :: Local :: env) witness) || (eval_qf (Regional :: Local :: env) guard && eval_qf (Regional :: Local :: env) witness) || (eval_qf (Local :: Local :: env) guard && eval_qf (Local :: Local :: env) witness))} =
 fun env guard witness ->
  ghost_ (game_def [Existential; Existential] env guard witness);
  ghost_ (game_def [Existential] (Global :: env) guard witness);
  ghost_ (admissible_def [Existential] (Global :: env) guard);
  ghost_ (game_def [] (Global :: Global :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Global :: Global :: env) guard);
  ghost_ (game_def [] (Regional :: Global :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Regional :: Global :: env) guard);
  ghost_ (game_def [] (Local :: Global :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Local :: Global :: env) guard);
  ghost_ (game_def [Existential] (Regional :: env) guard witness);
  ghost_ (admissible_def [Existential] (Regional :: env) guard);
  ghost_ (game_def [] (Global :: Regional :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Global :: Regional :: env) guard);
  ghost_ (game_def [] (Regional :: Regional :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Regional :: Regional :: env) guard);
  ghost_ (game_def [] (Local :: Regional :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Local :: Regional :: env) guard);
  ghost_ (game_def [Existential] (Local :: env) guard witness);
  ghost_ (admissible_def [Existential] (Local :: env) guard);
  ghost_ (game_def [] (Global :: Local :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Global :: Local :: env) guard);
  ghost_ (game_def [] (Regional :: Local :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Regional :: Local :: env) guard);
  ghost_ (game_def [] (Local :: Local :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Local :: Local :: env) guard);
  ()

let (coalesce_universal @ total) :
    (env : elt list) -> (guard : qf) -> (witness : qf) ->
    {u : unit | game [Universal; Universal] env guard witness =
      ((not (eval_qf (Global :: Global :: env) guard) || eval_qf (Global :: Global :: env) witness) && (not (eval_qf (Regional :: Global :: env) guard) || eval_qf (Regional :: Global :: env) witness) && (not (eval_qf (Local :: Global :: env) guard) || eval_qf (Local :: Global :: env) witness) && (not (eval_qf (Global :: Regional :: env) guard) || eval_qf (Global :: Regional :: env) witness) && (not (eval_qf (Regional :: Regional :: env) guard) || eval_qf (Regional :: Regional :: env) witness) && (not (eval_qf (Local :: Regional :: env) guard) || eval_qf (Local :: Regional :: env) witness) && (not (eval_qf (Global :: Local :: env) guard) || eval_qf (Global :: Local :: env) witness) && (not (eval_qf (Regional :: Local :: env) guard) || eval_qf (Regional :: Local :: env) witness) && (not (eval_qf (Local :: Local :: env) guard) || eval_qf (Local :: Local :: env) witness))} =
 fun env guard witness ->
  ghost_ (game_def [Universal; Universal] env guard witness);
  ghost_ (game_def [Universal] (Global :: env) guard witness);
  ghost_ (admissible_def [Universal] (Global :: env) guard);
  ghost_ (game_def [] (Global :: Global :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Global :: Global :: env) guard);
  ghost_ (game_def [] (Regional :: Global :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Regional :: Global :: env) guard);
  ghost_ (game_def [] (Local :: Global :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Local :: Global :: env) guard);
  ghost_ (game_def [Universal] (Regional :: env) guard witness);
  ghost_ (admissible_def [Universal] (Regional :: env) guard);
  ghost_ (game_def [] (Global :: Regional :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Global :: Regional :: env) guard);
  ghost_ (game_def [] (Regional :: Regional :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Regional :: Regional :: env) guard);
  ghost_ (game_def [] (Local :: Regional :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Local :: Regional :: env) guard);
  ghost_ (game_def [Universal] (Local :: env) guard witness);
  ghost_ (admissible_def [Universal] (Local :: env) guard);
  ghost_ (game_def [] (Global :: Local :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Global :: Local :: env) guard);
  ghost_ (game_def [] (Regional :: Local :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Regional :: Local :: env) guard);
  ghost_ (game_def [] (Local :: Local :: env) guard witness);
  ghost_ (admissible_def ([] : quantifier list) (Local :: Local :: env) guard);
  ()
