(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_zap_obstruction.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

let[@def] (equal_in_order @ total) a b = le a b && le b a

let (pointwise_witness @ total) :
    (rigid : elt) -> {u : unit | equal_in_order rigid rigid} =
 fun rigid ->
  ghost_ (equal_in_order_def rigid rigid);
  ghost_ (le_def rigid rigid);
  ()

let (no_constant_witness @ total) :
    (constant : elt) ->
    {u : unit |
      not
        (equal_in_order Global constant
         && equal_in_order Local constant)} =
 fun constant ->
  ghost_ (equal_in_order_def Global constant);
  ghost_ (equal_in_order_def Local constant);
  ghost_ (le_def Global constant);
  ghost_ (le_def constant Global);
  ghost_ (le_def Local constant);
  ghost_ (le_def constant Local);
  (match constant with Global | Regional | Local -> ());
  ghost_ (rank_def constant);
  ghost_ (rank_def Global);
  ghost_ (rank_def Local);
  ()
