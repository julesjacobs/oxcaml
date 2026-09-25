(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_irreducible_bounds.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

let (order_by_join_irreducibles @ total) :
    (a : elt) -> (b : elt) ->
    {u : unit | le a b =
      ((not (le Regional a) || le Regional b)
       && (not (le Local a) || le Local b))} =
 fun a b ->
  ghost_ (le_def a b);
  ghost_ (le_def Regional a);
  ghost_ (le_def Regional b);
  ghost_ (le_def Local a);
  ghost_ (le_def Local b);
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def Regional);
  ghost_ (rank_def Local);
  ()

let (join_lower_bound @ total) :
    (c : elt) -> (a : elt) -> (b : elt) ->
    {u : unit | le c (join a b) =
      ((le c Global || not (le a Global && le b Global))
       && (le c Regional || not (le a Regional && le b Regional)))} =
 fun c a b ->
  ghost_ (join_def a b);
  ghost_ (le_def a b);
  ghost_ (le_def c (join a b));
  ghost_ (le_def c Global);
  ghost_ (le_def c Regional);
  ghost_ (le_def a Global);
  ghost_ (le_def b Global);
  ghost_ (le_def a Regional);
  ghost_ (le_def b Regional);
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def c);
  ghost_ (rank_def Global);
  ghost_ (rank_def Regional);
  ()

let (meet_upper_bound @ total) :
    (c : elt) -> (a : elt) -> (b : elt) ->
    {u : unit | le (meet a b) c =
      ((le Regional c || not (le Regional a && le Regional b))
       && (le Local c || not (le Local a && le Local b)))} =
 fun c a b ->
  ghost_ (meet_def a b);
  ghost_ (le_def a b);
  ghost_ (le_def (meet a b) c);
  ghost_ (le_def Regional c);
  ghost_ (le_def Local c);
  ghost_ (le_def Regional a);
  ghost_ (le_def Regional b);
  ghost_ (le_def Local a);
  ghost_ (le_def Local b);
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def c);
  ghost_ (rank_def Regional);
  ghost_ (rank_def Local);
  ()
