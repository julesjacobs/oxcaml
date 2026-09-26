(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_qe.ml mode_solver_projection.ml";
 native;
*)

open Mode_solver_qe

let[@def] rec (lower_join @ total) xs =
  match xs with
  | [] -> { a = false; b = false }
  | x :: rest -> join x (lower_join rest)

let[@def] rec (upper_meet @ total) xs =
  match xs with
  | [] -> { a = true; b = true }
  | x :: rest -> meet x (upper_meet rest)

let[@def] rec (above_all @ total) xs z =
  match xs with
  | [] -> true
  | x :: rest -> le x z && above_all rest z

let[@def] rec (below_all @ total) xs z =
  match xs with
  | [] -> true
  | x :: rest -> le z x && below_all rest z

let rec (lower_join_exact @ total) :
    (xs : elt list) -> (z : elt) ->
    {u : unit | le (lower_join xs) z = above_all xs z} =
 fun xs z ->
  ghost_ (lower_join_def xs);
  ghost_ (above_all_def xs z);
  match xs with
  | [] ->
    ghost_ (le_def { a = false; b = false } z);
    ()
  | x :: rest ->
    ghost_ (lower_join_exact rest z);
    ghost_ (join_def x (lower_join rest));
    ghost_ (le_def x z);
    ghost_ (le_def (lower_join rest) z);
    ghost_ (le_def (join x (lower_join rest)) z);
    ()

let rec (upper_meet_exact @ total) :
    (xs : elt list) -> (z : elt) ->
    {u : unit | le z (upper_meet xs) = below_all xs z} =
 fun xs z ->
  ghost_ (upper_meet_def xs);
  ghost_ (below_all_def xs z);
  match xs with
  | [] ->
    ghost_ (le_def z { a = true; b = true });
    ()
  | x :: rest ->
    ghost_ (upper_meet_exact rest z);
    ghost_ (meet_def x (upper_meet rest));
    ghost_ (le_def z x);
    ghost_ (le_def z (upper_meet rest));
    ghost_ (le_def z (meet x (upper_meet rest)));
    ()

let[@def] (admits_many @ total) lowers uppers z =
  above_all lowers z && below_all uppers z

let[@def] (project_many @ total) lowers uppers =
  le (lower_join lowers) (upper_meet uppers)

let[@def] rec (all_pairs @ total) lowers uppers =
  match lowers with
  | [] -> true
  | x :: rest -> below_all uppers x && all_pairs rest uppers

let rec (pairs_exact @ total) :
    (lowers : elt list) -> (uppers : elt list) ->
    {u : unit |
      above_all lowers (upper_meet uppers) =
        all_pairs lowers uppers} =
 fun lowers uppers ->
  ghost_ (above_all_def lowers (upper_meet uppers));
  ghost_ (all_pairs_def lowers uppers);
  match lowers with
  | [] -> ()
  | x :: rest ->
    ghost_ (upper_meet_exact uppers x);
    ghost_ (pairs_exact rest uppers);
    ()

let (project_pairs_exact @ total) :
    (lowers : elt list) -> (uppers : elt list) ->
    {u : unit | project_many lowers uppers = all_pairs lowers uppers} =
 fun lowers uppers ->
  ghost_ (project_many_def lowers uppers);
  ghost_ (lower_join_exact lowers (upper_meet uppers));
  ghost_ (pairs_exact lowers uppers);
  ()

let (project_sound @ total) :
    (lowers : elt list) -> (uppers : elt list) -> (z : elt) ->
    {u : unit | not (admits_many lowers uppers z) || project_many lowers uppers} =
 fun lowers uppers z ->
  ghost_ (lower_join_exact lowers z);
  ghost_ (upper_meet_exact uppers z);
  ghost_ (admits_many_def lowers uppers z);
  ghost_ (project_many_def lowers uppers);
  ghost_ (le_def (lower_join lowers) z);
  ghost_ (le_def z (upper_meet uppers));
  ghost_ (le_def (lower_join lowers) (upper_meet uppers));
  ()

let (project_complete @ total) :
    (lowers : elt list) -> (uppers : elt list) ->
    {u : unit |
      not (project_many lowers uppers)
      || admits_many lowers uppers (lower_join lowers)} =
 fun lowers uppers ->
  ghost_ (project_many_def lowers uppers);
  ghost_ (admits_many_def lowers uppers (lower_join lowers));
  ghost_ (lower_join_exact lowers (lower_join lowers));
  ghost_ (upper_meet_exact uppers (lower_join lowers));
  ghost_ (le_def (lower_join lowers) (lower_join lowers));
  ()
