(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_rigid_slice.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

type relation : immutable_data =
  { gg : bool; gr : bool; gl : bool;
    rg : bool; rr : bool; rl : bool;
    lg : bool; lr : bool; ll : bool }

let[@def] (holds @ total) r v u =
  match v, u with
  | Global, Global -> r.gg
  | Global, Regional -> r.gr
  | Global, Local -> r.gl
  | Regional, Global -> r.rg
  | Regional, Regional -> r.rr
  | Regional, Local -> r.rl
  | Local, Global -> r.lg
  | Local, Regional -> r.lr
  | Local, Local -> r.ll

let[@def] (feasible @ total) r v =
  holds r v Global || holds r v Regional || holds r v Local

let[@def] (conditional_floor @ total) r v =
  if holds r v Global then Global
  else if holds r v Regional then Regional
  else Local

let[@def] (valid_at @ total) r v =
  (not (holds r v Global) || le v (regional_to_global Global))
  && (not (holds r v Regional) || le v (regional_to_global Regional))
  && (not (holds r v Local) || le v (regional_to_global Local))

let[@def] (check_at @ total) r v =
  not (feasible r v)
  || le v (regional_to_global (conditional_floor r v))

let (slice_exact_at @ total) :
    (r : relation) -> (v : elt) ->
    {u : unit | check_at r v = valid_at r v} =
 fun r v ->
  ghost_ (feasible_def r v);
  ghost_ (conditional_floor_def r v);
  ghost_ (check_at_def r v);
  ghost_ (valid_at_def r v);
  ghost_ (holds_def r v Global);
  ghost_ (holds_def r v Regional);
  ghost_ (holds_def r v Local);
  ghost_ (regional_to_global_def Global);
  ghost_ (regional_to_global_def Regional);
  ghost_ (regional_to_global_def Local);
  ghost_ (regional_to_global_def (conditional_floor r v));
  ghost_ (le_def v (regional_to_global Global));
  ghost_ (le_def v (regional_to_global Regional));
  ghost_ (le_def v (regional_to_global Local));
  ghost_ (le_def v (regional_to_global (conditional_floor r v)));
  ghost_ (rank_def v);
  ghost_ (rank_def Global);
  ghost_ (rank_def Regional);
  ghost_ (rank_def Local);
  (match v with Global | Regional | Local -> ());
  ()

let[@def] (valid @ total) r =
  valid_at r Global && valid_at r Regional && valid_at r Local

let[@def] (check @ total) r =
  check_at r Global && check_at r Regional && check_at r Local

let (slice_exact @ total) :
    (r : relation) -> {u : unit | check r = valid r} =
 fun r ->
  ghost_ (slice_exact_at r Global);
  ghost_ (slice_exact_at r Regional);
  ghost_ (slice_exact_at r Local);
  ghost_ (check_def r);
  ghost_ (valid_def r);
  ()
