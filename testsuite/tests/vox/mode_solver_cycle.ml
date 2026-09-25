(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_atomic.ml mode_solver_incoming.ml mode_solver_cycle.ml";
 native;
*)

open Mode_solver_atomic
open Mode_solver_incoming

let[@def] (cycle_model @ total) g x y =
  graph_model g x y && y <= x

let[@def] (cycle_round @ total) g =
  let y_upper = if g.y_upper <= g.x_upper
                then g.y_upper else g.x_upper in
  let x_cap = backward y_upper in
  let x_upper = if g.x_upper <= x_cap then g.x_upper else x_cap in
  { g with x_upper; y_upper }

let[@def] (cycle_closed @ total) g =
  cycle_round (cycle_round (cycle_round g))

let (cycle_round_exact_at @ total) :
    (g : {g : graph | g.incoming
      && 0 <= g.x_upper && g.x_upper <= 3
      && 0 <= g.y_upper && g.y_upper <= 3}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    (y : {y : int | 0 <= y && y <= 3}) ->
    {u : unit |
      cycle_model (cycle_round g) x y = cycle_model g x y} =
 fun g x y ->
  ghost_ (cycle_round_def g);
  ghost_ (cycle_model_def (cycle_round g) x y);
  ghost_ (cycle_model_def g x y);
  ghost_ (graph_model_def (cycle_round g) x y);
  ghost_ (graph_model_def g x y);
  ghost_ (forward_def x);
  let y_cap = if g.y_upper <= g.x_upper
              then g.y_upper else g.x_upper in
  ghost_ (backward_def y_cap);
  ()

let (cycle_exact_at @ total) :
    (g : {g : graph | valid_graph g && g.incoming}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    (y : {y : int | 0 <= y && y <= 3}) ->
    {u : unit |
      cycle_model (cycle_closed g) x y =
      (graph_model g x y && y <= x)} =
 fun g x y ->
  ghost_ (valid_graph_def g);
  ghost_ (cycle_closed_def g);
  ghost_ (cycle_round_def g);
  ghost_ (cycle_round_def (cycle_round g));
  let y_cap = if g.y_upper <= g.x_upper
              then g.y_upper else g.x_upper in
  ghost_ (backward_def y_cap);
  let g1 = cycle_round g in
  let y_cap1 = if g1.y_upper <= g1.x_upper
               then g1.y_upper else g1.x_upper in
  ghost_ (backward_def y_cap1);
  ghost_ (cycle_round_exact_at g x y);
  ghost_ (cycle_round_exact_at (cycle_round g) x y);
  ghost_ (cycle_round_exact_at
            (cycle_round (cycle_round g)) x y);
  ghost_ (cycle_model_def (cycle_closed g) x y);
  ghost_ (cycle_model_def g x y);
  ghost_ (graph_model_def (cycle_closed g) x y);
  ghost_ (graph_model_def g x y);
  ghost_ (forward_def x);
  ghost_ (backward_def g.y_upper);
  ghost_ (backward_def (cycle_round g).y_upper);
  ghost_ (backward_def (cycle_round (cycle_round g)).y_upper);
  ()

let (cycle_failure_refutes @ total) :
    (g : {g : graph | valid_graph g && g.incoming
      && (g.x_lower > (cycle_closed g).x_upper
          || g.y_lower > (cycle_closed g).y_upper)}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    (y : {y : int | 0 <= y && y <= 3}) ->
    {u : unit | not (graph_model g x y && y <= x)} =
 fun g x y ->
  ghost_ (cycle_exact_at g x y);
  ghost_ (cycle_closed_def g);
  ghost_ (cycle_round_def g);
  ghost_ (cycle_round_def (cycle_round g));
  ghost_ (cycle_round_def (cycle_round (cycle_round g)));
  ghost_ (cycle_model_def (cycle_closed g) x y);
  ghost_ (graph_model_def (cycle_closed g) x y);
  ()

let (cycle_greatest @ total) :
    (g : {g : graph | valid_graph g && g.incoming
      && g.x_lower <= (cycle_closed g).x_upper
      && g.y_lower <= (cycle_closed g).y_upper}) ->
    {u : unit |
      cycle_model (cycle_closed g)
        (cycle_closed g).x_upper (cycle_closed g).y_upper} =
 fun g ->
  ghost_ (valid_graph_def g);
  ghost_ (cycle_closed_def g);
  ghost_ (cycle_round_def g);
  ghost_ (cycle_round_def (cycle_round g));
  ghost_ (cycle_round_def (cycle_round (cycle_round g)));
  ghost_ (cycle_model_def (cycle_closed g)
            (cycle_closed g).x_upper (cycle_closed g).y_upper);
  ghost_ (graph_model_def (cycle_closed g)
            (cycle_closed g).x_upper (cycle_closed g).y_upper);
  ghost_ (forward_def (cycle_closed g).x_upper);
  ghost_ (backward_def g.y_upper);
  ghost_ (backward_def (cycle_round g).y_upper);
  ghost_ (backward_def (cycle_round (cycle_round g)).y_upper);
  ()

type cycle_result = Added of graph * graph | Rejected of graph

let (add_reverse @ total) (g : {g : graph | valid_graph g && g.incoming}) :
    {r : cycle_result |
      match r with
      | Added (before, after) ->
        before === g && after === cycle_closed g
        && cycle_model after after.x_upper after.y_upper
      | Rejected before ->
        before === g
        && (g.x_lower > (cycle_closed g).x_upper
            || g.y_lower > (cycle_closed g).y_upper)} =
  let after = cycle_closed g in
  ghost_ (cycle_closed_def g);
  if g.x_lower <= after.x_upper && g.y_lower <= after.y_upper then
    (ghost_ (cycle_greatest g);
     Added (g, after))
  else Rejected g

let (undo_reverse @ total) :
    (g : graph) ->
    (r : {r : cycle_result |
      match r with
      | Added (before, _) | Rejected before -> before === g}) ->
    {restored : graph | restored === g} =
 fun g r ->
  match r with
  | Added (restored, _) | Rejected restored -> restored
