(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_atomic.ml mode_solver_incoming.ml";
 native;
*)

open Mode_solver_atomic

type graph : immutable_data =
  { x_lower : int; x_upper : int;
    y_lower : int; y_upper : int;
    incoming : bool }

let[@def] (valid_graph @ total) g =
  0 <= g.x_lower && g.x_lower <= g.x_upper && g.x_upper <= 3
  && 0 <= g.y_lower && g.y_lower <= g.y_upper && g.y_upper <= 3
  && (not g.incoming || forward g.x_upper <= g.y_upper)

let[@def] (graph_model @ total) g x y =
  g.x_lower <= x && x <= g.x_upper
  && g.y_lower <= y && y <= g.y_upper
  && (not g.incoming || forward x <= y)

let[@def] (with_incoming @ total) g =
  { g with
    x_upper = if g.x_upper <= backward g.y_upper
              then g.x_upper else backward g.y_upper;
    incoming = true }

let (incoming_exact_at @ total) :
    (g : {g : graph | valid_graph g}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    (y : {y : int | 0 <= y && y <= 3}) ->
    {u : unit |
      graph_model (with_incoming g) x y =
      (graph_model g x y && forward x <= y)} =
 fun g x y ->
  ghost_ (valid_graph_def g);
  ghost_ (with_incoming_def g);
  ghost_ (graph_model_def g x y);
  ghost_ (graph_model_def (with_incoming g) x y);
  ghost_ (forward_def x);
  ghost_ (backward_def g.y_upper);
  ()

let (incoming_failure_refutes @ total) :
    (g : {g : graph | valid_graph g
      && g.x_lower > (with_incoming g).x_upper}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    (y : {y : int | 0 <= y && y <= 3}) ->
    {u : unit | not (graph_model g x y && forward x <= y)} =
 fun g x y ->
  ghost_ (incoming_exact_at g x y);
  ghost_ (with_incoming_def g);
  ghost_ (graph_model_def (with_incoming g) x y);
  ()

let (incoming_greatest @ total) :
    (g : {g : graph | valid_graph g
      && g.x_lower <= (with_incoming g).x_upper}) ->
    {u : unit |
      valid_graph (with_incoming g)
      && graph_model (with_incoming g)
           (with_incoming g).x_upper (with_incoming g).y_upper} =
 fun g ->
  ghost_ (valid_graph_def g);
  ghost_ (with_incoming_def g);
  ghost_ (valid_graph_def (with_incoming g));
  ghost_ (graph_model_def (with_incoming g)
            (with_incoming g).x_upper (with_incoming g).y_upper);
  ghost_ (forward_def (with_incoming g).x_upper);
  ghost_ (backward_def g.y_upper);
  ()

let (incoming_greatest_at @ total) :
    (g : {g : graph | valid_graph g}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    (y : {y : int | 0 <= y && y <= 3}) ->
    {u : unit | not (graph_model (with_incoming g) x y)
      || (x <= (with_incoming g).x_upper
          && y <= (with_incoming g).y_upper)} =
 fun g x y ->
  ghost_ (graph_model_def (with_incoming g) x y);
  ()

type result = Added of graph * graph | Rejected of graph

let (add_incoming @ total) (g : {g : graph | valid_graph g}) :
    {r : result |
      match r with
      | Added (before, after) ->
        before === g && after === with_incoming g && valid_graph after
      | Rejected before ->
        before === g && g.x_lower > (with_incoming g).x_upper} =
  let after = with_incoming g in
  ghost_ (with_incoming_def g);
  if g.x_lower <= after.x_upper then
    (ghost_ (incoming_greatest g);
     Added (g, after))
  else Rejected g

let (undo_incoming @ total) :
    (g : graph) ->
    (r : {r : result |
      match r with
      | Added (before, _) | Rejected before -> before === g}) ->
    {restored : graph | restored === g} =
 fun g r ->
  match r with
  | Added (restored, _) | Rejected restored -> restored
