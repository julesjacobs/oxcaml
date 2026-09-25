(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_residual_gap.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

type assignment : immutable_data = { x : elt; y : elt; v : elt }

type selector : immutable_data = X | Y | V | Constant of elt

type edge : immutable_data = { left : selector; right : selector }

let[@def] (select @ total) assignment selector = match selector with
  | X -> assignment.x
  | Y -> assignment.y
  | V -> assignment.v
  | Constant value -> value

let[@def] (meet_assignment @ total) a b =
  { x = meet a.x b.x; y = meet a.y b.y; v = meet a.v b.v }

let (meet_monotone @ total) :
    (a : elt) -> (b : elt) -> (c : elt) -> (d : elt) ->
    {u : unit |
      not (le a c && le b d) || le (meet a b) (meet c d)} =
 fun a b c d ->
  ghost_ (le_def a c);
  ghost_ (le_def b d);
  ghost_ (le_def (meet a b) (meet c d));
  ghost_ (meet_def a b);
  ghost_ (meet_def c d);
  ghost_ (le_def a b);
  ghost_ (le_def c d);
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def c);
  ghost_ (rank_def d);
  ghost_ (rank_def (meet a b));
  ghost_ (rank_def (meet c d));
  ()

let (select_meet @ total) :
    (a : assignment) -> (b : assignment) -> (s : selector) ->
    {u : unit |
      select (meet_assignment a b) s === meet (select a s) (select b s)} =
 fun a b s ->
  ghost_ (meet_assignment_def a b);
  ghost_ (select_def (meet_assignment a b) s);
  ghost_ (select_def a s);
  ghost_ (select_def b s);
  match s with
  | X | Y | V -> ()
  | Constant value ->
    ghost_ (meet_def value value);
    ghost_ (le_def value value);
    ()

let[@def] (holds @ total) edge assignment =
  le (select assignment edge.left) (select assignment edge.right)

let (edge_meet_closed @ total) :
    (edge : edge) -> (a : assignment) -> (b : assignment) ->
    {u : unit |
      not (holds edge a && holds edge b)
      || holds edge (meet_assignment a b)} =
 fun edge a b ->
  ghost_ (holds_def edge a);
  ghost_ (holds_def edge b);
  ghost_ (holds_def edge (meet_assignment a b));
  ghost_ (select_meet a b edge.left);
  ghost_ (select_meet a b edge.right);
  ghost_
    (meet_monotone
       (select a edge.left) (select b edge.left)
       (select a edge.right) (select b edge.right));
  ()

let[@def] rec (models @ total) graph assignment =
  match graph with
  | [] -> true
  | edge :: rest -> holds edge assignment && models rest assignment

let rec (graph_meet_closed @ total) :
    (graph : edge list) -> (a : assignment) -> (b : assignment) ->
    {u : unit |
      not (models graph a && models graph b)
      || models graph (meet_assignment a b)} =
 fun graph a b ->
  ghost_ (models_def graph a);
  ghost_ (models_def graph b);
  ghost_ (models_def graph (meet_assignment a b));
  match graph with
  | [] -> ()
  | edge :: rest ->
    ghost_ (edge_meet_closed edge a b);
    ghost_ (graph_meet_closed rest a b);
    ()

let[@def] (residual @ total) assignment =
  le assignment.v (join assignment.x assignment.y)

let first = {x = Regional; y = Global; v = Regional}
let second = {x = Global; y = Regional; v = Regional}

let (no_edge_graph_for_residual @ total) :
    (graph : edge list) ->
    {u : unit |
      not
        (models graph first = residual first
         && models graph second = residual second
         && models graph (meet_assignment first second) =
              residual (meet_assignment first second))} =
 fun graph ->
  ghost_ (residual_def first);
  ghost_ (residual_def second);
  ghost_ (residual_def (meet_assignment first second));
  ghost_ (meet_assignment_def first second);
  ghost_ (join_def Regional Global);
  ghost_ (join_def Global Regional);
  ghost_ (join_def Global Global);
  ghost_ (meet_def Regional Global);
  ghost_ (meet_def Global Regional);
  ghost_ (meet_def Regional Regional);
  ghost_ (le_def Regional Regional);
  ghost_ (le_def Regional Global);
  ghost_ (le_def Global Regional);
  ghost_ (rank_def Global);
  ghost_ (rank_def Regional);
  ghost_ (graph_meet_closed graph first second);
  ()
