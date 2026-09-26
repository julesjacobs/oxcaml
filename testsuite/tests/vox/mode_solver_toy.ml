(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_qe.ml mode_solver_toy.ml";
 native;
*)

open Mode_solver_qe

type valuation : immutable_data = { x : elt; y : elt }

type atom : immutable_data =
  | Lower_x of elt
  | Upper_x of elt
  | Lower_y of elt
  | Upper_y of elt
  | X_le_y
  | Y_le_x

let[@def] (satisfies @ total) a v =
  match a with
  | Lower_x c -> le c v.x
  | Upper_x c -> le v.x c
  | Lower_y c -> le c v.y
  | Upper_y c -> le v.y c
  | X_le_y -> le v.x v.y
  | Y_le_x -> le v.y v.x

let[@def] rec (models @ total) cs v =
  match cs with
  | [] -> true
  | a :: rest -> satisfies a v && models rest v

let zero = { a = false; b = false }
let first = { a = true; b = false }
let second = { a = false; b = true }
let both = { a = true; b = true }

let[@def] (has_model @ total) cs =
  let at x y = models cs { x; y } in
  at zero zero || at zero first || at zero second || at zero both
  || at first zero || at first first || at first second || at first both
  || at second zero || at second first || at second second
  || at second both || at both zero || at both first
  || at both second || at both both

let (enumeration_complete @ total) :
    (cs : atom list) -> (v : valuation) ->
    {u : unit | not (models cs v) || has_model cs} =
 fun cs v ->
  ghost_ (has_model_def cs);
  ghost_ (le_def v.x v.x);
  ()

type result = Success of atom list | Failure

let (add @ total) (cs : atom list) (a : atom) :
    {r : result |
      match r with
      | Success after -> after === a :: cs && has_model after
      | Failure -> not (has_model (a :: cs))} =
  let after = a :: cs in
  if has_model after
  then Success after
  else Failure

let (success_exact_at @ total) :
    (cs : atom list) -> (a : atom) -> (v : valuation) ->
    {u : unit |
      models (a :: cs) v = (models cs v && satisfies a v)} =
 fun cs a v ->
  ghost_ (models_def (a :: cs) v);
  ()

let (failure_refutes @ total) :
    (cs : atom list) -> (a : atom) ->
    (v : valuation) ->
    {u : unit | has_model (a :: cs)
      || not (models cs v && satisfies a v)} =
 fun cs a v ->
  ghost_ (enumeration_complete (a :: cs) v);
  ghost_ (success_exact_at cs a v);
  ()
