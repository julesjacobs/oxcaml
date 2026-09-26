(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_symbolic_projection.ml mode_solver_level_cut.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof
open Mode_solver_symbolic_projection

type cut : immutable_data =
  { outer_lower : elt;
    outer_upper : elt;
    inner_lower : elt;
    inner_upper : elt;
    outer_to_inner : bool;
    inner_to_outer : bool }

let[@def] (model @ total) cut outer inner =
  le cut.outer_lower outer && le outer cut.outer_upper
  && le cut.inner_lower inner && le inner cut.inner_upper
  && (not cut.outer_to_inner || le outer inner)
  && (not cut.inner_to_outer || le inner outer)

let[@def] (bounds_of_cut @ total) cut =
  { lowers =
      (if cut.outer_to_inner
       then [Const cut.inner_lower; Var 0]
       else [Const cut.inner_lower]);
    uppers =
      (if cut.inner_to_outer
       then [Const cut.inner_upper; Var 0]
       else [Const cut.inner_upper]) }

let[@def] (outer_ok @ total) cut outer =
  le cut.outer_lower outer && le outer cut.outer_upper

let (cut_bounds_exact @ total) :
    (cut : cut) -> (outer : elt) -> (inner : elt) ->
    {u : unit |
      (lower_ok [outer] inner (bounds_of_cut cut).lowers
       && upper_ok [outer] inner (bounds_of_cut cut).uppers)
      =
      (le cut.inner_lower inner && le inner cut.inner_upper
       && (not cut.outer_to_inner || le outer inner)
       && (not cut.inner_to_outer || le inner outer))} =
 fun cut outer inner ->
  ghost_ (bounds_of_cut_def cut);
  ghost_ (lower_ok_def [outer] inner (bounds_of_cut cut).lowers);
  ghost_ (upper_ok_def [outer] inner (bounds_of_cut cut).uppers);
  ghost_ (lower_ok_def [outer] inner [Const cut.inner_lower]);
  ghost_ (lower_ok_def [outer] inner [Const cut.inner_lower; Var 0]);
  ghost_ (lower_ok_def [outer] inner [Var 0]);
  ghost_ (lower_ok_def [outer] inner []);
  ghost_ (upper_ok_def [outer] inner [Const cut.inner_upper]);
  ghost_ (upper_ok_def [outer] inner [Const cut.inner_upper; Var 0]);
  ghost_ (upper_ok_def [outer] inner [Var 0]);
  ghost_ (upper_ok_def [outer] inner []);
  ghost_ (eval_term_def [outer] (Const cut.inner_lower));
  ghost_ (eval_term_def [outer] (Const cut.inner_upper));
  ghost_ (eval_term_def [outer] (Var 0));
  ghost_ (lookup_def [outer] 0);
  (if cut.outer_to_inner then () else ());
  (if cut.inner_to_outer then () else ());
  ()

let[@def] (cut_residual @ total) cut outer =
  outer_ok cut outer
  && eval_qf [outer] (project_bounds (bounds_of_cut cut))

let (cut_projection_exact @ total) :
    (cut : cut) -> (outer : elt) ->
    {u : unit |
      cut_residual cut outer =
      (model cut outer Global
       || model cut outer Regional
       || model cut outer Local)} =
 fun cut outer ->
  ghost_ (cut_residual_def cut outer);
  ghost_ (outer_ok_def cut outer);
  ghost_ (model_def cut outer Global);
  ghost_ (model_def cut outer Regional);
  ghost_ (model_def cut outer Local);
  ghost_ (project_bounds_exact [outer] (bounds_of_cut cut));
  ghost_ (has_witness_def [outer] (bounds_of_cut cut));
  ghost_ (cut_bounds_exact cut outer Global);
  ghost_ (cut_bounds_exact cut outer Regional);
  ghost_ (cut_bounds_exact cut outer Local);
  ()

let[@def] (greatest_extension @ total) cut outer =
  meet cut.inner_upper (if cut.inner_to_outer then outer else Local)

let (greatest_dominates @ total) :
    (cut : cut) -> (outer : elt) -> (inner : elt) ->
    {u : unit |
      not (model cut outer inner)
      || le inner (greatest_extension cut outer)} =
 fun cut outer inner ->
  ghost_ (model_def cut outer inner);
  ghost_ (greatest_extension_def cut outer);
  ghost_ (meet_def cut.inner_upper
            (if cut.inner_to_outer then outer else Local));
  ghost_ (le_def inner cut.inner_upper);
  ghost_ (le_def inner outer);
  ghost_ (le_def inner (greatest_extension cut outer));
  ghost_ (rank_def cut.inner_upper);
  ghost_ (rank_def Local);
  ghost_ (rank_def outer);
  ghost_ (rank_def inner);
  ghost_ (rank_def (greatest_extension cut outer));
  (match cut.inner_upper with Global | Regional | Local -> ());
  (match outer with Global | Regional | Local -> ());
  (match inner with Global | Regional | Local -> ());
  (if cut.inner_to_outer then () else ());
  ()

let (greatest_below_caps @ total) :
    (cut : cut) -> (outer : elt) ->
    {u : unit |
      le (greatest_extension cut outer) cut.inner_upper
      && (not cut.inner_to_outer
          || le (greatest_extension cut outer) outer)} =
 fun cut outer ->
  ghost_ (greatest_extension_def cut outer);
  ghost_ (rank_def cut.inner_upper);
  ghost_ (rank_def outer);
  ghost_ (rank_def Local);
  if cut.inner_to_outer then begin
    ghost_ (meet_def cut.inner_upper outer);
    ghost_ (le_def cut.inner_upper outer);
    ghost_ (le_def (meet cut.inner_upper outer) cut.inner_upper);
    ghost_ (le_def (meet cut.inner_upper outer) outer);
    (match cut.inner_upper with Global | Regional | Local -> ());
    (match outer with Global | Regional | Local -> ());
    ()
  end else begin
    ghost_ (meet_def cut.inner_upper Local);
    ghost_ (le_def cut.inner_upper Local);
    ghost_ (le_def (meet cut.inner_upper Local) cut.inner_upper);
    (match cut.inner_upper with Global | Regional | Local -> ());
    ()
  end

let (greatest_extension_exact @ total) :
    (cut : cut) -> (outer : elt) -> (inner : elt) ->
    {u : unit |
      not (model cut outer inner)
      || model cut outer (greatest_extension cut outer)} =
 fun cut outer inner ->
  ghost_ (greatest_dominates cut outer inner);
  ghost_ (greatest_below_caps cut outer);
  ghost_ (model_def cut outer inner);
  ghost_ (greatest_extension_def cut outer);
  ghost_ (model_def cut outer (greatest_extension cut outer));
  ghost_ (le_transitive cut.inner_lower inner
            (greatest_extension cut outer));
  ghost_ (le_transitive outer inner
            (greatest_extension cut outer));
  ghost_ (le_def cut.inner_lower inner);
  ghost_ (le_def outer inner);
  ghost_ (le_def cut.inner_lower (greatest_extension cut outer));
  ghost_ (le_def outer (greatest_extension cut outer));
  ghost_ (le_def (greatest_extension cut outer) cut.inner_upper);
  ghost_ (le_def (greatest_extension cut outer) outer);
  ghost_ (meet_def cut.inner_upper
            (if cut.inner_to_outer then outer else Local));
  ghost_ (rank_def cut.inner_upper);
  ghost_ (rank_def outer);
  ghost_ (rank_def (greatest_extension cut outer));
  (match cut.inner_upper with Global | Regional | Local -> ());
  (match outer with Global | Regional | Local -> ());
  (if cut.inner_to_outer then () else ());
  ()
