(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_symbolic_projection.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

type bounds : immutable_data = { lowers : term list; uppers : term list }

let[@def] rec (lower_ok @ total) env value lowers =
  match lowers with
  | [] -> true
  | lower :: rest ->
    le (eval_term env lower) value && lower_ok env value rest

let[@def] rec (upper_ok @ total) env value uppers =
  match uppers with
  | [] -> true
  | upper :: rest ->
    le value (eval_term env upper) && upper_ok env value rest

let[@def] rec (lower_join @ total) env lowers =
  match lowers with
  | [] -> Global
  | lower :: rest -> join (eval_term env lower) (lower_join env rest)

let (le_transitive @ total) :
    (a : elt) -> (b : elt) -> (c : elt) ->
    {u : unit | not (le a b && le b c) || le a c} =
 fun a b c ->
  ghost_ (le_def a b);
  ghost_ (le_def b c);
  ghost_ (le_def a c);
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def c);
  ()

let (join_above_left @ total) :
    (a : elt) -> (b : elt) ->
    {u : unit | le a (join a b)} =
 fun a b ->
  ghost_ (join_def a b);
  ghost_ (le_def a b);
  ghost_ (le_def a (join a b));
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def (join a b));
  ()

let (join_above_right @ total) :
    (a : elt) -> (b : elt) ->
    {u : unit | le b (join a b)} =
 fun a b ->
  ghost_ (join_def a b);
  ghost_ (le_def a b);
  ghost_ (le_def b (join a b));
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def (join a b));
  ()

let (join_below_upper @ total) :
    (a : elt) -> (b : elt) -> (upper : elt) ->
    {u : unit |
      not (le a upper && le b upper)
      || le (join a b) upper} =
 fun a b upper ->
  ghost_ (join_def a b);
  ghost_ (le_def a b);
  ghost_ (le_def a upper);
  ghost_ (le_def b upper);
  ghost_ (le_def (join a b) upper);
  ghost_ (rank_def a);
  ghost_ (rank_def b);
  ghost_ (rank_def upper);
  ghost_ (rank_def (join a b));
  ()

let rec (lower_ok_monotone @ total) :
    (env : elt list) -> (low : elt) -> (high : elt) ->
    (lowers : term list) ->
    {u : unit |
      not (le low high && lower_ok env low lowers)
      || lower_ok env high lowers} =
 fun env low high lowers ->
  ghost_ (lower_ok_def env low lowers);
  ghost_ (lower_ok_def env high lowers);
  match lowers with
  | [] -> ()
  | lower :: rest ->
    ghost_ (le_transitive (eval_term env lower) low high);
    ghost_ (lower_ok_monotone env low high rest);
    ()

let rec (upper_ok_antitone @ total) :
    (env : elt list) -> (low : elt) -> (high : elt) ->
    (uppers : term list) ->
    {u : unit |
      not (le low high && upper_ok env high uppers)
      || upper_ok env low uppers} =
 fun env low high uppers ->
  ghost_ (upper_ok_def env low uppers);
  ghost_ (upper_ok_def env high uppers);
  match uppers with
  | [] -> ()
  | upper :: rest ->
    ghost_ (le_transitive low high (eval_term env upper));
    ghost_ (upper_ok_antitone env low high rest);
    ()

let rec (lower_join_le_witness @ total) :
    (env : elt list) -> (value : elt) -> (lowers : term list) ->
    {u : unit |
      not (lower_ok env value lowers)
      || le (lower_join env lowers) value} =
 fun env value lowers ->
  ghost_ (lower_ok_def env value lowers);
  ghost_ (lower_join_def env lowers);
  match lowers with
  | [] ->
    ghost_ (le_def Global value);
    ghost_ (rank_def Global);
    ghost_ (rank_def value);
    (match value with Global | Regional | Local -> ());
    ()
  | lower :: rest ->
    ghost_ (lower_join_le_witness env value rest);
    ghost_
      (join_below_upper
         (eval_term env lower) (lower_join env rest) value);
    ()

let rec (lower_ok_at_join @ total) :
    (env : elt list) -> (lowers : term list) ->
    {u : unit | lower_ok env (lower_join env lowers) lowers} =
 fun env lowers ->
  ghost_ (lower_ok_def env (lower_join env lowers) lowers);
  ghost_ (lower_join_def env lowers);
  match lowers with
  | [] -> ()
  | lower :: rest ->
    ghost_ (lower_ok_at_join env rest);
    ghost_
      (join_above_left (eval_term env lower) (lower_join env rest));
    ghost_
      (join_above_right (eval_term env lower) (lower_join env rest));
    ghost_
      (lower_ok_monotone env (lower_join env rest)
         (join (eval_term env lower) (lower_join env rest)) rest);
    ()

let[@def] (has_witness @ total) env bounds =
  (lower_ok env Global bounds.lowers
   && upper_ok env Global bounds.uppers)
  || (lower_ok env Regional bounds.lowers
      && upper_ok env Regional bounds.uppers)
  || (lower_ok env Local bounds.lowers
      && upper_ok env Local bounds.uppers)

let[@def] (projected @ total) env bounds =
  upper_ok env (lower_join env bounds.lowers) bounds.uppers

let (symbolic_projection_exact @ total) :
    (env : elt list) -> (bounds : bounds) ->
    {u : unit | projected env bounds = has_witness env bounds} =
 fun env bounds ->
  ghost_ (has_witness_def env bounds);
  ghost_ (projected_def env bounds);
  ghost_ (lower_join_le_witness env Global bounds.lowers);
  ghost_ (lower_join_le_witness env Regional bounds.lowers);
  ghost_ (lower_join_le_witness env Local bounds.lowers);
  ghost_ (upper_ok_antitone env (lower_join env bounds.lowers)
            Global bounds.uppers);
  ghost_ (upper_ok_antitone env (lower_join env bounds.lowers)
            Regional bounds.uppers);
  ghost_ (upper_ok_antitone env (lower_join env bounds.lowers)
            Local bounds.uppers);
  ghost_ (lower_ok_at_join env bounds.lowers);
  (match lower_join env bounds.lowers with
   | Global | Regional | Local -> ());
  ()

let[@def] rec (lower_join_term @ total) lowers =
  match lowers with
  | [] -> Const Global
  | lower :: rest -> Join (lower, lower_join_term rest)

let[@def] rec (upper_formula @ total) lower uppers =
  match uppers with
  | [] -> Le (Const Global, Const Global)
  | upper :: rest ->
    And (Le (lower, upper), upper_formula lower rest)

let[@def] (project_bounds @ total) bounds =
  upper_formula (lower_join_term bounds.lowers) bounds.uppers

let rec (lower_join_term_exact @ total) :
    (env : elt list) -> (lowers : term list) ->
    {u : unit |
      eval_term env (lower_join_term lowers) ===
        lower_join env lowers} =
 fun env lowers ->
  ghost_ (lower_join_term_def lowers);
  ghost_ (eval_term_def env (lower_join_term lowers));
  ghost_ (lower_join_def env lowers);
  match lowers with
  | [] -> ()
  | lower :: rest ->
    ghost_ (lower_join_term_exact env rest);
    ()

let rec (upper_formula_exact @ total) :
    (env : elt list) -> (lower : term) ->
    (uppers : term list) ->
    {u : unit |
      eval_qf env (upper_formula lower uppers) =
        upper_ok env (eval_term env lower) uppers} =
 fun env lower uppers ->
  ghost_ (upper_formula_def lower uppers);
  ghost_ (eval_qf_def env (upper_formula lower uppers));
  ghost_ (upper_ok_def env (eval_term env lower) uppers);
  match uppers with
  | [] ->
    ghost_ (eval_qf_def env (Le (Const Global, Const Global)));
    ghost_ (eval_term_def env (Const Global));
    ghost_ (le_def Global Global);
    ghost_ (rank_def Global);
    ()
  | upper :: rest ->
    ghost_ (eval_qf_def env (Le (lower, upper)));
    ghost_ (upper_formula_exact env lower rest);
    ()

let (project_bounds_exact @ total) :
    (env : elt list) -> (bounds : bounds) ->
    {u : unit |
      eval_qf env (project_bounds bounds) = has_witness env bounds} =
 fun env bounds ->
  ghost_ (project_bounds_def bounds);
  ghost_
    (upper_formula_exact env (lower_join_term bounds.lowers)
       bounds.uppers);
  ghost_ (lower_join_term_exact env bounds.lowers);
  ghost_ (symbolic_projection_exact env bounds);
  ghost_ (projected_def env bounds);
  ()
