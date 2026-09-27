open Vox_sat_spec

let (unsat_at @ total) : (n : int) ->
    (formula : {f : formula | unsatisfiable n f}) ->
    (assignment : bool list) ->
    {u : unit | not (eval_formula assignment formula)} @ ghost =
  fun n formula assignment -> ghost_ (
  Vox_sat_proof.semantic_unsat_at n formula assignment;
  ())
