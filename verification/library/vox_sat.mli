open Vox_sat_spec

(** An unsatisfiable formula is falsified by every Boolean assignment, of any
    length. *)
val unsat_at : (n : int) ->
  (formula : {f : formula | unsatisfiable n f}) ->
  (assignment : bool list) ->
  {u : unit | not (eval_formula assignment formula)} @ ghost @@ total
