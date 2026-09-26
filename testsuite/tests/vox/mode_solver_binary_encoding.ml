(* TEST
 flags = "-extension refinement_types";
 has-z3;
 all_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_binary_encoding.ml";
 native;
*)

open Mode_solver_semantics
open Mode_solver_three_qe_proof

let[@def] (decode @ total) high low =
  if high then Local else if low then Regional else Global

let[@def] (binary_exists @ total) env body =
  eval_qf (decode false false :: env) body
  || eval_qf (decode false true :: env) body
  || eval_qf (decode true false :: env) body
  || eval_qf (decode true true :: env) body

let[@def] (binary_forall @ total) env body =
  eval_qf (decode false false :: env) body
  && eval_qf (decode false true :: env) body
  && eval_qf (decode true false :: env) body
  && eval_qf (decode true true :: env) body

let (binary_exists_exact @ total) :
    (env : elt list) -> (body : qf) ->
    {u : unit |
      binary_exists env body = eval env (Exists (Plain body))} =
 fun env body ->
  ghost_ (binary_exists_def env body);
  ghost_ (decode_def false false);
  ghost_ (decode_def false true);
  ghost_ (decode_def true false);
  ghost_ (decode_def true true);
  ghost_ (eval_def env (Exists (Plain body)));
  ghost_ (eval_def (Global :: env) (Plain body));
  ghost_ (eval_def (Regional :: env) (Plain body));
  ghost_ (eval_def (Local :: env) (Plain body));
  ()

let (binary_forall_exact @ total) :
    (env : elt list) -> (body : qf) ->
    {u : unit |
      binary_forall env body = eval env (Forall (Plain body))} =
 fun env body ->
  ghost_ (binary_forall_def env body);
  ghost_ (decode_def false false);
  ghost_ (decode_def false true);
  ghost_ (decode_def true false);
  ghost_ (decode_def true true);
  ghost_ (eval_def env (Forall (Plain body)));
  ghost_ (eval_def (Global :: env) (Plain body));
  ghost_ (eval_def (Regional :: env) (Plain body));
  ghost_ (eval_def (Local :: env) (Plain body));
  ()

let (binary_exists_projection @ total) :
    (env : elt list) -> (body : qf) ->
    {u : unit |
      binary_exists env body = eval_qf env (eliminate (Exists (Plain body)))} =
 fun env body ->
  ghost_ (binary_exists_exact env body);
  ghost_ (eliminate_exact env (Exists (Plain body)));
  ()

let (binary_forall_projection @ total) :
    (env : elt list) -> (body : qf) ->
    {u : unit |
      binary_forall env body = eval_qf env (eliminate (Forall (Plain body)))} =
 fun env body ->
  ghost_ (binary_forall_exact env body);
  ghost_ (eliminate_exact env (Forall (Plain body)));
  ()

let (binary_complement @ total) :
    (env : elt list) -> (body : qf) -> (high : bool) -> (low : bool) ->
    {u : unit |
      eval_qf (decode high low :: env) (Not body)
      = not (eval_qf (decode high low :: env) body)} =
 fun env body high low ->
  ghost_ (eval_qf_def (decode high low :: env) (Not body));
  ()

let (retained_conjunction @ total) :
    (env : elt list) -> (witness : qf) -> (first : qf) -> (second : qf) ->
    {u : unit |
      eval_qf env (And (And (witness, first), second)) =
      (eval_qf env witness && eval_qf env first && eval_qf env second)} =
 fun env witness first second ->
  ghost_ (eval_qf_def env (And (And (witness, first), second)));
  ghost_ (eval_qf_def env (And (witness, first)));
  ()

let[@def] (guarded_exists @ total) env domain winning =
  binary_exists env (And (domain, winning))

let[@def] (guarded_forall @ total) env domain winning =
  binary_forall env (Or (Not domain, winning))

let (guarded_exists_exact @ total) :
    (env : elt list) -> (domain : qf) -> (winning : qf) ->
    {u : unit |
      guarded_exists env domain winning =
      eval env (Exists (Plain (And (domain, winning))))} =
 fun env domain winning ->
  ghost_ (guarded_exists_def env domain winning);
  ghost_ (binary_exists_exact env (And (domain, winning)));
  ()

let (guarded_forall_exact @ total) :
    (env : elt list) -> (domain : qf) -> (winning : qf) ->
    {u : unit |
      guarded_forall env domain winning =
      eval env (Forall (Plain (Or (Not domain, winning))))} =
 fun env domain winning ->
  ghost_ (guarded_forall_def env domain winning);
  ghost_ (binary_forall_exact env (Or (Not domain, winning)));
  ()
