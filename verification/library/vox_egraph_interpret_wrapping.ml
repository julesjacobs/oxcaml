module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation

let rec (sound @ total) :
    (rules : R.t) @ immutable -> (proof : E.evidence) @ immutable ->
    (env : L.env) @ immutable ->
    ((index : int) -> (rule : R.rule) @ immutable ->
      (subst : R.subst) @ immutable ->
      {u : unit | R.lookup_rule rules index === Some rule &&
        R.rule_valid rule && R.subst_valid rule.vars subst} ->
      {u : unit | L.eval (R.instantiate rule.lhs subst) env ===
        L.eval (R.instantiate rule.rhs subst) env} @ ghost) @ total ->
    {u : unit | E.valid rules proof} ->
    {u : unit | L.eval (E.left proof) env ===
      L.eval (E.right proof) env} @ ghost =
    fun rules proof env holds premise -> ghost_ (
  E.valid_def rules proof;
  E.left_def proof;
  E.right_def proof;
  E.endpoints_def proof;
  match proof with
  | E.Refl _ -> ()
  | E.Sym child ->
    sound rules child env holds ();
    E.left_def child;
    E.right_def child;
    ()
  | E.Trans (first, second) ->
    sound rules first env holds ();
    sound rules second env holds ();
    E.right_def first;
    E.left_def second;
    E.left_def first;
    E.right_def second;
    ()
  | E.Add_cong (a, b) ->
    sound rules a env holds ();
    sound rules b env holds ();
    E.left_def a;
    E.right_def a;
    E.left_def b;
    E.right_def b;
    L.eval_def (L.Add (E.left a, E.left b)) env;
    L.eval_def (L.Add (E.right a, E.right b)) env;
    L.as_int_def (L.eval (E.left a) env);
    L.as_int_def (L.eval (E.left b) env);
    L.as_int_def (L.eval (E.right a) env);
    L.as_int_def (L.eval (E.right b) env);
    ()
  | E.Eq_cong (a, b) ->
    sound rules a env holds ();
    sound rules b env holds ();
    E.left_def a; E.right_def a; E.left_def b; E.right_def b;
    L.eval_def (L.Eq_int (E.left a, E.left b)) env;
    L.eval_def (L.Eq_int (E.right a, E.right b)) env;
    L.as_int_def (L.eval (E.left a) env);
    L.as_int_def (L.eval (E.left b) env);
    L.as_int_def (L.eval (E.right a) env);
    L.as_int_def (L.eval (E.right b) env);
    ()
  | E.Int_if_cong (c, y, n) ->
    sound rules c env holds ();
    sound rules y env holds ();
    sound rules n env holds ();
    E.left_def c; E.right_def c;
    E.left_def y; E.right_def y;
    E.left_def n; E.right_def n;
    L.eval_def (L.Int_if (E.left c, E.left y, E.left n)) env;
    L.eval_def (L.Int_if (E.right c, E.right y, E.right n)) env;
    L.as_bool_def (L.eval (E.left c) env);
    L.as_bool_def (L.eval (E.right c) env);
    L.as_int_def (L.eval (E.left y) env);
    L.as_int_def (L.eval (E.right y) env);
    L.as_int_def (L.eval (E.left n) env);
    L.as_int_def (L.eval (E.right n) env);
    ()
  | E.Bool_if_cong (c, y, n) ->
    sound rules c env holds ();
    sound rules y env holds ();
    sound rules n env holds ();
    E.left_def c; E.right_def c;
    E.left_def y; E.right_def y;
    E.left_def n; E.right_def n;
    L.eval_def (L.Bool_if (E.left c, E.left y, E.left n)) env;
    L.eval_def (L.Bool_if (E.right c, E.right y, E.right n)) env;
    L.as_bool_def (L.eval (E.left c) env);
    L.as_bool_def (L.eval (E.right c) env);
    L.as_bool_def (L.eval (E.left y) env);
    L.as_bool_def (L.eval (E.right y) env);
    L.as_bool_def (L.eval (E.left n) env);
    L.as_bool_def (L.eval (E.right n) env);
    ()
  | E.Rule (index, rule, subst) -> holds index rule subst ())
