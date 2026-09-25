open Vox_egraph_derivation_spec
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules

let[@def] (well_sorted @ total) (proof : evidence @ immutable) =
  ghost_ (
    not (L.sort (left proof) === None) &&
    L.sort (left proof) === L.sort (right proof))

let (symmetric @ total) :
    (rules : R.t) @ immutable ->
    (proof : {p : evidence | valid rules p}) @ immutable ->
    {result : evidence | valid rules result &&
      left result === right proof && right result === left proof}
      @ ghost = fun rules proof -> ghost_ (
  let result = Sym proof in
  valid_def rules result;
  left_def result;
  right_def result;
  endpoints_def result;
  left_def proof;
  right_def proof;
  result)

let (transitive @ total) :
    (rules : R.t) @ immutable ->
    (first : {p : evidence | valid rules p}) @ immutable ->
    (second : {p : evidence | valid rules p}) @ immutable ->
    {u : unit | right first === left second} ->
    {result : evidence | valid rules result &&
      left result === left first && right result === right second}
      @ ghost = fun rules first second premise -> ghost_ (
  let result = Trans (first, second) in
  valid_def rules result;
  left_def result;
  right_def result;
  endpoints_def result;
  left_def first;
  right_def first;
  left_def second;
  right_def second;
  result)

let (add_congruence @ total) :
    (rules : R.t) @ immutable ->
    (first : {p : evidence | valid rules p}) @ immutable ->
    (second : {p : evidence | valid rules p}) @ immutable ->
    {u : unit | L.sort (left first) === Some L.Integer &&
      L.sort (left second) === Some L.Integer} ->
    {result : evidence | valid rules result &&
      left result === L.Add (left first, left second) &&
      right result === L.Add (right first, right second)}
      @ ghost = fun rules first second premise -> ghost_ (
  let result = Add_cong (first, second) in
  valid_def rules result;
  left_def result;
  right_def result;
  endpoints_def result;
  left_def first;
  right_def first;
  left_def second;
  right_def second;
  result)

let (eq_congruence @ total) :
    (rules : R.t) @ immutable ->
    (first : {p : evidence | valid rules p}) @ immutable ->
    (second : {p : evidence | valid rules p}) @ immutable ->
    {u : unit | L.sort (left first) === Some L.Integer &&
      L.sort (left second) === Some L.Integer} ->
    {result : evidence | valid rules result &&
      left result === L.Eq_int (left first, left second) &&
      right result === L.Eq_int (right first, right second)}
      @ ghost = fun rules first second premise -> ghost_ (
  let result = Eq_cong (first, second) in
  valid_def rules result;
  left_def result;
  right_def result;
  endpoints_def result;
  left_def first;
  right_def first;
  left_def second;
  right_def second;
  result)

let (int_if_congruence @ total) :
    (rules : R.t) @ immutable ->
    (condition : {p : evidence | valid rules p}) @ immutable ->
    (yes : {p : evidence | valid rules p}) @ immutable ->
    (no : {p : evidence | valid rules p}) @ immutable ->
    {u : unit | L.sort (left condition) === Some L.Boolean &&
      L.sort (left yes) === Some L.Integer &&
      L.sort (left no) === Some L.Integer} ->
    {result : evidence | valid rules result &&
      left result === L.Int_if
        (left condition, left yes, left no) &&
      right result === L.Int_if
        (right condition, right yes, right no)}
      @ ghost = fun rules condition yes no premise -> ghost_ (
  let result = Int_if_cong (condition, yes, no) in
  valid_def rules result;
  left_def result;
  right_def result;
  endpoints_def result;
  left_def condition;
  right_def condition;
  left_def yes;
  right_def yes;
  left_def no;
  right_def no;
  result)

let (bool_if_congruence @ total) :
    (rules : R.t) @ immutable ->
    (condition : {p : evidence | valid rules p}) @ immutable ->
    (yes : {p : evidence | valid rules p}) @ immutable ->
    (no : {p : evidence | valid rules p}) @ immutable ->
    {u : unit | L.sort (left condition) === Some L.Boolean &&
      L.sort (left yes) === Some L.Boolean &&
      L.sort (left no) === Some L.Boolean} ->
    {result : evidence | valid rules result &&
      left result === L.Bool_if
        (left condition, left yes, left no) &&
      right result === L.Bool_if
        (right condition, right yes, right no)}
      @ ghost = fun rules condition yes no premise -> ghost_ (
  let result = Bool_if_cong (condition, yes, no) in
  valid_def rules result;
  left_def result;
  right_def result;
  endpoints_def result;
  left_def condition;
  right_def condition;
  left_def yes;
  right_def yes;
  left_def no;
  right_def no;
  result)

let rec (sort_sound @ total) : (rules : R.t) @ immutable ->
    (proof : evidence) @ immutable ->
    {u : unit | valid rules proof} -> {u : unit | well_sorted proof}
    @ ghost = fun rules proof premise -> ghost_ (
  valid_def rules proof;
  well_sorted_def proof;
  left_def proof;
  right_def proof;
  endpoints_def proof;
  match proof with
  | Refl expr -> ()
  | Sym child ->
    sort_sound rules child ();
    well_sorted_def child;
    left_def child; right_def child;
    ()
  | Trans (first, second) ->
    sort_sound rules first ();
    sort_sound rules second ();
    well_sorted_def first; well_sorted_def second;
    left_def first; right_def first;
    left_def second; right_def second;
    ()
  | Add_cong (a, b) | Eq_cong (a, b) ->
    sort_sound rules a ();
    sort_sound rules b ();
    well_sorted_def a; well_sorted_def b;
    left_def a; right_def a;
    left_def b; right_def b;
    L.sort_def (L.Add (left a, left b));
    L.sort_def (L.Add (right a, right b));
    L.sort_def (L.Eq_int (left a, left b));
    L.sort_def (L.Eq_int (right a, right b));
    ()
  | Int_if_cong (c, y, n) | Bool_if_cong (c, y, n) ->
    sort_sound rules c ();
    sort_sound rules y ();
    sort_sound rules n ();
    well_sorted_def c; well_sorted_def y; well_sorted_def n;
    left_def c; right_def c;
    left_def y; right_def y;
    left_def n; right_def n;
    L.sort_def (L.Int_if (left c, left y, left n));
    L.sort_def (L.Int_if (right c, right y, right n));
    L.sort_def (L.Bool_if (left c, left y, left n));
    L.sort_def (L.Bool_if (right c, right y, right n));
    ()
  | Rule (_, rule, subst) -> RP.instance_sorted rule subst ())

let (rule_instance @ total) :
    (rules : R.t) @ immutable -> (index : int) ->
    (rule : R.rule) @ immutable -> (subst : R.subst) @ immutable ->
    {u : unit | R.lookup_rule rules index === Some rule &&
      R.rule_valid rule && R.subst_valid rule.vars subst} ->
    {proof : evidence | valid rules proof &&
      left proof === R.instantiate rule.lhs subst &&
      right proof === R.instantiate rule.rhs subst} @ ghost =
    fun rules index rule subst premise -> ghost_ (
  let proof = Rule (index, rule, subst) in
  valid_def rules proof;
  endpoints_def proof;
  left_def proof;
  right_def proof;
  proof)
