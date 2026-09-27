(* A cursor into the rule list for [Vox_egraph_rules_scan]: [drop rules i]
   is the list without its first [i] rules. [lookup] says the head of that
   suffix is [R.lookup_rule rules i], and [advance] that its tail is
   [drop rules (i + 1)]; the bound on [index] keeps [index + 1] from
   overflowing. *)

module R = Vox_egraph_rule_spec

let[@def] rec (drop @ total) (rules : R.t @ immutable) (index : int) =
  if index <= 0 then rules
  else match rules with
  | R.No_rules -> R.No_rules
  | R.Rule_cons (_, rest) -> drop rest (index - 1)

let rec (lookup @ total) : (rules : R.t) @ immutable -> (index : int) ->
    {u : unit | 0 <= index} ->
    {u : unit | R.lookup_rule rules index ===
      (match drop rules index with R.No_rules -> None | R.Rule_cons (rule, _) -> Some rule)}
      @ ghost = fun rules index premise -> ghost_ (
  drop_def rules index;
  R.lookup_rule_def rules index;
  (match rules with
   | R.No_rules -> ()
   | R.Rule_cons (_, rest) -> if index > 0 then lookup rest (index - 1) ());
  ())

let rec (advance @ total) : (rules : R.t) @ immutable -> (index : int) ->
    {u : unit | 0 <= index && index < 4611686018427387903} ->
    {u : unit | drop rules (index + 1) ===
      (match drop rules index with R.No_rules -> R.No_rules | R.Rule_cons (_, rest) -> rest)}
      @ ghost = fun rules index premise -> ghost_ (
  drop_def rules index;
  drop_def rules (index + 1);
  (match rules with
   | R.No_rules -> ()
   | R.Rule_cons (_, rest) ->
     if index > 0 then advance rest (index - 1) () else drop_def rest 0);
  ())
