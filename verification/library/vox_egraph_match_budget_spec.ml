(* Cost model of the bounded matcher [Vox_egraph_match_bounded], a diagnostic
   that is separate from the saturation fuel. [work count pat] is the exact
   number of units that matching [pat] against a graph of [count] nodes
   charges. *)

module R = Vox_egraph_rule_spec

(* One unit per pattern constructor and per node position scanned.
   Root traversal, binding lookup, and class-list membership are not units. *)
let[@def] rec (work @ total) (count : int) (pat : R.pat @ immutable) =
  match pat with
  | R.Var _ -> 1Z
  | R.Int_lit _ | R.Bool_lit _ | R.Int_input | R.Bool_input ->
    Bigint.add 1Z (Bigint.of_int count)
  | R.Add (a, b) | R.Eq_int (a, b) ->
    Bigint.add (Bigint.add 1Z (Bigint.of_int count))
      (Bigint.add (work count a) (work count b))
  | R.Int_if (c, a, b) | R.Bool_if (c, a, b) ->
    Bigint.add (Bigint.add 1Z (Bigint.of_int count))
      (Bigint.add (work count c) (Bigint.add (work count a) (work count b)))
