(* TEST
 flags = "-extension refinement_types";
 has-z3;
 native;
*)

type state : immutable_data =
  { lower : int; upper : int; self_edge : bool }

let[@def] (forward @ total) x =
  if x <= 0 then 0 else if x = 1 then 2 else 3

let[@def] (backward @ total) x =
  if x <= 1 then 0 else if x = 2 then 1 else 3

let[@def] (valid @ total) s =
  0 <= s.lower && s.lower <= s.upper && s.upper <= 3
  && (not s.self_edge || forward s.upper <= s.upper)

let[@def] (model @ total) s x =
  s.lower <= x && x <= s.upper
  && (not s.self_edge || forward x <= x)

let[@def] (closed @ total) s =
  { s with upper = backward (backward s.upper); self_edge = true }

let[@def] (raised_lower @ total) s c =
  { s with lower = if s.lower <= c then c else s.lower }

let (lower_exact_at @ total) :
    (s : {s : state | valid s}) ->
    (c : {c : int | 0 <= c && c <= 3}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit |
      model (raised_lower s c) x = (model s x && c <= x)} =
 fun s c x ->
  ghost_ (raised_lower_def s c);
  ghost_ (model_def s x);
  ghost_ (model_def (raised_lower s c) x);
  ()

let (lower_failure_refutes @ total) :
    (s : {s : state | valid s}) ->
    (c : {c : int | 0 <= c && c <= 3
                       && (raised_lower s c).lower > s.upper}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit | not (model s x && c <= x)} =
 fun s c x ->
  ghost_ (lower_exact_at s c x);
  ghost_ (raised_lower_def s c);
  ghost_ (model_def (raised_lower s c) x);
  ()

let (lower_greatest @ total) :
    (s : {s : state | valid s}) ->
    (c : {c : int | 0 <= c && c <= 3
                       && (raised_lower s c).lower <= s.upper}) ->
    {u : unit |
      valid (raised_lower s c)
      && model (raised_lower s c) s.upper} =
 fun s c ->
  ghost_ (valid_def s);
  ghost_ (raised_lower_def s c);
  ghost_ (valid_def (raised_lower s c));
  ghost_ (model_def (raised_lower s c) s.upper);
  ()

let[@def] (lowered_upper @ total) s c =
  let cap = if s.upper <= c then s.upper else c in
  let upper =
    if s.self_edge then backward (backward cap) else cap
  in
  { s with upper }

let (upper_exact_at @ total) :
    (s : {s : state | valid s}) ->
    (c : {c : int | 0 <= c && c <= 3}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit |
      model (lowered_upper s c) x = (model s x && x <= c)} =
 fun s c x ->
  ghost_ (valid_def s);
  ghost_ (lowered_upper_def s c);
  ghost_ (model_def s x);
  ghost_ (model_def (lowered_upper s c) x);
  ghost_ (forward_def x);
  let cap = if s.upper <= c then s.upper else c in
  ghost_ (backward_def cap);
  ghost_ (backward_def (backward cap));
  ()

let (upper_failure_refutes @ total) :
    (s : {s : state | valid s}) ->
    (c : {c : int | 0 <= c && c <= 3
                       && s.lower > (lowered_upper s c).upper}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit | not (model s x && x <= c)} =
 fun s c x ->
  ghost_ (upper_exact_at s c x);
  ghost_ (lowered_upper_def s c);
  ghost_ (model_def (lowered_upper s c) x);
  ()

let (upper_greatest @ total) :
    (s : {s : state | valid s}) ->
    (c : {c : int | 0 <= c && c <= 3
                       && s.lower <= (lowered_upper s c).upper}) ->
    {u : unit |
      valid (lowered_upper s c)
      && model (lowered_upper s c) (lowered_upper s c).upper} =
 fun s c ->
  ghost_ (valid_def s);
  ghost_ (lowered_upper_def s c);
  ghost_ (valid_def (lowered_upper s c));
  ghost_ (model_def (lowered_upper s c) (lowered_upper s c).upper);
  let cap = if s.upper <= c then s.upper else c in
  ghost_ (backward_def cap);
  ghost_ (backward_def (backward cap));
  ghost_ (forward_def (lowered_upper s c).upper);
  ()

let (closure_exact_at @ total) :
    (s : {s : state | valid s}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit | model (closed s) x = (model s x && forward x <= x)} =
 fun s x ->
  ghost_ (valid_def s);
  ghost_ (closed_def s);
  ghost_ (model_def s x);
  ghost_ (model_def (closed s) x);
  ghost_ (forward_def x);
  ghost_ (forward_def s.upper);
  ghost_ (backward_def s.upper);
  ghost_ (backward_def (backward s.upper));
  ()

let (closure_greatest @ total) :
    (s : {s : state | valid s}) ->
    {u : unit |
      not (s.lower <= (closed s).upper) ||
      model (closed s) (closed s).upper} =
 fun s ->
  ghost_ (valid_def s);
  ghost_ (closed_def s);
  ghost_ (model_def (closed s) (closed s).upper);
  ghost_ (forward_def (closed s).upper);
  ghost_ (backward_def s.upper);
  ghost_ (backward_def (backward s.upper));
  ()

let (success_forward_at @ total) :
    (s : {s : state | valid s}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit |
      not (model (closed s) x)
      || (model s x && forward x <= x)} =
 fun s x ->
  ghost_ (closure_exact_at s x);
  ()

let (success_backward_at @ total) :
    (s : {s : state | valid s}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit |
      not (model s x && forward x <= x)
      || model (closed s) x} =
 fun s x ->
  ghost_ (closure_exact_at s x);
  ()

let (failure_refutes @ total) :
    (s : {s : state | valid s && s.lower > (closed s).upper}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit | not (model s x && forward x <= x)} =
 fun s x ->
  ghost_ (closure_exact_at s x);
  ghost_ (closed_def s);
  ghost_ (model_def (closed s) x);
  ()

let (greatest_at @ total) :
    (s : {s : state | valid s}) ->
    (x : {x : int | 0 <= x && x <= 3}) ->
    {u : unit | not (model (closed s) x) || x <= (closed s).upper} =
 fun s x ->
  ghost_ (model_def (closed s) x);
  ()

type outcome = Success of state * state | Failure of state

let (add_lower @ total) (s : {s : state | valid s})
    (c : {c : int | 0 <= c && c <= 3}) :
    {r : outcome |
      match r with
      | Success (before, after) ->
        before === s && after === raised_lower s c && valid after
      | Failure before ->
        before === s && (raised_lower s c).lower > s.upper} =
  let after = raised_lower s c in
  ghost_ (raised_lower_def s c);
  if after.lower <= s.upper then
    (ghost_ (lower_greatest s c);
     Success (s, after))
  else Failure s

let (add_upper @ total) (s : {s : state | valid s})
    (c : {c : int | 0 <= c && c <= 3}) :
    {r : outcome |
      match r with
      | Success (before, after) ->
        before === s && after === lowered_upper s c && valid after
      | Failure before ->
        before === s && s.lower > (lowered_upper s c).upper} =
  let after = lowered_upper s c in
  ghost_ (lowered_upper_def s c);
  if s.lower <= after.upper then
    (ghost_ (upper_greatest s c);
     Success (s, after))
  else Failure s

let (add_self_edge @ total) (s : {s : state | valid s}) :
    {r : outcome |
      match r with
      | Success (before, after) ->
        before === s && after === closed s && valid after
      | Failure before ->
        before === s && s.lower > (closed s).upper} =
  let after = closed s in
  ghost_ (valid_def s);
  ghost_ (closed_def s);
  ghost_ (backward_def s.upper);
  ghost_ (backward_def (backward s.upper));
  ghost_ (forward_def after.upper);
  ghost_ (valid_def after);
  if s.lower <= after.upper then
    (ghost_ (closure_greatest s);
     Success (s, after))
  else Failure s

let (undo @ total) : outcome -> state = function
  | Success (before, _) | Failure before -> before

let (rollback @ total) :
    (s : state) ->
    (r : {r : outcome |
      match r with
      | Success (before, _) | Failure before -> before === s}) ->
    {restored : state | restored === s} =
 fun s r ->
  match r with
  | Success (restored, _) | Failure restored -> restored
