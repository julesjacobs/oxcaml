(* TEST
 flags = "-extension refinement_types";
 has-z3;
 native;
*)

type valuation : immutable_data = { x : bool; y : bool }

type atom : immutable_data =
  | Lower_x of bool
  | Upper_x of bool
  | Lower_y of bool
  | Upper_y of bool
  | X_le_y

let[@def] (le @ total) x y = not x || y

let[@def] (satisfies @ total) a v =
  match a with
  | Lower_x c -> le c v.x
  | Upper_x c -> le v.x c
  | Lower_y c -> le c v.y
  | Upper_y c -> le v.y c
  | X_le_y -> le v.x v.y

let[@def] rec (models @ total) cs v =
  match cs with
  | [] -> true
  | a :: rest -> satisfies a v && models rest v

type state : immutable_data =
  { lower_x : bool; upper_x : bool;
    lower_y : bool; upper_y : bool;
    edge : bool }

let empty =
  { lower_x = false; upper_x = true;
    lower_y = false; upper_y = true;
    edge = false }

let[@def] (represents @ total) s v =
  le s.lower_x v.x && le v.x s.upper_x
  && le s.lower_y v.y && le v.y s.upper_y
  && (not s.edge || le v.x v.y)

let[@def] (consistent @ total) s =
  le s.lower_x s.upper_x && le s.lower_y s.upper_y

let[@def] (normalized @ total) s =
  not s.edge ||
    (le s.lower_x s.lower_y && le s.upper_x s.upper_y)

let[@def] (close @ total) s =
  if s.edge then
    { s with
      lower_y = s.lower_y || s.lower_x;
      upper_x = s.upper_x && s.upper_y }
  else s

let[@def] (raw_add @ total) s a =
  match a with
  | Lower_x c -> { s with lower_x = s.lower_x || c }
  | Upper_x c -> { s with upper_x = s.upper_x && c }
  | Lower_y c -> { s with lower_y = s.lower_y || c }
  | Upper_y c -> { s with upper_y = s.upper_y && c }
  | X_le_y -> { s with edge = true }

let (close_exact @ total) :
    (s : state) -> (v : valuation) ->
    {u : unit | represents (close s) v = represents s v} =
 fun s v ->
  ghost_ (close_def s);
  ghost_ (represents_def s v);
  ghost_ (represents_def (close s) v);
  ghost_ (le_def s.lower_x v.x);
  ghost_ (le_def v.x s.upper_x);
  ghost_ (le_def s.lower_y v.y);
  ghost_ (le_def v.y s.upper_y);
  ghost_ (le_def v.x v.y);
  ghost_ (le_def (s.lower_y || s.lower_x) v.y);
  ghost_ (le_def v.x (s.upper_x && s.upper_y));
  ()

let (raw_add_exact @ total) :
    (s : state) -> (a : atom) -> (v : valuation) ->
    {u : unit |
      represents (raw_add s a) v =
        (represents s v && satisfies a v)} =
 fun s a v ->
  ghost_ (raw_add_def s a);
  ghost_ (represents_def s v);
  ghost_ (represents_def (raw_add s a) v);
  ghost_ (satisfies_def a v);
  ghost_ (le_def s.lower_x v.x);
  ghost_ (le_def v.x s.upper_x);
  ghost_ (le_def s.lower_y v.y);
  ghost_ (le_def v.y s.upper_y);
  ghost_ (le_def v.x v.y);
  (match a with
   | Lower_x c ->
     ghost_ (le_def c v.x);
     ghost_ (le_def (s.lower_x || c) v.x)
   | Upper_x c ->
     ghost_ (le_def v.x c);
     ghost_ (le_def v.x (s.upper_x && c))
   | Lower_y c ->
     ghost_ (le_def c v.y);
     ghost_ (le_def (s.lower_y || c) v.y)
   | Upper_y c ->
     ghost_ (le_def v.y c);
     ghost_ (le_def v.y (s.upper_y && c))
   | X_le_y -> ());
  ()

let (inconsistent_refutes @ total) :
    (s : {s : state | not (consistent s)}) -> (v : valuation) ->
    {u : unit | not (represents s v)} =
 fun s v ->
  ghost_ (consistent_def s);
  ghost_ (represents_def s v);
  ghost_ (le_def s.lower_x s.upper_x);
  ghost_ (le_def s.lower_y s.upper_y);
  ghost_ (le_def s.lower_x v.x);
  ghost_ (le_def v.x s.upper_x);
  ghost_ (le_def s.lower_y v.y);
  ghost_ (le_def v.y s.upper_y);
  ()

let (close_normalized @ total) :
    (s : state) -> {u : unit | normalized (close s)} =
 fun s ->
  ghost_ (close_def s);
  ghost_ (normalized_def (close s));
  ghost_ (le_def s.lower_x (s.lower_y || s.lower_x));
  ghost_ (le_def (s.upper_x && s.upper_y) s.upper_y);
  ()

type result = Success of state | Failure

let (add @ total) (s : state) (a : atom) :
    {r : result |
      match r with
      | Success after ->
        after === close (raw_add s a) && consistent after
        && normalized after
      | Failure -> not (consistent (close (raw_add s a)))} =
  let after = close (raw_add s a) in
  ghost_ (close_normalized (raw_add s a));
  if consistent after then
    let r = Success after in
    r
  else
    let r = Failure in
    r

let (add_exact_at @ total) :
    (s : state) -> (a : atom) -> (v : valuation) ->
    {u : unit |
      match add s a with
      | Success after ->
        represents after v = (represents s v && satisfies a v)
      | Failure -> not (represents s v && satisfies a v)} =
 fun s a v ->
  ghost_ (raw_add_exact s a v);
  ghost_ (close_exact (raw_add s a) v);
  match add s a with
  | Success _ -> ()
  | Failure ->
    ghost_ (inconsistent_refutes (close (raw_add s a)) v);
    ()

let[@def] (greatest @ total) s =
  { x = s.upper_x; y = s.upper_y }

let (greatest_model @ total) :
    (s : {s : state | consistent s && normalized s}) ->
    {u : unit | represents s (greatest s)} =
 fun s ->
  ghost_ (consistent_def s);
  ghost_ (normalized_def s);
  ghost_ (greatest_def s);
  ghost_ (represents_def s (greatest s));
  ghost_ (le_def s.lower_x s.upper_x);
  ghost_ (le_def s.lower_y s.upper_y);
  ghost_ (le_def s.upper_x s.upper_y);
  ghost_ (le_def s.upper_x s.upper_x);
  ghost_ (le_def s.upper_y s.upper_y);
  ()

let (greatest_dominates @ total) :
    (s : state) -> (v : {v : valuation | represents s v}) ->
    {u : unit |
      le v.x (greatest s).x && le v.y (greatest s).y} =
 fun s v ->
  ghost_ (greatest_def s);
  ghost_ (represents_def s v);
  ghost_ (le_def v.x s.upper_x);
  ghost_ (le_def v.y s.upper_y);
  ghost_ (le_def v.x (greatest s).x);
  ghost_ (le_def v.y (greatest s).y);
  ()

let[@def] rec (solve @ total) cs =
  match cs with
  | [] -> Success empty
  | a :: rest ->
    match solve rest with
    | Failure -> Failure
    | Success s -> add s a

let rec (solve_exact_at @ total) :
    (cs : atom list) -> (v : valuation) ->
    {u : unit |
      match solve cs with
      | Success s -> represents s v = models cs v
      | Failure -> not (models cs v)} =
 fun cs v ->
  match cs with
  | [] ->
    ghost_ (solve_def cs);
    ghost_ (models_def cs v);
    ghost_ (represents_def empty v);
    ghost_ (le_def false v.x);
    ghost_ (le_def v.x true);
    ghost_ (le_def false v.y);
    ghost_ (le_def v.y true);
    ()
  | a :: rest ->
    ghost_ (solve_exact_at rest v);
    ghost_ (solve_def cs);
    ghost_ (models_def cs v);
    (match solve rest with
     | Failure -> ()
     | Success s ->
       ghost_ (add_exact_at s a v);
       ())

let rec (solve_invariant @ total) :
    (cs : atom list) ->
    {u : unit |
      match solve cs with
      | Success s -> consistent s && normalized s
      | Failure -> true} =
 fun cs ->
  match cs with
  | [] ->
    ghost_ (solve_def cs);
    ghost_ (consistent_def empty);
    ghost_ (normalized_def empty);
    ghost_ (le_def false true);
    ()
  | a :: rest ->
    ghost_ (solve_invariant rest);
    ghost_ (solve_def cs);
    (match solve rest with
     | Failure -> ()
     | Success s ->
       match add s a with
       | Failure -> ()
       | Success _ -> ())

let (solve_greatest @ total) :
    (cs : atom list) -> (v : valuation) ->
    {u : unit |
      match solve cs with
      | Failure -> not (models cs v)
      | Success s ->
        models cs (greatest s)
        && (not (models cs v)
            || (le v.x (greatest s).x
                && le v.y (greatest s).y))} =
 fun cs v ->
  ghost_ (solve_invariant cs);
  ghost_ (solve_exact_at cs v);
  match solve cs with
  | Failure -> ()
  | Success s ->
    ghost_ (greatest_model s);
    ghost_ (solve_exact_at cs (greatest s));
    if models cs v then begin
      ghost_ (greatest_dominates s v);
      ()
    end else ()

let[@def] (outer @ total) s y =
  le s.lower_y y && le y s.upper_y

let[@def] (lift @ total) s y =
  { x = s.lower_x; y }

let (project_sound @ total) :
    (s : state) -> (v : {v : valuation | represents s v}) ->
    {u : unit | outer s v.y} =
 fun s v ->
  ghost_ (outer_def s v.y);
  ghost_ (represents_def s v);
  ()

let (project_complete @ total) :
    (s : {s : state | consistent s && normalized s}) ->
    (y : {y : bool | outer s y}) ->
    {u : unit | represents s (lift s y)} =
 fun s y ->
  ghost_ (consistent_def s);
  ghost_ (normalized_def s);
  ghost_ (outer_def s y);
  ghost_ (lift_def s y);
  ghost_ (represents_def s (lift s y));
  ghost_ (le_def s.lower_x s.lower_x);
  ghost_ (le_def s.lower_x s.upper_x);
  ghost_ (le_def s.lower_y y);
  ghost_ (le_def y s.upper_y);
  ghost_ (le_def s.lower_x s.lower_y);
  ghost_ (le_def s.lower_x y);
  ()

let (solve_project_sound @ total) :
    (cs : atom list) -> (v : valuation) ->
    {u : unit |
      match solve cs with
      | Failure -> not (models cs v)
      | Success s -> not (models cs v) || outer s v.y} =
 fun cs v ->
  ghost_ (solve_exact_at cs v);
  match solve cs with
  | Failure -> ()
  | Success s ->
    if models cs v then begin
      ghost_ (project_sound s v);
      ()
    end else ()

let (solve_project_complete @ total) :
    (cs : atom list) -> (y : bool) ->
    {u : unit |
      match solve cs with
      | Failure -> true
      | Success s -> not (outer s y) || models cs (lift s y)} =
 fun cs y ->
  ghost_ (solve_invariant cs);
  match solve cs with
  | Failure -> ()
  | Success s ->
    if outer s y then begin
      ghost_ (project_complete s y);
      ghost_ (solve_exact_at cs (lift s y));
      ()
    end else ()
