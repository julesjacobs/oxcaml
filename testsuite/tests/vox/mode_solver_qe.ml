(* TEST
 flags = "-extension refinement_types";
 has-z3;
 native;
*)

type elt : immutable_data = { a : bool; b : bool }

let[@def] (le @ total) x y =
  (not x.a || y.a) && (not x.b || y.b)

let[@def] (join @ total) x y =
  { a = x.a || y.a; b = x.b || y.b }

let[@def] (meet @ total) x y =
  { a = x.a && y.a; b = x.b && y.b }

let bottom = { a = false; b = false }
let top = { a = true; b = true }

type bounds : immutable_data =
  { lower1 : elt; lower2 : elt; upper1 : elt; upper2 : elt }

let[@def] (admits @ total) p z =
  le p.lower1 z && le p.lower2 z
  && le z p.upper1 && le z p.upper2

let[@def] (project @ total) p =
  le (join p.lower1 p.lower2) (meet p.upper1 p.upper2)

let[@def] (choose @ total) p = join p.lower1 p.lower2

let (project_sound @ total) :
    (p : bounds) -> (z : elt) ->
    {u : unit | not (admits p z) || project p} =
 fun p z ->
  ghost_ (admits_def p z);
  ghost_ (project_def p);
  ghost_ (le_def p.lower1 z);
  ghost_ (le_def p.lower2 z);
  ghost_ (le_def z p.upper1);
  ghost_ (le_def z p.upper2);
  ghost_ (join_def p.lower1 p.lower2);
  ghost_ (meet_def p.upper1 p.upper2);
  ghost_ (le_def (join p.lower1 p.lower2)
            (meet p.upper1 p.upper2));
  ()

let (project_complete @ total) :
    (p : bounds) ->
    {u : unit | not (project p) || admits p (choose p)} =
 fun p ->
  ghost_ (project_def p);
  ghost_ (choose_def p);
  ghost_ (admits_def p (choose p));
  ghost_ (join_def p.lower1 p.lower2);
  ghost_ (meet_def p.upper1 p.upper2);
  ghost_ (le_def (join p.lower1 p.lower2)
            (meet p.upper1 p.upper2));
  ghost_ (le_def p.lower1 (choose p));
  ghost_ (le_def p.lower2 (choose p));
  ghost_ (le_def (choose p) p.upper1);
  ghost_ (le_def (choose p) p.upper2);
  ()

type context : immutable_data = { fixed_lower : elt; fixed_upper : elt }

let[@def] (for_rigid @ total) c r =
  { lower1 = c.fixed_lower;
    lower2 = r;
    upper1 = c.fixed_upper;
    upper2 = top }

let[@def] (all_project @ total) c =
  project (for_rigid c { a = false; b = false })
  && project (for_rigid c { a = false; b = true })
  && project (for_rigid c { a = true; b = false })
  && project (for_rigid c { a = true; b = true })

let (all_project_sound @ total) :
    (c : {c : context | all_project c}) ->
    (r : elt) ->
    {u : unit | admits (for_rigid c r) (choose (for_rigid c r))} =
 fun c r ->
  ghost_ (all_project_def c);
  ghost_ (for_rigid_def c r);
  ghost_ (project_complete (for_rigid c r));
  ()

let (all_project_failure @ total) :
    (c : {c : context | not (all_project c)}) ->
    {r : elt | not (project (for_rigid c r))} =
 fun c ->
  ghost_ (all_project_def c);
  if not (project (for_rigid c { a = false; b = false }))
  then { a = false; b = false }
  else if not (project (for_rigid c { a = false; b = true }))
  then { a = false; b = true }
  else if not (project (for_rigid c { a = true; b = false }))
  then { a = true; b = false }
  else { a = true; b = true }

let (rigid_counterexample @ total) :
    (c : {c : context | not (all_project c)}) ->
    (z : elt) ->
    {u : unit |
      not (admits (for_rigid c (all_project_failure c)) z)} =
 fun c z ->
  let r = all_project_failure c in
  ghost_ (project_sound (for_rigid c r) z);
  ()
