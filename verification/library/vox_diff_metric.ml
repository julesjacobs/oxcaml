open Vox_diff_spec

let[@def] min (a : Bigint.t) (b : Bigint.t) = if a <= b then a else b

let[@def] rec distance (fuel : Bigint.t) (old : int list) (fresh : int list) =
  if fuel <= 0Z then 0Z else
  match old, fresh with
  | [], _ -> size fresh
  | _, [] -> size old
  | a :: ats, b :: bts ->
    if a = b then
      if fuel < 2Z then 0Z else distance (Bigint.sub fuel 2Z) ats bts
    else Bigint.add 1Z
      (min (distance (Bigint.sub fuel 1Z) ats fresh)
        (distance (Bigint.sub fuel 1Z) old bts))
[@@decreases fuel]

let[@def] metric old fresh = distance (Bigint.add (size old) (size fresh)) old
  fresh

let (equation @ total) (old : int list) (fresh : int list) :
    {u : unit | metric old fresh ===
      (match old, fresh with
       | [], _ -> size fresh
       | _, [] -> size old
       | a :: ats, b :: bts ->
         if a = b then metric ats bts
         else Bigint.add 1Z (min (metric ats fresh) (metric old bts)))} =
  size_nonnegative old;
  size_nonnegative fresh;
  size_def old;
  size_def fresh;
  metric_def old fresh;
  distance_def (Bigint.add (size old) (size fresh)) old fresh;
  (match old, fresh with
   | a :: ats, b :: bts ->
     size_nonnegative ats;
     size_nonnegative bts;
     metric_def ats bts;
     metric_def ats fresh;
     metric_def old bts
   | _ -> ());
  let u = () in refine_ u

let rec (neighbors_aux @ total) : (fuel : Bigint.t) ->
    (old : int list) -> (fresh : int list) -> (a : int) -> (b : int) ->
    {u : unit | if fuel = Bigint.add (size old) (size fresh) then
      metric old fresh <= Bigint.add 1Z (metric (a :: old) fresh)
      && metric (a :: old) fresh <= Bigint.add 1Z (metric old fresh)
      && metric old fresh <= Bigint.add 1Z (metric old (b :: fresh))
      && metric old (b :: fresh) <= Bigint.add 1Z (metric old fresh)
      else true} = fun fuel old fresh a b ->
  if fuel <> Bigint.add (size old) (size fresh) then
    let u = () in refine_ u
  else (
    size_def old;
    size_def fresh;
    size_nonnegative old;
    size_nonnegative fresh;
    equation old fresh;
    equation (a :: old) fresh;
    equation old (b :: fresh);
    size_def (a :: old);
    size_def (b :: fresh);
    (match fresh with
     | [] -> ()
     | head :: tail ->
       size_nonnegative tail;
       neighbors_aux (Bigint.sub fuel 1Z) old tail a head;
       min_def (metric old fresh) (metric (a :: old) tail));
    (match old with
     | [] -> ()
     | head :: tail ->
       size_nonnegative tail;
       neighbors_aux (Bigint.sub fuel 1Z) tail fresh head b;
       min_def (metric tail (b :: fresh)) (metric old fresh));
    let u = () in refine_ u)
[@@decreases fuel]

let (neighbors @ total) (old : int list) (fresh : int list) (a : int) (b : int)
  :
    {u : unit |
      metric old fresh <= Bigint.add 1Z (metric (a :: old) fresh)
      && metric (a :: old) fresh <= Bigint.add 1Z (metric old fresh)
      && metric old fresh <= Bigint.add 1Z (metric old (b :: fresh))
      && metric old (b :: fresh) <= Bigint.add 1Z (metric old fresh)} =
  neighbors_aux (Bigint.add (size old) (size fresh)) old fresh a b;
  let u = () in refine_ u

let (strip @ total) (old : int list) (fresh : int list) (a : int) (b : int) :
    {u : unit | metric old fresh <= metric (a :: old) (b :: fresh)} =
  equation (a :: old) (b :: fresh);
  neighbors old fresh a b;
  min_def (metric old (b :: fresh)) (metric (a :: old) fresh);
  let u = () in refine_ u

let rec (lower_bound @ total) (script : script) :
    {u : unit | metric (source script) (target script) <= cost script} =
  source_def script;
  target_def script;
  cost_def script;
  match script with
  | [] ->
    equation [] [];
    size_def [];
    let u = () in refine_ u
  | op :: rest ->
    lower_bound rest;
    (match op with
     | Keep a -> equation (a :: source rest) (a :: target rest)
     | Delete a -> neighbors (source rest) (target rest) a a
     | Insert b -> neighbors (source rest) (target rest) b b);
    let u = () in refine_ u

let rec (crop @ total) : (old : int list) -> (fresh : int list) ->
    (old_tail : int list) -> (new_tail : int list) ->
    {u : unit | if suffix old old_tail && suffix fresh new_tail
      && Bigint.sub (size old) (size old_tail) =
         Bigint.sub (size fresh) (size new_tail)
      then metric old_tail new_tail <= metric old fresh else true} =
    fun old fresh old_tail new_tail ->
  suffix_bounds old old_tail;
  suffix_bounds fresh new_tail;
  suffix_def old old_tail;
  suffix_def fresh new_tail;
  size_def old;
  size_def fresh;
  size_nonnegative old_tail;
  size_nonnegative new_tail;
  (match old, fresh with
   | a :: ats, b :: bts ->
     crop ats bts old_tail new_tail;
     strip ats bts a b
   | _ -> ());
  let u = () in refine_ u

let rec (properties_aux @ total) : (fuel : Bigint.t) ->
    (old : int list) -> (fresh : int list) ->
    {u : unit | if fuel = Bigint.add (size old) (size fresh) then
      0Z <= metric old fresh && metric old fresh <= fuel
      && (if metric old fresh = 0Z then old === fresh else true)
      else true} = fun fuel old fresh ->
  if fuel <> Bigint.add (size old) (size fresh) then let u = () in refine_ u
  else (
    equation old fresh;
    size_def old;
    size_def fresh;
    size_nonnegative old;
    size_nonnegative fresh;
    size_zero old;
    size_zero fresh;
    (match old, fresh with
     | a :: ats, b :: bts ->
       size_nonnegative ats;
       size_nonnegative bts;
       properties_aux (Bigint.sub fuel 2Z) ats bts;
       properties_aux (Bigint.sub fuel 1Z) ats fresh;
       properties_aux (Bigint.sub fuel 1Z) old bts;
       min_def (metric ats fresh) (metric old bts)
     | _ -> ());
    let u = () in refine_ u)
[@@decreases fuel]

let (properties @ total) (old : int list) (fresh : int list) :
    {u : unit | 0Z <= metric old fresh
      && metric old fresh <= Bigint.add (size old) (size fresh)
      && (if metric old fresh = 0Z then old === fresh else true)} =
  properties_aux (Bigint.add (size old) (size fresh)) old fresh;
  let u = () in refine_ u
