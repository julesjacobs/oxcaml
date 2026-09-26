let[@def] clamp (lo : int) (hi : int) (x : int) = if x < lo then lo else if hi < x then hi else x

let (bounds @ total) :
    (lo : int) -> (hi : {hi : int | lo <= hi}) -> (x : int) ->
    {r : int | lo <= r && r <= (let h = hi in h)} =
  fun lo hi x ->
  let result = clamp lo hi x in
  ghost_ (clamp_def lo hi x);
  result

let (identity @ total) (lo : int) (hi : int) (x : int) :
    {u : unit |
      if lo <= x && x <= hi then clamp (lo : int) (hi : int) (x : int) === x else true} =
  clamp_def lo hi x;
  ()

let (idempotent @ total) (lo : int) (hi : int) (x : int) :
    {u : unit |
      if lo <= hi then
        clamp lo hi (clamp lo hi x) === clamp lo hi x
      else true} =
  let first = clamp lo hi x in
  clamp_def lo hi x;
  clamp_def lo hi first;
  ()
