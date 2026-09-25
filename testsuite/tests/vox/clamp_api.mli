val clamp : int -> int -> int -> int @@ total
val clamp_def : (lo : int) -> (hi : int) -> (x : int) ->
  {u : unit | clamp lo hi x ===
    (if x < lo then lo else if hi < x then hi else x)} @@ total
val bounds : (lo : int) -> (hi : {hi : int | lo <= hi}) -> (x : int) ->
  {r : int | lo <= r && r <= (let refine_ h = hi in h)} @@ total
val identity : (lo : int) -> (hi : int) -> (x : int) ->
  {u : unit | if lo <= x && x <= hi then clamp lo hi x === x else true}
  @@ total
val idempotent : (lo : int) -> (hi : int) -> (x : int) ->
  {u : unit | if lo <= hi then
    clamp lo hi (clamp lo hi x) === clamp lo hi x else true} @@ total
