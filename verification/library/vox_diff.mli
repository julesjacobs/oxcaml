open Vox_diff_spec

type error = Input_too_large [@@inductive]

val diff : (old : int list) -> (fresh : int list) ->
  {r : (script, error) result | match r with
    | Error Input_too_large -> 1000000Z < size old || 1000000Z < size fresh
    | Ok s -> size old <= 1000000Z && size fresh <= 1000000Z
      && source s === old && target s === fresh
      && apply old s === Some fresh
      && cost s = Vox_diff_metric.metric old fresh
      && 0Z <= cost s && script_size s <= Bigint.add (size old) (size fresh)}
  @@ total

val optimal_at : (old : int list) -> (fresh : int list) ->
  (computed : {s : script | cost s = Vox_diff_metric.metric old fresh}) ->
  (other : script) ->
  {u : unit | let refine_ s = computed in
    if apply old other === Some fresh then cost s <= cost other else true}
  @@ total
