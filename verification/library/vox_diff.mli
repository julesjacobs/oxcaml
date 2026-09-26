(** Verified diff on finite immutable integer sequences; the demo encodes
    bytes as integers. Keep costs zero; insertion and deletion cost one.

    Inputs of at most 1,000,000 elements each always succeed. Larger inputs
    return Input_too_large. Totality uses Vox's convention excluding runtime
    resource exhaustion; the size limit is not a RAM or stack guarantee.

    The implementation greedily keeps equal heads. At each frontier slot it
    compares old-input positions after the candidate edits and before the
    next Keep run, choosing insertion on equal positions. This is the tie
    policy, not a claim of global lexicographic minimality.

    The optimality theorem compares every finite script whose application
    succeeds, without imposing the input-size limit on that script. *)

open Vox_diff_spec

type error = Input_too_large [@@inductive]

val diff : (old : int list) -> (fresh : int list) ->
  {r : (script, error) result | match r with
    | Error Input_too_large -> 1000000Z < size old || 1000000Z < size fresh
    | Ok s -> size old <= 1000000Z && size fresh <= 1000000Z
      && source s === old && target s === fresh
      && apply old s === Some fresh
      && cost s = minimum_cost old fresh
      && 0Z <= cost s && script_size s <= Bigint.add (size old) (size fresh)}
  @@ total

val optimal_at : (old : int list) -> (fresh : int list) ->
  (computed : {s : script | cost s = minimum_cost old fresh}) ->
  (other : script) ->
  {u : unit |
    if apply old other === Some fresh then cost computed <= cost other
    else true}
  @@ total

val invert_correct : (script : script) ->
  {u : unit | source (invert script) === target script
    && target (invert script) === source script
    && cost (invert script) = cost script} @@ total

val inverse_patch : (script : script) ->
  {u : unit | apply (target script) (invert script) === Some (source script)}
  @@ total
