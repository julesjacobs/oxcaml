type sequence = {
  position : int;
  distance : int;
  length : int;
}

type plan = End | Sequence of sequence * plan [@@inductive]

let[@def] rec (valid_plan @ total) (source : char iarray @ immutable)
    (anchor : int) (plan : plan @ immutable) = ghost_ (
  match plan with
  | End -> 0 <= anchor && anchor <= Iarray.length source
  | Sequence (step, rest) ->
    0 <= anchor && anchor <= step.position
    && step.position <= Iarray.length source - 12
    && 0 < step.distance && step.distance <= 65535
    && step.distance <= step.position
    && 4 <= step.length
    && step.length <= Iarray.length source - 5 - step.position
    && Vox_lz4_spec_match.source_matches_distance source step.position
         step.distance step.length
    && valid_plan source (step.position + step.length) rest)
