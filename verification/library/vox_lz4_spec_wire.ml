let[@def] (extra_count @ total) (length : int) :
    {e : int | 0 <= e && e <= 16449} = ghost_ (
  if length < 15 || length > 4194304 then 0
  else Vox_lz4_spec_bytes.extension_count (length - 15))

let[@def] rec (wire_matches_plan @ total)
    (source : char iarray @ immutable)
    (wire : char iarray @ immutable)
    (anchor : int) (cursor : int)
    (plan : Vox_lz4_spec_plan.plan @ immutable) = ghost_ (
  Vox_lz4_spec_plan.valid_plan_def source anchor plan;
  if Iarray.length source > 4194304
     || Iarray.length wire > 4210768
     || cursor < 0 || cursor >= Iarray.length wire
     || not (Vox_lz4_spec_plan.valid_plan source anchor plan) then false
  else
    match plan with
    | Vox_lz4_spec_plan.End ->
      let literals = Iarray.length source - anchor in
      let extensions = extra_count literals in
      Iarray.length wire = cursor + 1 + extensions + literals
      && Vox_lz4_spec_bytes.wire_byte wire cursor (Vox_lz4_spec_bytes.literal_token literals)
      && (literals < 15
          || Vox_lz4_spec_bytes.extension_bytes wire (cursor + 1) (literals - 15))
      && Vox_lz4_spec_bytes.literal_bytes wire (cursor + 1 + extensions)
           source anchor literals
    | Vox_lz4_spec_plan.Sequence (step, rest) ->
      let literals : {n : int | 0 <= n && n <= 4194304} =
        refine_ (step.position - anchor) in
      let match_code : {n : int | 0 <= n && n <= 4194304} =
        refine_ (step.length - 4) in
      let literal_extensions = extra_count literals in
      let match_extensions = extra_count match_code in
      let literal_pos = cursor + 1 + literal_extensions in
      let distance_pos = literal_pos + literals in
      let next_cursor = distance_pos + 2 + match_extensions in
      let distance = Vox_lz4_spec_token.split_distance step.distance in
      next_cursor < Iarray.length wire
      && Vox_lz4_spec_bytes.wire_byte wire cursor (Vox_lz4_spec_token.match_token literals match_code)
      && (literals < 15
          || Vox_lz4_spec_bytes.extension_bytes wire (cursor + 1) (literals - 15))
      && Vox_lz4_spec_bytes.literal_bytes wire literal_pos source anchor literals
      && Vox_lz4_spec_bytes.wire_byte wire distance_pos distance.low
      && Vox_lz4_spec_bytes.wire_byte wire (distance_pos + 1) distance.high
      && (match_code < 15
          || Vox_lz4_spec_bytes.extension_bytes wire (distance_pos + 2)
               (match_code - 15))
      && wire_matches_plan source wire
           (step.position + step.length) next_cursor rest)
