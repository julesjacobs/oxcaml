let[@def] rec (scan @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (entries : Vox_lz4_spec_hashes.hashes) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length source - position + 1}) ->
    {r : Vox_lz4_spec_plan.plan | Vox_lz4_spec_plan.valid_plan source anchor r} =
  fun source entries position anchor fuel ->
    if position > Iarray.length source - 12 then begin
      ghost_ (Vox_lz4_spec_plan.valid_plan_def source anchor Vox_lz4_spec_plan.End);
      Vox_lz4_spec_plan.End
    end else
      let hash = Vox_lz4_spec_match.hash4 source position in
      let candidate = Vox_lz4_spec_hashes.lookup hash entries in
      let entries = (hash, position) :: entries in
      let limit = Iarray.length source - 5 - position in
      match Vox_lz4_spec_match.choose_match source position limit candidate with
      | None -> scan source entries (position + 1) anchor (fuel - 1)
      | Some choice ->
        let next = position + choice.length in
        let step = { Vox_lz4_spec_plan.position; distance = choice.distance; length = choice.length } in
        let rest = scan source entries next next (fuel - choice.length) in
        ghost_ (Vox_lz4_spec_plan.valid_plan_def source anchor (Vox_lz4_spec_plan.Sequence (step, rest)));
        Vox_lz4_spec_plan.Sequence (step, rest)
[@@decreases fuel]

let[@def] (from_source @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    {r : Vox_lz4_spec_plan.plan | Vox_lz4_spec_plan.valid_plan source 0 r} =
  fun source -> scan source [] 0 0 (Iarray.length source + 1)
