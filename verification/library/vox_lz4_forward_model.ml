module M = Vox_lz4_general_match
module P = Vox_lz4_general_plan
module F = Vox_lz4_fast_plan_model

let[@def] rec (scan @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    (entries : F.hashes) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (anchor : {a : int | 0 <= a && a <= position}) ->
    (fuel : {f : int | f = Iarray.length source - position + 1}) ->
    {r : P.plan | P.valid_plan source anchor r} =
  fun source entries position anchor fuel ->
    if position > Iarray.length source - 12 then begin
      ghost_ (P.valid_plan_def source anchor P.End);
      P.End
    end else
      let hash = M.hash4 source position in
      let candidate = F.lookup hash entries in
      let entries = (hash, position) :: entries in
      let limit = Iarray.length source - 5 - position in
      match M.choose_match source position limit candidate with
      | None -> scan source entries (position + 1) anchor (fuel - 1)
      | Some choice ->
        let next = position + choice.length in
        let step = { P.position; distance = choice.distance; length = choice.length } in
        let rest = scan source entries next next (fuel - choice.length) in
        ghost_ (P.valid_plan_def source anchor (P.Sequence (step, rest)));
        P.Sequence (step, rest)
[@@decreases fuel]

let[@def] (from_source @ total) :
    (source : {s : char iarray | Iarray.length s <= 4194304}) ->
    {r : P.plan | P.valid_plan source 0 r} =
  fun source -> scan source [] 0 0 (Iarray.length source + 1)
