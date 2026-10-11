open Vox_lz4_spec_bytes

let rec (source_matches_distance_extend @ total) :
    (source : char iarray) ->
    (index : {i : int | 0 <= i && i < Iarray.length source}) ->
    (distance : {d : int | 0 < d && d <= index}) ->
    (count : {n : int | 0 <= n && n < Iarray.length source - index}) ->
    {u : unit | not (source_matches_distance source index distance count
      && source_at source (index + count) ===
         source_at source (index + count - distance))
      || source_matches_distance source index distance (count + 1)} @ ghost =
  fun source index distance count -> ghost_ (
    source_matches_distance_def source index distance (count + 1);
    source_matches_distance_def source index distance count;
    if count > 0
       && source_matches_distance source index distance count
       && source_at source (index + count) ===
          source_at source (index + count - distance) then begin
      source_matches_distance_extend source (index + 1) distance
        (count - 1);
    end;
    source_matches_distance_def source (index + 1) distance count;
    ())
[@@decreases count]
