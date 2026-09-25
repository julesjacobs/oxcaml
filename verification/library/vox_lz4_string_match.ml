module V = Vox_string_view
module M = Vox_lz4_general_match
module E = Vox_lz4_packed_encode
module D = Vox_lz4_packed

let rec (scan_match @ total) :
    (source : string) ->
    (model : {m : char iarray | m === V.contents source}) @ ghost ->
    (position : {p : int | 0 <= p && p <= Iarray.length model}) ->
    (distance : {d : int | 0 < d && d <= position}) ->
    (limit : {n : int | 0 <= n && n <= Iarray.length model - position}) ->
    (count : {n : int | 0 <= n && n <= limit
      && M.source_matches_distance model position distance n}) ->
    {r : int | count <= r && r <= limit
      && M.source_matches_distance model position distance r
      && r = M.scan_match model position distance limit count} =
  fun source model position distance limit count ->
    ghost_ (M.scan_match_def model position distance limit count);
    if count = limit then count
    else
      let current = position + count in
      let prior = current - distance in
      let current_char = V.get source current in
      let prior_char = V.get source prior in
      ghost_ (
        Vox_iarray.at_get model current;
        Vox_iarray.at_get model prior);
      if E.same_char current_char prior_char then begin
        ghost_ (
          E.source_at_def model current;
          E.source_at_def model prior;
          M.source_matches_distance_extend model
            position distance count);
        scan_match source model position distance limit (count + 1)
      end else count
[@@decreases limit - count]

let (match_length @ total) :
    (source : string) ->
    (model : {m : char iarray | m === V.contents source}) @ ghost ->
    (position : {p : int | 0 <= p && p <= Iarray.length model}) ->
    (distance : {d : int | 0 < d && d <= position}) ->
    (limit : {n : int | 0 <= n && n <= Iarray.length model - position}) ->
    {r : int | 0 <= r && r <= limit
      && M.source_matches_distance model position distance r
      && r = M.match_length model position distance limit} =
  fun source model position distance limit ->
    ghost_ (
      M.match_length_def model position distance limit;
      M.source_matches_distance_def model position distance 0);
    scan_match source model position distance limit 0

let[@inline always] (choose_match @ total) :
    (source : string) ->
    (model : {m : char iarray | m === V.contents source}) @ ghost ->
    (position : {p : int | 0 <= p && p <= Iarray.length model}) ->
    (limit : {n : int | 0 <= n && n <= Iarray.length model - position}) ->
    (hint : int) ->
    {r : M.match_choice option |
      r === M.choose_match model position limit hint
      && match r with
      | None -> true
      | Some m -> 0 < m.distance && m.distance <= 65535
        && m.distance <= position && 4 <= m.length && m.length <= limit
        && M.source_matches_distance model
             position m.distance m.length} =
  fun source model position limit hint ->
    ghost_ (M.choose_match_def model position limit hint);
    if hint < 0 || hint >= position || position - hint > 65535 || limit < 4
    then None
    else
      let distance = position - hint in
      let length = match_length source model position distance limit in
      if length < 4 then None else Some { M.distance; length }

let[@inline always] (hash4 @ total) :
    (source : string) ->
    (model : {m : char iarray | m === V.contents source}) @ ghost ->
    (position : {p : int | 0 <= p && p <= Iarray.length model - 4}) ->
    {h : int | 0 <= h && h < 65536
      && h = M.hash4 model position} =
  fun source model position ->
    ghost_ (M.hash4_def model position);
    let c0 = V.get source position in
    let c1 = V.get source (position + 1) in
    let c2 = V.get source (position + 2) in
    let c3 = V.get source (position + 3) in
    ghost_ (
      Vox_iarray.at_get model position;
      Vox_iarray.at_get model (position + 1);
      Vox_iarray.at_get model (position + 2);
      Vox_iarray.at_get model (position + 3));
    M.hash_bytes c0 c1 c2 c3
