let[@def] rec (source_matches_distance @ total)
    (source : char iarray @ immutable) (index : int)
    (distance : int) (remaining : int) = ghost_ (
  if remaining <= 0 then true
  else
    Vox_lz4_spec_bytes.source_at source index === Vox_lz4_spec_bytes.source_at source (index - distance)
    && source_matches_distance source (index + 1) distance
         (remaining - 1))
[@@decreases remaining]

let rec (source_matches_distance_extend @ total) :
    (source : char iarray) ->
    (index : {i : int | 0 <= i && i < Iarray.length source}) ->
    (distance : {d : int | 0 < d && d <= index}) ->
    (count : {n : int | 0 <= n && n < Iarray.length source - index}) ->
    {u : unit | not (source_matches_distance source index distance count
      && Vox_lz4_spec_bytes.source_at source (index + count) ===
         Vox_lz4_spec_bytes.source_at source (index + count - distance))
      || source_matches_distance source index distance (count + 1)} @ ghost =
  fun source index distance count -> ghost_ (
    source_matches_distance_def source index distance (count + 1);
    source_matches_distance_def source index distance count;
    if count > 0
       && source_matches_distance source index distance count
       && Vox_lz4_spec_bytes.source_at source (index + count) ===
          Vox_lz4_spec_bytes.source_at source (index + count - distance) then begin
      source_matches_distance_extend source (index + 1) distance
        (count - 1);
    end;
    source_matches_distance_def source (index + 1) distance count;
    ())
[@@decreases count]

let[@def] rec (scan_match @ total) :
    (source : char iarray) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (distance : {d : int | 0 < d && d <= position}) ->
    (limit : {n : int | 0 <= n
      && n <= Iarray.length source - position}) ->
    (count : {n : int | 0 <= n && n <= limit
      && source_matches_distance source position distance n}) ->
    {r : int | count <= r && r <= limit
      && source_matches_distance source position distance r} =
  fun source position distance limit count ->
    if count = limit then count
    else
      let current = position + count in
      let prior = current - distance in
      let current_char = Vox_sequence.iarray_get source current in
      let prior_char = Vox_sequence.iarray_get source prior in
      if Vox_lz4_spec_bytes.same_char current_char prior_char then begin
        ghost_ (
          Vox_lz4_spec_bytes.source_at_def source current;
          Vox_lz4_spec_bytes.source_at_def source prior;
          source_matches_distance_extend source position distance count);
        scan_match source position distance limit (count + 1)
      end else count
[@@decreases limit - count]

let[@def] (match_length @ total) :
    (source : char iarray) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (distance : {d : int | 0 < d && d <= position}) ->
    (limit : {n : int | 0 <= n
      && n <= Iarray.length source - position}) ->
    {r : int | 0 <= r && r <= limit
      && source_matches_distance source position distance r} =
  fun source position distance limit ->
    ghost_ (source_matches_distance_def source position distance 0);
    scan_match source position distance limit 0

type match_choice = { distance : int; length : int }

external int32_of_int : int -> int32 @@ total = "%int32_of_int"
external int32_to_int : int32 -> int @@ total = "%int32_to_int"
external int32_or : int32 -> int32 -> int32 @@ total = "%int32_or"
external int32_mul : int32 -> int32 -> int32 @@ total = "%int32_mul"
external int32_lsl : int32 -> int -> int32 @@ total = "%int32_lsl"
external int32_lsr : int32 -> int -> int32 @@ total = "%int32_lsr"

(* The hint can be arbitrary; the returned match is checked against source. *)
let[@def] (choose_match @ total) :
    (source : char iarray) ->
    (position : {p : int | 0 <= p && p <= Iarray.length source}) ->
    (limit : {n : int | 0 <= n
      && n <= Iarray.length source - position}) ->
    (hint : int) ->
    {r : match_choice option | match r with
      | None -> true
      | Some m -> 0 < m.distance && m.distance <= 65535
        && m.distance <= position
        && 4 <= m.length && m.length <= limit
        && source_matches_distance source position m.distance m.length} =
  fun source position limit hint ->
    if hint < 0 || hint >= position || position - hint > 65535
       || limit < 4 then None
    else
      let distance = position - hint in
      let length = match_length source position distance limit in
      if length < 4 then None
      else Some { distance; length }

let (hash_bytes @ total) (c0 : char) (c1 : char) (c2 : char) (c3 : char) :
    {h : int | 0 <= h && h < 65536} =
  let b0 = int32_of_int (Vox_lz4_spec_parse.byte_of_char c0) in
  let b1 = int32_of_int (Vox_lz4_spec_parse.byte_of_char c1) in
  let b2 = int32_of_int (Vox_lz4_spec_parse.byte_of_char c2) in
  let b3 = int32_of_int (Vox_lz4_spec_parse.byte_of_char c3) in
  let word = int32_or b0
    (int32_or (int32_lsl b1 8)
       (int32_or (int32_lsl b2 16) (int32_lsl b3 24))) in
  let hash = int32_to_int
    (int32_lsr (int32_mul word 0x9e3779b1l) 16) in
  if 0 <= hash && hash < 65536 then hash else 0

let[@def] (hash4 @ total) :
    (source : char iarray) ->
    (position : {p : int | 0 <= p
      && p <= Iarray.length source - 4}) ->
    {h : int | 0 <= h && h < 65536} =
  fun source position ->
    hash_bytes
      (Vox_sequence.iarray_get source position)
      (Vox_sequence.iarray_get source (position + 1))
      (Vox_sequence.iarray_get source (position + 2))
      (Vox_sequence.iarray_get source (position + 3))
