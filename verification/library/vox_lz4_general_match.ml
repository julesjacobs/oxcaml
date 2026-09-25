module D = Vox_lz4_packed
module E = Vox_lz4_packed_encode
module R = Vox_lz4_roundtrip
module M = Raw_memory
module P = Ghost_pref
module H = P.Heap

let rec (output_matches_get @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) -> (count : int) -> (index : int) ->
    {u : unit | not (R.output_matches heap block source count
      && 0 <= index && index < count)
      || match Vox_lz4_spec_bytes.source_at source index with
         | Some c -> H.at heap (M.location block index) ===
                     Some (Some (Vox_lz4_spec_decode.byte_of_char c))
         | None -> false} @ ghost =
  fun heap block source count index -> ghost_ (
    R.output_matches_def heap block source count;
    if count > 0 && index >= 0 && index < count - 1 then
      output_matches_get heap block source (count - 1) index;
    ())
[@@decreases count]

let rec (output_matches_put_outside @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) -> (count : {n : int | 0 <= n}) ->
    (index : {i : int | count <= i}) -> (byte : M.byte) ->
    {u : unit | not (R.output_matches heap block source count)
      || R.output_matches
           (H.put heap (M.location block index) (Some byte))
           block source count} @ ghost =
  fun heap block source count index byte -> ghost_ (
    R.output_matches_def heap block source count;
    R.output_matches_def
      (H.put heap (M.location block index) (Some byte))
      block source count;
    if count > 0 then begin
      M.location_law block block index (count - 1);
      output_matches_put_outside heap block source (count - 1) index byte
    end;
    ())
[@@decreases count]

let (output_matches_append @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) ->
    (count : {n : int | 0 <= n && n < Iarray.length source}) ->
    {u : unit | not (R.output_matches heap block source count)
      || R.output_matches
           (H.put heap (M.location block count)
             (Some (Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source count))))
           block source (count + 1)} @ ghost =
  fun heap block source count -> ghost_ (
    let byte = Vox_lz4_spec_decode.byte_of_char (Vox_sequence.iarray_get source count) in
    output_matches_put_outside heap block source count count byte;
    R.output_matches_def
      (H.put heap (M.location block count) (Some byte))
      block source (count + 1);
    Vox_lz4_spec_bytes.source_at_def source count;
    ())

include Vox_lz4_spec_match

let rec (source_matches_distance_at @ total) :
    (source : char iarray) ->
    (index : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (distance : {d : int | 0 < d && d <= index}) ->
    (count : {n : int | 0 <= n && n <= Iarray.length source - index}) ->
    (offset : {j : int | 0 <= j && j < count}) ->
    {u : unit | not (Vox_lz4_spec_match.source_matches_distance source index distance count)
      || Vox_lz4_spec_bytes.source_at source (index + offset) ===
         Vox_lz4_spec_bytes.source_at source (index + offset - distance)} @ ghost =
  fun source index distance count offset -> ghost_ (
    Vox_lz4_spec_match.source_matches_distance_def source index distance count;
    if offset > 0 then
      source_matches_distance_at source (index + 1) distance
        (count - 1) (offset - 1);
    ())
[@@decreases count]

let rec (copy_literals_preserves_source @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) ->
    (used : {n : int | 0 <= n && n <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n
      && n <= Iarray.length source - used}) ->
    {u : unit | not (R.output_matches heap block source used)
      || R.output_matches
           (Vox_lz4_spec_decode.literal_heap heap block used source used remaining)
           block source (used + remaining)} @ ghost =
  fun heap block source used remaining -> ghost_ (
    Vox_lz4_spec_decode.literal_heap_def heap block used source used remaining;
    if remaining > 0 && R.output_matches heap block source used then begin
      Vox_lz4_spec_bytes.source_at_def source used;
      let byte = Vox_lz4_spec_decode.byte_of_char
        (Vox_sequence.iarray_get source used) in
      let next = H.put heap (M.location block used) (Some byte) in
      output_matches_append heap block source used;
      copy_literals_preserves_source next block source (used + 1)
        (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (copy_match_preserves_source @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) ->
    (used : {n : int | 0 <= n && n <= Iarray.length source}) ->
    (distance : {d : int | 0 < d && d <= used}) ->
    (remaining : {n : int | 0 <= n
      && n <= Iarray.length source - used}) ->
    {u : unit | not (R.output_matches heap block source used
      && Vox_lz4_spec_match.source_matches_distance source used distance remaining)
      || R.output_matches
           (Vox_lz4_spec_decode.copy_heap heap block used distance remaining)
           block source (used + remaining)} @ ghost =
  fun heap block source used distance remaining -> ghost_ (
    Vox_lz4_spec_decode.copy_heap_def heap block used distance remaining;
    if remaining > 0 && R.output_matches heap block source used
       && Vox_lz4_spec_match.source_matches_distance source used distance remaining then begin
      Vox_lz4_spec_match.source_matches_distance_def source used distance remaining;
      output_matches_get heap block source used (used - distance);
      Vox_lz4_spec_bytes.source_at_def source used;
      let byte = Vox_lz4_spec_decode.byte_of_char
        (Vox_sequence.iarray_get source used) in
      let next = H.put heap (M.location block used) (Some byte) in
      output_matches_append heap block source used;
      copy_match_preserves_source next block source (used + 1) distance
        (remaining - 1)
    end;
    ())
[@@decreases remaining]

let (sequence_preserves_source @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (source : char iarray) ->
    (used : {n : int | 0 <= n && n <= Iarray.length source}) ->
    (literals : {n : int | 0 <= n
      && n <= Iarray.length source - used}) ->
    (distance : {d : int | 0 < d && d <= used + literals}) ->
    (match_length : {n : int | 0 <= n
      && n <= Iarray.length source - used - literals}) ->
    {u : unit | not (R.output_matches heap block source used
      && Vox_lz4_spec_match.source_matches_distance source (used + literals)
           distance match_length)
      || R.output_matches
           (Vox_lz4_spec_decode.copy_heap
              (Vox_lz4_spec_decode.literal_heap heap block used source used literals)
              block (used + literals) distance match_length)
           block source (used + literals + match_length)} @ ghost =
  fun heap block source used literals distance match_length -> ghost_ (
    copy_literals_preserves_source heap block source used literals;
    copy_match_preserves_source
      (Vox_lz4_spec_decode.literal_heap heap block used source used literals)
      block source (used + literals) distance match_length;
    ())
