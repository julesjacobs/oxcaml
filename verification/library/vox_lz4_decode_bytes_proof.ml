module D = Vox_lz4_spec_decode
module S = Vox_lz4_spec_decode_bytes
module M = Raw_memory
module P = Ghost_pref
module H = P.Heap

let[@def] rec (heap_matches @ total) (heap : P.heap @ immutable)
    (block : M.t @ immutable) (used : int)
    (reversed : S.byte list @ immutable) = ghost_ (
  match reversed with
  | [] -> used = 0
  | byte :: rest ->
    used > 0 && H.at heap (M.location block (used - 1)) === Some (Some byte)
    && heap_matches heap block (used - 1) rest)

let rec (put_outside @ total) :
    (heap : P.heap) -> (block : M.t) -> (used : int) ->
    (reversed : S.byte list) -> (index : {i : int | used <= i}) ->
    (byte : S.byte) ->
    {u : unit | not (heap_matches heap block used reversed)
      || heap_matches (H.put heap (M.location block index) (Some byte))
           block used reversed} @ ghost =
  fun heap block used reversed index byte -> ghost_ (
    heap_matches_def heap block used reversed;
    heap_matches_def (H.put heap (M.location block index) (Some byte))
      block used reversed;
    (match reversed with
    | [] -> ()
    | _ :: rest ->
      if heap_matches heap block used reversed then begin
        M.location_law block block index (used - 1);
        put_outside heap block (used - 1) rest index byte
      end);
    ())

let (append @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n < 4194304}) ->
    (reversed : S.byte list) -> (byte : S.byte) ->
    {u : unit | not (heap_matches heap block used reversed)
      || heap_matches (H.put heap (M.location block used) (Some byte))
           block (used + 1) (byte :: reversed)} @ ghost =
  fun heap block used reversed byte -> ghost_ (
    put_outside heap block used reversed used byte;
    heap_matches_def (H.put heap (M.location block used) (Some byte))
      block (used + 1) (byte :: reversed))

let rec (read_distance @ total) :
    (heap : P.heap) -> (block : M.t) -> (used : int) ->
    (reversed : S.byte list) ->
    (distance : {d : int | 0 < d && d <= used}) ->
    {u : unit | not (heap_matches heap block used reversed)
      || match S.at_distance reversed distance with
         | None -> false
         | Some byte -> H.at heap (M.location block (used - distance))
                          === Some (Some byte)} @ ghost =
  fun heap block used reversed distance -> ghost_ (
    heap_matches_def heap block used reversed;
    S.at_distance_def reversed distance;
    (match reversed with
    | [] -> ()
    | _ :: rest ->
      if heap_matches heap block used reversed && distance > 1 then
        read_distance heap block (used - 1) rest (distance - 1));
    ())

let rec (literals @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4194304}) ->
    (reversed : S.byte list) -> (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length source - first
      && n <= 4194304 - used}) ->
    {u : unit | not (heap_matches heap block used reversed)
      || heap_matches (D.literal_heap heap block used source first remaining)
           block (used + remaining)
           (S.literal_bytes reversed source first remaining)} @ ghost =
  fun heap block used reversed source first remaining -> ghost_ (
    D.literal_heap_def heap block used source first remaining;
    S.literal_bytes_def reversed source first remaining;
    if remaining > 0 && heap_matches heap block used reversed then begin
      let byte = Vox_lz4_spec_parse.byte_of_char (Vox_sequence.iarray_get source first) in
      append heap block used reversed byte;
      literals (H.put heap (M.location block used) (Some byte))
        block (used + 1) (byte :: reversed) source (first + 1) (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (matches @ total) :
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= 4194304}) ->
    (reversed : S.byte list) ->
    (distance : {d : int | 0 < d && d <= used}) ->
    (remaining : {n : int | 0 <= n && n <= 4194304 - used}) ->
    {u : unit | not (heap_matches heap block used reversed)
      || heap_matches (D.copy_heap heap block used distance remaining)
           block (used + remaining)
           (S.copy_bytes reversed distance remaining)} @ ghost =
  fun heap block used reversed distance remaining -> ghost_ (
    D.copy_heap_def heap block used distance remaining;
    S.copy_bytes_def reversed distance remaining;
    if remaining > 0 && heap_matches heap block used reversed then begin
      read_distance heap block used reversed distance;
      match S.at_distance reversed distance with
      | None -> ()
      | Some byte ->
        append heap block used reversed byte;
        matches (H.put heap (M.location block used) (Some byte))
          block (used + 1) (byte :: reversed) distance (remaining - 1)
    end;
    ())
[@@decreases remaining]

let rec (decode @ total) :
    (source : char iarray) -> (input_pos : int) ->
    (last_match_start : int) -> (fuel : int) ->
    (capacity : {n : int | 0 <= n && n <= 4194304}) ->
    (heap : P.heap) -> (block : M.t) ->
    (used : {n : int | 0 <= n && n <= capacity}) ->
    (reversed : S.byte list) ->
    {u : unit | not (heap_matches heap block used reversed)
      || let pure = S.decode_model source input_pos last_match_start fuel
                      capacity used reversed in
         let raw = D.decode_model source input_pos last_match_start fuel
                     capacity block used heap in
         pure.kind === raw.kind && pure.count = raw.count
         && 0 <= raw.count && raw.count <= capacity
         && heap_matches raw.state block raw.count pure.reversed} @ ghost =
  fun source input_pos last_match_start fuel capacity heap block used reversed ->
  ghost_ (
    S.decode_model_def source input_pos last_match_start fuel
      capacity used reversed;
    D.decode_model_def source input_pos last_match_start fuel
      capacity block used heap;
    let n = Iarray.length source in
    if heap_matches heap block used reversed
       && 0 <= input_pos && input_pos < n && fuel > 0 then begin
      let token = Vox_lz4_spec_parse.byte_of_char (Vox_sequence.iarray_get source input_pos) in
      match Vox_lz4_spec_parse.read_length source (input_pos + 1) (Vox_lz4_spec_parse.high4 token) with
      | D.Length_truncated | D.Length_limit -> ()
      | D.Length (literal_pos, literal_count) ->
        if 0 <= literal_pos && literal_pos <= n && 0 <= literal_count
           && literal_count <= n - literal_pos
           && literal_count <= capacity - used then begin
          let after_literals = literal_pos + literal_count in
          if after_literals = n then begin
            if Vox_lz4_spec_parse.low15 token = 0
               && (last_match_start < 0
                   || (literal_count >= 5
                       && last_match_start <= used + literal_count - 12)) then
              literals heap block used reversed source literal_pos literal_count
          end else if n - after_literals >= 2 then begin
            let low = Vox_lz4_spec_parse.byte_of_char
              (Vox_sequence.iarray_get source after_literals) in
            let high = Vox_lz4_spec_parse.byte_of_char
              (Vox_sequence.iarray_get source (after_literals + 1)) in
            let distance = low + high * 256 in
            if 0 < distance && distance <= used + literal_count then
              match Vox_lz4_spec_parse.read_length source (after_literals + 2) (Vox_lz4_spec_parse.low15 token) with
              | D.Length_truncated | D.Length_limit -> ()
              | D.Length (next_pos, match_code) ->
                if 0 <= next_pos && next_pos <= n && 0 <= match_code
                   && match_code <= capacity - used - literal_count - 4 then begin
                  literals heap block used reversed source literal_pos literal_count;
                  let next_heap = D.literal_heap heap block used source
                    literal_pos literal_count in
                  let next_reversed = S.literal_bytes reversed source
                    literal_pos literal_count in
                  let current = used + literal_count in
                  matches next_heap block current next_reversed distance
                    (match_code + 4);
                  decode source next_pos current (fuel - 1) capacity
                    (D.copy_heap next_heap block current distance (match_code + 4))
                    block (current + match_code + 4)
                    (S.copy_bytes next_reversed distance (match_code + 4))
                end
          end
        end
    end;
    ())
[@@decreases fuel]

let rec (observations @ total) :
    (values : char iarray) -> (heap : P.heap) -> (block : M.t) ->
    (used : int) -> (reversed : S.byte list) ->
    {u : unit | not (heap_matches heap block used reversed)
      || S.matches_bytes values used reversed =
           Vox_lz4_heap_bytes.prefix_matches values heap block used} @ ghost =
  fun values heap block used reversed -> ghost_ (
    heap_matches_def heap block used reversed;
    S.matches_bytes_def values used reversed;
    Vox_lz4_heap_bytes.prefix_matches_def values heap block used;
    (match reversed with
     | [] -> ()
     | _ :: rest ->
       if heap_matches heap block used reversed then
         observations values heap block (used - 1) rest);
    ())

let (initial_observations @ total) :
    (source : char iarray) ->
    (capacity : {n : int | 0 <= n && n <= 4194304}) ->
    (heap : P.heap) -> (block : M.t) -> (output : char iarray) ->
    {u : unit |
      let pure = S.decode_model source 0 (-1) (Iarray.length source)
                   capacity 0 [] in
      let raw = D.decode_model source 0 (-1) (Iarray.length source)
                  capacity block 0 heap in
      pure.kind === raw.kind && pure.count = raw.count
      && 0 <= raw.count && raw.count <= capacity
      && S.matches_bytes output pure.count pure.reversed =
           Vox_lz4_heap_bytes.prefix_matches output raw.state block raw.count}
      @ ghost =
  fun source capacity heap block output -> ghost_ (
    heap_matches_def heap block 0 [];
    decode source 0 (-1) (Iarray.length source) capacity heap block 0 [];
    let pure = S.decode_model source 0 (-1) (Iarray.length source) capacity 0 [] in
    let raw = D.decode_model source 0 (-1) (Iarray.length source)
      capacity block 0 heap in
    observations output raw.state block raw.count pure.reversed)

let rec (extended_cursor @ total) :
    (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (fuel : {f : int | 0 <= f}) ->
    {u : unit | match Vox_lz4_spec_parse.read_extended_length
        source cursor length fuel with
      | Vox_lz4_spec_parse.Length (next, _) ->
        cursor <= next && next <= Iarray.length source
      | _ -> true} @ ghost =
  fun source cursor length fuel -> ghost_ (
    Vox_lz4_spec_parse.read_extended_length_def source cursor length fuel;
    if fuel > 0 && cursor < Iarray.length source then begin
      let extension = Vox_lz4_spec_parse.byte_of_char
        (Vox_sequence.iarray_get source cursor) in
      if extension <= 4194304 - length && extension = 255 then
        extended_cursor source (cursor + 1) (length + extension) (fuel - 1)
    end;
    ())
[@@decreases fuel]

let (read_length_cursor @ total) :
    (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (initial : {n : int | 0 <= n && n <= 15}) ->
    {u : unit | match Vox_lz4_spec_parse.read_length source cursor initial with
      | Vox_lz4_spec_parse.Length (next, _) ->
        cursor <= next && next <= Iarray.length source
      | _ -> true} @ ghost =
  fun source cursor initial -> ghost_ (
    Vox_lz4_spec_parse.read_length_def source cursor initial;
    if initial >= 15 then
      extended_cursor source cursor 15 (Iarray.length source - cursor);
    ())

let[@def] (remaining_length @ total) :
    (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    Vox_lz4_spec_parse.length_result @ ghost =
  fun source cursor length -> ghost_ (
    Vox_lz4_spec_parse.read_extended_length source cursor length
      (Iarray.length source - cursor))
