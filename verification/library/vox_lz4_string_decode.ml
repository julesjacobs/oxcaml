module D = Vox_lz4_packed
module B = Vox_lz4_buffer
module M = Raw_memory
module P = Ghost_pref
module V = Vox_string_view
open D

let rec copy_literals_into :
    (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (first : {i : int | 0 <= i && i <= Iarray.length model}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length model - first}) ->
    (block : {b : M.t | M.length b <= 4194304}) ->
    (used : {n : int | 0 <= n && n <= M.length block
      && remaining <= M.length block - n}) ->
    (permission : {p : P.token |
      M.covers (P.own p) block 0 (M.length block)
      && P.Heap.mem (P.own p) (M.location block (-1))
      && Vox_lz4_spec_storage.initialized (P.own p) block used}) @ unique ghost ->
    {after : B.t | after.block === block
      && after.used = used + remaining
      && P.own after.permission ===
           Vox_lz4_spec_decode.literal_heap (P.own permission) block used
             model first remaining} @ unique =
  fun model source first remaining block used permission ->
    ghost_ (Vox_lz4_spec_decode.literal_heap_def (P.own (borrow_ permission))
      block used model first remaining);
    if remaining = 0 then { B.block; permission; used }
    else
      let value = Vox_lz4_spec_decode.byte_of_char (V.get source first) in
      ghost_ (Vox_iarray.at_get model first);
      let buffer = B.append { B.block; permission; used } value in
      let { B.block; permission; used } = buffer in
      copy_literals_into model source (first + 1) (remaining - 1)
        block used permission
[@@decreases remaining]

let copy_literals :
    (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (first : {i : int | 0 <= i && i <= Iarray.length model}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length model - first}) ->
    (buffer : {b : B.t | remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.B.block
      && after.used = buffer.B.used + remaining
      && P.own after.permission ===
           Vox_lz4_spec_decode.literal_heap (P.own buffer.B.permission) buffer.B.block buffer.B.used
             model first remaining} @ unique =
  fun model source first remaining buffer ->
    let { B.block; permission; used } = buffer in
    copy_literals_into model source first remaining block used permission

let rec copy_match_into :
    (distance : {d : int | 0 < d}) ->
    (remaining : {n : int | 0 <= n}) ->
    (block : {b : M.t | M.length b <= 4194304}) ->
    (used : {n : int | distance <= n && n <= M.length block
      && remaining <= M.length block - n}) ->
    (permission : {p : P.token |
      M.covers (P.own p) block 0 (M.length block)
      && P.Heap.mem (P.own p) (M.location block (-1))
      && Vox_lz4_spec_storage.initialized (P.own p) block used}) @ unique ghost ->
    {after : B.t | after.block === block
      && after.used = used + remaining
      && P.own after.permission ===
           Vox_lz4_spec_decode.copy_heap (P.own permission) block used distance remaining} @ unique =
  fun distance remaining block used permission ->
    let before = ghost_ (P.own (borrow_ permission)) in
    ghost_ (Vox_lz4_spec_decode.copy_heap_def before block used distance remaining);
    if remaining = 0 then { B.block; permission; used }
    else
      let index = used - distance in
      ghost_ (
        B.initialized_get before block used index;
        M.covers_get before block 0 (M.length block) index);
      let value = M.read block index (borrow_ permission) in
      let buffer = B.append { B.block; permission; used } value in
      let { B.block; permission; used } = buffer in
      copy_match_into distance (remaining - 1) block used permission
[@@decreases remaining]

let copy_match :
    (distance : {d : int | 0 < d}) ->
    (remaining : {n : int | 0 <= n}) ->
    (buffer : {b : B.t | distance <= b.used
      && remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.B.block
      && after.used = buffer.B.used + remaining
      && P.own after.permission ===
           Vox_lz4_spec_decode.copy_heap (P.own buffer.B.permission) buffer.B.block buffer.B.used
             distance remaining} @ unique =
  fun distance remaining buffer ->
    let { B.block; permission; used } = buffer in
    copy_match_into distance remaining block used permission

include Vox_lz4_spec

type length_read = { length_result : D.length_result; cursor : int }

type outcome : value mod portable contended = {
  status : D.status;
  buffer : B.t;
  error : decode_error option;
}

let rec read_extended_length :
    (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length model}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (fuel : {f : int | 0 <= f}) ->
    {r : length_read | r.length_result === Vox_lz4_spec_decode.read_extended_length model cursor length fuel} =
  fun model source cursor length fuel ->
    ghost_ (Vox_lz4_spec_decode.read_extended_length_def model cursor length fuel);
    if fuel = 0 || cursor = V.length source then
      { length_result = Length_truncated; cursor }
    else
      let extension = Vox_lz4_spec_decode.byte_of_char (V.get source cursor) in
      ghost_ (Vox_iarray.at_get model cursor);
      if extension > 4194304 - length then
        { length_result = Length_limit; cursor = cursor + 1 }
      else
        let length = length + extension in
        let cursor = cursor + 1 in
        if extension = 255 then
          read_extended_length model source cursor length (fuel - 1)
        else { length_result = Length (cursor, length); cursor }
[@@decreases fuel]

let read_length :
    (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length model}) ->
    (initial : {n : int | 0 <= n && n <= 15}) ->
    {r : length_read | r.length_result === Vox_lz4_spec_decode.read_length model cursor initial} =
  fun model source cursor initial ->
    ghost_ (Vox_lz4_spec_decode.read_length_def model cursor initial);
    if initial < 15 then { length_result = Length (cursor, initial); cursor }
    else read_extended_length model source cursor 15 (V.length source - cursor)

let rec decode_sequences : (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) ->
    (input_pos : int) -> (last_match_start : int) -> (fuel : int) ->
    (buffer : B.t) @ unique ->
    {r : outcome |
      let model =
        Vox_lz4_spec_decode.decode_model model input_pos last_match_start fuel
          (M.length buffer.B.block) buffer.B.block buffer.B.used
          (P.own buffer.B.permission) in
      (match r.error with None -> r.status === D.Done
        | Some _ -> not (r.status === D.Done))
      && r.status === model.kind && r.buffer.B.block === buffer.B.block
      && r.buffer.B.used = model.count
      && P.own r.buffer.B.permission === model.state} @ unique =
  fun model source input_pos last_match_start fuel buffer ->
  let { B.block; permission; used } = buffer in
  let before = ghost_ (P.own (borrow_ permission)) in
  ghost_ (Vox_lz4_spec_decode.decode_model_def model input_pos last_match_start fuel
            (M.length block) block used before);
  let buffer : B.t = { B.block; permission; used } in
  let n = V.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { status = D.Malformed; buffer; error = Some (Malformed
        ((if n = 0 then Empty_block else Invalid_terminal_sequence), input_pos)) }
  else
    let token = Vox_lz4_spec_decode.byte_of_char (V.get source input_pos) in
    ghost_ (Vox_iarray.at_get model input_pos);
    let after_token = input_pos + 1 in
    let literal_length = read_length model source after_token (Vox_lz4_spec_decode.high4 token) in
    match literal_length.length_result with
    | Length_truncated -> { status = D.Malformed; buffer;
        error = Some (Malformed (Truncated_length, literal_length.cursor)) }
    | Length_limit -> { status = D.Output_limit; buffer; error = Some Output_limit }
    | Length (literal_pos, literal_count) ->
      if literal_pos < 0 || literal_pos > n || literal_count < 0
         || literal_count > n - literal_pos then
        { status = D.Malformed; buffer; error = Some (Malformed (Truncated_literals, literal_pos)) }
      else
        let used = B.used (borrow_ buffer) in
        let capacity = B.capacity (borrow_ buffer) in
        if literal_count > capacity - used then
          { status = D.Output_limit; buffer; error = Some Output_limit }
        else
          let after_literals = literal_pos + literal_count in
          if after_literals = n then
            if Vox_lz4_spec_decode.low15 token <> 0
               || (last_match_start >= 0
                   && (literal_count < 5
                       || last_match_start > used + literal_count - 12))
            then { status = D.Malformed; buffer; error = Some (Malformed (Invalid_terminal_sequence, after_literals)) }
            else
              { status = D.Done; error = None;
                buffer = copy_literals model source literal_pos literal_count buffer }
          else if n - after_literals < 2 then
            { status = D.Malformed; buffer; error = Some (Malformed (Truncated_offset, after_literals)) }
          else
            let low = Vox_lz4_spec_decode.byte_of_char (V.get source after_literals) in
            let high =
              Vox_lz4_spec_decode.byte_of_char (V.get source (after_literals + 1)) in
            ghost_ (Vox_iarray.at_get model after_literals;
              Vox_iarray.at_get model (after_literals + 1));
            let distance = low + high * 256 in
            if distance <= 0 || distance > used + literal_count then
              { status = D.Malformed; buffer; error = Some (Malformed
                ((if distance = 0 then Zero_offset else Offset_beyond_output), after_literals + 2)) }
            else
              let match_length = read_length model source (after_literals + 2)
                      (Vox_lz4_spec_decode.low15 token) in
              match match_length.length_result with
              | Length_truncated -> { status = D.Malformed; buffer;
                  error = Some (Malformed (Truncated_length, match_length.cursor)) }
              | Length_limit -> { status = D.Output_limit; buffer; error = Some Output_limit }
              | Length (next_pos, match_code) ->
                if next_pos < 0 || next_pos > n || match_code < 0
                   || match_code > capacity - used - literal_count - 4 then
                  { status = D.Output_limit; buffer; error = Some Output_limit }
                else
                  let buffer =
                    copy_literals model source literal_pos literal_count buffer in
                  let current = B.used (borrow_ buffer) in
                  let room = B.capacity (borrow_ buffer) - current in
                  if distance > current || match_code + 4 > room then
                    { status = D.Malformed; buffer; error = Some (Malformed (Offset_beyond_output, next_pos)) }
                  else
                    let buffer = copy_match distance (match_code + 4) buffer in
                    decode_sequences model source next_pos current (fuel - 1) buffer
[@@decreases fuel]

let decode : (model : char iarray) @ ghost ->
    (source : {s : string | V.contents s === model}) -> (capacity : int) ->
    {r : outcome option | match r with
      | None -> true
      | Some outcome ->
        let buffer = outcome.buffer in
        let status = outcome.status in
        let model =
          Vox_lz4_spec_decode.decode_model model 0 (-1) (Iarray.length model) capacity
            buffer.B.block 0 (M.footprint buffer.B.block) in
        (match outcome.error with None -> status === D.Done
          | Some _ -> not (status === D.Done))
        && M.length buffer.B.block = capacity
        && status === model.kind && buffer.B.used = model.count
        && P.own buffer.B.permission === model.state} @ unique =
  fun model source capacity ->
  if capacity < 0 || capacity > 4194304 then None
  else
    match B.create capacity with
    | None -> None
    | Some buffer ->
      (* Exceptions consume authority; unreachable storage is reclaimed by
         the raw carrier finalizer without restoring the input token. *)
      Some (decode_sequences model source 0 (-1) (V.length source) buffer)

module EB = Vox_lz4_encode_buffer
module Copy = Vox_lz4_string_copy
module Snapshot = Vox_lz4_snapshot

let rec (initialized_bridge @ total) :
    (heap : P.heap) -> (block : M.t) -> (count : int) ->
    {u : unit | Vox_lz4_spec_storage.initialized heap block count =
      Vox_lz4_spec_storage.initialized heap block count} @ ghost =
  fun heap block count -> ghost_ (
    Vox_lz4_spec_storage.initialized_def heap block count;
    Vox_lz4_spec_storage.initialized_def heap block count;
    if count > 0 then initialized_bridge heap block (count - 1);
    ())
[@@decreases count]

let decode_string : (wire : string) ->
    (capacity : {n : int | 0 <= n && n <= 4194304}) ->
    {r : decoded | Vox_lz4_spec.matches_model wire capacity r} =
  fun wire capacity ->
    let model = ghost_ (V.contents wire) in
    match decode model wire capacity with
    | None -> raise Out_of_memory
    | Some { status; buffer; error } ->
      let { B.block; permission; used } = buffer in
      match status with
      | D.Malformed | D.Output_limit ->
        let decoded = { output = None; status; error; block = ghost_ block } in
        ghost_ (Vox_lz4_spec.matches_model_def wire capacity decoded);
        B.release { B.block; permission; used };
        decoded
      | D.Done ->
        let _ = ghost_ (initialized_bridge (P.own (borrow_ permission)) block used) in
        let output : {s : string | Iarray.length (V.contents s) = used
            && Vox_lz4_spec_bytes.prefix_matches (V.contents s)
                 (P.own permission) block used} =
          try Copy.copy_prefix block used (borrow_ permission)
          with exn ->
            B.release { B.block; permission; used };
            raise exn
        in
        let decoded = { output = Some output; status; error; block = ghost_ block } in
        ghost_ (Vox_lz4_spec.matches_model_def wire capacity decoded);
        B.release { B.block; permission; used };
        decoded
