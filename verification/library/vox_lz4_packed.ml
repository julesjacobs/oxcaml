module B = Vox_lz4_buffer
module M = Raw_memory
module S = Vox_sequence
module P = Ghost_pref
module H = P.Heap

include Vox_lz4_spec_decode

let rec copy_match :
    (distance : {d : int | 0 < d}) ->
    (remaining : {n : int | 0 <= n}) ->
    (buffer : {b : B.t | distance <= b.used
      && remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && after.used = buffer.used + remaining
      && P.own after.permission ===
           Vox_lz4_spec_decode.copy_heap (P.own buffer.permission) buffer.block buffer.used
             distance remaining} @ unique =
  fun distance remaining buffer ->
    if remaining = 0 then begin
      let { B.block; permission; used } = buffer in
      ghost_ (Vox_lz4_spec_decode.copy_heap_def (P.own (borrow_ permission)) block used
                distance remaining);
      { B.block; permission; used }
    end else
      let { B.block; permission; used } = buffer in
      let before = ghost_ (P.own (borrow_ permission)) in
      ghost_ (Vox_lz4_spec_decode.copy_heap_def before block used distance remaining);
      let buffer : B.t = { B.block; permission; used } in
      let source = B.used (borrow_ buffer) - distance in
      let observed = B.observe buffer source in
      let value = observed.value in
      let buffer = observed.next in
      let buffer = B.append buffer value in
      copy_match distance (remaining - 1) buffer
[@@decreases remaining]

let rec copy_literals :
    (source : char iarray) ->
    (first : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (remaining : {n : int | 0 <= n && n <= Iarray.length source - first}) ->
    (buffer : {b : B.t | remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && after.used = buffer.used + remaining
      && P.own after.permission ===
           Vox_lz4_spec_decode.literal_heap (P.own buffer.permission) buffer.block buffer.used
             source first remaining} @ unique =
  fun source first remaining buffer ->
    if remaining = 0 then begin
      let { B.block; permission; used } = buffer in
      ghost_ (Vox_lz4_spec_decode.literal_heap_def (P.own (borrow_ permission)) block used
                source first remaining);
      { B.block; permission; used }
    end else
      let { B.block; permission; used } = buffer in
      let before = ghost_ (P.own (borrow_ permission)) in
      ghost_ (Vox_lz4_spec_decode.literal_heap_def before block used source first remaining);
      let buffer : B.t = { B.block; permission; used } in
      let value = Vox_lz4_spec_decode.byte_of_char (S.iarray_get source first) in
      let buffer = B.append buffer value in
      copy_literals source (first + 1) (remaining - 1) buffer
[@@decreases remaining]

type outcome : value mod portable contended = {
  status : status;
  buffer : B.t;
}

let rec decode_sequences : (source : char iarray) ->
    (input_pos : int) -> (last_match_start : int) -> (fuel : int) ->
    (buffer : B.t) @ unique ->
    {r : outcome |
      let refine_ model =
        Vox_lz4_spec_decode.decode_model source input_pos last_match_start fuel
          (M.length buffer.block) buffer.block buffer.used
          (P.own buffer.permission) in
      r.status === model.kind && r.buffer.block === buffer.block
      && r.buffer.used = model.count
      && P.own r.buffer.permission === model.state} @ unique =
  fun source input_pos last_match_start fuel buffer ->
  let { B.block; permission; used } = buffer in
  let before = ghost_ (P.own (borrow_ permission)) in
  ghost_ (Vox_lz4_spec_decode.decode_model_def source input_pos last_match_start fuel
            (M.length block) block used before);
  let buffer : B.t = { B.block; permission; used } in
  let n = Iarray.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { status = Malformed; buffer }
  else
    let token = Vox_lz4_spec_decode.byte_of_char (S.iarray_get source input_pos) in
    let after_token = input_pos + 1 in
    match Vox_lz4_spec_decode.read_length source after_token (Vox_lz4_spec_decode.high4 token) with
    | Length_truncated -> { status = Malformed; buffer }
    | Length_limit -> { status = Output_limit; buffer }
    | Length (literal_pos, literal_count) ->
      if literal_pos < 0 || literal_pos > n || literal_count < 0
         || literal_count > n - literal_pos then
        { status = Malformed; buffer }
      else
        let used = B.used (borrow_ buffer) in
        let capacity = B.capacity (borrow_ buffer) in
        if literal_count > capacity - used then
          { status = Output_limit; buffer }
        else
          let after_literals = literal_pos + literal_count in
          if after_literals = n then
            if Vox_lz4_spec_decode.low15 token <> 0
               || (last_match_start >= 0
                   && (literal_count < 5
                       || last_match_start > used + literal_count - 12))
            then { status = Malformed; buffer }
            else
              { status = Done;
                buffer = copy_literals source literal_pos literal_count buffer }
          else if n - after_literals < 2 then
            { status = Malformed; buffer }
          else
            let low = Vox_lz4_spec_decode.byte_of_char (S.iarray_get source after_literals) in
            let high =
              Vox_lz4_spec_decode.byte_of_char (S.iarray_get source (after_literals + 1)) in
            let distance = low + high * 256 in
            if distance <= 0 || distance > used + literal_count then
              { status = Malformed; buffer }
            else
              match Vox_lz4_spec_decode.read_length source (after_literals + 2)
                      (Vox_lz4_spec_decode.low15 token) with
              | Length_truncated -> { status = Malformed; buffer }
              | Length_limit -> { status = Output_limit; buffer }
              | Length (next_pos, match_code) ->
                if next_pos < 0 || next_pos > n || match_code < 0
                   || match_code > capacity - used - literal_count - 4 then
                  { status = Output_limit; buffer }
                else
                  let buffer =
                    copy_literals source literal_pos literal_count buffer in
                  let current = B.used (borrow_ buffer) in
                  let room = B.capacity (borrow_ buffer) - current in
                  if distance > current || match_code + 4 > room then
                    { status = Malformed; buffer }
                  else
                    let buffer = copy_match distance (match_code + 4) buffer in
                    decode_sequences source next_pos current (fuel - 1) buffer
[@@decreases fuel]

let decode : (source : char iarray) -> (capacity : int) ->
    {r : (status * B.t) option | match r with
      | None -> true
      | Some (status, buffer) ->
        let refine_ model =
          Vox_lz4_spec_decode.decode_model source 0 (-1) (Iarray.length source) capacity
            buffer.block 0 (M.footprint buffer.block) in
        M.length buffer.block = capacity
        && status === model.kind && buffer.used = model.count
        && P.own buffer.permission === model.state} @ unique =
  fun source capacity ->
  if capacity < 0 || capacity > 4194304 then None
  else
    match B.create capacity with
    | None -> None
    | Some buffer ->
      let { status; buffer } =
        decode_sequences source 0 (-1) (Iarray.length source) buffer in
      Some (status, buffer)
