module B = Vox_lz4_buffer
module M = Raw_memory
module S = Vox_sequence
module P = Ghost_pref
module H = P.Heap

(* The inverse clause states the byte-preserving semantics of Char.code. *)
external char_of_byte : M.byte -> char @@ total = "%identity"
external byte_of_char : (c : char) ->
  {byte : M.byte | char_of_byte byte === c} @@ total = "%identity"

let (token_high @ total) :
    (token : M.byte) -> {shift : int | shift = 4} ->
  {high : int | 0 <= high && high <= 15
      && 16 * high <= token && token < 16 * (high + 1)} =
  fun token _ ->
    if token < 128 then
      if token < 64 then
        if token < 32 then (if token < 16 then 0 else 1)
        else if token < 48 then 2 else 3
      else if token < 96 then
        if token < 80 then 4 else 5
      else if token < 112 then 6 else 7
    else if token < 192 then
      if token < 160 then (if token < 144 then 8 else 9)
      else if token < 176 then 10 else 11
    else if token < 224 then
      if token < 208 then 12 else 13
    else if token < 240 then 14 else 15

let (high4 @ total) : (token : M.byte) ->
    {high : int | 0 <= high && high <= 15
      && 16 * high <= token && token < 16 * (high + 1)} =
  fun token -> token_high token 4

let (token_low @ total) :
    (token : M.byte) -> {mask : int | mask = 15} ->
    {low : int | 0 <= low && low <= 15
      && token = 16 * high4 token + low} =
  fun token _ -> token - 16 * high4 token

let (low15 @ total) : (token : M.byte) ->
    {low : int | 0 <= low && low <= 15
      && token = 16 * high4 token + low} =
  fun token -> token_low token 15

let[@def] rec (copy_heap @ total) (h : P.heap @ immutable)
    (block : M.t @ immutable) (used : int) (distance : int)
    (remaining : int) = ghost_ (
  if remaining <= 0 then h
  else
    match H.at h (M.location block (used - distance)) with
    | Some (Some value) ->
      copy_heap (H.put h (M.location block used) (Some value))
        block (used + 1) distance (remaining - 1)
    | _ -> h)
[@@decreases remaining]

let[@def] rec (literal_heap @ total) (h : P.heap @ immutable)
    (block : M.t @ immutable) (used : int)
    (source : char iarray @ immutable) (first : int) (remaining : int) =
  ghost_ (
    if remaining <= 0 || first < 0 || first >= Iarray.length source then h
    else
      let value = byte_of_char (S.iarray_get source first) in
      literal_heap (H.put h (M.location block used) (Some value))
        block (used + 1) source (first + 1) (remaining - 1))
[@@decreases remaining]

let rec copy_match :
    (distance : {d : int | 0 < d}) ->
    (remaining : {n : int | 0 <= n}) ->
    (buffer : {b : B.t | distance <= b.used
      && remaining <= M.length b.block - b.used}) @ unique ->
    {after : B.t | after.block === buffer.block
      && after.used = buffer.used + remaining
      && P.own after.permission ===
           copy_heap (P.own buffer.permission) buffer.block buffer.used
             distance remaining} @ unique =
  fun distance remaining buffer ->
    if remaining = 0 then begin
      let { B.block; permission; used } = buffer in
      ghost_ (copy_heap_def (P.own (borrow_ permission)) block used
                distance remaining);
      { B.block; permission; used }
    end else
      let { B.block; permission; used } = buffer in
      let before = ghost_ (P.own (borrow_ permission)) in
      ghost_ (copy_heap_def before block used distance remaining);
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
           literal_heap (P.own buffer.permission) buffer.block buffer.used
             source first remaining} @ unique =
  fun source first remaining buffer ->
    if remaining = 0 then begin
      let { B.block; permission; used } = buffer in
      ghost_ (literal_heap_def (P.own (borrow_ permission)) block used
                source first remaining);
      { B.block; permission; used }
    end else
      let { B.block; permission; used } = buffer in
      let before = ghost_ (P.own (borrow_ permission)) in
      ghost_ (literal_heap_def before block used source first remaining);
      let buffer : B.t = { B.block; permission; used } in
      let value = byte_of_char (S.iarray_get source first) in
      let buffer = B.append buffer value in
      copy_literals source (first + 1) (remaining - 1) buffer
[@@decreases remaining]

type length_result =
  | Length of int * int
  | Length_truncated
  | Length_limit

let[@def] rec (read_extended_length @ total) :
    (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (length : {n : int | 0 <= n && n <= 4194304}) ->
    (fuel : {f : int | 0 <= f}) ->
    length_result =
  fun source cursor length fuel ->
    if fuel = 0 || cursor = Iarray.length source then Length_truncated
    else
      let extension = byte_of_char (S.iarray_get source cursor) in
      if extension > 4194304 - length then Length_limit
      else
        let length = length + extension in
        let cursor = cursor + 1 in
        if extension = 255 then
          read_extended_length source cursor length (fuel - 1)
        else Length (cursor, length)
[@@decreases fuel]

let[@def] (read_length @ total) : (source : char iarray) ->
    (cursor : {i : int | 0 <= i && i <= Iarray.length source}) ->
    (initial : {n : int | 0 <= n && n <= 15}) -> length_result =
  fun source cursor initial ->
  if initial < 15 then Length (cursor, initial)
  else read_extended_length source cursor 15 (Iarray.length source - cursor)

type status = Done | Malformed | Output_limit

type model_result = {
  kind : status;
  count : int;
  state : P.heap @@ ghost;
}

let[@def] rec (decode_model @ total) (source : char iarray @ immutable)
    (input_pos : int) (last_match_start : int) (fuel : int)
    (capacity : int) (block : M.t @ immutable) (used : int)
    (state : P.heap @ immutable) : model_result @ ghost = ghost_ (
  let n = Iarray.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { kind = Malformed; count = used; state }
  else
    let token = byte_of_char (S.iarray_get source input_pos) in
    let after_token = input_pos + 1 in
    match read_length source after_token (high4 token) with
    | Length_truncated -> { kind = Malformed; count = used; state }
    | Length_limit -> { kind = Output_limit; count = used; state }
    | Length (literal_pos, literal_count) ->
      if literal_pos < 0 || literal_pos > n || literal_count < 0
         || literal_count > n - literal_pos then
        { kind = Malformed; count = used; state }
      else if literal_count > capacity - used then
        { kind = Output_limit; count = used; state }
      else
        let after_literals = literal_pos + literal_count in
        if after_literals = n then
          if low15 token <> 0
             || (last_match_start >= 0
                 && (literal_count < 5
                     || last_match_start > used + literal_count - 12))
          then { kind = Malformed; count = used; state }
          else
            { kind = Done; count = used + literal_count;
              state = literal_heap state block used source literal_pos
                        literal_count }
        else if n - after_literals < 2 then
          { kind = Malformed; count = used; state }
        else
          let low = byte_of_char (S.iarray_get source after_literals) in
          let high = byte_of_char (S.iarray_get source (after_literals + 1)) in
          let distance = low + high * 256 in
          if distance <= 0 || distance > used + literal_count then
            { kind = Malformed; count = used; state }
          else
            match read_length source (after_literals + 2)
                    (low15 token) with
            | Length_truncated -> { kind = Malformed; count = used; state }
            | Length_limit -> { kind = Output_limit; count = used; state }
            | Length (next_pos, match_code) ->
              if next_pos < 0 || next_pos > n || match_code < 0
                 || match_code > capacity - used - literal_count - 4 then
                { kind = Output_limit; count = used; state }
              else
                let state = literal_heap state block used source literal_pos
                              literal_count in
                let current = used + literal_count in
                let state = copy_heap state block current distance
                              (match_code + 4) in
                decode_model source next_pos current (fuel - 1) capacity
                  block (current + match_code + 4) state)
[@@decreases fuel]

type outcome : value mod portable contended = {
  status : status;
  buffer : B.t;
}

let rec decode_sequences : (source : char iarray) ->
    (input_pos : int) -> (last_match_start : int) -> (fuel : int) ->
    (buffer : B.t) @ unique ->
    {r : outcome |
      let refine_ model =
        decode_model source input_pos last_match_start fuel
          (M.length buffer.block) buffer.block buffer.used
          (P.own buffer.permission) in
      r.status === model.kind && r.buffer.block === buffer.block
      && r.buffer.used = model.count
      && P.own r.buffer.permission === model.state} @ unique =
  fun source input_pos last_match_start fuel buffer ->
  let { B.block; permission; used } = buffer in
  let before = ghost_ (P.own (borrow_ permission)) in
  ghost_ (decode_model_def source input_pos last_match_start fuel
            (M.length block) block used before);
  let buffer : B.t = { B.block; permission; used } in
  let n = Iarray.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { status = Malformed; buffer }
  else
    let token = byte_of_char (S.iarray_get source input_pos) in
    let after_token = input_pos + 1 in
    match read_length source after_token (high4 token) with
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
            if low15 token <> 0
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
            let low = byte_of_char (S.iarray_get source after_literals) in
            let high =
              byte_of_char (S.iarray_get source (after_literals + 1)) in
            let distance = low + high * 256 in
            if distance <= 0 || distance > used + literal_count then
              { status = Malformed; buffer }
            else
              match read_length source (after_literals + 2)
                      (low15 token) with
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
          decode_model source 0 (-1) (Iarray.length source) capacity
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
