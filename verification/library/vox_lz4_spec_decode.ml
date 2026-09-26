include Vox_lz4_spec_parse

let[@def] rec (copy_heap @ total) (h : Raw_memory.contents Ghost_pref.heap @ immutable)
    (block : Raw_memory.t @ immutable) (used : int) (distance : int)
    (remaining : int) = ghost_ (
  if remaining <= 0 then h
  else
    match Ghost_pref.Heap.at h (Raw_memory.location block (used - distance)) with
    | Some (Some value) ->
      copy_heap (Ghost_pref.Heap.put h (Raw_memory.location block used) (Some value))
        block (used + 1) distance (remaining - 1)
    | _ -> h)
[@@decreases remaining]

let[@def] rec (literal_heap @ total) (h : Raw_memory.contents Ghost_pref.heap @ immutable)
    (block : Raw_memory.t @ immutable) (used : int)
    (source : char iarray @ immutable) (first : int) (remaining : int) =
  ghost_ (
    if remaining <= 0 || first < 0 || first >= Iarray.length source then h
    else
      let value = Vox_lz4_spec_parse.byte_of_char (Vox_sequence.iarray_get source first) in
      literal_heap (Ghost_pref.Heap.put h (Raw_memory.location block used) (Some value))
        block (used + 1) source (first + 1) (remaining - 1))
[@@decreases remaining]

type model_result = {
  kind : status;
  count : int;
  state : Raw_memory.contents Ghost_pref.heap @@ ghost;
}

let[@def] rec (decode_model @ total) (source : char iarray @ immutable)
    (input_pos : int) (last_match_start : int) (fuel : int)
    (capacity : int) (block : Raw_memory.t @ immutable) (used : int)
    (state : Raw_memory.contents Ghost_pref.heap @ immutable) : model_result @ ghost = ghost_ (
  let n = Iarray.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { kind = Malformed; count = used; state }
  else
    let token = Vox_lz4_spec_parse.byte_of_char (Vox_sequence.iarray_get source input_pos) in
    let after_token = input_pos + 1 in
    match Vox_lz4_spec_parse.read_length source after_token (Vox_lz4_spec_parse.high4 token) with
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
          if Vox_lz4_spec_parse.low15 token <> 0
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
          let low = Vox_lz4_spec_parse.byte_of_char (Vox_sequence.iarray_get source after_literals) in
          let high = Vox_lz4_spec_parse.byte_of_char (Vox_sequence.iarray_get source (after_literals + 1)) in
          let distance = low + high * 256 in
          if distance <= 0 || distance > used + literal_count then
            { kind = Malformed; count = used; state }
          else
            match Vox_lz4_spec_parse.read_length source (after_literals + 2)
                    (Vox_lz4_spec_parse.low15 token) with
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
