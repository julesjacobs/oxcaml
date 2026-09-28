module D = Vox_lz4_spec_parse

type byte = D.byte

let[@def] rec (at_distance @ total) (reversed : byte list @ immutable)
    (distance : int) : byte option =
  match reversed with
  | [] -> None
  | byte :: rest ->
    if distance = 1 then Some byte
    else if distance > 1 then at_distance rest (distance - 1)
    else None

let[@def] rec (copy_bytes @ total) (reversed : byte list @ immutable)
    (distance : int) (remaining : int) = ghost_ (
  if remaining <= 0 then reversed
  else match at_distance reversed distance with
    | None -> reversed
    | Some byte -> copy_bytes (byte :: reversed) distance (remaining - 1))
[@@decreases remaining]

let[@def] rec (literal_bytes @ total) (reversed : byte list @ immutable)
    (source : char iarray @ immutable) (first : int) (remaining : int) =
  ghost_ (
    if remaining <= 0 || first < 0 || first >= Iarray.length source then reversed
    else
      let byte = D.byte_of_char (Vox_sequence.iarray_get source first) in
      literal_bytes (byte :: reversed) source (first + 1) (remaining - 1))
[@@decreases remaining]

type model_result = {
  kind : D.status;
  count : int;
  reversed : byte list @@ ghost;
}

let[@def] rec (decode_model @ total) (source : char iarray @ immutable)
    (input_pos : int) (last_match_start : int) (fuel : int)
    (capacity : int) (used : int) (reversed : byte list @ immutable) : model_result @ ghost = ghost_ (
  let n = Iarray.length source in
  if input_pos < 0 || input_pos >= n || fuel <= 0 then
    { kind = D.Malformed; count = used; reversed }
  else
    let token = D.byte_of_char (Vox_sequence.iarray_get source input_pos) in
    let after_token = input_pos + 1 in
    match D.read_length source after_token (D.high4 token) with
    | D.Length_truncated -> { kind = D.Malformed; count = used; reversed }
    | D.Length_limit -> { kind = D.Output_limit; count = used; reversed }
    | D.Length (literal_pos, literal_count) ->
      if literal_pos < 0 || literal_pos > n || literal_count < 0
         || literal_count > n - literal_pos then
        { kind = D.Malformed; count = used; reversed }
      else if literal_count > capacity - used then
        { kind = D.Output_limit; count = used; reversed }
      else
        let after_literals = literal_pos + literal_count in
        if after_literals = n then
          if D.low15 token <> 0
             || (last_match_start >= 0
                 && (literal_count < 5
                     || last_match_start > used + literal_count - 12))
          then { kind = D.Malformed; count = used; reversed }
          else
            { kind = D.Done; count = used + literal_count;
              reversed = literal_bytes reversed source literal_pos
                        literal_count }
        else if n - after_literals < 2 then
          { kind = D.Malformed; count = used; reversed }
        else
          let low = D.byte_of_char (Vox_sequence.iarray_get source after_literals) in
          let high = D.byte_of_char (Vox_sequence.iarray_get source (after_literals + 1)) in
          let distance = low + high * 256 in
          if distance <= 0 || distance > used + literal_count then
            { kind = D.Malformed; count = used; reversed }
          else
            match D.read_length source (after_literals + 2)
                    (D.low15 token) with
            | D.Length_truncated -> { kind = D.Malformed; count = used; reversed }
            | D.Length_limit -> { kind = D.Output_limit; count = used; reversed }
            | D.Length (next_pos, match_code) ->
              if next_pos < 0 || next_pos > n || match_code < 0
                 || match_code > capacity - used - literal_count - 4 then
                { kind = D.Output_limit; count = used; reversed }
              else
                let reversed = literal_bytes reversed source literal_pos
                              literal_count in
                let current = used + literal_count in
                let reversed = copy_bytes reversed distance
                              (match_code + 4) in
                decode_model source next_pos current (fuel - 1) capacity
                  (current + match_code + 4) reversed)
[@@decreases fuel]

let[@def] rec (matches_bytes @ total) (values : char iarray @ immutable)
    (count : int) (reversed : byte list @ immutable) = ghost_ (
  match reversed with
  | [] -> count = 0
  | byte :: rest ->
    count > 0
    && (match Vox_iarray.at values (count - 1) with
        | None -> false
        | Some c -> D.byte_of_char c = byte)
    && matches_bytes values (count - 1) rest)
