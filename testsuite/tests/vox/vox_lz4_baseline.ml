(* Independent raw LZ4 blocks. The input and output limits are deliberately
   below the maximum OCaml integer so all cursor arithmetic stays bounded. *)

let max_block_size = 4 * 1024 * 1024
let min_match = 4
let last_literals = 5
let match_start_margin = 12
let max_offset = 65535

type malformed =
  | Empty_block
  | Truncated_length
  | Truncated_literals
  | Truncated_offset
  | Zero_offset
  | Offset_beyond_output
  | Invalid_terminal_sequence

type decode_error =
  | Malformed of malformed * int
  | Output_limit
  | Invalid_capacity

let emit_extended put length =
  let remaining = ref length in
  while !remaining >= 255 do
    put 255;
    remaining := !remaining - 255
  done;
  put !remaining

let compress source =
  let n = String.length source in
  if n > max_block_size then invalid_arg "Vox_lz4.compress: block too large";
  let bound = n + (n / 255) + 16 in
  let output = Bytes.create bound in
  let written = ref 0 in
  let put byte =
    assert (0 <= byte && byte <= 255 && !written < bound);
    Bytes.set output !written (Char.chr byte);
    incr written
  in
  let put_literals first length =
    for j = first to first + length - 1 do
      put (Char.code source.[j])
    done
  in
  let emit first literal_length offset match_length =
    let match_code = match_length - min_match in
    put ((min 15 literal_length lsl 4) lor min 15 match_code);
    if literal_length >= 15 then emit_extended put (literal_length - 15);
    put_literals first literal_length;
    put (offset land 255);
    put (offset lsr 8);
    if match_code >= 15 then emit_extended put (match_code - 15)
  in
  let hash_at pos =
    let byte k = Int32.of_int (Char.code source.[pos + k]) in
    let word = Int32.logor (byte 0)
        (Int32.logor (Int32.shift_left (byte 1) 8)
           (Int32.logor (Int32.shift_left (byte 2) 16)
              (Int32.shift_left (byte 3) 24))) in
    Int32.to_int (Int32.shift_right_logical
                    (Int32.mul word 0x9e3779b1l) 16)
  in
  let table = Array.make 65536 (-1) in
  let anchor = ref 0 in
  let pos = ref 0 in
  let search_end = n - match_start_margin in
  let match_end = n - last_literals in
  while !pos <= search_end do
    let hash = hash_at !pos in
    let candidate = table.(hash) in
    table.(hash) <- !pos;
    if candidate >= 0 && !pos - candidate <= max_offset
       && source.[candidate] = source.[!pos]
       && source.[candidate + 1] = source.[!pos + 1]
       && source.[candidate + 2] = source.[!pos + 2]
       && source.[candidate + 3] = source.[!pos + 3]
    then begin
      let length = ref min_match in
      while !pos + !length < match_end
            && source.[candidate + !length] = source.[!pos + !length] do
        incr length
      done;
      emit !anchor (!pos - !anchor) (!pos - candidate) !length;
      pos := !pos + !length;
      anchor := !pos
    end else incr pos
  done;
  let literal_length = n - !anchor in
  put (min 15 literal_length lsl 4);
  if literal_length >= 15 then emit_extended put (literal_length - 15);
  put_literals !anchor literal_length;
  Bytes.sub_string output 0 !written

let decompress ?(capacity = max_block_size) source =
  if capacity < 0 || capacity > max_block_size then Error Invalid_capacity
  else
    let n = String.length source in
    if n = 0 then Error (Malformed (Empty_block, 0))
    else
      let output = Bytes.create capacity in
      let input_pos = ref 0 in
      let output_pos = ref 0 in
      let last_match_start = ref (-1) in
      let fail reason = Error (Malformed (reason, !input_pos)) in
      let byte pos = Char.code source.[pos] in
      let read_length initial =
        if initial < 15 then Ok initial
        else begin
          let length = ref 15 in
          let more = ref true in
          while !more && !input_pos < n && !length <= max_block_size do
            let extension = byte !input_pos in
            incr input_pos;
            length := !length + extension;
            more := extension = 255
          done;
          if !more then
            if !length > max_block_size then Error Output_limit
            else fail Truncated_length
          else if !length > max_block_size then Error Output_limit
          else Ok !length
        end
      in
      let rec sequences () =
        if !input_pos >= n then fail Invalid_terminal_sequence
        else begin
          let token = byte !input_pos in
          incr input_pos;
          match read_length (token lsr 4) with
          | Error _ as error -> error
          | Ok literal_length ->
            if literal_length > n - !input_pos then fail Truncated_literals
            else if literal_length > capacity - !output_pos then
              Error Output_limit
            else begin
              let literal_start = !input_pos in
              let after_literals = !output_pos + literal_length in
              input_pos := literal_start + literal_length;
              if !input_pos = n then
                if token land 15 <> 0
                   || (!last_match_start >= 0
                       && (literal_length < last_literals
                           || !last_match_start > after_literals
                              - match_start_margin))
                then fail Invalid_terminal_sequence
                else begin
                  Bytes.blit_string source literal_start output !output_pos
                    literal_length;
                  output_pos := after_literals;
                  Ok (Bytes.sub_string output 0 !output_pos)
                end
              else if n - !input_pos < 2 then fail Truncated_offset
              else begin
                let distance =
                  byte !input_pos lor (byte (!input_pos + 1) lsl 8) in
                input_pos := !input_pos + 2;
                if distance = 0 then fail Zero_offset
                else if distance > after_literals then fail Offset_beyond_output
                else
                  match read_length (token land 15) with
                  | Error _ as error -> error
                  | Ok match_code ->
                    let match_length = match_code + min_match in
                    if match_length > capacity - after_literals then
                      Error Output_limit
                    else begin
                      Bytes.blit_string source literal_start output !output_pos
                        literal_length;
                      output_pos := after_literals;
                      last_match_start := !output_pos;
                      for j = 0 to match_length - 1 do
                        let value =
                          Bytes.get output (!output_pos + j - distance) in
                        Bytes.set output (!output_pos + j) value
                      done;
                      output_pos := !output_pos + match_length;
                      sequences ()
                    end
              end
            end
        end
      in
      sequences ()
