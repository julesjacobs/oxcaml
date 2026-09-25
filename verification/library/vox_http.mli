open Vox_http_spec

type state : immutable_data mod total
type result : immutable_data mod total = { state : state; rest : bytes }

val status : state -> status @@ total
val total_consumed : state -> int @@ total
val consumed : state -> state -> int @@ total
val consumed_def : (before : state) -> (after : state) ->
  {u : unit | consumed before after ===
    total_consumed after - total_consumed before} @@ total
val processed : state -> bytes @ ghost @@ total
val state_sound : (state : state) ->
  {u : unit | 0 <= total_consumed state && total_consumed state <= 16384
    && (match status state with
        | Incomplete -> Vox_sequence.length (processed state) ===
            Bigint.of_int (total_consumed state)
        | Complete request -> well_formed request
          && serialize request === processed state
          && Vox_sequence.length (processed state) ===
            Bigint.of_int (total_consumed state)
        | Malformed _ | Limit _ -> processed state === [])} @@ total

val initial : unit ->
  {state : state | status state === Incomplete
    && total_consumed state = 0 && processed state === []} @@ total
val feed : (state : state) -> (input : bytes) ->
  {result : result |
    total_consumed state <= total_consumed result.state
    && total_consumed result.state <= 16384
    && Vox_sequence.length input === Bigint.add
      (Bigint.of_int (consumed state result.state))
      (Vox_sequence.length result.rest)
    && Vox_sequence.drop (Bigint.of_int (consumed state result.state)) input
      === result.rest
    && Vox_sequence.append
      (Vox_sequence.take (Bigint.of_int (consumed state result.state)) input)
      result.rest === input
    && (match status result.state with
        | Incomplete -> result.rest === [] | _ -> true)
    && (match status result.state with
        | Incomplete | Complete _ -> processed result.state ===
            Vox_sequence.append (processed state)
              (Vox_sequence.take
                (Bigint.of_int (consumed state result.state)) input)
        | Malformed _ | Limit _ -> true)
    && (match status result.state with
        | Complete request -> well_formed request
          && serialize request === processed result.state
        | _ -> true)} @@ total

val parse : (input : bytes) ->
  {result : result |
    result === feed (initial ()) input
    && Vox_sequence.length input === Bigint.add
      (Bigint.of_int (consumed (initial ()) result.state))
      (Vox_sequence.length result.rest)
    && Vox_sequence.drop
      (Bigint.of_int (consumed (initial ()) result.state)) input === result.rest
    && (match status result.state with
        | Complete request -> well_formed request
          && Vox_sequence.take
            (Bigint.of_int (consumed (initial ()) result.state)) input ===
            serialize request
          && input === Vox_sequence.append (serialize request) result.rest
        | _ -> true)} @@ total

val chunking_invariance : (state : state) ->
  (left : bytes) -> (right : bytes) ->
  {u : unit | feed state (Vox_sequence.append left right) ===
    (let first = feed state left in
     feed first.state (Vox_sequence.append first.rest right))} @@ total
val request_separation : (state : state) ->
  (prefix : bytes) -> (suffix : bytes) ->
  {u : unit | let first = feed state prefix in
    match status first.state with
    | Complete _ -> feed state (Vox_sequence.append prefix suffix) ===
        {state = first.state; rest = Vox_sequence.append first.rest suffix}
    | _ -> true} @@ total
val terminal_preservation : (state : state) -> (input : bytes) ->
  {u : unit | if is_terminal (status state) then
    feed state input === {state; rest = input} else true} @@ total
val roundtrip : (request : request) -> (suffix : bytes) ->
  {u : unit | if well_formed request then
    let result = feed (initial ())
      (Vox_sequence.append (serialize request) suffix) in
    status result.state === Complete request && result.rest === suffix
    && Bigint.of_int (consumed (initial ()) result.state) ===
      Vox_sequence.length (serialize request)
    else true} @@ total

val framing_rejection : (headers : int list list) ->
  {u : unit | if has_transfer_encoding headers then
    framing headers ===
      (if has_content_length headers then Bad Transfer_encoding_content_length
       else Bad Unsupported_transfer_encoding)
    else true} @@ total
val body_agreement : (request : request) ->
  {u : unit | if well_formed request then
    not (has_transfer_encoding request.headers)
    && (match framing request.headers with
        | Length n -> 0 <= n && n <= 8192
          && Vox_sequence.length request.body === Bigint.of_int n
          && content_lengths_match request.headers n
        | _ -> false)
    else true} @@ total
val no_content_length_body : (request : request) ->
  {u : unit | if well_formed request
      && not (has_content_length request.headers) then
    request.body === [] else true} @@ total
val header_outcome : (line : bytes) -> (headers : int list list) ->
  {u : unit | let request = {request_line = line; headers; body = []} in
    if valid_request_line line && safe_line line && header_lines headers
      && fits 16384 (serialize request) then
    let result = feed (initial ()) (serialize request) in
    result.rest === [] && status result.state ===
      (match framing headers with
       | Bad e -> Malformed e
       | Too_large r -> Limit r
       | Length n -> if n = 0 then Complete request else Incomplete)
    else true} @@ total
