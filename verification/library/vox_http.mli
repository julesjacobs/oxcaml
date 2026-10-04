(** An incremental HTTP/1.1 request parser, verified against the request
    grammar of [Vox_http_spec].

    A parser state is abstract. [feed state chunk] consumes a prefix of
    [chunk] and returns the next state and the unconsumed rest; [status]
    reports [Incomplete], [Complete request], [Malformed reason] or
    [Limit resource]. Input is [bytes], a list of integers, one per byte.
    The laws below hold for every state and every input: a completed request
    is [well_formed] and is exactly the bytes consumed since [initial ()];
    every [well_formed] request followed by any suffix completes; and the way
    the input is split into chunks does not matter.

    Laws return a refined [unit] [@ ghost] and are stated as
    [if premise then conclusion else true]. [===] is logical equality at any
    type. [S] is [Vox_sequence]; [S.length] is a [Bigint.t], hence the
    [Bigint.of_int] conversions. *)

open Vox_http_spec

module S := Vox_sequence
module Model = Vox_http_model

type state : logical_data

(** The state after a call and the part of the input it did not consume. *)
type result : immutable_data mod total = { state : state; rest : bytes }

(** {2 Observations} *)

(** Forward-order semantic state; storage and simulation invariants are private. *)
val model : state -> Model.state @ ghost @@ total
val status : state -> status @@ total

(** Bytes spent on the current request, at most 16,384. *)
val total_consumed : state -> int @@ total
val consumed : state -> state -> int @@ total

(** Consumed bytes, or [[]] after an error. *)
val processed : state -> bytes @ ghost @@ total

(** {2 Parsing} *)

val initial : unit ->
  {state : state | status state === Incomplete
    && total_consumed state = 0 && processed state === []
    && model state === Model.initial ()} @@ total

(** Feed one chunk according to the pure incremental model. *)
val feed : (state : state) -> (input : bytes) ->
  {result : result | let answer = Model.feed (model state) input in
    model result.state === answer.state && result.rest === answer.rest}
  @@ total

(** Parses a request from the start of [input]: [feed (initial ()) input],
    with the facts about a completed request stated directly. *)
val parse : (input : bytes) ->
  {result : result |
    result === feed (initial ()) input
    && (match status result.state with
        | Complete request -> well_formed request
          && S.take (Bigint.of_int (consumed (initial ()) result.state)) input
            === serialize request
          && input === S.append (serialize request) result.rest
        | _ -> true)} @@ total

(** {2 Derived observation laws} *)

val status_equation : (state : state) ->
  {u : unit | status state === Model.status (model state).core} @ ghost @@ total

val total_consumed_equation : (state : state) ->
  {u : unit | total_consumed state = 16384 - (model state).budget} @ ghost
  @@ total

val consumed_def : (before : state) -> (after : state) ->
  {u : unit | consumed before after ===
    total_consumed after - total_consumed before} @@ total

val processed_equation : (state : state) ->
  {u : unit | processed state === Model.core_wire (model state).core} @ ghost
  @@ total

(** What the observations mean in every state. *)
val state_sound : (state : state) ->
  {u : unit | 0 <= total_consumed state && total_consumed state <= 16384
    && (match status state with
        | Incomplete ->
          S.length (processed state) === Bigint.of_int (total_consumed state)
        | Complete request -> well_formed request
          && serialize request === processed state
          && S.length (processed state) === Bigint.of_int (total_consumed state)
        | Malformed _ | Limit _ -> processed state === [])} @ ghost @@ total

(** {2 Laws of [feed]} *)

(** Feeding [left @ right] is feeding [left], then the rest of [left]
    followed by [right]. *)
val chunking_invariance : (state : state) ->
  (left : bytes) -> (right : bytes) ->
  {u : unit | feed state (S.append left right) ===
    (let first = feed state left in
     feed first.state (S.append first.rest right))} @ ghost @@ total

(** Bytes after a completed request are returned untouched. *)
val request_separation : (state : state) ->
  (prefix : bytes) -> (suffix : bytes) ->
  {u : unit | let first = feed state prefix in
    match status first.state with
    | Complete _ -> feed state (S.append prefix suffix) ===
        {state = first.state; rest = S.append first.rest suffix}
    | _ -> true} @ ghost @@ total

(** A terminal state consumes nothing. *)
val terminal_preservation : (state : state) -> (input : bytes) ->
  {u : unit | if is_terminal (status state) then
    feed state input === {state; rest = input} else true} @ ghost @@ total

(** Completeness: every [well_formed] request is parsed from its
    serialization, whatever follows it. *)
val roundtrip : (request : request) -> (suffix : bytes) ->
  {u : unit | if well_formed request then
    let result = feed (initial ()) (S.append (serialize request) suffix) in
    status result.state === Complete request && result.rest === suffix
    && Bigint.of_int (consumed (initial ()) result.state) ===
      S.length (serialize request)
    else true} @ ghost @@ total

(** {2 Framing} *)

(** Any Transfer-Encoding field is an error. *)
val framing_rejection : (headers : int list list) ->
  {u : unit | if has_transfer_encoding headers then
    framing headers ===
      (if has_content_length headers then Bad Transfer_encoding_content_length
       else Bad Unsupported_transfer_encoding)
    else true} @ ghost @@ total

(** A [well_formed] request has no Transfer-Encoding field, and its body has
    the length that [framing] gives, at most 8,192 bytes, which every
    Content-Length field states. *)
val body_agreement : (request : request) ->
  {u : unit | if well_formed request then
    not (has_transfer_encoding request.headers)
    && (match framing request.headers with
        | Length n -> 0 <= n && n <= 8192
          && S.length request.body === Bigint.of_int n
          && content_lengths_match request.headers n
        | _ -> false)
    else true} @ ghost @@ total

(** Without Content-Length the body is empty. *)
val no_content_length_body : (request : request) ->
  {u : unit | if well_formed request
      && not (has_content_length request.headers) then
    request.body === [] else true} @ ghost @@ total

(** A valid request line and valid header lines within the budget give the
    outcome that [framing] prescribes: an error, or a request that is
    complete or waiting for its body. *)
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
    else true} @ ghost @@ total

(** {2 Rejection} *)

(** A request line with no CR or LF that is not [valid_request_line] is
    rejected exactly when its CRLF is consumed. *)
val request_line_rejection : (line : bytes) -> (suffix : bytes) ->
  {u : unit | if safe_line line && not (valid_request_line line)
      && fits 16384 (S.append line [13; 10]) then
    let result =
      feed (initial ()) (S.append line (13 :: 10 :: suffix)) in
    status result.state === Malformed Invalid_request_line
    && result.rest === suffix
    else true} @ ghost @@ total

(** After a valid request line and valid header lines, the first header line
    that is not [valid_header] is rejected exactly when its CRLF is
    consumed. *)
val header_rejection : (request_line : bytes) -> (headers : int list list) ->
  (line : bytes) -> (suffix : bytes) ->
  {u : unit | if valid_request_line request_line && safe_line request_line
      && header_lines headers && nonempty line && safe_line line
      && not (valid_header line)
      && fits 16384 (S.append request_line (13 :: 10 ::
        header_prefix headers (S.append line [13; 10]))) then
    let result = feed (initial ()) (S.append request_line
      (13 :: 10 :: header_prefix headers
        (S.append line (13 :: 10 :: suffix)))) in
    status result.state === Malformed Invalid_header
    && result.rest === suffix
    else true} @ ghost @@ total

(** A value outside 0..255 is rejected as soon as it is fed. *)
val invalid_byte_rejection : (state : state) -> (b : int) -> (rest : bytes) ->
  {u : unit | if status state === Incomplete && total_consumed state < 16384
      && not (byte b) then
    let result = feed state (b :: rest) in
    status result.state === Malformed Invalid_byte && result.rest === rest
    else true} @ ghost @@ total
