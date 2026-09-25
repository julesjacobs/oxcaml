module S = Vox_sequence

type bytes : immutable_data mod total = int list
type request : immutable_data mod total = {
  request_line : bytes;
  headers : int list list;
  body : bytes;
}
type malformed : immutable_data mod total =
  | Invalid_byte | Invalid_crlf | Invalid_request_line | Invalid_header
  | Invalid_content_length | Conflicting_content_length
  | Unsupported_transfer_encoding | Transfer_encoding_content_length
  | Invalid_host
[@@inductive]
type resource : immutable_data mod total = Message_bytes | Body_bytes
[@@inductive]
type framing : immutable_data mod total =
  | Length of int | Bad of malformed | Too_large of resource
[@@inductive]
type phase : immutable_data mod total =
  | Request_line
  | Headers of bytes * int list list
[@@inductive]
type core : immutable_data mod total =
  | Line of phase * bytes * bool
  | Body of bytes * int list list * int * bytes
  | Complete of request
  | Malformed of malformed
  | Limit of resource
[@@inductive]
type state : immutable_data mod total = private { core : core; budget : int }
type result : immutable_data mod total = { state : state; rest : bytes }

val valid_request_line : bytes -> bool @@ total
val valid_header : bytes -> bool @@ total
val request_parts : request -> bytes * bytes @@ total
val header_field : bytes -> (bytes * bytes) option @@ total
val framing : int list list -> framing @@ total
val serialize : request -> bytes @ immutable total @@ total
val safe_line : bytes -> bool @@ total
val header_lines : int list list -> bool @@ total
val sized : int -> bytes -> bool @@ total
val fits : int -> bytes -> bool @@ total
val well_formed : request -> bool @@ total
val well_formed_def : (request : request) ->
  {u : unit | well_formed request ===
    (valid_request_line request.request_line && safe_line request.request_line
     && header_lines request.headers
     && (match framing request.headers with
         | Length n -> sized n request.body | _ -> false)
     && fits 16384 (serialize request))} @@ total
val initial : unit -> state @ immutable total @@ total
val terminal : core @ immutable total -> bool @@ total
val consumed : state -> state -> int @@ total
val feed : state @ immutable total -> bytes @ immutable total ->
  result @ immutable total @@ total

val chunking_invariance : (state : state) @ immutable ->
  (left : bytes) -> (right : bytes) ->
  {u : unit | feed state (S.append left right) ===
    (let first = feed state left in feed first.state (S.append first.rest
      right))}
  @@ total
val request_separation : (state : state) @ immutable ->
  (prefix : bytes) -> (suffix : bytes) ->
  {u : unit | let first = feed state prefix in
    match first.state.core with
    | Complete _ -> feed state (S.append prefix suffix) ===
        {state = first.state; rest = S.append first.rest suffix}
    | _ -> true} @@ total
val accounting : (state : state) @ immutable -> (input : bytes) @ immutable ->
  {u : unit | if 0 <= state.budget && state.budget <= 16384 then
    let result = feed state input in
    0 <= result.state.budget && result.state.budget <= state.budget
    && S.length input === Bigint.add
      (Bigint.of_int (consumed state result.state)) (S.length result.rest)
    && S.drop (Bigint.of_int (consumed state result.state)) input ===
      result.rest
    else true} @@ total
val suffix_preservation : (state : state) @ immutable -> (input : bytes) @
  immutable ->
  {u : unit | if 0 <= state.budget && state.budget <= 16384 then
    let result = feed state input in
    S.append (S.take (Bigint.of_int (consumed state result.state)) input)
      result.rest === input
    else true} @@ total
val roundtrip : (request : request) -> (suffix : bytes) ->
  {u : unit | if well_formed request then
    let result = feed (initial ()) (S.append (serialize request) suffix) in
    result.state.core === Complete request && result.rest === suffix
    && Bigint.of_int (consumed (initial ()) result.state) === S.length
      (serialize request)
    else true} @@ total
val parse : (input : bytes) @ immutable ->
  {result : result |
      result === feed (initial ()) input &&
    S.length input === Bigint.add
      (Bigint.of_int (consumed (initial ()) result.state)) (S.length
        result.rest)
    && S.drop (Bigint.of_int (consumed (initial ()) result.state)) input ===
      result.rest}
  @@ total
