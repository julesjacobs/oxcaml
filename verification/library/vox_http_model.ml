(** Pure incremental HTTP semantics. Lines, headers and body bytes are in
    wire order. [feed] stops at the first completion, error or exhausted
    message budget and returns the untouched suffix. *)
open Vox_http_spec

module S = Vox_sequence

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

type state : immutable_data mod total = { core : core; budget : int }
type result : immutable_data mod total = { state : state; rest : bytes }

let[@def] finish_line
    (phase : phase @ immutable total) (line : bytes @ immutable total) =
  match phase with
  | Request_line ->
    if valid_request_line line then Line (Headers (line, []), [], false)
    else Malformed Invalid_request_line
  | Headers (request_line, headers) ->
    if nonempty line then
      if valid_header line then
        Line (Headers (request_line, S.append headers [line]), [], false)
      else Malformed Invalid_header
    else
      match framing headers with
      | Bad e -> Malformed e
      | Too_large r -> Limit r
      | Length n ->
        if n = 0 then Complete {request_line; headers; body = []}
        else Body (request_line, headers, n, [])

let[@def] terminal (core : core @ immutable total) =
  match core with Complete _ | Malformed _ | Limit _ -> true | _ -> false

let[@def] step (core : core @ immutable total) (b : int) =
  if not (byte b) then Malformed Invalid_byte else
  match core with
  | Complete _ | Malformed _ | Limit _ -> core
  | Line (phase, line, cr) ->
    if cr then
      if b = 10 then finish_line phase line else Malformed Invalid_crlf
    else if b = 13 then Line (phase, line, true)
    else if b = 10 then Malformed Invalid_crlf
    else Line (phase, S.append line [b], false)
  | Body (request_line, headers, remaining, body) ->
    let body = S.append body [b] in
    if remaining = 1 then Complete {request_line; headers; body}
    else Body (request_line, headers, remaining - 1, body)

let[@def] initial (_unit : unit) =
  {core = Line (Request_line, [], false); budget = 16384}

let[@def] advance (state : state @ immutable total) b =
  if state.budget <= 0 then {state with core = Limit Message_bytes}
  else {core = step state.core b; budget = state.budget - 1}

let[@def] rec feed
    (state : state @ immutable total) (input : bytes @ immutable total) =
  if terminal state.core then {state; rest = input}
  else match input with
  | [] -> {state; rest = []}
  | b :: bs ->
    if state.budget <= 0 then
      {state = {state with core = Limit Message_bytes}; rest = input}
    else feed (advance state b) bs

let[@def] consumed before after = before.budget - after.budget

let[@def] status (core : core) : Vox_http_spec.status =
  match core with
  | Line _ | Body _ -> Incomplete
  | Complete request -> Complete request
  | Malformed error -> Malformed error
  | Limit resource -> Limit resource

let[@def] core_wire core =
  match core with
  | Line (phase, line, cr) ->
    let tail = S.append line (if cr then [13] else []) in
    (match phase with
     | Request_line -> tail
     | Headers (request_line, headers) ->
       S.append request_line (13 :: 10 :: header_prefix headers tail))
  | Body (request_line, headers, _, body) ->
    serialize {request_line; headers; body}
  | Complete request -> serialize request
  | Malformed _ | Limit _ -> []
