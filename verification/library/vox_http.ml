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
type state : immutable_data mod total = { core : core; budget : int }
type result : immutable_data mod total = { state : state; rest : bytes }

let[@def] rec equal_bytes (left : bytes) (right : bytes) =
  match left, right with
  | [], [] -> true
  | x :: xs, y :: ys -> x = y && equal_bytes xs ys
  | _ -> false
let[@def] byte b = 0 <= b && b <= 255
let[@def] token b =
  (65 <= b && b <= 90) || (97 <= b && b <= 122)
  || (48 <= b && b <= 57) || b = 33 || b = 35 || b = 36 || b = 37
  || b = 38 || b = 39 || b = 42 || b = 43 || b = 45 || b = 46
  || b = 94 || b = 95 || b = 96 || b = 124 || b = 126
let[@def] visible b = 33 <= b && b <= 126
let[@def] value_byte b = b = 9 || (32 <= b && b <= 126) || (128 <= b && b <=
  255)
let[@def] lower b = if 65 <= b && b <= 90 then b + 32 else b
let[@def] rec lower_all xs = match xs with [] -> [] | b :: bs -> lower b ::
  lower_all bs
let[@def] rec all_token xs = match xs with [] -> true | b :: bs -> token b &&
  all_token bs
let[@def] rec all_visible xs = match xs with [] -> true | b :: bs -> visible b
  && all_visible bs
let[@def] rec all_value xs = match xs with [] -> true | b :: bs -> value_byte b
  && all_value bs
let[@def] nonempty (xs : bytes) = match xs with [] -> false | _ :: _ -> true
let[@def] rec split (delimiter : int) (xs : bytes) =
  match xs with
  | [] -> ([], [])
  | b :: bs -> if b = delimiter then ([], bs)
    else let (before, after) = split delimiter bs in (b :: before, after)
let[@def] rec trim_left xs =
  match xs with b :: bs when b = 32 || b = 9 -> trim_left bs | _ -> xs
let[@def] rec trim_right xs =
  match xs with
  | [] -> []
  | b :: bs -> let rest = trim_right bs in
    if (b = 32 || b = 9) && not (nonempty rest) then [] else b :: rest
let[@def] trim xs = trim_right (trim_left xs)
let[@def] valid_request_line line =
  let (meth, rest) = split 32 line in
  let (target, version) = split 32 rest in
  nonempty meth && all_token meth && nonempty target && all_visible target
  && equal_bytes version [72; 84; 84; 80; 47; 49; 46; 49]
let[@def] header line =
  let (name, value) = split 58 line in
  if nonempty name && all_token name && all_value value
  then Some (lower_all name, trim value) else None
let[@def] rec has_colon line =
  match line with [] -> false | b :: bs -> b = 58 || has_colon bs
let[@def] valid_header line = has_colon line && (match header line with None ->
  false | Some _ -> true)
let[@def] request_parts request =
  let (meth, rest) = split 32 request.request_line in
  let (target, _) = split 32 rest in
  (meth, target)
let[@def] header_field line =
  if valid_header line then header line else None
let[@def] rec decimal value digits =
  match digits with
  | [] -> Length value
  | b :: bs ->
    if b < 48 || b > 57 then Bad Invalid_content_length
    else if value > 819 || (value = 819 && b > 50) then Too_large Body_bytes
    else decimal (value * 10 + b - 48) bs
let[@def] content_length digits =
  if nonempty digits then decimal 0 digits else Bad Invalid_content_length
let[@def] is_cl name = equal_bytes name
  [99;111;110;116;101;110;116;45;108;101;110;103;116;104]
let[@def] is_host name = equal_bytes name [104;111;115;116]
let[@def] rec has_name (which : bytes) (headers : int list list) =
  match headers with
  | [] -> false
  | line :: rest -> let (name, _) = split 58 line in
    equal_bytes (lower_all name) which || has_name which rest
let[@def] rec frame_fields headers previous hosts =
  match headers with
  | [] -> if hosts <> 1 then Bad Invalid_host
    else (match previous with None -> Length 0 | Some n -> Length n)
  | line :: rest ->
    if not (valid_header line) then Bad Invalid_header
    else match header line with
    | None -> Bad Invalid_header
    | Some (name, value) ->
      if is_host name then
        if hosts <> 0 || not (nonempty value) then Bad Invalid_host
        else frame_fields rest previous 1
      else if is_cl name then
        (match content_length value with
        | Bad e -> Bad e | Too_large r -> Too_large r
        | Length n -> match previous with
          | Some old when old <> n -> Bad Conflicting_content_length
          | _ -> frame_fields rest (Some n) hosts)
      else frame_fields rest previous hosts
let[@def] framing headers =
  if has_name [116;114;97;110;115;102;101;114;45;101;110;99;111;100;105;110;103]
    headers then
    if has_name [99;111;110;116;101;110;116;45;108;101;110;103;116;104] headers
    then Bad Transfer_encoding_content_length else Bad
      Unsupported_transfer_encoding
  else frame_fields headers None 0
let[@def] finish_line (phase : phase @ immutable total) (line : bytes @
  immutable total) =
  match phase with
  | Request_line -> if valid_request_line line then Line (Headers (line, []),
    [], false)
    else Malformed Invalid_request_line
  | Headers (request_line, headers) ->
    if nonempty line then
      if valid_header line then Line (Headers (request_line, S.append headers
        [line]), [], false)
      else Malformed Invalid_header
    else match framing headers with
      | Bad e -> Malformed e | Too_large r -> Limit r
      | Length n -> if n = 0 then Complete {request_line; headers; body = []}
        else Body (request_line, headers, n, [])
let[@def] terminal (core : core @ immutable total) =
  match core with Complete _ | Malformed _ | Limit _ -> true | _ -> false
let[@def] step (core : core @ immutable total) (b : int) =
  if not (byte b) then Malformed Invalid_byte else
  match core with
  | Complete _ | Malformed _ | Limit _ -> core
  | Line (phase, line, cr) ->
    if cr then if b = 10 then finish_line phase line else Malformed Invalid_crlf
    else if b = 13 then Line (phase, line, true)
    else if b = 10 then Malformed Invalid_crlf
    else Line (phase, S.append line [b], false)
  | Body (request_line, headers, remaining, body) ->
    let body = S.append body [b] in
    if remaining = 1 then Complete {request_line; headers; body}
    else Body (request_line, headers, remaining - 1, body)
let[@def] initial (_unit : unit) = {core = Line (Request_line, [], false);
  budget = 16384}
let[@def] advance (state : state @ immutable total) b =
  if state.budget <= 0 then {state with core = Limit Message_bytes}
  else {core = step state.core b; budget = state.budget - 1}
let[@def] rec feed (state : state @ immutable total) (input : bytes @ immutable
  total) =
  if terminal state.core then {state; rest = input}
  else match input with
  | [] -> {state; rest = []}
  | b :: bs -> if state.budget <= 0 then
      {state = {state with core = Limit Message_bytes}; rest = input}
    else feed (advance state b) bs
let[@def] consumed before after = before.budget - after.budget
let[@def] rec wire_headers headers tail =
  match headers with [] -> 13 :: 10 :: tail
  | line :: rest -> S.append line (13 :: 10 :: wire_headers rest tail)
let[@def] serialize request =
  S.append request.request_line (13 :: 10 :: wire_headers request.headers
    request.body)
let[@def] rec safe_line line =
  match line with [] -> true | b :: bs -> byte b && b <> 13 && b <> 10 &&
    safe_line bs
let[@def] rec header_lines headers =
  match headers with [] -> true
  | line :: rest -> nonempty line && safe_line line && valid_header line &&
    header_lines rest
let[@def] rec sized n bytes =
  match bytes with [] -> n = 0
  | b :: rest -> n > 0 && byte b && sized (n - 1) rest
let[@def] rec fits budget bytes =
  match bytes with [] -> budget >= 0
  | _ :: rest -> budget > 0 && fits (budget - 1) rest
let[@def] well_formed request =
  valid_request_line request.request_line && safe_line request.request_line
  && header_lines request.headers
  && (match framing request.headers with Length n -> sized n request.body | _ ->
    false)
  && fits 16384 (serialize request)

let[@def] rec drain (core : core @ immutable total) (input : bytes @ immutable
  total) =
  if terminal core then (core, input)
  else match input with [] -> (core, []) | b :: bs -> drain (step core b) bs

let (drain_empty @ total) (core : core @ immutable total) :
    {u : unit | drain core [] === (core, [])} =
  drain_def core []; let u = () in refine_ u

let rec (drain_append @ total) : (core : core) @ immutable total -> (left :
  bytes) -> (right : bytes) ->
    {u : unit | drain core (S.append left right) ===
      (let (next, rest) = drain core left in
       if terminal next then (next, S.append rest right) else drain next right)}
         =
  fun core left right ->
  S.append_def left right;
  drain_def core left;
  drain_def core (S.append left right);
  if terminal core then (let u = () in refine_ u)
  else match left with
  | [] -> let u = () in refine_ u
  | b :: bs ->
    let joined = S.append left right in
    drain_def core joined;
    drain_append (step core b) bs right;
    let u = () in refine_ u

let rec (drain_line @ total) : (phase : phase) @ immutable total -> (prefix :
  bytes) -> (line : bytes) @ immutable total ->
    {u : unit | if safe_line line then
      drain (Line (phase, prefix, false)) (S.append line [13;10]) ===
      (finish_line phase (S.append prefix line), []) else true} =
  fun phase prefix line ->
  safe_line_def line;
  S.append_def line [13;10];
  let start = Line (phase, prefix, false) in
  terminal_def start;
  drain_def start (S.append line [13;10]);
  match line with
  | [] ->
    S.append_nil prefix;
    step_def start 13; byte_def 13;
    let cr = Line (phase, prefix, true) in
    terminal_def cr; drain_def cr [10]; step_def cr 10; byte_def 10;
    drain_empty (finish_line phase prefix);
    let u = () in refine_ u
  | b :: bs ->
    if safe_line line then (
      step_def start b;
      let next = S.append prefix [b] in
      drain_line phase next bs;
      S.append_associative prefix [b] bs;
      S.append_def [b] bs;
      S.append_def [] bs;
      let u = () in refine_ u)
    else let u = () in refine_ u

let rec (drain_headers @ total) : (request_line : bytes) -> (prefix : int list
  list) ->
    (headers : int list list) -> (body : bytes) ->
    {u : unit | if header_lines headers then
      drain (Line (Headers (request_line, prefix), [], false)) (wire_headers
        headers body) ===
      drain (finish_line (Headers (request_line, S.append prefix headers)) [])
        body
      else true} =
  fun request_line prefix headers body ->
  header_lines_def headers;
  wire_headers_def headers body;
  let phase = Headers (request_line, prefix) in
  let start = Line (phase, [], false) in
  match headers with
  | [] ->
    S.append_nil prefix;
    terminal_def start; drain_def start (13 :: 10 :: body);
    step_def start 13; byte_def 13;
    let cr = Line (phase, [], true) in
    terminal_def cr; drain_def cr (10 :: body); step_def cr 10; byte_def 10;
    let u = () in refine_ u
  | line :: rest ->
    if header_lines headers then (
      let segment = S.append line [13;10] in
      let tail = wire_headers rest body in
      drain_line phase [] line;
      S.append_def [] line;
      drain_append start segment tail;
      S.append_associative line [13;10] tail;
      S.append_def [13;10] tail; S.append_def [10] tail; S.append_def [] tail;
      finish_line_def phase line;
      let next_prefix = S.append prefix [line] in
      let next = Line (Headers (request_line, next_prefix), [], false) in
      terminal_def next; S.append_def [] tail;
      drain_headers request_line next_prefix rest body;
      S.append_associative prefix [line] rest;
      S.append_def [line] rest; S.append_def [] rest;
      let u = () in refine_ u)
    else let u = () in refine_ u

let rec (drain_body @ total) : (request_line : bytes) -> (headers : int list
  list) ->
    (n : int) -> (prefix : bytes) -> (body : bytes) ->
    {u : unit | if n > 0 && sized n body then
      drain (Body (request_line, headers, n, prefix)) body ===
      (Complete {request_line; headers; body = S.append prefix body}, []) else
        true} =
  fun request_line headers n prefix body ->
  sized_def n body;
  match body with
  | [] -> let u = () in refine_ u
  | b :: bs ->
    if n > 0 && sized n body then (
      let start = Body (request_line, headers, n, prefix) in
      terminal_def start; drain_def start body; step_def start b;
      let next_prefix = S.append prefix [b] in
      if n = 1 then (
        sized_def (n - 1) bs;
        let done_ = Complete {request_line; headers; body = next_prefix} in
        terminal_def done_; drain_def done_ bs;
        S.append_def [b] bs; S.append_def [] bs;
        S.append_associative prefix [b] bs;
        S.append_nil next_prefix;
        let u = () in refine_ u)
      else (
        drain_body request_line headers (n - 1) next_prefix bs;
        S.append_associative prefix [b] bs;
        S.append_def [b] bs; S.append_def [] bs;
        let u = () in refine_ u))
    else let u = () in refine_ u

let (serializer_core_roundtrip @ total) (request : request) :
    {u : unit | if well_formed request then
      drain (Line (Request_line, [], false)) (serialize request) === (Complete
        request, [])
      else true} =
  well_formed_def request;
  if well_formed request then (
    serialize_def request;
    let line = request.request_line in
    let segment = S.append line [13;10] in
    let tail = wire_headers request.headers request.body in
    let start = Line (Request_line, [], false) in
    drain_line Request_line [] line; S.append_def [] line;
    drain_append start segment tail;
    S.append_associative line [13;10] tail;
    S.append_def [13;10] tail; S.append_def [10] tail; S.append_def [] tail;
    finish_line_def Request_line line;
    let headers = Line (Headers (line, []), [], false) in
    terminal_def headers;
    drain_headers line [] request.headers request.body;
    S.append_def [] request.headers;
    finish_line_def (Headers (line, request.headers)) [];
    nonempty_def [];
    match framing request.headers with
    | Bad _ | Too_large _ -> let u = () in refine_ u
    | Length n ->
      sized_def n request.body;
      if n = 0 then (
        drain_empty (Complete {request_line = line; headers = request.headers;
          body = []});
        let u = () in refine_ u)
      else (
        drain_body line request.headers n [] request.body;
        S.append_def [] request.body;
        let u = () in refine_ u))
  else let u = () in refine_ u

let rec (chunking_invariance @ total) : (state : state) @ immutable total ->
  (left : bytes) -> (right : bytes) ->
    {u : unit | feed state (S.append left right) ===
      (let first = feed state left in feed first.state (S.append first.rest
        right))} =
  fun state left right ->
  S.append_def left right;
  feed_def state left;
  feed_def state (S.append left right);
  if terminal state.core then (
    feed_def state (S.append left right);
    let u = () in refine_ u)
  else match left with
  | [] -> S.append_def [] right; let u = () in refine_ u
  | b :: bs ->
    if state.budget <= 0 then (
      let stopped = {state with core = Limit Message_bytes} in
      terminal_def stopped.core;
      feed_def stopped (S.append left right);
      let u = () in refine_ u)
    else (
      chunking_invariance (advance state b) bs right;
      let u = () in refine_ u)

let (request_separation @ total) (state : state @ immutable total)
    (prefix : bytes) (suffix : bytes) :
    {u : unit | let first = feed state prefix in
      match first.state.core with
      | Complete _ -> feed state (S.append prefix suffix) ===
          {state = first.state; rest = S.append first.rest suffix}
      | _ -> true} =
  chunking_invariance state prefix suffix;
  let first = feed state prefix in
  match first.state.core with
  | Complete _ ->
    terminal_def first.state.core;
    feed_def first.state (S.append first.rest suffix);
    let u = () in refine_ u
  | _ -> let u = () in refine_ u

let rec (feed_matches_drain @ total) : (state : state) @ immutable total ->
  (input : bytes) @ immutable total ->
    {u : unit | if fits state.budget input then
      let result = feed state input in
      (result.state.core, result.rest) === drain state.core input else true} =
  fun state input ->
  fits_def state.budget input;
  feed_def state input; drain_def state.core input;
  if terminal state.core then let u = () in refine_ u
  else match input with
  | [] -> let u = () in refine_ u
  | b :: bs ->
    if fits state.budget input then (
      advance_def state b;
      feed_matches_drain (advance state b) bs;
      let u = () in refine_ u)
    else let u = () in refine_ u

let rec (accounting @ total) : (state : state) @ immutable total -> (input :
  bytes) @ immutable total ->
    {u : unit | if 0 <= state.budget && state.budget <= 16384 then
      let result = feed state input in
      0 <= result.state.budget && result.state.budget <= state.budget
      && S.length input === Bigint.add
        (Bigint.of_int (consumed state result.state)) (S.length result.rest)
      && S.drop (Bigint.of_int (consumed state result.state)) input ===
        result.rest
      else true} =
  fun state input ->
  feed_def state input;
  if 0 <= state.budget && state.budget <= 16384 then (
    if terminal state.core then (
      consumed_def state state;
      S.drop_def 0Z input;
      let u = () in refine_ u)
    else match input with
    | [] ->
      consumed_def state state;
      S.length_def input; S.drop_def 0Z input;
      let u = () in refine_ u
    | b :: bs ->
      if state.budget <= 0 then (
        let stopped = {state with core = Limit Message_bytes} in
        consumed_def state stopped;
        S.drop_def 0Z input;
        let u = () in refine_ u)
      else (
        let next = advance state b in
        advance_def state b;
        accounting next bs;
        let result = feed next bs in
        consumed_def next result.state; consumed_def state result.state;
        S.length_def input;
        S.drop_def (Bigint.of_int (consumed state result.state)) input;
        let u = () in refine_ u))
  else let u = () in refine_ u

let (suffix_preservation @ total) (state : state @ immutable total) (input :
  bytes @ immutable total) :
    {u : unit | if 0 <= state.budget && state.budget <= 16384 then
      let result = feed state input in
      S.append (S.take (Bigint.of_int (consumed state result.state)) input)
        result.rest === input
      else true} =
  accounting state input;
  let result = feed state input in
  consumed_def state result.state;
  S.cut input (Bigint.of_int (consumed state result.state));
  let u = () in refine_ u

let (roundtrip @ total) (request : request) (suffix : bytes) :
    {u : unit | if well_formed request then
      let result = feed (initial ()) (S.append (serialize request) suffix) in
      result.state.core === Complete request && result.rest === suffix
      && Bigint.of_int (consumed (initial ()) result.state) === S.length
        (serialize request)
      else true} =
  well_formed_def request;
  if well_formed request then (
    let start = initial () in initial_def ();
    let wire = serialize request in
    serializer_core_roundtrip request;
    feed_matches_drain start wire;
    accounting start wire;
    let first = feed start wire in
    S.length_def ([] : bytes);
    chunking_invariance start wire suffix;
    S.append_def [] suffix;
    terminal_def first.state.core;
    feed_def first.state suffix;
    let u = () in refine_ u)
  else let u = () in refine_ u

let (parse @ total) (input : bytes @ immutable total) :
    {result : result |
      result === feed (initial ()) input &&
      S.length input === Bigint.add
        (Bigint.of_int (consumed (initial ()) result.state)) (S.length
          result.rest)
      && S.drop (Bigint.of_int (consumed (initial ()) result.state)) input ===
        result.rest} =
  let start = initial () in
  let result = feed start input in
  ghost_ (initial_def (); accounting start input);
  refine_ result
