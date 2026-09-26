open Vox_http_spec

module Internal = struct
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
let[@def] rec drain (core : core @ immutable total) (input : bytes @ immutable
  total) =
  if terminal core then (core, input)
  else match input with [] -> (core, []) | b :: bs -> drain (step core b) bs

let (drain_empty @ total) (core : core @ immutable total) :
    {u : unit | drain core [] === (core, [])} =
  drain_def core []; ()

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
  if terminal core then (())
  else match left with
  | [] -> ()
  | b :: bs ->
    let joined = S.append left right in
    drain_def core joined;
    drain_append (step core b) bs right;
    ()

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
    ()
  | b :: bs ->
    if safe_line line then (
      step_def start b;
      let next = S.append prefix [b] in
      drain_line phase next bs;
      S.append_associative prefix [b] bs;
      S.append_def [b] bs;
      S.append_def [] bs;
      ())
    else ()

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
    ()
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
      ())
    else ()

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
  | [] -> ()
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
        ())
      else (
        drain_body request_line headers (n - 1) next_prefix bs;
        S.append_associative prefix [b] bs;
        S.append_def [b] bs; S.append_def [] bs;
        ()))
    else ()

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
    | Bad _ | Too_large _ -> ()
    | Length n ->
      sized_def n request.body;
      if n = 0 then (
        drain_empty (Complete {request_line = line; headers = request.headers;
          body = []});
        ())
      else (
        drain_body line request.headers n [] request.body;
        S.append_def [] request.body;
        ()))
  else ()

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
    ())
  else match left with
  | [] -> S.append_def [] right; ()
  | b :: bs ->
    if state.budget <= 0 then (
      let stopped = {state with core = Limit Message_bytes} in
      terminal_def stopped.core;
      feed_def stopped (S.append left right);
      ())
    else (
      chunking_invariance (advance state b) bs right;
      ())

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
    ()
  | _ -> ()

let rec (feed_matches_drain @ total) : (state : state) @ immutable total ->
  (input : bytes) @ immutable total ->
    {u : unit | if fits state.budget input then
      let result = feed state input in
      (result.state.core, result.rest) === drain state.core input else true} =
  fun state input ->
  fits_def state.budget input;
  feed_def state input; drain_def state.core input;
  if terminal state.core then ()
  else match input with
  | [] -> ()
  | b :: bs ->
    if fits state.budget input then (
      advance_def state b;
      feed_matches_drain (advance state b) bs;
      ())
    else ()

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
      ())
    else match input with
    | [] ->
      consumed_def state state;
      S.length_def input; S.drop_def 0Z input;
      ()
    | b :: bs ->
      if state.budget <= 0 then (
        let stopped = {state with core = Limit Message_bytes} in
        consumed_def state stopped;
        S.drop_def 0Z input;
        ())
      else (
        let next = advance state b in
        advance_def state b;
        accounting next bs;
        let result = feed next bs in
        consumed_def next result.state; consumed_def state result.state;
        S.length_def input;
        S.drop_def (Bigint.of_int (consumed state result.state)) input;
        ()))
  else ()

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
  ()

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
    ())
  else ()

let[@def] semantic_request request =
  valid_request_line request.request_line && safe_line request.request_line
  && header_lines request.headers
  && (match framing request.headers with
      | Length n -> 0 <= n && n <= 8192 && sized n request.body
      | _ -> false)

let[@def] valid_core core =
  match core with
  | Line (phase, line, _) -> safe_line line &&
      (match phase with
       | Request_line -> true
       | Headers (request_line, headers) ->
         valid_request_line request_line && safe_line request_line
         && header_lines headers)
  | Body (request_line, headers, remaining, body) ->
    valid_request_line request_line && safe_line request_line
    && header_lines headers &&
      (match framing headers with
       | Length n -> 0 < remaining && remaining <= n && n <= 8192
         && sized (n - remaining) body
       | _ -> false)
  | Complete request -> semantic_request request
  | Malformed _ | Limit _ -> true

let[@def] rec header_prefix headers tail =
  match headers with
  | [] -> tail
  | line :: rest -> S.append line (13 :: 10 :: header_prefix rest tail)

let[@def] observed core =
  match core with Malformed _ | Limit _ -> false | _ -> true

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

let[@def] reachable state prefix = ghost_ (
  0 <= state.budget && state.budget <= 16384 && valid_core state.core
  && S.length prefix === Bigint.of_int (16384 - state.budget)
  && (if observed state.core then core_wire state.core === prefix else true))

let rec (safe_append @ total) : (xs : bytes) -> (ys : bytes) ->
    {u : unit | safe_line (S.append xs ys) =
      (safe_line xs && safe_line ys)} =
  fun xs ys ->
  safe_line_def xs; S.append_def xs ys;
  match xs with
  | [] -> ()
  | b :: bs ->
    safe_line_def (S.append xs ys); safe_append bs ys;
    ()

let rec (headers_append @ total) : (xs : int list list) ->
    (ys : int list list) ->
    {u : unit | header_lines (S.append xs ys) =
      (header_lines xs && header_lines ys)} =
  fun xs ys ->
  header_lines_def xs; S.append_def xs ys;
  match xs with
  | [] -> ()
  | line :: rest ->
    header_lines_def (S.append xs ys); headers_append rest ys;
    ()

let rec (sized_snoc @ total) : (n : int) -> (xs : bytes) -> (b : int) ->
    {u : unit | if 0 <= n && n < 8192 && sized n xs && byte b then
      sized (n + 1) (S.append xs [b]) else true} =
  fun n xs b ->
  sized_def n xs; S.append_def xs [b];
  match xs with
  | [] -> sized_def (n + 1) [b]; sized_def n [];
    ()
  | h :: rest ->
    if 0 <= n && n < 8192 && sized n xs && byte b then (
      sized_snoc (n - 1) rest b;
      sized_def (n + 1) (S.append xs [b]);
      ())
    else ()

let rec (decimal_bounds @ total) : (value : int) -> (digits : bytes) ->
    {u : unit | if 0 <= value && value <= 8192 then
      match decimal value digits with
      | Length n -> 0 <= n && n <= 8192 | _ -> true
      else true} =
  fun value digits ->
  decimal_def value digits;
  match digits with
  | [] -> ()
  | b :: bs ->
    if 0 <= value && value <= 8192 && 48 <= b && b <= 57
       && not (value > 819 || (value = 819 && b > 50)) then (
      decimal_bounds (value * 10 + b - 48) bs;
      ())
    else ()

let[@def] previous_bounded previous =
  match previous with None -> true | Some n -> 0 <= n && n <= 8192

let rec (frame_bounds @ total) : (headers : int list list) ->
    (previous : int option) -> (hosts : int) ->
    {u : unit | if previous_bounded previous then
      match frame_fields headers previous hosts with
      | Length n -> 0 <= n && n <= 8192 | _ -> true
      else true} =
  fun headers previous hosts ->
  previous_bounded_def previous;
  frame_fields_def headers previous hosts;
  match headers with
  | [] -> ()
  | line :: rest ->
    if valid_header line then (
      match header line with
      | None -> ()
      | Some (name, value) ->
        if is_host name then (
          frame_bounds rest previous 1; ())
        else if is_cl name then (
          content_length_def value; decimal_bounds 0 value;
          match content_length value with
          | Bad _ | Too_large _ -> ()
          | Length n -> previous_bounded_def (Some n);
            frame_bounds rest (Some n) hosts; ())
        else (frame_bounds rest previous hosts; ()))
    else ()

let (framing_bounds @ total) (headers : int list list) :
    {u : unit | match framing headers with
      | Length n -> 0 <= n && n <= 8192 | _ -> true} =
  framing_def headers;
  previous_bounded_def None; frame_bounds headers None 0;
  ()

let (finish_valid @ total) (phase : phase) (line : bytes) :
    {u : unit | if valid_core (Line (phase, line, true)) then
      valid_core (finish_line phase line) else true} =
  valid_core_def (Line (phase, line, true));
  finish_line_def phase line;
  match phase with
  | Request_line ->
    valid_core_def (Line (Headers (line, []), [], false));
    valid_core_def (Malformed Invalid_request_line);
    safe_line_def []; header_lines_def [];
    ()
  | Headers (request_line, headers) ->
    if nonempty line then (
      valid_core_def (Line
        (Headers (request_line, S.append headers [line]), [], false));
      valid_core_def (Malformed Invalid_header);
      safe_line_def []; header_lines_def [line]; header_lines_def [];
      headers_append headers [line];
      ())
    else (
      framing_bounds headers;
      match framing headers with
      | Bad e -> valid_core_def (Malformed e); ()
      | Too_large r -> valid_core_def (Limit r); ()
      | Length n ->
        if n = 0 then (
          let request = {request_line; headers; body = []} in
          valid_core_def (Complete request); semantic_request_def request;
          sized_def n []; ())
        else (
          valid_core_def (Body (request_line, headers, n, []));
          sized_def (n - n) []; ()))

let (step_valid @ total) (core : core) (b : int) :
    {u : unit | if valid_core core && not (terminal core) then
      valid_core (step core b) else true} =
  valid_core_def core; terminal_def core; step_def core b;
  if not (byte b) then (
    valid_core_def (Malformed Invalid_byte); ())
  else match core with
  | Complete _ | Malformed _ | Limit _ -> ()
  | Line (phase, line, cr) ->
    if cr then (
      finish_valid phase line;
      valid_core_def (Malformed Invalid_crlf); ())
    else if b = 13 then (
      valid_core_def (Line (phase, line, true)); ())
    else if b = 10 then (
      valid_core_def (Malformed Invalid_crlf); ())
    else (
      safe_line_def [b]; safe_line_def []; safe_append line [b];
      valid_core_def (Line (phase, S.append line [b], false));
      ())
  | Body (request_line, headers, remaining, body) ->
    match framing headers with
    | Bad _ | Too_large _ -> ()
    | Length n ->
      sized_snoc (n - remaining) body b;
      let next = S.append body [b] in
      if remaining = 1 then (
        let request = {request_line; headers; body = next} in
        valid_core_def (Complete request); semantic_request_def request;
        ())
      else (
        valid_core_def (Body
          (request_line, headers, remaining - 1, next));
        ())

let (crlf_append @ total) (xs : bytes) (ys : bytes) :
    {u : unit | S.append (13 :: 10 :: xs) ys ===
      13 :: 10 :: S.append xs ys} =
  S.append_def (13 :: 10 :: xs) ys;
  S.append_def (10 :: xs) ys;
  ()

let rec (header_prefix_append @ total) : (headers : int list list) ->
    (xs : bytes) -> (ys : bytes) ->
    {u : unit | S.append (header_prefix headers xs) ys ===
      header_prefix headers (S.append xs ys)} =
  fun headers xs ys ->
  header_prefix_def headers xs;
  header_prefix_def headers (S.append xs ys);
  match headers with
  | [] -> ()
  | line :: rest ->
    header_prefix_append rest xs ys;
    let tail = 13 :: 10 :: header_prefix rest xs in
    S.append_associative line tail ys;
    crlf_append (header_prefix rest xs) ys;
    ()

let rec (header_prefix_concat @ total) : (left : int list list) ->
    (right : int list list) -> (tail : bytes) ->
    {u : unit | header_prefix (S.append left right) tail ===
      header_prefix left (header_prefix right tail)} =
  fun left right tail ->
  S.append_def left right;
  header_prefix_def left (header_prefix right tail);
  match left with
  | [] -> ()
  | line :: rest ->
    header_prefix_def (S.append left right) tail;
    header_prefix_concat rest right tail;
    ()

let rec (wire_headers_prefix @ total) : (headers : int list list) ->
    (body : bytes) ->
    {u : unit | wire_headers headers body ===
      header_prefix headers (13 :: 10 :: body)} =
  fun headers body ->
  wire_headers_def headers body;
  header_prefix_def headers (13 :: 10 :: body);
  match headers with
  | [] -> ()
  | _ :: rest -> wire_headers_prefix rest body;
    ()

let[@def] request_prefix request_line headers tail =
  S.append request_line (13 :: 10 :: header_prefix headers tail)

let (request_prefix_append @ total) (line : bytes)
    (headers : int list list) (xs : bytes) (ys : bytes) :
    {u : unit | S.append (request_prefix line headers xs) ys ===
      request_prefix line headers (S.append xs ys)} =
  request_prefix_def line headers xs;
  request_prefix_def line headers (S.append xs ys);
  S.append_associative line (13 :: 10 :: header_prefix headers xs) ys;
  crlf_append (header_prefix headers xs) ys;
  header_prefix_append headers xs ys;
  ()

let (serialize_prefix @ total) (request : request) :
    {u : unit | serialize request === request_prefix
      request.request_line request.headers (13 :: 10 :: request.body)} =
  serialize_def request;
  wire_headers_prefix request.headers request.body;
  request_prefix_def request.request_line request.headers
    (13 :: 10 :: request.body);
  ()

let (serialize_append @ total) (line : bytes) (headers : int list list)
    (body : bytes) (tail : bytes) :
    {u : unit | S.append (serialize {request_line = line; headers; body})
      tail === serialize
        {request_line = line; headers; body = S.append body tail}} =
  serialize_prefix {request_line = line; headers; body};
  serialize_prefix
    {request_line = line; headers; body = S.append body tail};
  request_prefix_append line headers (13 :: 10 :: body) tail;
  crlf_append body tail;
  ()

let (line_wire @ total) (phase : phase) (line : bytes) (cr : bool) :
    {u : unit | core_wire (Line (phase, line, cr)) ===
      (match phase with
       | Request_line -> S.append line (if cr then [13] else [])
       | Headers (request_line, headers) -> request_prefix request_line
           headers (S.append line (if cr then [13] else [])))} =
  core_wire_def (Line (phase, line, cr));
  match phase with
  | Request_line -> ()
  | Headers (request_line, headers) ->
    request_prefix_def request_line headers
      (S.append line (if cr then [13] else []));
    ()

let (finish_wire @ total) (phase : phase) (line : bytes) :
    {u : unit | if observed (finish_line phase line) then
      core_wire (finish_line phase line) ===
      S.append (core_wire (Line (phase, line, true))) [10] else true} =
  finish_line_def phase line;
  line_wire phase line true;
  S.append_associative line [13] [10]; S.append_def [13] [10];
  S.append_def [] [10];
  match phase with
  | Request_line ->
    observed_def (Malformed Invalid_request_line);
    let next = Line (Headers (line, []), [], false) in
    core_wire_def next; S.append_def ([] : bytes) [];
    header_prefix_def [] [];
    ()
  | Headers (request_line, headers) ->
    request_prefix_append request_line headers (S.append line [13]) [10];
    if nonempty line then (
      observed_def (Malformed Invalid_header);
      line_wire (Headers (request_line, S.append headers [line])) [] false;
      S.append_def ([] : bytes) [];
      request_prefix_def request_line (S.append headers [line]) [];
      request_prefix_def request_line headers (S.append line [13;10]);
      header_prefix_concat headers [line] [];
      header_prefix_def [line] []; header_prefix_def [] [];
      ())
    else (
      nonempty_def line;
      S.append_def ([] : bytes) [13;10];
      match framing headers with
      | Bad e -> observed_def (Malformed e); ()
      | Too_large r -> observed_def (Limit r); ()
      | Length n ->
        let request = {request_line; headers; body = []} in
        serialize_prefix request;
        if n = 0 then (
          core_wire_def (Complete request); ())
        else (
          core_wire_def (Body (request_line, headers, n, []));
          ()))

let (step_wire @ total) (core : core) (b : int) :
    {u : unit | if not (terminal core) && observed (step core b) then
      core_wire (step core b) === S.append (core_wire core) [b]
      else true} =
  terminal_def core; step_def core b;
  if not (byte b) then (
    observed_def (Malformed Invalid_byte); ())
  else match core with
  | Complete _ | Malformed _ | Limit _ -> ()
  | Body (request_line, headers, n, body) ->
    core_wire_def core;
    let next = S.append body [b] in
    serialize_append request_line headers body [b];
    core_wire_def (Complete {request_line; headers; body = next});
    core_wire_def (Body (request_line, headers, n - 1, next));
    ()
  | Line (phase, line, cr) ->
    if cr then (
      finish_wire phase line;
      observed_def (Malformed Invalid_crlf); ())
    else (
      line_wire phase line false; S.append_nil line;
      if b = 10 then (
        observed_def (Malformed Invalid_crlf); ())
      else (
        line_wire phase line true;
        line_wire phase (S.append line [b]) false;
        S.append_nil (S.append line [b]);
        match phase with
        | Request_line -> ()
        | Headers (request_line, headers) ->
          request_prefix_append request_line headers line [b];
          ()))

let (initial_reachable @ total) (_unit : unit) :
    {u : unit | reachable (initial ()) []} =
  initial_def (); reachable_def (initial ()) [];
  valid_core_def (Line (Request_line, [], false)); safe_line_def [];
  observed_def (Line (Request_line, [], false));
  core_wire_def (Line (Request_line, [], false));
  S.append_def ([] : bytes) []; S.length_def ([] : bytes);
  ()

let (advance_reachable @ total) (state : state) (prefix : bytes) (b : int) :
    {u : unit | if reachable state prefix && not (terminal state.core)
      && state.budget > 0 then
      reachable (advance state b) (S.append prefix [b]) else true} =
  reachable_def state prefix; advance_def state b;
  reachable_def (advance state b) (S.append prefix [b]);
  step_valid state.core b; step_wire state.core b;
  S.append_length prefix [b]; S.length_def [b]; S.length_def ([] : bytes);
  observed_def state.core; terminal_def state.core;
  ()

let rec (feed_reachable @ total) : (state : state) -> (prefix : bytes) ->
    (input : bytes) ->
    {u : unit | if reachable state prefix then
      let result = feed state input in
      reachable result.state (S.append prefix
        (S.take (Bigint.of_int (consumed state result.state)) input))
      else true} =
  fun state prefix input ->
  reachable_def state prefix; feed_def state input;
  if terminal state.core then (
    consumed_def state state; S.take_def 0Z input; S.append_nil prefix;
    ())
  else match input with
  | [] ->
    consumed_def state state; S.take_def 0Z input; S.append_nil prefix;
    ()
  | b :: bs ->
    if state.budget <= 0 then (
      let stopped = {state with core = Limit Message_bytes} in
      consumed_def state stopped; S.take_def 0Z input; S.append_nil prefix;
      reachable_def stopped prefix;
      valid_core_def stopped.core; observed_def stopped.core;
      ())
    else (
      let next = advance state b in
      advance_reachable state prefix b; advance_def state b;
      feed_reachable next (S.append prefix [b]) bs;
      accounting next bs;
      let result = feed next bs in
      consumed_def state result.state; consumed_def next result.state;
      let count = Bigint.of_int (consumed state result.state) in
      let tail_count = Bigint.of_int (consumed next result.state) in
      S.take_def count input;
      S.append_associative prefix [b] (S.take tail_count bs);
      S.append_def [b] (S.take tail_count bs);
      S.append_def [] (S.take tail_count bs);
      ())

let rec (length_nonnegative @ total) : (xs : bytes) ->
    {u : unit | 0Z <= S.length xs} =
  fun xs -> S.length_def xs;
  match xs with
  | [] -> ()
  | _ :: rest -> length_nonnegative rest; ()

let rec (fits_length @ total) : (budget : int) -> (xs : bytes) ->
    {u : unit | if 0 <= budget && budget <= 16384 then
      fits budget xs = (S.length xs <= Bigint.of_int budget) else true} =
  fun budget xs ->
  fits_def budget xs; S.length_def xs;
  match xs with
  | [] -> ()
  | _ :: rest ->
    if budget > 0 && budget <= 16384 then (
      fits_length (budget - 1) rest; ())
    else (length_nonnegative rest; ())

let (reachable_completion @ total) (state : state) (prefix : bytes) :
    {u : unit | if reachable state prefix then
      match state.core with
      | Complete request -> well_formed request
        && serialize request === prefix
      | _ -> true else true} =
  reachable_def state prefix;
  match state.core with
  | Complete request ->
    valid_core_def state.core; observed_def state.core;
    core_wire_def state.core;
    semantic_request_def request; well_formed_def request;
    fits_length 16384 (serialize request);
    ()
  | _ -> ()

let (accepted_input @ total) (input : bytes) :
    {u : unit | let result = feed (initial ()) input in
      reachable result.state
        (S.take (Bigint.of_int (consumed (initial ()) result.state)) input)
      && (match result.state.core with
          | Complete request -> well_formed request
            && S.take (Bigint.of_int
                (consumed (initial ()) result.state)) input ===
              serialize request
            && input === S.append (serialize request) result.rest
          | _ -> true)} =
  initial_reachable ();
  let start = initial () in
  feed_reachable start [] input;
  let result = feed start input in
  let prefix = S.take (Bigint.of_int (consumed start result.state)) input in
  S.append_def [] prefix;
  reachable_completion result.state prefix;
  initial_def (); suffix_preservation start input;
  ()

let (framing_rejection @ total) (headers : int list list) :
    {u : unit | if has_transfer_encoding headers then
      framing headers ===
        (if has_content_length headers then Bad Transfer_encoding_content_length
         else Bad Unsupported_transfer_encoding)
      else true} =
  has_transfer_encoding_def headers; has_content_length_def headers;
  framing_def headers; ()

let rec (equal_bytes_sound @ total) : (xs : bytes) -> (ys : bytes) ->
    {u : unit | if equal_bytes xs ys then xs === ys else true} =
  fun xs ys ->
  equal_bytes_def xs ys;
  match xs, ys with
  | x :: rest, y :: tail ->
    equal_bytes_sound rest tail; ()
  | _ -> ()

let (host_not_cl @ total) (name : bytes) :
    {u : unit | not (is_host name && is_cl name)} =
  is_host_def name; is_cl_def name;
  equal_bytes_sound name [104;111;115;116];
  equal_bytes_sound name
    [99;111;110;116;101;110;116;45;108;101;110;103;116;104];
  ()

let[@def] previous_matches (previous : int option) (n : int) =
  match previous with None -> true | Some old -> old = n

let rec (frame_agreement @ total) : (headers : int list list) ->
    (previous : int option) -> (hosts : int) ->
    {u : unit | match frame_fields headers previous hosts with
      | Length n -> content_lengths_match headers n
        && previous_matches previous n
      | _ -> true} =
  fun headers previous hosts ->
  frame_fields_def headers previous hosts;
  match frame_fields headers previous hosts with
  | Bad _ | Too_large _ -> ()
  | Length n ->
    content_lengths_match_def headers n;
    previous_matches_def previous n;
    match headers with
    | [] -> ()
    | line :: rest ->
      header_field_def line;
      match header line with
      | None -> ()
      | Some (name, value) ->
        if is_host name then (
          host_not_cl name;
          frame_agreement rest previous 1;
          ())
        else if is_cl name then (
          match content_length value with
          | Bad _ | Too_large _ -> ()
          | Length m ->
            frame_agreement rest (Some m) hosts;
            previous_matches_def (Some m) n;
            ())
        else (
          frame_agreement rest previous hosts;
          ())

let (well_formed_framing @ total) (request : request) :
    {u : unit | if well_formed request then
      not (has_transfer_encoding request.headers)
      && (match framing request.headers with
          | Length n -> 0 <= n && n <= 8192 && sized n request.body
            && content_lengths_match request.headers n
          | _ -> false)
      else true} =
  well_formed_def request;
  framing_def request.headers; has_transfer_encoding_def request.headers;
  frame_agreement request.headers None 0;
  framing_bounds request.headers;
  ()

let rec (sized_length @ total) : (n : int) -> (xs : bytes) ->
    {u : unit | if 0 <= n && n <= 8192 && sized n xs then
      S.length xs === Bigint.of_int n else true} =
  fun n xs ->
  sized_def n xs; S.length_def xs;
  match xs with
  | [] -> ()
  | _ :: rest ->
    if 0 <= n && n <= 8192 && sized n xs then (
      sized_length (n - 1) rest; ())
    else ()

let[@def] previous_value previous =
  match previous with None -> 0 | Some n -> n

let rec (frame_default @ total) : (headers : int list list) ->
    (previous : int option) -> (hosts : int) ->
    {u : unit | if not (has_content_length headers) then
      match frame_fields headers previous hosts with
      | Length n -> n = previous_value previous | _ -> true
      else true} =
  fun headers previous hosts ->
  frame_fields_def headers previous hosts;
  has_content_length_def headers;
  previous_value_def previous;
  match headers with
  | [] -> ()
  | line :: rest ->
    has_name_def
      [99;111;110;116;101;110;116;45;108;101;110;103;116;104] headers;
    has_content_length_def rest;
    header_def line;
    match header line with
    | None -> ()
    | Some (name, _) ->
      is_cl_def name;
      if is_host name then (
        frame_default rest previous 1; ())
      else (
        frame_default rest previous hosts; ())

let (no_content_length_body @ total) (request : request) :
    {u : unit | if well_formed request
        && not (has_content_length request.headers) then
      request.body === [] else true} =
  well_formed_def request; well_formed_framing request;
  framing_def request.headers; has_transfer_encoding_def request.headers;
  frame_default request.headers None 0; previous_value_def None;
  sized_def 0 request.body;
  ()

let (body_agreement @ total) (request : request) :
    {u : unit | if well_formed request then
      not (has_transfer_encoding request.headers)
      && (match framing request.headers with
          | Length n -> 0 <= n && n <= 8192
            && S.length request.body === Bigint.of_int n
            && content_lengths_match request.headers n
          | _ -> false)
      else true} =
  well_formed_framing request;
  match framing request.headers with
  | Bad _ | Too_large _ -> ()
  | Length n -> sized_length n request.body; ()

let (header_outcome @ total) (line : bytes) (headers : int list list) :
    {u : unit | let request = {request_line = line; headers; body = []} in
      if valid_request_line line && safe_line line && header_lines headers
        && fits 16384 (serialize request) then
      let result = feed (initial ()) (serialize request) in
      result.rest === [] && result.state.core ===
        (match framing headers with
         | Bad e -> Malformed e
         | Too_large r -> Limit r
         | Length n -> if n = 0 then Complete request
             else Body (line, headers, n, []))
      else true} =
  let request = {request_line = line; headers; body = []} in
  let wire = serialize request in
  serialize_def request;
  let segment = S.append line [13;10] in
  let tail = wire_headers headers [] in
  let start = Line (Request_line, [], false) in
  drain_line Request_line [] line; S.append_def [] line;
  drain_append start segment tail;
  S.append_associative line [13;10] tail;
  S.append_def [13;10] tail; S.append_def [10] tail; S.append_def [] tail;
  finish_line_def Request_line line;
  terminal_def (Line (Headers (line, []), [], false));
  drain_headers line [] headers [];
  S.append_def [] headers;
  let ending = finish_line (Headers (line, headers)) [] in
  drain_empty ending;
  finish_line_def (Headers (line, headers)) []; nonempty_def [];
  initial_def (); feed_matches_drain (initial ()) wire;
  ()

let rec (incomplete_suffix @ total) : (state : state) -> (input : bytes) ->
    {u : unit | let result = feed state input in
      if not (terminal result.state.core) then result.rest === [] else true} =
  fun state input ->
  feed_def state input;
  if terminal state.core then ()
  else match input with
  | [] -> ()
  | b :: rest ->
    if state.budget <= 0 then (
      terminal_def (Limit Message_bytes); ())
    else (
      incomplete_suffix (advance state b) rest;
      ())

end

module Reverse = struct
module S = Vox_sequence
let[@def] rec onto (xs : ('a : immutable_data) list @ immutable total)
    (tail : 'a list @ immutable total) : 'a list @ immutable total =
  match xs with [] -> tail | x :: rest -> onto rest (x :: tail)
let[@def] rev (xs : ('a : immutable_data) list @ immutable total) :
    'a list @ immutable total = onto xs []
let rec (onto_append @ total) :
    (xs : ('a : immutable_data) list) @ immutable total ->
    (tail : 'a list) @ immutable total ->
    {u : unit | onto xs tail === S.append (rev xs) tail} =
  fun xs tail ->
  onto_def xs tail; onto_def xs []; rev_def xs;
  match xs with
  | [] -> S.append_def [] tail; ()
  | x :: rest ->
    onto_append rest [x]; onto_append rest (x :: tail);
    S.append_associative (rev rest) [x] tail;
    S.append_def [x] tail; S.append_def [] tail; ()
let (cons @ total) :
    (x : ('a : immutable_data)) @ immutable total ->
    (xs : 'a list) @ immutable total ->
    {u : unit | rev (x :: xs) === S.append (rev xs) [x]} =
  fun x xs ->
  rev_def (x :: xs); onto_def (x :: xs) []; onto_append xs [x]; ()
let rec (append @ total) :
    (xs : ('a : immutable_data) list) @ immutable total ->
    (ys : 'a list) @ immutable total ->
    {u : unit | rev (S.append xs ys) === S.append (rev ys) (rev xs)} =
  fun xs ys ->
  S.append_def xs ys;
  match xs with
  | [] -> rev_def xs; onto_def xs []; S.append_nil (rev ys); ()
  | x :: rest ->
    cons x rest; cons x (S.append rest ys); append rest ys;
    S.append_associative (rev ys) (rev rest) [x]; ()
let rec (involution @ total) :
    (xs : ('a : immutable_data) list) @ immutable total ->
    {u : unit | rev (rev xs) === xs} =
  fun xs ->
  match xs with
  | [] -> rev_def xs; onto_def xs []; ()
  | x :: rest ->
    cons x rest; append (rev rest) [x]; involution rest;
    rev_def [x]; onto_def [x] []; onto_def [] [x];
    S.append_def [x] rest; S.append_def [] rest; ()
end

module Driver = struct
open Internal
let[@def] phase (p : phase @ immutable total) =
  match p with Request_line -> Request_line
  | Headers (line, headers) -> Headers (line, Reverse.rev headers)
let[@def] core (c : core @ immutable total) =
  match c with
  | Line (p, line, cr) -> Line (phase p, Reverse.rev line, cr)
  | Body (line, headers, n, body) -> Body (line, headers, n, Reverse.rev body)
  | Complete _ | Malformed _ | Limit _ -> c
let[@def] model (s : state @ immutable total) =
  {core = core s.core; budget = s.budget}
let[@def] finish (p : phase @ immutable total)
    (line : bytes @ immutable total) =
  match p with
  | Request_line -> if valid_request_line line then
      Line (Headers (line, []), [], false) else Malformed Invalid_request_line
  | Headers (request_line, headers) ->
    if nonempty line then
      if valid_header line then
        Line (Headers (request_line, line :: headers), [], false)
      else Malformed Invalid_header
    else
      let headers = Reverse.rev headers in
      match framing headers with
      | Bad e -> Malformed e | Too_large r -> Limit r
      | Length n -> if n = 0 then Complete {request_line; headers; body = []}
        else Body (request_line, headers, n, [])
let[@def] push (c : core @ immutable total) (b : int) =
  if not (byte b) then Malformed Invalid_byte else
  match c with
  | Complete _ | Malformed _ | Limit _ -> c
  | Line (p, line, cr) ->
    if cr then if b = 10 then finish p (Reverse.rev line)
      else Malformed Invalid_crlf
    else if b = 13 then Line (p, line, true)
    else if b = 10 then Malformed Invalid_crlf
    else Line (p, b :: line, false)
  | Body (request_line, headers, remaining, body) ->
    let body = b :: body in
    if remaining = 1 then
      Complete {request_line; headers; body = Reverse.rev body}
    else Body (request_line, headers, remaining - 1, body)
let[@def] rec consume (state : state @ immutable total)
    (input : bytes @ immutable total) =
  if terminal state.core then {state; rest = input}
  else match input with
  | [] -> {state; rest = []}
  | b :: bs -> if state.budget <= 0 then
      {state = {state with core = Limit Message_bytes}; rest = input}
    else consume {core = push state.core b; budget = state.budget - 1} bs

let (core_terminal @ total) (c : core @ immutable total) :
    {u : unit | terminal (core c) === terminal c} =
  core_def c; terminal_def c; terminal_def (core c); ()
let (finish_simulation @ total) (p : phase @ immutable total)
    (line : bytes @ immutable total) :
    {u : unit | core (finish p line) ===
      Internal.finish_line (phase p) line} =
  finish_def p line; phase_def p;
  Internal.finish_line_def (phase p) line;
  Reverse.rev_def ([] : bytes); Reverse.onto_def ([] : bytes) [];
  Reverse.rev_def ([] : bytes list); Reverse.onto_def ([] : bytes list) [];
  match p with
  | Request_line ->
    core_def (finish p line); phase_def (Headers (line, [])); ()
  | Headers (request_line, headers) ->
    Reverse.cons line headers;
    core_def (finish p line);
    phase_def (Headers (request_line, line :: headers)); ()
let (step_simulation @ total) (c : core @ immutable total) (b : int) :
    {u : unit | core (push c b) === Internal.step (core c) b} =
  push_def c b; core_def c; Internal.step_def (core c) b;
  core_def (push c b);
  match c with
  | Line (p, line, cr) ->
    Reverse.cons b line;
    finish_simulation p (Reverse.rev line); ()
  | Body (_, _, _, body) -> Reverse.cons b body; ()
  | Complete _ | Malformed _ | Limit _ -> ()
let rec (simulation @ total) : (s : state) @ immutable total ->
    (input : bytes) @ immutable total ->
    {u : unit | let actual = consume s input in
      let expected = Internal.feed (model s) input in
      model actual.state === expected.state && actual.rest === expected.rest} =
  fun s input ->
  consume_def s input; model_def s; Internal.feed_def (model s) input;
  core_terminal s.core;
  if terminal s.core then () else
  match input with
  | [] -> ()
  | b :: bs ->
    if s.budget <= 0 then (
      model_def {s with core = Limit Message_bytes};
      core_def (Limit Message_bytes); ())
    else (
      step_simulation s.core b;
      let next = {core = push s.core b; budget = s.budget - 1} in
      model_def next; Internal.advance_def (model s) b;
      simulation next bs; ())
let (involution @ total) (s : state @ immutable total) :
    {u : unit | model (model s) === s} =
  model_def s; model_def (model s); core_def s.core;
  core_def (core s.core);
  match s.core with
  | Line (p, line, _) ->
    Reverse.involution line; phase_def p; phase_def (phase p);
    (match p with Request_line -> ()
     | Headers (_, headers) -> Reverse.involution headers); ()
  | Body (_, _, _, body) -> Reverse.involution body; ()
  | Complete _ | Malformed _ | Limit _ -> ()
end

module S = Vox_sequence

let[@def] good (machine : Internal.state) = ghost_ (
  0 <= machine.budget && machine.budget <= 16384
  && Internal.valid_core machine.core
  && (if Internal.observed machine.core then
        S.length (Internal.core_wire machine.core) ===
          Bigint.of_int (16384 - machine.budget)
      else true))

type state = {machine : Internal.state | good (Driver.model machine)}
type result : immutable_data mod total = { state : state; rest : bytes }

let[@def] machine_of (state : state) = Driver.model state

let[@def] total_consumed (state : state) =
  let machine = state in 16384 - machine.budget

let[@def] consumed (before : state) (after : state) =
  total_consumed after - total_consumed before

let[@def] processed (state : state) = ghost_ (
  let machine = Driver.model state in Internal.core_wire machine.core)

let[@def] status (state : state) =
  let machine = state in
  match machine.core with
  | Internal.Line _ | Internal.Body _ -> Incomplete
  | Internal.Complete request -> Complete request
  | Internal.Malformed error -> Malformed error
  | Internal.Limit resource -> Limit resource

let (status_model @ total) (state : state) :
    {u : unit | status state ===
      (match (machine_of state).core with
       | Internal.Line _ | Internal.Body _ -> Incomplete
       | Internal.Complete request -> Complete request
       | Internal.Malformed error -> Malformed error
       | Internal.Limit resource -> Limit resource)} =
  status_def state; machine_of_def state;
  Driver.model_def state; Driver.core_def state.core; ()

let (initial_model @ total) (_unit : unit) :
    {u : unit | Driver.model (Internal.initial ()) === Internal.initial ()} =
  Internal.initial_def (); Driver.model_def (Internal.initial ());
  Driver.core_def (Internal.initial ()).core;
  Driver.phase_def Internal.Request_line;
  Reverse.rev_def ([] : bytes); Reverse.onto_def ([] : bytes) []; ()

let (reachable_good @ total) (machine : Internal.state) (prefix : bytes) :
    {u : unit | if Internal.reachable machine prefix then good machine
      else true} =
  Internal.reachable_def machine prefix; good_def machine;
  ()

let (good_reachable @ total) (machine : Internal.state) :
    {u : unit | if good machine && Internal.observed machine.core then
      Internal.reachable machine (Internal.core_wire machine.core)
      else true} =
  good_def machine;
  Internal.reachable_def machine (Internal.core_wire machine.core);
  ()

let (feed_good @ total) (machine : Internal.state) (input : bytes) :
    {u : unit | if good machine then
      good (Internal.feed machine input).state else true} =
  good_reachable machine;
  if Internal.observed machine.core then (
    Internal.feed_reachable machine (Internal.core_wire machine.core) input;
    let answer = Internal.feed machine input in
    let prefix = S.append (Internal.core_wire machine.core)
      (S.take (Bigint.of_int (Internal.consumed machine answer.state)) input) in
    reachable_good answer.state prefix;
    ())
  else (
    Internal.observed_def machine.core;
    Internal.terminal_def machine.core;
    Internal.feed_def machine input;
    ())

let (state_sound @ total) (state : state) :
    {u : unit | 0 <= total_consumed state && total_consumed state <= 16384
      && (match status state with
          | Incomplete -> S.length (processed state) ===
              Bigint.of_int (total_consumed state)
          | Complete request -> well_formed request
            && serialize request === processed state
            && S.length (processed state) ===
              Bigint.of_int (total_consumed state)
          | Malformed _ | Limit _ -> processed state === [])} =
  let machine = Driver.model state in
  good_def machine; status_model state; machine_of_def state;
  Driver.model_def state; processed_def state;
  total_consumed_def state;
  good_reachable machine;
  Internal.reachable_completion machine (Internal.core_wire machine.core);
  Internal.observed_def machine.core;
  match machine.core with
  | Internal.Line _ | Internal.Body _ | Internal.Complete _ ->
    ()
  | Internal.Malformed _ | Internal.Limit _ ->
    Internal.core_wire_def machine.core;
    ()

let[@def] transition (state : state) (input : bytes) (result : result) =
  ghost_ (total_consumed state <= total_consumed result.state
      && total_consumed result.state <= 16384
      && S.length input === Bigint.add
        (Bigint.of_int (consumed state result.state)) (S.length result.rest)
      && S.drop (Bigint.of_int (consumed state result.state)) input ===
        result.rest
      && S.append
        (S.take (Bigint.of_int (consumed state result.state)) input)
        result.rest === input
      && (match status result.state with
          | Incomplete -> result.rest === [] | _ -> true)
      && (match status result.state with
          | Incomplete | Complete _ -> processed result.state ===
              S.append (processed state)
                (S.take (Bigint.of_int (consumed state result.state)) input)
          | Malformed _ | Limit _ -> true)
      && (match status result.state with
          | Complete request -> well_formed request
            && serialize request === processed result.state
          | _ -> true))

let (make_initial @ total) (_unit : unit) :
    {state : state | status state === Incomplete
      && total_consumed state = 0 && processed state === []
      && machine_of state === Internal.initial ()} =
  let machine = Internal.initial () in
  ghost_ (initial_model (); Internal.initial_reachable ();
    reachable_good machine []);
  let state : state = machine in
  ghost_ (
    status_model state; total_consumed_def state; processed_def state;
    machine_of_def state; Driver.model_def state;
    Internal.initial_def ();
    Internal.core_wire_def machine.core;
    S.append_def ([] : bytes) []);
  state

let (driver_result_facts @ total) (state : state) (input : bytes)
    (result : {r : result | let answer = Driver.consume state input in
      r.state === answer.state && r.rest === answer.rest}) :
    {u : unit |
      machine_of result.state ===
        (Internal.feed (machine_of state) input).state
      && result.rest === (Internal.feed (machine_of state) input).rest
      && transition state input result} =
  Driver.simulation state input;
  let next = result.state in
  let machine = Driver.model state in
  let answer = Internal.feed machine input in
  Driver.model_def state; Driver.model_def next;
  good_def machine; good_reachable machine;
  machine_of_def state; machine_of_def next;
  Internal.accounting machine input;
  Internal.incomplete_suffix machine input;
  Internal.terminal_def answer.state.core;
  Internal.suffix_preservation machine input;
  consumed_def state next;
  total_consumed_def state; total_consumed_def next;
  Internal.consumed_def machine answer.state;
  processed_def state; processed_def next;
  status_model next; status_model state;
  Internal.feed_reachable machine (Internal.core_wire machine.core) input;
  let prefix = S.append (Internal.core_wire machine.core)
    (S.take (Bigint.of_int (Internal.consumed machine answer.state)) input) in
  Internal.reachable_def answer.state prefix;
  Internal.observed_def answer.state.core;
  Internal.observed_def machine.core;
  Internal.terminal_def machine.core;
  Internal.feed_def machine input;
  state_sound next; transition_def state input result;
  ()

let (run @ total) (state : state) (input : bytes) :
    {result : result |
      machine_of result.state ===
        (Internal.feed (machine_of state) input).state
      && result.rest === (Internal.feed (machine_of state) input).rest
      && transition state input result} =
  let answer = Driver.consume state input in
  ghost_ (Driver.simulation state input; feed_good (Driver.model state) input);
  let next : state = answer.state in
  let result = {state = next; rest = answer.rest} in
  ghost_ (driver_result_facts state input result);
  result

let[@def] initial (_unit : unit) :
    {state : state | status state === Incomplete
      && total_consumed state = 0 && processed state === []} =
  make_initial _unit

let[@def] feed (state : state) (input : bytes) :
    {result : result | transition state input result} =
  run state input

let (initial_machine @ total) (_unit : unit) :
    {u : unit | machine_of (initial ()) === Internal.initial ()} =
  initial_def ();
  let _ = make_initial () in
  ()

let (feed_machine @ total) (state : state) (input : bytes) :
    {u : unit | let answer = Internal.feed (machine_of state) input in
      let result = feed state input in
      machine_of result.state === answer.state && result.rest === answer.rest} =
  feed_def state input;
  let _ = run state input in
  ()

let (machine_injective @ total) (left : state) (right : state) :
    {u : unit | if machine_of left === machine_of right then left === right
      else true} =
  machine_of_def left; Driver.model_def left;
  machine_of_def right; Driver.model_def right;
  Driver.involution left; Driver.involution right; ()

let (consumed_machine @ total) (left : state) (right : state) :
    {u : unit | consumed left right =
      Internal.consumed (machine_of left) (machine_of right)} =
  consumed_def left right;
  total_consumed_def left; total_consumed_def right;
  machine_of_def left; Driver.model_def left;
  machine_of_def right; Driver.model_def right;
  Internal.consumed_def (machine_of left) (machine_of right);
  ()

let (chunking_invariance @ total) (state : state)
    (left : bytes) (right : bytes) :
    {u : unit | feed state (S.append left right) ===
      (let first = feed state left in
       feed first.state (S.append first.rest right))} =
  let first = feed state left in
  let last = feed first.state (S.append first.rest right) in
  let whole = feed state (S.append left right) in
  feed_machine state left;
  feed_machine first.state (S.append first.rest right);
  feed_machine state (S.append left right);
  Internal.chunking_invariance (machine_of state) left right;
  machine_injective last.state whole.state;
  ()

let (request_separation @ total) (state : state)
    (prefix : bytes) (suffix : bytes) :
    {u : unit | let first = feed state prefix in
      match status first.state with
      | Complete _ -> feed state (S.append prefix suffix) ===
          {state = first.state; rest = S.append first.rest suffix}
      | _ -> true} =
  let first = feed state prefix in
  let whole = feed state (S.append prefix suffix) in
  feed_machine state prefix; feed_machine state (S.append prefix suffix);
  status_model first.state; machine_of_def first.state;
  Driver.model_def first.state;
  Internal.request_separation (machine_of state) prefix suffix;
  machine_injective first.state whole.state;
  ()

let (terminal_preservation @ total) (state : state) (input : bytes) :
    {u : unit | if is_terminal (status state) then
      feed state input === {state; rest = input} else true} =
  let result = feed state input in
  feed_machine state input;
  machine_of_def state; Driver.model_def state; status_model state;
  is_terminal_def (status state);
  Internal.terminal_def (machine_of state).core;
  Internal.feed_def (machine_of state) input;
  machine_injective state result.state;
  ()

let (roundtrip @ total) (request : request) (suffix : bytes) :
    {u : unit | if well_formed request then
      let result = feed (initial ()) (S.append (serialize request) suffix) in
      status result.state === Complete request && result.rest === suffix
      && Bigint.of_int (consumed (initial ()) result.state) ===
        S.length (serialize request)
      else true} =
  let start = initial () in
  let input = S.append (serialize request) suffix in
  let result = feed start input in
  initial_machine (); feed_machine start input;
  Internal.roundtrip request suffix;
  consumed_machine start result.state;
  status_model result.state; machine_of_def result.state;
  Driver.model_def result.state;
  ()

let (header_outcome @ total) (line : bytes) (headers : int list list) :
    {u : unit | let request = {request_line = line; headers; body = []} in
      if valid_request_line line && safe_line line && header_lines headers
        && fits 16384 (serialize request) then
      let result = feed (initial ()) (serialize request) in
      result.rest === [] && status result.state ===
        (match framing headers with
         | Bad e -> Malformed e
         | Too_large r -> Limit r
         | Length n -> if n = 0 then Complete request else Incomplete)
      else true} =
  let request = {request_line = line; headers; body = []} in
  let result = feed (initial ()) (serialize request) in
  initial_machine (); feed_machine (initial ()) (serialize request);
  Internal.header_outcome line headers;
  status_model result.state; machine_of_def result.state;
  Driver.model_def result.state;
  ()

let (framing_rejection @ total) (headers : int list list) :
    {u : unit | if has_transfer_encoding headers then
      framing headers ===
        (if has_content_length headers then
           Bad Transfer_encoding_content_length
         else Bad Unsupported_transfer_encoding)
      else true} =
  Internal.framing_rejection headers

let (body_agreement @ total) (request : request) :
    {u : unit | if well_formed request then
      not (has_transfer_encoding request.headers)
      && (match framing request.headers with
          | Length n -> 0 <= n && n <= 8192
            && S.length request.body === Bigint.of_int n
            && content_lengths_match request.headers n
          | _ -> false)
      else true} =
  Internal.body_agreement request

let (no_content_length_body @ total) (request : request) :
    {u : unit | if well_formed request
        && not (has_content_length request.headers) then
      request.body === [] else true} =
  Internal.no_content_length_body request

let (parse @ total) (input : bytes) :
    {result : result |
      result === feed (initial ()) input
      && transition (initial ()) input result
      && (match status result.state with
          | Complete request -> well_formed request
            && S.take (Bigint.of_int
                (consumed (initial ()) result.state)) input ===
              serialize request
            && input === S.append (serialize request) result.rest
          | _ -> true)} =
  let start = initial () in
  let result = feed start input in
  ghost_ (transition_def start input result; S.append_def []
    (S.take (Bigint.of_int (consumed start result.state)) input));
  result
