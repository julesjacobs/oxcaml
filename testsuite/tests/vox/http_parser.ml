(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_http_spec.mli vox_http_spec.ml vox_http.mli vox_http.ml http_parser.ml";
 { native; }
*)
open Vox_http_spec
open Vox_http
module S = Vox_sequence

let (feed_clauses @ total) (state : state) (input : bytes) :
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
        | _ -> true)} =
  let result = feed state input in
  ghost_ (transition_def state input result);
  result

let (parse_clauses @ total) (input : bytes) :
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
        | _ -> true)} =
  let result = parse input in
  ghost_ (transition_def (initial ()) input result);
  result

let (decode_serialized @ total) (request : request) (suffix : bytes) :
    {result : result | if well_formed request then
      (status result.state) === Complete request && result.rest === suffix else true} =
  let input = S.append (serialize request) suffix in
  let result = parse input in
  ghost_ (roundtrip request suffix);
  result

let (accept_untrusted @ total) (input : bytes) :
    {result : result | match (status result.state) with
      | Complete request -> well_formed request
        && not (has_transfer_encoding request.headers)
        && (match framing request.headers with
            | Length n -> S.length request.body === Bigint.of_int n
              && content_lengths_match request.headers n
            | _ -> false)
      | _ -> true} =
  let result = parse input in
  match (status result.state) with
  | Complete request -> ghost_ (body_agreement request); result
  | _ -> result

let (stream_two @ total) (first : bytes) (second : bytes) :
    {result : result | match status result.state with
      | Complete request -> well_formed request
        && S.append first second === S.append (serialize request) result.rest
      | _ -> true} =
  let start = initial () in
  let part = feed start first in
  let result = feed part.state (S.append part.rest second) in
  ghost_ (
    chunking_invariance start first second;
    let whole = parse (S.append first second) in
    (() : {u : unit | result === whole
      && (match status result.state with
          | Complete request -> S.append first second ===
              S.append (serialize request) result.rest
          | _ -> true)}));
  result

let (reject_transfer_encoding @ total) (line : bytes)
    (headers : int list list) :
    {result : result |
      let request = {request_line = line; headers; body = []} in
      if valid_request_line line && safe_line line && header_lines headers
        && fits 16384 (serialize request) && has_transfer_encoding headers then
      (status result.state) ===
        (if has_content_length headers then
           Malformed Transfer_encoding_content_length
         else Malformed Unsupported_transfer_encoding)
      else true} =
  let request = {request_line = line; headers; body = []} in
  let result = feed (initial ()) (serialize request) in
  ghost_ (header_outcome line headers; framing_rejection headers);
  result

let bytes text = List.init (String.length text) (fun i -> Char.code text.[i])
let text bs = String.of_seq (List.to_seq (List.map Char.chr bs))
let run text =
  let input = bytes text in
  let result = parse input in
  (match (status result.state) with
   | Complete request ->
     assert (well_formed request);
     assert (input = serialize request @ result.rest)
   | _ -> ());
  result
let complete result = match (status result.state) with Complete request -> request | _ -> assert false
let malformed expected wire =
  match (status (run wire).state) with Malformed actual -> assert (actual = expected) | _ -> assert false
let limited expected wire =
  match (status (run wire).state) with Limit actual -> assert (actual = expected) | _ -> assert false
let request = {
  request_line = bytes "POST /submit?q=1 HTTP/1.1";
  headers = List.map bytes ["Host: example.test"; "Content-Length: 5"; "X-Trace: yes"];
  body = bytes "a\000\r\nb";
}
let get = "GET /health HTTP/1.1\r\nHost: example.test\r\n\r\n"

let () =
  assert (well_formed request);
  assert (request_parts request = (bytes "POST", bytes "/submit?q=1"));
  assert (header_field (bytes "cOnTeNt-LeNgTh:\t005 ") =
    Some (bytes "content-length", bytes "005"));
  assert (header_field (bytes "Missing-colon") = None);
  let wire = serialize request in
  let result = decode_serialized request (bytes get) in
  assert (complete result = request);
  assert (result.rest = bytes get);
  assert (consumed (initial ()) result.state = List.length wire);
  assert ((complete (feed (initial ()) result.rest)).request_line = bytes "GET /health HTTP/1.1");
  let joined = wire @ bytes get in
  let parsed = accept_untrusted joined in
  assert (parsed = result);
  for cut = 0 to List.length joined do
    let left = List.filteri (fun i _ -> i < cut) joined in
    let right = List.filteri (fun i _ -> i >= cut) joined in
    let first = feed (initial ()) left in
    assert (feed first.state (first.rest @ right) = feed (initial ()) joined);
    let streamed = stream_two left right in
    assert (streamed = parsed)
  done;
  let state = List.fold_left (fun state byte ->
    let result = feed state [byte] in assert (result.rest = []); result.state)
    (initial ()) wire in
  assert (state = (feed (initial ()) wire).state);
  for cut = 0 to List.length wire - 1 do
    let prefix = List.filteri (fun i _ -> i < cut) wire in
    let result = feed (initial ()) prefix in
    assert (not (is_terminal (status result.state)));
    assert (result.rest = []);
    assert (consumed (initial ()) result.state = cut)
  done;
  malformed Invalid_request_line "GET / HTTP/1.0\r\n";
  malformed Invalid_request_line "GET  / HTTP/1.1\r\n";
  malformed Invalid_crlf "GET / HTTP/1.1\n";
  malformed Invalid_crlf "GET / HTTP/1.1\rX";
  malformed Invalid_header "GET / HTTP/1.1\r\nHost : example\r\n";
  malformed Invalid_header "GET / HTTP/1.1\r\nBroken\r\n";
  malformed Invalid_header "GET / HTTP/1.1\r\n folded\r\n";
  malformed Invalid_host "GET / HTTP/1.1\r\n\r\n";
  malformed Invalid_host "GET / HTTP/1.1\r\nHost:\r\n\r\n";
  malformed Invalid_host "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n";
  let framing fields = "POST / HTTP/1.1\r\nHost: a\r\n" ^ fields ^ "\r\n" in
  List.iter (fun value -> malformed Invalid_content_length
    (framing ("Content-Length: " ^ value ^ "\r\n"))) [""; "-1"; "+1"; "1, 1"; "1x"; "1 0"];
  malformed Conflicting_content_length
    (framing "Content-Length: 1\r\ncontent-length: 2\r\n");
  malformed Unsupported_transfer_encoding (framing "Transfer-Encoding: chunked\r\n");
  malformed Unsupported_transfer_encoding (framing "Transfer-Encoding: identity\r\n");
  malformed Transfer_encoding_content_length
    (framing "Transfer-Encoding: chunked\r\nContent-Length: 0\r\n");
  malformed Transfer_encoding_content_length
    (framing "content-length: 0\r\nTRANSFER-ENCODING: gzip, chunked\r\n");
  List.iter (fun fields ->
    let headers = List.map bytes fields in
    let result = reject_transfer_encoding
      (bytes "POST / HTTP/1.1") headers in
    assert ((status result.state) =
      (if has_content_length headers then
         Malformed Transfer_encoding_content_length
       else Malformed Unsupported_transfer_encoding)))
    [["Host: a"; "Transfer-Encoding: chunked"];
     ["Host: a"; "Content-Length: 0"; "Transfer-Encoding: chunked"]];
  assert ((complete (run (framing "cOnTeNt-LeNgTh:\t0001 \t\r\nContent-Length: 1\r\n" ^ "x"))).body = bytes "x");
  assert ((complete (run (framing ""))).body = []);
  let binary = List.init 256 Fun.id in
  let result = feed (initial ()) (bytes (framing "Content-Length: 256\r\n") @ binary @ bytes get) in
  assert ((complete result).body = binary && result.rest = bytes get);
  List.iter (fun wire ->
    let input = bytes wire in
    let expected = feed (initial ()) input in
    for cut = 0 to List.length input do
      let first = feed (initial ()) (List.filteri (fun i _ -> i < cut) input) in
      let last = List.filteri (fun i _ -> i >= cut) input in
      assert (feed first.state (first.rest @ last) = expected)
    done)
    ["GET / HTTP/1.1\rX";
     framing "Transfer-Encoding: chunked\r\nContent-Length: 0\r\n";
     framing "Content-Length: 8193\r\n"];

  limited Body_bytes (framing "Content-Length: 8193\r\n");
  limited Body_bytes (framing "Content-Length: 999999999999999999999999999999\r\n");
  assert (List.length (complete (run (framing "Content-Length: 8192\r\n" ^ String.make 8192 'x'))).body = 8192);
  let short = "GET / HTTP/1.1\r\nHost: a\r\nX: " in
  let exact = short ^ String.make (16384 - String.length short - 4) 'x' ^ "\r\n\r\n" in
  let result = run (exact ^ get) in
  ignore (complete result);
  assert (total_consumed result.state = 16384 && result.rest = bytes get);
  let over = short ^ String.make 16384 'x' in
  let result = run over in
  assert ((status result.state) = Limit Message_bytes);
  assert (consumed (initial ()) result.state = 16384);
  assert (List.length result.rest = String.length over - 16384);
  assert ((status (feed (initial ()) [256]).state) = Malformed Invalid_byte);
  assert ((status (feed (initial ()) [-1]).state) = Malformed Invalid_byte);
  let boundary = bytes "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  let result = feed (initial ()) (boundary @ [256]) in
  ignore (complete result); assert (result.rest = [256]);
  let chunks = ["POST /submit?q=1 HTTP/1.1\r"; "\nHost: example.test\r\nContent-Len";
    "gth: 5\r\nX-Trace: yes\r\n\r"; "\na\000"; "\r"; "\nbGET /health HTTP/1.1\r\n";
    "Host: example.test\r\n\r\n"] in
  let rec dispatch state input requests =
    let result = feed state input in
    match (status result.state) with
    | Complete request -> dispatch (initial ()) result.rest (request :: requests)
    | Incomplete -> assert (result.rest = []); (result.state, requests)
    | Malformed _ | Limit _ -> assert false
  in
  let (_, requests) = List.fold_left (fun (state, requests) chunk ->
    dispatch state (bytes chunk) requests) (initial (), []) chunks in
  assert (List.length requests = 2);
  assert (List.rev requests = [request; complete (run get)])

let () =
  let check_splits wire cuts =
    let input = bytes wire in
    let expected = feed (initial ()) input in
    List.iter (fun cut ->
      let first = feed (initial ()) (List.filteri (fun i _ -> i < cut) input) in
      let rest = List.filteri (fun i _ -> i >= cut) input in
      assert (feed first.state (first.rest @ rest) = expected)) cuts;
    let suffix = [256; 13; 10; 0] in
    assert (feed expected.state suffix = {state = expected.state; rest = suffix})
  in
  let headers = ["X-First: one"; "Host: a"; "Content-Length: 0003";
    "X-First: two"; "content-length: 3"; "X-Last: three"] in
  let wire = "POST / HTTP/1.1\r\n" ^ String.concat "\r\n" headers ^ "\r\n\r\nabc" in
  assert ((complete (run wire)).headers = List.map bytes headers);
  check_splits wire (List.init (String.length wire + 1) Fun.id);
  malformed Invalid_host
    "GET / HTTP/1.1\r\nHost:\r\nContent-Length: bad\r\n\r\n";
  malformed Invalid_content_length
    "GET / HTTP/1.1\r\nContent-Length: bad\r\nHost:\r\n\r\n";
  let prefix = "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 8192\r\n\r\n" in
  let body = String.init 8192 (fun i -> Char.chr (i mod 256)) in
  let wire = prefix ^ body in
  check_splits wire [0; 1; String.length prefix - 1; String.length prefix;
    String.length prefix + 1; String.length wire - 1; String.length wire];
  let prefix = "GET / HTTP/1.1\r\nHost: a\r\nX: " in
  let wire = prefix ^ String.make (16384 - String.length prefix - 4) 'x'
    ^ "\r\n\r\n" in
  check_splits wire [0; 1; String.length prefix; 16380; 16381; 16382; 16383; 16384];
  let over = prefix ^ String.make 16384 'x' in
  check_splits over [0; String.length prefix; 16383; 16384; 16385]
