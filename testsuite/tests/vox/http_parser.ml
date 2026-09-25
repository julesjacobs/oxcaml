(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_http.mli vox_http.ml http_parser.ml";
 { bytecode; }
 { native; }
*)
open Vox_http

let (decode_serialized @ total) (request : request) (suffix : bytes) :
    {result : result | if well_formed request then
      result.state.core === Complete request && result.rest === suffix else true} =
  let input = S.append (serialize request) suffix in
  let refine_ result = parse input in
  ghost_ (roundtrip request suffix);
  refine_ result

let bytes text = List.init (String.length text) (fun i -> Char.code text.[i])
let text bs = String.of_seq (List.to_seq (List.map Char.chr bs))
let run text = feed (initial ()) (bytes text)
let complete result = match result.state.core with Complete request -> request | _ -> assert false
let malformed expected wire =
  match (run wire).state.core with Malformed actual -> assert (actual = expected) | _ -> assert false
let limited expected wire =
  match (run wire).state.core with Limit actual -> assert (actual = expected) | _ -> assert false
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
  let refine_ result = decode_serialized request (bytes get) in
  assert (complete result = request);
  assert (result.rest = bytes get);
  assert (consumed (initial ()) result.state = List.length wire);
  assert ((complete (feed (initial ()) result.rest)).request_line = bytes "GET /health HTTP/1.1");
  let joined = wire @ bytes get in
  let refine_ parsed = parse joined in
  assert (parsed = result);
  for cut = 0 to List.length joined do
    let left = List.filteri (fun i _ -> i < cut) joined in
    let right = List.filteri (fun i _ -> i >= cut) joined in
    let first = feed (initial ()) left in
    assert (feed first.state (first.rest @ right) = feed (initial ()) joined)
  done;
  let state = List.fold_left (fun state byte ->
    let result = feed state [byte] in assert (result.rest = []); result.state)
    (initial ()) wire in
  assert (state = (feed (initial ()) wire).state);
  for cut = 0 to List.length wire - 1 do
    let prefix = List.filteri (fun i _ -> i < cut) wire in
    let result = feed (initial ()) prefix in
    assert (not (terminal result.state.core));
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
  assert (result.state.budget = 0 && result.rest = bytes get);
  let over = short ^ String.make 16384 'x' in
  let result = run over in
  assert (result.state.core = Limit Message_bytes);
  assert (consumed (initial ()) result.state = 16384);
  assert (List.length result.rest = String.length over - 16384);
  assert ((feed (initial ()) [256]).state.core = Malformed Invalid_byte);
  assert ((feed (initial ()) [-1]).state.core = Malformed Invalid_byte);
  let boundary = bytes "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  let result = feed (initial ()) (boundary @ [256]) in
  ignore (complete result); assert (result.rest = [256]);
  let chunks = ["POST /submit?q=1 HTTP/1.1\r"; "\nHost: example.test\r\nContent-Len";
    "gth: 5\r\nX-Trace: yes\r\n\r"; "\na\000"; "\r"; "\nbGET /health HTTP/1.1\r\n";
    "Host: example.test\r\n\r\n"] in
  let rec dispatch state input requests =
    let result = feed state input in
    match result.state.core with
    | Complete request -> dispatch (initial ()) result.rest (request :: requests)
    | Line _ | Body _ -> assert (result.rest = []); (result.state, requests)
    | Malformed _ | Limit _ -> assert false
  in
  let (_, requests) = List.fold_left (fun (state, requests) chunk ->
    dispatch state (bytes chunk) requests) (initial (), []) chunks in
  assert (List.length requests = 2);
  assert (List.rev requests = [request; complete (run get)])
