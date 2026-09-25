open Vox_http

let bytes text = List.init (String.length text) (fun i -> Char.code text.[i])

let text bytes = String.of_seq (List.to_seq (List.map Char.chr bytes))

let chunks =
  [ "POST /submit HTTP/1.1\r";
    "\nHost: example.test\r\nContent-Len";
    "gth: 5\r\n\r";
    "\nhe";
    "lloGET /health HTTP/1.1\r\nHost: ex";
    "ample.test\r\n\r\n" ]

let rec deliver state input =
  let result = feed state input in
  match result.state.core with
  | Complete request ->
    Printf.printf "complete: %s; body=%S; consumed=%d; suffix=%d bytes\n"
      (text request.request_line)
      (text request.body)
      (consumed (initial ()) result.state)
      (List.length result.rest);
    if result.rest = [] then initial () else deliver (initial ()) result.rest
  | Line _ | Body _ ->
    Printf.printf "incomplete: consumed=%d\n"
      (consumed (initial ()) result.state);
    result.state
  | Malformed _ -> failwith "malformed request"
  | Limit _ -> failwith "resource limit"

let () =
  ignore
    (List.fold_left
       (fun state chunk ->
         Printf.printf "chunk: %S\n" chunk;
         deliver state (bytes chunk))
       (initial ()) chunks)
