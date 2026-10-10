module B = Wasm_u32
module F = Wasm_functions
module Body = Wasm_function_body
module Frame = Wasm_framing
module C = Wasm_code
module Stream = Wasm_instruction_stream
module Budget = Wasm_execution_budget
module N = Wasm_nesting
type bodies = Empty | Body of Body.t * bodies [@@inductive]
let[@def] rec (count @ total) (bodies : bodies @ immutable) : B.u32 option =
  match bodies with Empty -> Some 0 | Body (_, rest) ->
    match count rest with None -> None | Some n -> if n = 4294967295 then None else Some (n + 1)
let[@def] rec (structured @ total) (bodies : bodies @ immutable) =
  match bodies with Empty -> true | Body (body, rest) -> N.structured body.Body.code && structured rest
let[@def] rec (decode_entries @ total) (fuel : C.count @ immutable) (number : B.u32) (bytes : B.bytes @ immutable) : (bodies * B.bytes) option @ immutable =
  if number = 0 then Some (Empty, bytes) else
  match fuel with C.Zero -> None | C.Succ fuel ->
    match Body.decode bytes with None -> None | Some (body, rest) ->
      match decode_entries fuel (number - 1) rest with None -> None | Some (bodies, tail) -> Some (Body (body, bodies), tail)
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with None -> None | Some (number, rest) -> decode_entries (Stream.length rest) number rest
let rec (split_suffix @ total) : (bytes : B.bytes) @ immutable -> (number : B.u32) ->
    {u : unit | match Frame.split bytes number with None -> true | Some (_, tail) -> Stream.suffix tail bytes} @ ghost =
  fun bytes number -> ghost_ (Frame.split_def bytes number;
    if number = 0 then Stream.suffix_def bytes bytes else
      match bytes with B.End -> () | B.Byte (_, rest) ->
        split_suffix rest (number - 1);
        match Frame.split bytes number with None -> () | Some (_, tail) -> Stream.suffix_def tail bytes)
let (progress @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match Body.decode bytes with None -> true | Some (_, tail) -> Budget.le (C.Succ (Stream.length tail)) (Stream.length bytes)} @ ghost =
  fun bytes -> ghost_ (Body.decode_def bytes; Frame.decode_def bytes;
    Wasm_index_vector.progress bytes;
    match B.decode_5 bytes with None -> () | Some (number, rest) ->
      split_suffix rest number;
      match Frame.split rest number with None -> () | Some (_, tail) ->
        Stream.suffix_length tail rest ();
        Budget.le_def (C.Succ (Stream.length tail)) (C.Succ (Stream.length rest));
        Stream.transitive (C.Succ (Stream.length tail)) (C.Succ (Stream.length rest)) (Stream.length bytes) ())
let[@def] rec (encoded @ total) (bodies : bodies @ immutable) (bytes : B.bytes @ immutable) (tail : B.bytes @ immutable) = ghost_ (
  match bodies with Empty -> bytes === tail | Body (body, rest) ->
    match Body.decode bytes with None -> false | Some (head, suffix) -> head === body && encoded rest suffix tail)
let[@def] rec (encodable_entries @ total) (bodies : bodies @ immutable) = ghost_ (
  match bodies with Empty -> true | Body (body, rest) -> Body.encodable body && encodable_entries rest)
let[@def] (encodable @ total) (bodies : bodies @ immutable) = ghost_ (
  not (count bodies === None) && encodable_entries bodies)
let rec (encode_entries @ total) : (bodies : bodies) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | structured bodies} ->
    {out : B.bytes option | match out with None -> not (encodable_entries bodies)
      | Some bytes -> encodable_entries bodies && encoded bodies bytes tail} @ immutable =
  fun bodies tail premise ->
    ghost_ (structured_def bodies; encodable_entries_def bodies);
    match bodies with
    | Empty -> ghost_ (encoded_def bodies tail tail); Some tail
    | Body (body, rest) -> match encode_entries rest tail () with
      | None -> None
      | Some suffix -> match Body.encode body suffix () with
        | None -> None | Some bytes -> ghost_ (encoded_def bodies bytes tail); Some bytes
let rec (roundtrip @ total) : (bodies : bodies) @ immutable -> (tail : B.bytes) @ immutable -> (number : B.u32) ->
    (fuel : C.count) @ immutable -> (bytes : B.bytes) @ immutable ->
    {u : unit | count bodies === Some number && encoded bodies bytes tail && Budget.le (Stream.length bytes) fuel} ->
    {u : unit | decode_entries fuel number bytes === Some (bodies, tail)} @ ghost =
  fun bodies tail number fuel bytes premise -> ghost_ (
    count_def bodies; encoded_def bodies bytes tail; decode_entries_def fuel number bytes;
    match bodies with Empty -> () | Body (_, rest) ->
      progress bytes;
      match Body.decode bytes with None -> () | Some (_, suffix) ->
        Stream.transitive (C.Succ (Stream.length suffix)) (Stream.length bytes) fuel ();
        Budget.le_def (C.Succ (Stream.length suffix)) fuel;
        match fuel with C.Zero -> () | C.Succ fuel -> roundtrip rest tail (number - 1) fuel suffix ())
let (encode @ total) : (bodies : bodies) @ immutable -> (tail : B.bytes) @ immutable -> {u : unit | structured bodies} ->
    {out : B.bytes option | match out with None -> not (encodable bodies) | Some bytes -> encodable bodies && decode bytes === Some (bodies, tail)} @ immutable =
  fun bodies tail premise -> ghost_ (encodable_def bodies); match count bodies with None -> None | Some number ->
    match encode_entries bodies tail () with None -> None | Some payload ->
      ghost_ (Stream.reflexive (Stream.length payload); roundtrip bodies tail number (Stream.length payload) payload ());
      let bytes = B.encode_u32 number payload in ghost_ (decode_def bytes); Some bytes
