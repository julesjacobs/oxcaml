module B = Wasm_u32
module F = Wasm_functions
module I = Wasm_instruction
module C = Wasm_code
module Stream = Wasm_instruction_stream
module Budget = Wasm_execution_budget
let[@def] rec (count @ total) (indices : F.table @ immutable) : B.u32 option =
  match indices with
  | F.No_elements -> Some 0
  | F.Element (None, _) -> None
  | F.Element (Some _, rest) ->
    match count rest with None -> None | Some n -> if n = 4294967295 then None else Some (n + 1)
let[@def] rec (decode_entries @ total) (fuel : C.count @ immutable) (number : B.u32) (bytes : B.bytes @ immutable) : (F.table * B.bytes) option @ immutable =
  if number = 0 then Some (F.No_elements, bytes) else
  match fuel with C.Zero -> None | C.Succ fuel ->
    match B.decode_5 bytes with None -> None | Some (index, rest) ->
      match decode_entries fuel (number - 1) rest with None -> None | Some (indices, tail) ->
        Some (F.Element (Some index, indices), tail)
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with None -> None | Some (number, rest) -> decode_entries (Stream.length rest) number rest
let (progress @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match B.decode_5 bytes with None -> true | Some (_, tail) -> Budget.le (C.Succ (Stream.length tail)) (Stream.length bytes)} @ ghost =
  fun bytes -> ghost_ (B.decode_5_def bytes; Stream.length_def bytes;
    match bytes with B.End -> () | B.Byte (_, rest) ->
      Stream.suffix_def rest rest; Stream.u32_4 rest;
      match B.decode_5 bytes with None -> () | Some (_, tail) ->
        Stream.suffix_length tail rest ();
        Budget.le_def (C.Succ (Stream.length tail)) (Stream.length bytes))
let[@def] rec (encoded @ total) (indices : F.table @ immutable) (bytes : B.bytes @ immutable) (tail : B.bytes @ immutable) = ghost_ (
  match indices with
  | F.No_elements -> bytes === tail
  | F.Element (None, _) -> false
  | F.Element (Some index, rest) -> match B.decode_5 bytes with None -> false | Some (value, suffix) -> value = index && encoded rest suffix tail)
let rec (encode_entries @ total) : (indices : F.table) @ immutable -> (tail : B.bytes) @ immutable -> (number : B.u32) ->
    {u : unit | count indices === Some number} -> {bytes : B.bytes | encoded indices bytes tail} @ immutable =
  fun indices tail number premise ->
    ghost_ (count_def indices);
    match indices with
    | F.No_elements -> ghost_ (encoded_def indices tail tail); tail
    | F.Element (None, _) -> tail
    | F.Element (Some index, rest) ->
      let bytes = B.encode_u32 index (encode_entries rest tail (number - 1) ()) in
      ghost_ (encoded_def indices bytes tail); bytes
let rec (roundtrip @ total) : (indices : F.table) @ immutable -> (tail : B.bytes) @ immutable -> (number : B.u32) ->
    (fuel : C.count) @ immutable -> (bytes : B.bytes) @ immutable ->
    {u : unit | count indices === Some number && encoded indices bytes tail && Budget.le (Stream.length bytes) fuel} ->
    {u : unit | decode_entries fuel number bytes === Some (indices, tail)} @ ghost =
  fun indices tail number fuel bytes premise -> ghost_ (
    count_def indices; encoded_def indices bytes tail; decode_entries_def fuel number bytes;
    match indices with
    | F.No_elements | F.Element (None, _) -> ()
    | F.Element (Some _, rest) ->
      progress bytes;
      match B.decode_5 bytes with None -> () | Some (_, suffix) ->
        Stream.transitive (C.Succ (Stream.length suffix)) (Stream.length bytes) fuel ();
        Budget.le_def (C.Succ (Stream.length suffix)) fuel;
        match fuel with C.Zero -> () | C.Succ fuel -> roundtrip rest tail (number - 1) fuel suffix ())
let (encode @ total) : (indices : F.table) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> count indices === None | Some bytes -> not (count indices === None) && decode bytes === Some (indices, tail)} @ immutable =
  fun indices tail -> match count indices with
  | None -> None
  | Some number ->
    let payload = encode_entries indices tail number () in
    ghost_ (Stream.reflexive (Stream.length payload); roundtrip indices tail number (Stream.length payload) payload ());
    let bytes = B.encode_u32 number payload in
    ghost_ (decode_def bytes); Some bytes

let[@def] (decode_element @ total) (bytes : B.bytes @ immutable) : (F.table * B.bytes) option @ immutable =
  match B.decode_5 bytes with
  | Some (1, segments) ->
    (match B.decode_5 segments with
    | Some (0, offset) ->
      (match I.decode offset with
      | Some (I.I32_const 0, stop) ->
        (match I.decode stop with Some (I.Plain I.End, payload) -> decode payload | _ -> None)
      | _ -> None)
    | _ -> None)
  | _ -> None
let (encode_element @ total) : (data : F.table) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> count data === None | Some bytes -> not (count data === None) && decode_element bytes === Some (data, tail)} @ immutable =
  fun data tail ->
    match encode data tail with
    | None -> None
    | Some payload ->
      let stop = I.encode (I.Plain I.End) payload in
      let offset = I.encode (I.I32_const 0) stop in
      let bytes = B.encode_u32 1 (B.encode_u32 0 offset) in
      ghost_ (decode_element_def bytes); Some bytes
