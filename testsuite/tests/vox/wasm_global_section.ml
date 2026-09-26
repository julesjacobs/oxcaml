module B = Wasm_u32
module S = Wasm_scalar
module G = Wasm_globals
module E = Wasm_global_entry
module C = Wasm_code
module Stream = Wasm_instruction_stream
module Budget = Wasm_execution_budget
let[@def] rec (count @ total) (values : S.stack @ immutable) (permissions : G.permissions @ immutable) : B.u32 option =
  match values, permissions with
  | S.Empty, G.Empty -> Some 0
  | S.Push (_, rest), G.Global (_, permissions) ->
    (match count rest permissions with None -> None | Some n -> if n = 4294967295 then None else Some (n + 1))
  | _ -> None
let[@def] rec (decode_entries @ total) (fuel : C.count @ immutable) (number : B.u32) (bytes : B.bytes @ immutable) : (G.t * B.bytes) option @ immutable =
  if number = 0 then Some ({G.values = S.Empty; permissions = G.Empty}, bytes) else
  match fuel with
  | C.Zero -> None
  | C.Succ fuel -> match E.decode bytes with
    | None -> None
    | Some (entry, rest) -> match decode_entries fuel (number - 1) rest with
      | None -> None
      | Some (globals, tail) -> Some ({G.values = S.Push (entry.E.value, globals.G.values);
          permissions = G.Global (entry.E.mutable_, globals.G.permissions)}, tail)
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match B.decode_5 bytes with None -> None | Some (number, rest) -> decode_entries (Stream.length rest) number rest
let (progress @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match E.decode bytes with None -> true | Some (_, tail) -> Budget.le (C.Succ (Stream.length tail)) (Stream.length bytes)} @ ghost =
  fun bytes -> ghost_ (
    E.decode_def bytes; Stream.length_def bytes;
    match bytes with B.End -> () | B.Byte (_, r1) ->
      Stream.length_def r1;
      match r1 with B.End -> () | B.Byte (_, rest) ->
        Stream.progress rest;
        match Wasm_instruction.decode rest with None -> () | Some (_, end_bytes) ->
          Stream.progress end_bytes;
          match Wasm_instruction.decode end_bytes with None -> () | Some (_, tail) ->
            Stream.reflexive (Stream.length end_bytes);
            Budget.weaken (Stream.length end_bytes) (Stream.length end_bytes) ();
            Stream.transitive (Stream.length end_bytes) (C.Succ (Stream.length end_bytes)) (Stream.length rest) ();
            Stream.transitive (C.Succ (Stream.length tail)) (Stream.length end_bytes) (Stream.length rest) ();
            Budget.weaken (C.Succ (Stream.length tail)) (Stream.length rest) ();
            Budget.weaken (C.Succ (Stream.length tail)) (C.Succ (Stream.length rest)) ())
let[@def] rec (encoded @ total) (values : S.stack @ immutable) (permissions : G.permissions @ immutable)
    (bytes : B.bytes @ immutable) (tail : B.bytes @ immutable) = ghost_ (
  match values, permissions with
  | S.Empty, G.Empty -> bytes === tail
  | S.Push (value, rest), G.Global (mutable_, remaining) ->
    (match E.decode bytes with None -> false | Some (entry, suffix) ->
      entry.E.value === value && entry.E.mutable_ = mutable_ && encoded rest remaining suffix tail)
  | _ -> false)
let rec (encode_entries @ total) : (values : S.stack) @ immutable -> (permissions : G.permissions) @ immutable ->
    (tail : B.bytes) @ immutable -> (number : B.u32) -> {u : unit | count values permissions === Some number} -> {bytes : B.bytes | encoded values permissions bytes tail} @ immutable =
  fun values permissions tail number premise ->
    ghost_ (count_def values permissions);
    match values, permissions with
    | S.Empty, G.Empty -> ghost_ (encoded_def values permissions tail tail); tail
    | S.Push (value, rest), G.Global (mutable_, permissions) ->
      let bytes = E.encode {E.mutable_; value} (encode_entries rest permissions tail (number - 1) ()) in
      ghost_ (encoded_def values (G.Global (mutable_, permissions)) bytes tail); bytes
    | _ -> tail
let rec (roundtrip @ total) : (values : S.stack) @ immutable -> (permissions : G.permissions) @ immutable ->
    (tail : B.bytes) @ immutable -> (number : B.u32) -> (fuel : C.count) @ immutable ->
    (bytes : B.bytes) @ immutable ->
    {u : unit | count values permissions === Some number &&
      encoded values permissions bytes tail && Budget.le (Stream.length bytes) fuel} ->
    {u : unit | decode_entries fuel number bytes === Some ({G.values; permissions}, tail)} @ ghost =
  fun values permissions tail number fuel bytes premise -> ghost_ (
    count_def values permissions; encoded_def values permissions bytes tail;
    decode_entries_def fuel number bytes;
    match values, permissions with
    | S.Empty, G.Empty -> ()
    | S.Push (_, rest), G.Global (_, remaining) ->
      progress bytes;
      (match E.decode bytes with None -> () | Some (_, suffix) ->
        Stream.transitive (C.Succ (Stream.length suffix)) (Stream.length bytes) fuel ();
        Budget.le_def (C.Succ (Stream.length suffix)) fuel;
        match fuel with C.Zero -> () | C.Succ fuel -> roundtrip rest remaining tail (number - 1) fuel suffix ())
    | _ -> ())

let (encode @ total) : (globals : G.t) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with None -> count globals.G.values globals.G.permissions === None
      | Some bytes -> not (count globals.G.values globals.G.permissions === None) && decode bytes === Some (globals, tail)} @ immutable =
  fun globals tail -> match count globals.G.values globals.G.permissions with
  | None -> None
  | Some number ->
    let payload = encode_entries globals.G.values globals.G.permissions tail number () in
    ghost_ (Stream.reflexive (Stream.length payload); roundtrip globals.G.values globals.G.permissions tail number (Stream.length payload) payload ());
    let bytes = B.encode_u32 number payload in
    ghost_ (decode_def bytes); Some bytes
