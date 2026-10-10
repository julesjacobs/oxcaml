module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module Budget = Wasm_execution_budget
let[@def] rec (length @ total) (bytes : B.bytes @ immutable) = match bytes with B.End -> C.Zero | B.Byte (_, rest) -> C.Succ (length rest)
let[@def] rec (suffix @ total) (tail : B.bytes @ immutable) (bytes : B.bytes @ immutable) = ghost_ (
  tail === bytes || match bytes with B.End -> false | B.Byte (_, rest) -> suffix tail rest)
let rec (suffix_transitive @ total) : (a : B.bytes) @ immutable -> (b : B.bytes) @ immutable -> (c : B.bytes) @ immutable ->
    {u : unit | suffix a b && suffix b c} -> {u : unit | suffix a c} @ ghost =
  fun a b c premise -> ghost_ (suffix_def b c; suffix_def a c;
    if b === c then () else match c with B.End -> () | B.Byte (_, rest) -> suffix_transitive a b rest ())
let rec (reflexive @ total) : (n : C.count) @ immutable -> {u : unit | Budget.le n n} @ ghost =
  fun n -> ghost_ (Budget.le_def n n; match n with C.Zero -> () | C.Succ rest -> reflexive rest)
let rec (transitive @ total) : (a : C.count) @ immutable -> (b : C.count) @ immutable -> (c : C.count) @ immutable ->
    {u : unit | Budget.le a b && Budget.le b c} -> {u : unit | Budget.le a c} @ ghost =
  fun a b c premise -> ghost_ (Budget.le_def a b; Budget.le_def b c; Budget.le_def a c;
    match a, b, c with C.Succ a, C.Succ b, C.Succ c -> transitive a b c () | _ -> ())
let rec (suffix_length @ total) : (tail : B.bytes) @ immutable -> (bytes : B.bytes) @ immutable ->
    {u : unit | suffix tail bytes} -> {u : unit | Budget.le (length tail) (length bytes)} @ ghost =
  fun tail bytes premise -> ghost_ (suffix_def tail bytes; length_def bytes;
    if tail === bytes then reflexive (length bytes) else
    match bytes with B.End -> () | B.Byte (_, rest) -> suffix_length tail rest (); Budget.weaken (length tail) (length rest) ())
let (u32_1 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match B.decode_1 bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (B.decode_1_def bytes;
    match bytes with B.End -> () | B.Byte (_, rest) ->
      suffix_def rest rest; suffix_def rest bytes;
      match B.decode_1 bytes with None -> () | Some (_, tail) -> suffix_def tail bytes)
let (u32_2 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match B.decode_2 bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (B.decode_2_def bytes;
    match bytes with B.End -> () | B.Byte (_, rest) ->
      suffix_def rest rest; suffix_def rest bytes;
      u32_1 rest;
      match B.decode_2 bytes with None -> () | Some (_, tail) -> suffix_def tail bytes)
let (u32_3 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match B.decode_3 bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (B.decode_3_def bytes;
    match bytes with B.End -> () | B.Byte (_, rest) ->
      suffix_def rest rest; suffix_def rest bytes;
      u32_2 rest;
      match B.decode_3 bytes with None -> () | Some (_, tail) -> suffix_def tail bytes)
let (u32_4 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match B.decode_4 bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (B.decode_4_def bytes;
    match bytes with B.End -> () | B.Byte (_, rest) ->
      suffix_def rest rest; suffix_def rest bytes;
      u32_3 rest;
      match B.decode_4 bytes with None -> () | Some (_, tail) -> suffix_def tail bytes)
let (u32_5 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match B.decode_5 bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (B.decode_5_def bytes;
    match bytes with B.End -> () | B.Byte (_, rest) ->
      suffix_def rest rest; suffix_def rest bytes;
      u32_4 rest;
      match B.decode_5 bytes with None -> () | Some (_, tail) -> suffix_def tail bytes)
let (signed_5 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match Wasm_i32.decode bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (Wasm_i32.decode_def bytes;
    match bytes with B.End -> () | B.Byte (_, r1) ->
    match r1 with B.End -> () | B.Byte (_, r2) ->
    match r2 with B.End -> () | B.Byte (_, r3) ->
    match r3 with B.End -> () | B.Byte (_, r4) ->
    match r4 with B.End -> () | B.Byte (_, r5) ->
    suffix_def r5 r5;
suffix_def r5 r4; suffix_def r5 r3; suffix_def r5 r2; suffix_def r5 r1; suffix_def r5 bytes)
let (signed_10 @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match Wasm_i64.decode bytes with None -> true | Some (_, tail) -> suffix tail bytes} @ ghost =
  fun bytes -> ghost_ (Wasm_i64.decode_def bytes;
    match bytes with B.End -> () | B.Byte (_, r1) ->
    match r1 with B.End -> () | B.Byte (_, r2) ->
    match r2 with B.End -> () | B.Byte (_, r3) ->
    match r3 with B.End -> () | B.Byte (_, r4) ->
    match r4 with B.End -> () | B.Byte (_, r5) ->
    match r5 with B.End -> () | B.Byte (_, r6) ->
    match r6 with B.End -> () | B.Byte (_, r7) ->
    match r7 with B.End -> () | B.Byte (_, r8) ->
    match r8 with B.End -> () | B.Byte (_, r9) ->
    match r9 with B.End -> () | B.Byte (_, r10) ->
    suffix_def r10 r10;
suffix_def r10 r9; suffix_def r10 r8; suffix_def r10 r7; suffix_def r10 r6; suffix_def r10 r5; suffix_def r10 r4; suffix_def r10 r3; suffix_def r10 r2; suffix_def r10 r1; suffix_def r10 bytes)
let (progress @ total) : (bytes : B.bytes) @ immutable ->
    {u : unit | match I.decode bytes with None -> true | Some (_, tail) -> Budget.le (C.Succ (length tail)) (length bytes)} @ ghost =
  fun bytes -> ghost_ (I.decode_def bytes; length_def bytes;
    match bytes with B.End -> () | B.Byte (op, payload) ->
      suffix_def payload payload;
      (match op with
      | 12 | 13 | 16 | 32 | 33 | 34 | 35 | 36 -> u32_5 payload
      | 65 -> signed_5 payload
      | 66 -> signed_10 payload
      | 2 | 3 | 4 ->
        (match payload with B.End -> () | B.Byte (_, rest) -> suffix_def rest rest; suffix_def rest payload)
      | 17 ->
        u32_5 payload;
        (match B.decode_5 payload with
        | Some (_, (B.Byte (_, rest) as middle)) ->
          suffix_def rest rest; suffix_def rest middle; suffix_transitive rest middle payload ()
        | _ -> ())
      | 40 | 41 | 54 | 55 ->
        u32_5 payload;
        (match B.decode_5 payload with
        | Some (_, middle) ->
          u32_5 middle;
          (match B.decode_5 middle with Some (_, rest) -> suffix_transitive rest middle payload () | _ -> ())
        | _ -> ())
      | _ -> ());
      match I.decode bytes with
      | None -> ()
      | Some (_, tail) -> suffix_length tail payload (); Budget.le_def (C.Succ (length tail)) (length bytes))
let[@def] rec (decode_fuel @ total) (fuel : C.count @ immutable) (bytes : B.bytes @ immutable) : C.t option @ immutable =
  match bytes with
  | B.End -> Some C.Empty
  | B.Byte _ -> match fuel with
    | C.Zero -> None
    | C.Succ rest -> match I.decode bytes with
      | None -> None
      | Some (instruction, tail) -> match decode_fuel rest tail with
        | None -> None | Some code -> Some (C.Next (instruction, code))
let[@def] (decode @ total) (bytes : B.bytes @ immutable) = decode_fuel (length bytes) bytes
let rec (roundtrip @ total) : (code : C.t) @ immutable -> (bytes : B.bytes) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | C.decode (C.length code) bytes === Some (code, B.End) && Budget.le (length bytes) fuel} ->
    {u : unit | decode_fuel fuel bytes === Some code} @ ghost =
  fun code bytes fuel premise -> ghost_ (
    C.length_def code; C.decode_def (C.length code) bytes; decode_fuel_def fuel bytes; I.decode_def bytes;
    match code with
    | C.Empty -> ()
    | C.Next (_, rest) ->
      progress bytes;
      match I.decode bytes with
      | None -> ()
      | Some (_, tail) ->
        transitive (C.Succ (length tail)) (length bytes) fuel ();
        Budget.le_def (C.Succ (length tail)) fuel;
        match fuel with C.Zero -> () | C.Succ fuel ->
          Budget.le_def (C.Succ (length tail)) (C.Succ fuel);
          let proof : {u : unit | C.decode (C.length rest) tail === Some (rest, B.End) && Budget.le (length tail) fuel} = () in
          roundtrip rest tail fuel proof)

let (encode @ total) : (code : C.t) @ immutable -> {bytes : B.bytes | decode bytes === Some code} @ immutable =
  fun code ->
    let bytes = C.encode code B.End in
    ghost_ (reflexive (length bytes); roundtrip code bytes (length bytes) (); decode_def bytes); bytes
