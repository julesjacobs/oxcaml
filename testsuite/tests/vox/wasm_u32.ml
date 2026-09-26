type byte = {b : int | 0 <= b && b < 256}
type bytes = End | Byte of byte * bytes [@@inductive]
type u32 = {n : int | 0 <= n && n < 4294967296}

let[@def] (decode_1 @ total) (input : bytes @ immutable) :
    {r : (int * bytes) option | match r with
      | None -> true | Some (n, _) -> 0 <= n && n < 16} @ immutable =
  match input with
  | End -> None
  | Byte (b, rest) ->
    if b < 16 then Some (b, rest) else None

let[@def] (decode_2 @ total) (input : bytes @ immutable) :
    {r : (int * bytes) option | match r with
      | None -> true | Some (n, _) -> 0 <= n && n < 2048} @ immutable =
  match input with
  | End -> None
  | Byte (b, rest) ->
    if b < 128 then Some (b, rest)
    else match decode_1 rest with
      | None -> None
      | Some (n, tail) -> Some (b - 128 + 128 * n, tail)

let[@def] (decode_3 @ total) (input : bytes @ immutable) :
    {r : (int * bytes) option | match r with
      | None -> true | Some (n, _) -> 0 <= n && n < 262144} @ immutable =
  match input with
  | End -> None
  | Byte (b, rest) ->
    if b < 128 then Some (b, rest)
    else match decode_2 rest with
      | None -> None
      | Some (n, tail) -> Some (b - 128 + 128 * n, tail)

let[@def] (decode_4 @ total) (input : bytes @ immutable) :
    {r : (int * bytes) option | match r with
      | None -> true | Some (n, _) -> 0 <= n && n < 33554432} @ immutable =
  match input with
  | End -> None
  | Byte (b, rest) ->
    if b < 128 then Some (b, rest)
    else match decode_3 rest with
      | None -> None
      | Some (n, tail) -> Some (b - 128 + 128 * n, tail)

let[@def] (decode_5 @ total) (input : bytes @ immutable) :
    {r : (int * bytes) option | match r with
      | None -> true | Some (n, _) -> 0 <= n && n < 4294967296} @ immutable =
  match input with
  | End -> None
  | Byte (b, rest) ->
    if b < 128 then Some (b, rest)
    else match decode_4 rest with
      | None -> None
      | Some (n, tail) -> Some (b - 128 + 128 * n, tail)

external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"
external remainder : int -> {d : int | d <> 0} -> int @@ total = "%modint"

let (encode_u32 @ total) (n : u32) (tail : bytes @ immutable) :
    {out : bytes | decode_5 out === Some (n, tail)} @ immutable =
  let b0 = remainder n 128 + 128 in
  let q1 = divide n 128 in
  let b1 = remainder q1 128 + 128 in
  let q2 = divide q1 128 in
  let b2 = remainder q2 128 + 128 in
  let q3 = divide q2 128 in
  let b3 = remainder q3 128 + 128 in
  let b4 = divide q3 128 in
  let s4 = Byte (b4, tail) in
  let s3 = Byte (b3, s4) in
  let s2 = Byte (b2, s3) in
  let s1 = Byte (b1, s2) in
  let out = Byte (b0, s1) in
  ghost_ (
    decode_1_def s4; decode_2_def s3; decode_3_def s2;
    decode_4_def s1; decode_5_def out);
  out
