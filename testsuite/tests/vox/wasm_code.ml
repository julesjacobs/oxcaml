module B = Wasm_u32
module I = Wasm_instruction

type count = Zero | Succ of count [@@inductive]
type t = Empty | Next of I.t * t [@@inductive]

let[@def] rec (length @ total) (code : t @ immutable) : count @ immutable =
  match code with Empty -> Zero | Next (_, rest) -> Succ (length rest)

let[@def] rec (decode @ total) (count : count @ immutable)
    (bytes : B.bytes @ immutable) : (t * B.bytes) option @ immutable =
  match count with
  | Zero -> Some (Empty, bytes)
  | Succ count ->
    match I.decode bytes with
    | None -> None
    | Some (instruction, rest) ->
      match decode count rest with
      | None -> None
      | Some (code, tail) -> Some (Next (instruction, code), tail)

let rec (encode @ total) : (code : t) @ immutable ->
    (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode (length code) bytes === Some (code, tail)} @ immutable =
  fun code tail ->
    match code with
    | Empty -> ghost_ (length_def code; decode_def Zero tail); tail
    | Next (instruction, rest) ->
      let suffix = encode rest tail in
      let bytes = I.encode instruction suffix in
      ghost_ (length_def code; decode_def (length code) bytes);
      bytes
