module B = Wasm_u32
module L = Hmc_linear_bytes
let rec (overlay @ total) : (payload : B.bytes) @ immutable -> (before : B.bytes) @ immutable ->
    (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    {u : unit | L.overlay payload before left && L.overlay payload before right} ->
    {u : unit | left === right} @ ghost = fun payload before left right premise -> ghost_ (
  L.overlay_def payload before left; L.overlay_def payload before right;
  match payload, before, left, right with
  | B.Byte (_, rest), B.Byte (_, old), B.Byte (_, a), B.Byte (_, b) -> overlay rest old a b ()
  | _ -> ())
let rec (updated @ total) : (before : B.bytes) @ immutable -> (address : B.u32) -> (payload : B.bytes) @ immutable ->
    (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    {u : unit | L.updated before address payload left && L.updated before address payload right} ->
    {u : unit | left === right} @ ghost = fun before address payload left right premise -> ghost_ (
  L.updated_def before address payload left; L.updated_def before address payload right;
  if address = 0 then overlay payload before left right () else
    match before, left, right with
    | B.Byte (_, rest), B.Byte (_, a), B.Byte (_, b) -> updated rest (address - 1) payload a b ()
    | _ -> ())
