module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module N = Wasm_nesting

type reversed = Empty | Instruction of I.t * reversed
  | Block of T.code * reversed | Loop of T.code * reversed
  | If of T.code * T.code * reversed [@@inductive]
type frames = Root | In_block of reversed * frames | In_loop of reversed * frames
  | In_then of reversed * frames | In_else of T.code * reversed * frames [@@inductive]
let[@def] rec (finish @ total) (prefix : reversed @ immutable) (tail : T.code @ immutable) : T.code @ immutable =
  match prefix with
  | Empty -> tail
  | Instruction (instruction, rest) -> finish rest (T.Instruction (instruction, tail))
  | Block (body, rest) -> finish rest (T.Block (body, tail))
  | Loop (body, rest) -> finish rest (T.Loop (body, tail))
  | If (yes, no, rest) -> finish rest (T.If (yes, no, tail))
let[@def] rec (push @ total) (code : T.code @ immutable) (prefix : reversed @ immutable) : reversed @ immutable =
  match code with
  | T.Empty -> prefix
  | T.Instruction (instruction, rest) -> push rest (Instruction (instruction, prefix))
  | T.Block (body, rest) -> push rest (Block (body, prefix))
  | T.Loop (body, rest) -> push rest (Loop (body, prefix))
  | T.If (yes, no, rest) -> push rest (If (yes, no, prefix))
let[@def] rec (parse @ total) (input : C.t @ immutable) (prefix : reversed @ immutable)
    (frames : frames @ immutable) : T.code option @ immutable =
  match input with
  | C.Empty -> (match frames with Root -> Some (finish prefix T.Empty) | _ -> None)
  | C.Next (I.Block, rest) -> parse rest Empty (In_block (prefix, frames))
  | C.Next (I.Loop, rest) -> parse rest Empty (In_loop (prefix, frames))
  | C.Next (I.If, rest) -> parse rest Empty (In_then (prefix, frames))
  | C.Next (I.Plain I.Else, rest) -> (match frames with
    | In_then (parent, outer) -> parse rest Empty (In_else (finish prefix T.Empty, parent, outer))
    | _ -> None)
  | C.Next (I.Plain I.End, rest) -> (match frames with
    | Root -> None
    | In_block (parent, outer) -> parse rest (Block (finish prefix T.Empty, parent)) outer
    | In_loop (parent, outer) -> parse rest (Loop (finish prefix T.Empty, parent)) outer
    | In_then (parent, outer) -> parse rest (If (finish prefix T.Empty, T.Empty, parent)) outer
    | In_else (yes, parent, outer) -> parse rest (If (yes, finish prefix T.Empty, parent)) outer)
  | C.Next (instruction, rest) -> parse rest (Instruction (instruction, prefix)) frames
let[@def] (decode @ total) (input : C.t @ immutable) = parse input Empty Root
let rec (finish_push @ total) : (code : T.code) @ immutable -> (prefix : reversed) @ immutable ->
    {u : unit | finish (push code prefix) T.Empty === finish prefix code} @ ghost =
  fun code prefix -> ghost_ (
    push_def code prefix;
    match code with
    | T.Empty -> ()
    | T.Instruction (instruction, rest) ->
      finish_push rest (Instruction (instruction, prefix)); finish_def (Instruction (instruction, prefix)) rest
    | T.Block (body, rest) -> finish_push rest (Block (body, prefix)); finish_def (Block (body, prefix)) rest
    | T.Loop (body, rest) -> finish_push rest (Loop (body, prefix)); finish_def (Loop (body, prefix)) rest
    | T.If (yes, no, rest) -> finish_push rest (If (yes, no, prefix)); finish_def (If (yes, no, prefix)) rest)
let rec (splice @ total) : (code : T.code) @ immutable -> (tail : C.t) @ immutable ->
    (prefix : reversed) @ immutable -> (frames : frames) @ immutable -> {u : unit | N.structured code} ->
    {u : unit | parse (T.flatten code tail) prefix frames === parse tail (push code prefix) frames} @ ghost =
  fun code tail prefix frames premise -> ghost_ (
    N.structured_def code; T.flatten_def code tail; push_def code prefix;
    match code with
    | T.Empty -> ()
    | T.Instruction (instruction, rest) ->
      N.ordinary_def instruction; parse_def (T.flatten code tail) prefix frames;
      splice rest tail (Instruction (instruction, prefix)) frames ()
    | T.Block (body, rest) ->
      let close = C.Next (I.Plain I.End, T.flatten rest tail) in
      parse_def (T.flatten code tail) prefix frames;
      splice body close Empty (In_block (prefix, frames)) ();
      finish_push body Empty; finish_def Empty body;
      parse_def close (push body Empty) (In_block (prefix, frames));
      splice rest tail (Block (body, prefix)) frames ()
    | T.Loop (body, rest) ->
      let close = C.Next (I.Plain I.End, T.flatten rest tail) in
      parse_def (T.flatten code tail) prefix frames;
      splice body close Empty (In_loop (prefix, frames)) ();
      finish_push body Empty; finish_def Empty body;
      parse_def close (push body Empty) (In_loop (prefix, frames));
      splice rest tail (Loop (body, prefix)) frames ()
    | T.If (yes, no, rest) ->
      let close = C.Next (I.Plain I.End, T.flatten rest tail) in
      let otherwise = C.Next (I.Plain I.Else, T.flatten no close) in
      parse_def (T.flatten code tail) prefix frames;
      splice yes otherwise Empty (In_then (prefix, frames)) ();
      finish_push yes Empty; finish_def Empty yes;
      parse_def otherwise (push yes Empty) (In_then (prefix, frames));
      splice no close Empty (In_else (yes, prefix, frames)) ();
      finish_push no Empty; finish_def Empty no;
      parse_def close (push no Empty) (In_else (yes, prefix, frames));
      splice rest tail (If (yes, no, prefix)) frames ())
let (roundtrip @ total) : (code : T.code) @ immutable -> {u : unit | N.structured code} ->
    {u : unit | decode (T.flatten code C.Empty) === Some code} @ ghost = fun code premise -> ghost_ (
  decode_def (T.flatten code C.Empty); splice code C.Empty Empty Root ();
  parse_def C.Empty (push code Empty) Root; finish_push code Empty; finish_def Empty code)

let (unique @ total) : (left : T.code) @ immutable -> (right : T.code) @ immutable ->
    {u : unit | N.structured left && N.structured right
      && T.flatten left C.Empty === T.flatten right C.Empty} ->
    {u : unit | left === right} @ ghost = fun left right premise -> ghost_ (
  roundtrip left (); roundtrip right ())
let (encode @ total) : (code : T.code) @ immutable -> (tail : Wasm_u32.bytes) @ immutable ->
    {u : unit | N.structured code} ->
    {bytes : Wasm_u32.bytes | match C.decode (C.length (T.flatten code C.Empty)) bytes with
      | None -> false
      | Some (tokens, suffix) -> decode tokens === Some code && suffix === tail} @ immutable =
  fun code tail premise ->
    let tokens = T.flatten code C.Empty in
    let bytes = C.encode tokens tail in
    ghost_ (roundtrip code ()); bytes
