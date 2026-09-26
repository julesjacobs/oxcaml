module B = Wasm_u32
module W = Hmc_word64
module Codec = Wasm_word_memory
module P = Hmc_linear_preservation
module L = Hmc_linear_bytes
type words = End | Word of W.t * words [@@inductive]
let[@def] rec (decode @ total) (words : words @ immutable) (bytes : B.bytes @ immutable) = match words with
  | End -> Some bytes
  | Word (value, rest) -> (match Codec.decode bytes with
    | Some (actual, tail) -> if W.equal value actual then decode rest tail else None
    | None -> None)
let[@def] rec (size @ total) (words : words @ immutable) (bytes : B.u32) = match words with
  | End -> bytes = 0 | Word (_, rest) -> bytes >= 8 && size rest (bytes - 8)
let rec (prefix @ total) : (words : words) @ immutable -> (before : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable -> (count : B.u32) ->
    {u : unit | decode words before === Some left && decode words after === Some right && size words count} ->
    {u : unit | P.equal_prefix count before after && L.drop before count === Some left && L.drop after count === Some right} @ ghost =
  fun words before after left right count premise -> ghost_ (
    decode_def words before; decode_def words after; size_def words count;
    match words with
    | End -> P.equal_prefix_def count before after; L.drop_def before count; L.drop_def after count
    | Word (value, rest) -> (match Codec.decode before, Codec.decode after with
      | Some (a, b), Some (c, d) ->
        W.equal_def value a; W.equal_def value c;
        prefix rest b d left right (count - 8) ();
        Wasm_word_prefix.word before after value b d (count - 8) count ();
        Wasm_cell.word_suffix before value b (); Wasm_cell.word_suffix after value d ();
        Wasm_cell.eight_def ();
        Wasm_cell.shift before 8 (count - 8) count b ();
        Wasm_cell.shift after 8 (count - 8) count d ()
      | _ -> ()))
let rec (encode @ total) : (words : words) @ immutable -> (tail : B.bytes) @ immutable ->
    {bytes : B.bytes | decode words bytes === Some tail} @ immutable = fun words tail ->
  match words with
  | End -> ghost_ (decode_def End tail); tail
  | Word (value, rest) ->
    let after = encode rest tail in
    let bytes = Codec.encode value after in
    ghost_ (decode_def words bytes; W.equal_def value value); bytes
let rec (unique @ total) : (words : words) @ immutable -> (left : B.bytes) @ immutable -> (right : B.bytes) @ immutable ->
    (tail : B.bytes) @ immutable -> {u : unit | decode words left === Some tail && decode words right === Some tail} ->
    {u : unit | left === right} @ ghost = fun words left right tail premise -> ghost_ (
    decode_def words left; decode_def words right;
    match words with
    | End -> ()
    | Word (value, rest) -> (match Codec.decode left, Codec.decode right with
      | Some (a, b), Some (c, d) ->
        W.equal_def value a; W.equal_def value c; unique rest b d tail ();
        Wasm_word_prefix.unique left right value b ()
      | _ -> ()))
let[@def] rec (append @ total) (left : words @ immutable) (right : words @ immutable) = match left with
  | End -> right | Word (value, rest) -> Word (value, append rest right)
let rec (split @ total) : (left : words) @ immutable -> (right : words) @ immutable ->
    (bytes : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | decode (append left right) bytes === Some tail} ->
    {middle : B.bytes | decode left bytes === Some middle && decode right middle === Some tail} @ immutable =
  fun left right bytes tail premise ->
    ghost_ (append_def left right; decode_def (append left right) bytes; decode_def left bytes);
    match left with
    | End -> bytes
    | Word (value, rest) -> (match Codec.decode bytes with
      | None -> unreachable_ ()
      | Some (_, after) -> split rest right after tail ())
