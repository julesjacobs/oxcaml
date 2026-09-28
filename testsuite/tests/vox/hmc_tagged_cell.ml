module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module M = Wasm_word_memory

type value = Boolean of bool | Word of W.t | Nil | Cons_pointer of W.limb | Closure_pointer of W.limb [@@inductive]
let[@def] (tag @ total) (value : value @ immutable) : W.t @ immutable =
  {W.lo = (match value with Boolean _ -> 0 | Word _ -> 1 | Nil -> 2 | Cons_pointer _ -> 3 | Closure_pointer _ -> 4); hi = 0}
let[@def] (payload @ total) (value : value @ immutable) : W.t @ immutable = match value with
  | Boolean b -> {W.lo = (if b then 1 else 0); hi = 0}
  | Word w -> w | Nil -> {W.lo = 0; hi = 0}
  | Cons_pointer address | Closure_pointer address -> {W.lo = address; hi = 0}
let[@def] (decode @ total) (bytes : B.bytes @ immutable) : (value * B.bytes) option @ immutable =
  match M.decode bytes with
  | None -> None
  | Some (tag, rest) -> (match M.decode rest with
    | None -> None
    | Some (payload, tail) ->
      if tag.W.hi <> 0 then None else
      match tag.W.lo with
      | 0 -> if payload.W.hi = 0 && payload.W.lo <= 1 then Some (Boolean (payload.W.lo = 1), tail) else None
      | 1 -> Some (Word payload, tail)
      | 2 -> if payload.W.hi = 0 && payload.W.lo = 0 then Some (Nil, tail) else None
      | 3 -> if payload.W.hi = 0 then Some (Cons_pointer payload.W.lo, tail) else None
      | 4 -> if payload.W.hi = 0 then Some (Closure_pointer payload.W.lo, tail) else None
      | _ -> None)
let[@def] rec (length @ total) (bytes : B.bytes @ immutable) = match bytes with B.End -> D.Z | B.Byte (_, rest) -> D.S (length rest)
let[@def] (four @ total) (n : D.index @ immutable) = D.S (D.S (D.S (D.S n)))
let[@def] (eight @ total) (n : D.index @ immutable) = four (four n)
let[@def] (sixteen @ total) (n : D.index @ immutable) = eight (eight n)
let (limb_size @ total) : (bytes : B.bytes) @ immutable -> (limb : W.limb) -> (tail : B.bytes) @ immutable ->
    {u : unit | M.decode_limb bytes === Some (limb, tail)} ->
    {u : unit | length bytes === four (length tail)} @ ghost = fun bytes limb tail premise -> ghost_ (
  M.decode_limb_def bytes; four_def (length tail);
  match bytes with
  | B.Byte (_, (B.Byte (_, (B.Byte (_, (B.Byte (_, _) as c)) as b)) as a)) ->
    length_def bytes; length_def a; length_def b; length_def c
  | _ -> ())
let (word_size @ total) : (bytes : B.bytes) @ immutable -> (word : W.t) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | M.decode bytes === Some (word, tail)} ->
    {u : unit | length bytes === eight (length tail)} @ ghost = fun bytes word tail premise -> ghost_ (
  M.decode_def bytes; eight_def (length tail);
  match M.decode_limb bytes with None -> () | Some (lo, middle) ->
    limb_size bytes lo middle ();
    (match M.decode_limb middle with None -> () | Some (hi, _) -> limb_size middle hi tail ()))
let (fixed_size @ total) : (bytes : B.bytes) @ immutable -> (value : value) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | decode bytes === Some (value, tail)} ->
    {u : unit | length bytes === sixteen (length tail)} @ ghost = fun bytes value tail premise -> ghost_ (
  decode_def bytes; sixteen_def (length tail);
  match M.decode bytes with None -> () | Some (tag, middle) ->
    word_size bytes tag middle ();
    (match M.decode middle with None -> () | Some (payload, _) -> word_size middle payload tail ()))
let (encode @ total) : (value : value) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes | decode out === Some (value, tail) && length out === sixteen (length tail)} @ immutable = fun value tail ->
  let payload_bytes = M.encode (payload value) tail in
  let out = M.encode (tag value) payload_bytes in
  ghost_ (tag_def value; payload_def value; decode_def out; fixed_size out value tail ());
  out
