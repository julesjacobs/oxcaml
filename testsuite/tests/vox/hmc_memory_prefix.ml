module D = Hm_declarative
module B = Wasm_u32
module W = Hmc_word64
module C = Hmc_tagged_cell
module M = Wasm_word_memory
module L = Hmc_linear_bytes
module Cap = Hmc_frame_capacity

let (limb @ total) : (bytes : B.bytes) @ immutable -> (count : D.index) @ immutable -> (value : W.limb) ->
    (rest : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | M.decode_limb bytes === Some (value, rest) && L.take count rest === Some tail} ->
    {out : B.bytes | L.take (C.four count) bytes === Some out && M.decode_limb out === Some (value, tail)} @ immutable =
  fun bytes count value rest tail premise ->
    ghost_ (M.decode_limb_def bytes; C.four_def count; L.take_def (C.four count) bytes);
    match bytes with
    | B.Byte (a, (B.Byte (b, (B.Byte (c, (B.Byte (d, _) as third)) as second)) as first)) ->
      let out = B.Byte (a, B.Byte (b, B.Byte (c, B.Byte (d, tail)))) in
      ghost_ (L.take_def (D.S (D.S (D.S count))) first;
        L.take_def (D.S (D.S count)) second; L.take_def (D.S count) third; M.decode_limb_def out); out
    | _ -> unreachable_ ()
let (word @ total) : (bytes : B.bytes) @ immutable -> (count : D.index) @ immutable -> (value : W.t) @ immutable ->
    (rest : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | M.decode bytes === Some (value, rest) && L.take count rest === Some tail} ->
    {out : B.bytes | L.take (C.eight count) bytes === Some out && M.decode out === Some (value, tail)} @ immutable =
  fun bytes count value rest tail premise ->
    ghost_ (M.decode_def bytes; C.eight_def count);
    match M.decode_limb bytes with
    | None -> unreachable_ ()
    | Some (lo, middle) ->
      let high = limb middle count value.W.hi rest tail () in
      let out = limb bytes (C.four count) lo middle high () in
      ghost_ (M.decode_def out); out
let (cell @ total) : (bytes : B.bytes) @ immutable -> (count : D.index) @ immutable -> (value : C.value) @ immutable ->
    (rest : B.bytes) @ immutable -> (tail : B.bytes) @ immutable ->
    {u : unit | C.decode bytes === Some (value, rest) && L.take count rest === Some tail} ->
    {out : B.bytes | L.take (C.sixteen count) bytes === Some out && C.decode out === Some (value, tail)} @ immutable =
  fun bytes count value rest tail premise ->
    ghost_ (C.decode_def bytes; C.sixteen_def count);
    match M.decode bytes with
    | None -> unreachable_ ()
    | Some (tag, middle) ->
      (match M.decode middle with
      | None -> unreachable_ ()
      | Some (payload, _) ->
        let payload_bytes = word middle count payload rest tail () in
        let out = word bytes (C.eight count) tag middle payload_bytes () in
        ghost_ (C.decode_def out); out)
let rec (take @ total) : (small : D.index) @ immutable -> (large : D.index) @ immutable ->
    (bytes : B.bytes) @ immutable -> (prefix : B.bytes) @ immutable ->
    {u : unit | Cap.le small large && L.take large bytes === Some prefix} ->
    {u : unit | L.take small bytes === L.take small prefix} @ ghost = fun small large bytes prefix premise -> ghost_ (
  Cap.le_def small large; L.take_def large bytes; L.take_def small bytes; L.take_def small prefix;
  match small, large, bytes with
  | D.S n, D.S m, B.Byte (_, rest) ->
    (match L.take m rest with None -> () | Some tail -> take n m rest tail ())
  | _ -> ())
let (four_le @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | Cap.le a b} -> {u : unit | Cap.le (C.four a) (C.four b)} @ ghost = fun a b premise -> ghost_ (
  C.four_def a; C.four_def b; Cap.le_def (C.four a) (C.four b);
  Cap.le_def (D.S (D.S (D.S a))) (D.S (D.S (D.S b)));
  Cap.le_def (D.S (D.S a)) (D.S (D.S b)); Cap.le_def (D.S a) (D.S b))
let (sixteen_le @ total) : (a : D.index) @ immutable -> (b : D.index) @ immutable ->
    {u : unit | Cap.le a b} -> {u : unit | Cap.le (C.sixteen a) (C.sixteen b)} @ ghost = fun a b premise -> ghost_ (
  C.sixteen_def a; C.sixteen_def b; C.eight_def a; C.eight_def b; C.eight_def (C.eight a); C.eight_def (C.eight b);
  four_le a b (); four_le (C.four a) (C.four b) (); four_le (C.eight a) (C.eight b) (); four_le (C.four (C.eight a)) (C.four (C.eight b)) ())
