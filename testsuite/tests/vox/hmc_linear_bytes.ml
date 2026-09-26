module D = Hm_declarative
module W = Hmc_word64
module B = Wasm_u32
module C = Hmc_tagged_cell

let[@def] rec (take @ total) (count : D.index @ immutable) (bytes : B.bytes @ immutable) = match count, bytes with
  | D.Z, _ -> Some B.End
  | D.S n, B.Byte (head, rest) -> (match take n rest with None -> None | Some tail -> Some (B.Byte (head, tail)))
  | _ -> None
let[@def] rec (drop @ total) (bytes : B.bytes @ immutable) (address : W.limb) =
  if address = 0 then Some bytes else match bytes with
  | B.End -> None | B.Byte (_, rest) -> drop rest (address - 1)
let[@def] (load @ total) (bytes : B.bytes @ immutable) (address : W.limb) (count : D.index @ immutable) =
  match drop bytes address with None -> None | Some rest -> take count rest
let[@def] rec (fits @ total) (payload : B.bytes @ immutable) (memory : B.bytes @ immutable) = match payload, memory with
  | B.End, _ -> true | B.Byte (_, a), B.Byte (_, b) -> fits a b | _ -> false
let[@def] (fits_at @ total) (memory : B.bytes @ immutable) (address : W.limb) (payload : B.bytes @ immutable) =
  match drop memory address with None -> false | Some rest -> fits payload rest
let[@def] rec (overlay @ total) (payload : B.bytes @ immutable) (before : B.bytes @ immutable) (after : B.bytes @ immutable) = ghost_ (
  match payload, before, after with
  | B.End, _, _ -> after === before
  | B.Byte (value, rest), B.Byte (_, old_tail), B.Byte (head, new_tail) -> value = head && overlay rest old_tail new_tail
  | _ -> false)
let[@def] rec (updated @ total) (before : B.bytes @ immutable) (address : W.limb)
    (payload : B.bytes @ immutable) (after : B.bytes @ immutable) = ghost_ (
  if address = 0 then overlay payload before after else match before, after with
  | B.Byte (old_head, old_tail), B.Byte (new_head, new_tail) -> old_head = new_head && updated old_tail (address - 1) payload new_tail
  | _ -> false)
let rec (overwrite @ total) : (payload : B.bytes) @ immutable -> (memory : B.bytes) @ immutable ->
    {out : B.bytes option | match out with
      | None -> not (fits payload memory)
      | Some bytes -> fits payload memory && overlay payload memory bytes && C.length bytes === C.length memory && take (C.length payload) bytes === Some payload} @ immutable =
  fun payload memory ->
    ghost_ (fits_def payload memory; C.length_def payload);
    match payload, memory with
    | B.End, _ -> ghost_ (take_def D.Z memory; overlay_def payload memory memory); Some memory
    | B.Byte (head, rest), B.Byte (_, tail) ->
      (match overwrite rest tail with None -> None | Some after ->
        let bytes = B.Byte (head, after) in
        ghost_ (overlay_def payload memory bytes; C.length_def bytes; C.length_def memory; take_def (C.length payload) bytes); Some bytes)
    | _ -> None
let rec (store @ total) : (memory : B.bytes) @ immutable -> (address : W.limb) -> (payload : B.bytes) @ immutable ->
    {out : B.bytes option | match out with
      | None -> not (fits_at memory address payload)
      | Some bytes -> fits_at memory address payload && updated memory address payload bytes && C.length bytes === C.length memory
        && load bytes address (C.length payload) === Some payload} @ immutable = fun memory address payload ->
    ghost_ (fits_at_def memory address payload; drop_def memory address);
    if address = 0 then (
      match overwrite payload memory with None -> None | Some bytes ->
        ghost_ (updated_def memory address payload bytes; load_def bytes address (C.length payload); drop_def bytes address); Some bytes)
    else match memory with
    | B.End -> None
    | B.Byte (head, rest) ->
      (match store rest (address - 1) payload with None -> ghost_ (fits_at_def rest (address - 1) payload); None | Some after ->
        let bytes = B.Byte (head, after) in
        ghost_ (updated_def memory address payload bytes; fits_at_def rest (address - 1) payload; C.length_def bytes; C.length_def memory;
          load_def bytes address (C.length payload); drop_def bytes address; load_def after (address - 1) (C.length payload)); Some bytes)
