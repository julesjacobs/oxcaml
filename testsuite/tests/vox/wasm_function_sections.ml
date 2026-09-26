module B = Wasm_u32
module F = Wasm_functions
module C = Wasm_code_section
module Body = Wasm_function_body
module Indices = Wasm_index_vector
module Signatures = Wasm_signature_section
let[@def] rec (contains @ total) (signatures : F.signatures @ immutable) (result : F.result_type @ immutable) =
  match signatures with F.No_signatures -> false | F.Signature (head, rest) -> F.same_result head result || contains rest result
let[@def] rec (supported @ total) (signatures : F.signatures @ immutable) (functions : F.functions @ immutable) =
  match functions with F.No_functions -> true | F.Function (head, rest) ->
    Wasm_nesting.structured head.F.code && contains signatures head.F.result && supported signatures rest
let rec (find_signature @ total) : (signatures : F.signatures) @ immutable -> (result : F.result_type) @ immutable ->
    {out : B.u32 option | match out with
      | None -> Signatures.count signatures === None || not (contains signatures result)
      | Some index -> F.signature signatures index === Some result && contains signatures result &&
        (match Signatures.count signatures with None -> true | Some count -> index < count)} =
  fun signatures result ->
    ghost_ (Signatures.count_def signatures; contains_def signatures result);
    match signatures with
  | F.No_signatures -> None
  | F.Signature (head, rest) ->
    ghost_ (F.same_result_def head result);
    if F.same_result head result then (ghost_ (F.signature_def signatures 0); Some 0) else
      match find_signature rest result with
      | None -> None
      | Some index -> if index = 4294967295 then None else
        (ghost_ (F.signature_def signatures (index + 1)); Some (index + 1))
let[@def] rec (link @ total) (signatures : F.signatures @ immutable) (indices : F.table @ immutable) (bodies : C.bodies @ immutable) : F.functions option @ immutable =
  match indices, bodies with
  | F.No_elements, C.Empty -> Some F.No_functions
  | F.Element (Some index, indices), C.Body (body, bodies) ->
    (match F.signature signatures index with None -> None | Some result ->
      match link signatures indices bodies with None -> None | Some rest ->
        Some (F.Function ({F.result; locals = body.Body.locals; code = body.Body.code}, rest)))
  | _ -> None
let rec (split @ total) : (signatures : F.signatures) @ immutable -> (functions : F.functions) @ immutable ->
    {out : (F.table * C.bodies) option | match out with
      | None -> Signatures.count signatures === None || not (supported signatures functions)
      | Some (indices, bodies) -> supported signatures functions &&
      link signatures indices bodies === Some functions && C.structured bodies} @ immutable =
  fun signatures functions ->
    ghost_ (supported_def signatures functions);
    match functions with
  | F.No_functions ->
    ghost_ (link_def signatures F.No_elements C.Empty; C.structured_def C.Empty); Some (F.No_elements, C.Empty)
  | F.Function (head, rest) ->
    if not (Wasm_nesting.structured head.F.code) then None else
    match find_signature signatures head.F.result with None -> None | Some index ->
      match split signatures rest with None -> None | Some (indices, bodies) ->
        let indices = F.Element (Some index, indices) in
        let bodies = C.Body ({Body.locals = head.F.locals; code = head.F.code}, bodies) in
        ghost_ (link_def signatures indices bodies; C.structured_def bodies);
        Some (indices, bodies)
type sections = {types : B.bytes; code : B.bytes}
let[@def] (decode @ total) (signatures : F.signatures @ immutable) (sections : sections @ immutable) =
  match Indices.decode sections.types, C.decode sections.code with
  | Some (indices, B.End), Some (bodies, B.End) -> link signatures indices bodies
  | _ -> None
let[@def] (encodable @ total) (signatures : F.signatures @ immutable) (functions : F.functions @ immutable) = ghost_ (
  match split signatures functions with None -> false | Some (indices, bodies) ->
    not (Indices.count indices === None) && C.encodable bodies)
let (encode @ total) : (signatures : F.signatures) @ immutable -> (functions : F.functions) @ immutable ->
    {out : sections option | match out with None -> not (encodable signatures functions)
      | Some sections -> encodable signatures functions && decode signatures sections === Some functions} @ immutable =
  fun signatures functions -> ghost_ (encodable_def signatures functions); match split signatures functions with None -> None | Some (indices, bodies) ->
    match Indices.encode indices B.End with None -> None | Some types ->
      match C.encode bodies B.End () with None -> None | Some code ->
        let sections = {types; code} in ghost_ (decode_def signatures sections); Some sections
