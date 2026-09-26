module B = Wasm_u32
module F = Wasm_framing
module S = Wasm_section

type t = {
  types : B.bytes;
  functions : B.bytes;
  table : B.bytes;
  memory : B.bytes;
  globals : B.bytes;
  exports : B.bytes;
  elements : B.bytes;
  code : B.bytes;
  data : B.bytes;
}
let[@def] (encodable @ total) (module_ : t @ immutable) = ghost_ (
  not (F.size module_.types === None) &&
  not (F.size module_.functions === None) &&
  not (F.size module_.table === None) &&
  not (F.size module_.memory === None) &&
  not (F.size module_.globals === None) &&
  not (F.size module_.exports === None) &&
  not (F.size module_.elements === None) &&
  not (F.size module_.code === None) &&
  not (F.size module_.data === None))
let[@def] (decode_sections @ total) (bytes : B.bytes @ immutable) =
  match S.decode bytes with
  | None -> None
  | Some (types, rest) -> if types.S.id <> 1 then None else
    match S.decode rest with
    | None -> None
    | Some (functions, rest) -> if functions.S.id <> 3 then None else
      match S.decode rest with
      | None -> None
      | Some (table, rest) -> if table.S.id <> 4 then None else
        match S.decode rest with
        | None -> None
        | Some (memory, rest) -> if memory.S.id <> 5 then None else
          match S.decode rest with
          | None -> None
          | Some (globals, rest) -> if globals.S.id <> 6 then None else
            match S.decode rest with
            | None -> None
            | Some (exports, rest) -> if exports.S.id <> 7 then None else
              match S.decode rest with
              | None -> None
              | Some (elements, rest) -> if elements.S.id <> 9 then None else
                match S.decode rest with
                | None -> None
                | Some (code, rest) -> if code.S.id <> 10 then None else
                  match S.decode rest with
                  | None -> None
                  | Some (data, rest) -> if data.S.id <> 11 then None else
                    Some ({types = types.S.payload; functions = functions.S.payload;
                      table = table.S.payload; memory = memory.S.payload;
                      globals = globals.S.payload; exports = exports.S.payload;
                      elements = elements.S.payload; code = code.S.payload;
                      data = data.S.payload}, rest)
let[@def] (decode @ total) (bytes : B.bytes @ immutable) =
  match bytes with
  | B.Byte (0, B.Byte (97, B.Byte (115, B.Byte (109,
      B.Byte (1, B.Byte (0, B.Byte (0, B.Byte (0, rest)))))))) -> decode_sections rest
  | _ -> None
let (encode @ total) : (module_ : t) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes option | match out with
      | None -> not (encodable module_)
      | Some bytes -> encodable module_ && decode bytes === Some (module_, tail)} @ immutable =
  fun module_ tail ->
    ghost_ (encodable_def module_);
    match S.encode {S.id = 11; payload = module_.data} tail with
    | None -> None
    | Some rest ->
      match S.encode {S.id = 10; payload = module_.code} rest with
      | None -> None
      | Some rest ->
        match S.encode {S.id = 9; payload = module_.elements} rest with
        | None -> None
        | Some rest ->
          match S.encode {S.id = 7; payload = module_.exports} rest with
          | None -> None
          | Some rest ->
            match S.encode {S.id = 6; payload = module_.globals} rest with
            | None -> None
            | Some rest ->
              match S.encode {S.id = 5; payload = module_.memory} rest with
              | None -> None
              | Some rest ->
                match S.encode {S.id = 4; payload = module_.table} rest with
                | None -> None
                | Some rest ->
                  match S.encode {S.id = 3; payload = module_.functions} rest with
                  | None -> None
                  | Some rest ->
                    match S.encode {S.id = 1; payload = module_.types} rest with
                    | None -> None
                    | Some rest ->
                      let bytes = B.Byte (0, B.Byte (97, B.Byte (115, B.Byte (109,
                        B.Byte (1, B.Byte (0, B.Byte (0, B.Byte (0, rest)))))))) in
                      ghost_ (decode_def bytes; decode_sections_def rest);
                      Some bytes
