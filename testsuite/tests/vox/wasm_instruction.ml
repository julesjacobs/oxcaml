module B = Wasm_u32
module W = Hmc_word64
type plain = Unreachable | Nop | Else | End | Return | Drop | Select | I32_eqz | I32_eq | I32_ne | I32_lt_u | I32_gt_u | I32_le_u | I32_ge_u | I64_eqz | I64_eq | I64_lt_u | I32_add | I32_sub | I32_mul | I32_and | I32_or | I64_add | I64_sub | I64_and | I64_or | I64_shl | I64_shr_u | I32_wrap_i64 | I64_extend_i32_u [@@inductive]
let[@def] (opcode @ total) (instruction : plain @ immutable) : B.byte = match instruction with
  | Unreachable -> 0
  | Nop -> 1
  | Else -> 5
  | End -> 11
  | Return -> 15
  | Drop -> 26
  | Select -> 27
  | I32_eqz -> 69
  | I32_eq -> 70
  | I32_ne -> 71
  | I32_lt_u -> 73
  | I32_gt_u -> 75
  | I32_le_u -> 77
  | I32_ge_u -> 79
  | I64_eqz -> 80
  | I64_eq -> 81
  | I64_lt_u -> 84
  | I32_add -> 106
  | I32_sub -> 107
  | I32_mul -> 108
  | I32_and -> 113
  | I32_or -> 114
  | I64_add -> 124
  | I64_sub -> 125
  | I64_and -> 131
  | I64_or -> 132
  | I64_shl -> 134
  | I64_shr_u -> 136
  | I32_wrap_i64 -> 167
  | I64_extend_i32_u -> 173
let[@def] (decode_plain @ total) (byte : B.byte) = match byte with
  | 0 -> Some Unreachable
  | 1 -> Some Nop
  | 5 -> Some Else
  | 11 -> Some End
  | 15 -> Some Return
  | 26 -> Some Drop
  | 27 -> Some Select
  | 69 -> Some I32_eqz
  | 70 -> Some I32_eq
  | 71 -> Some I32_ne
  | 73 -> Some I32_lt_u
  | 75 -> Some I32_gt_u
  | 77 -> Some I32_le_u
  | 79 -> Some I32_ge_u
  | 80 -> Some I64_eqz
  | 81 -> Some I64_eq
  | 84 -> Some I64_lt_u
  | 106 -> Some I32_add
  | 107 -> Some I32_sub
  | 108 -> Some I32_mul
  | 113 -> Some I32_and
  | 114 -> Some I32_or
  | 124 -> Some I64_add
  | 125 -> Some I64_sub
  | 131 -> Some I64_and
  | 132 -> Some I64_or
  | 134 -> Some I64_shl
  | 136 -> Some I64_shr_u
  | 167 -> Some I32_wrap_i64
  | 173 -> Some I64_extend_i32_u
  | _ -> None
type align32 = {n : int | 0 <= n && n <= 2}
type align64 = {n : int | 0 <= n && n <= 3}
type t = Plain of plain | I32_const of B.u32 | I64_const of W.t
  | Br of B.u32
  | Br_if of B.u32
  | Call of B.u32
  | Local_get of B.u32
  | Local_set of B.u32
  | Local_tee of B.u32
  | Global_get of B.u32
  | Global_set of B.u32
  | Block
  | Loop
  | If
  | Call_indirect of B.u32
  | I32_load of align32 * B.u32
  | I64_load of align64 * B.u32
  | I32_store of align32 * B.u32
  | I64_store of align64 * B.u32
  [@@inductive]
let[@def] (decode @ total) (input : B.bytes @ immutable) = match input with
  | B.End -> None
  | B.Byte (op, bytes) -> (match op with
    | 12 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Br index, rest))
    | 13 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Br_if index, rest))
    | 16 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Call index, rest))
    | 32 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Local_get index, rest))
    | 33 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Local_set index, rest))
    | 34 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Local_tee index, rest))
    | 35 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Global_get index, rest))
    | 36 -> (match B.decode_5 bytes with None -> None | Some (index, rest) -> Some (Global_set index, rest))
    | 2 -> (match bytes with B.Byte (64, rest) -> Some (Block, rest) | _ -> None)
    | 3 -> (match bytes with B.Byte (64, rest) -> Some (Loop, rest) | _ -> None)
    | 4 -> (match bytes with B.Byte (64, rest) -> Some (If, rest) | _ -> None)
    | 17 -> (match B.decode_5 bytes with Some (ty, B.Byte (0, rest)) -> Some (Call_indirect ty, rest) | _ -> None)
    | 65 -> (match Wasm_i32.decode bytes with None -> None | Some (value, rest) -> Some (I32_const value, rest))
    | 66 -> (match Wasm_i64.decode bytes with None -> None | Some (value, rest) -> Some (I64_const value, rest))
    | 40 -> (match B.decode_5 bytes with None -> None | Some (alignment, tail) ->
      if alignment > 2 then None else match B.decode_5 tail with None -> None | Some (offset, rest) -> Some (I32_load (alignment, offset), rest))
    | 41 -> (match B.decode_5 bytes with None -> None | Some (alignment, tail) ->
      if alignment > 3 then None else match B.decode_5 tail with None -> None | Some (offset, rest) -> Some (I64_load (alignment, offset), rest))
    | 54 -> (match B.decode_5 bytes with None -> None | Some (alignment, tail) ->
      if alignment > 2 then None else match B.decode_5 tail with None -> None | Some (offset, rest) -> Some (I32_store (alignment, offset), rest))
    | 55 -> (match B.decode_5 bytes with None -> None | Some (alignment, tail) ->
      if alignment > 3 then None else match B.decode_5 tail with None -> None | Some (offset, rest) -> Some (I64_store (alignment, offset), rest))
    | _ -> (match decode_plain op with None -> None | Some plain -> Some (Plain plain, bytes)))
let (encode @ total) : (instruction : t) @ immutable -> (tail : B.bytes) @ immutable ->
    {out : B.bytes | decode out === Some (instruction, tail)} @ immutable = fun instruction tail ->
  let out = match instruction with
  | Plain plain -> ghost_ (opcode_def plain; decode_plain_def (opcode plain)); B.Byte (opcode plain, tail)
  | I32_const value -> B.Byte (65, Wasm_i32.encode value tail)
  | I64_const value -> B.Byte (66, Wasm_i64.encode value tail)
  | Br index -> B.Byte (12, B.encode_u32 index tail)
  | Br_if index -> B.Byte (13, B.encode_u32 index tail)
  | Call index -> B.Byte (16, B.encode_u32 index tail)
  | Local_get index -> B.Byte (32, B.encode_u32 index tail)
  | Local_set index -> B.Byte (33, B.encode_u32 index tail)
  | Local_tee index -> B.Byte (34, B.encode_u32 index tail)
  | Global_get index -> B.Byte (35, B.encode_u32 index tail)
  | Global_set index -> B.Byte (36, B.encode_u32 index tail)
  | Block -> B.Byte (2, B.Byte (64, tail))
  | Loop -> B.Byte (3, B.Byte (64, tail))
  | If -> B.Byte (4, B.Byte (64, tail))
  | Call_indirect ty -> B.Byte (17, B.encode_u32 ty (B.Byte (0, tail)))
  | I32_load (alignment, offset) -> B.Byte (40, B.encode_u32 alignment (B.encode_u32 offset tail))
  | I64_load (alignment, offset) -> B.Byte (41, B.encode_u32 alignment (B.encode_u32 offset tail))
  | I32_store (alignment, offset) -> B.Byte (54, B.encode_u32 alignment (B.encode_u32 offset tail))
  | I64_store (alignment, offset) -> B.Byte (55, B.encode_u32 alignment (B.encode_u32 offset tail))
  in ghost_ (decode_def out); out
