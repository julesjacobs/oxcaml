module B = Wasm_u32
module V = Hmc_tagged_cell
module W = Hmc_word64
module H = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Bytes = Hmc_linear_bytes
let (value_equal @ total) : (left : V.value) @ immutable -> (right : V.value) @ immutable ->
    {out : bool | out === (left === right)} = fun left right ->
  match left, right with
  | V.Boolean a, V.Boolean b -> a = b
  | V.Word a, V.Word b -> a.W.lo = b.W.lo && a.W.hi = b.W.hi
  | V.Nil, V.Nil -> true
  | V.Cons_pointer a, V.Cons_pointer b | V.Closure_pointer a, V.Closure_pointer b -> a = b
  | _ -> false
let rec (cells_equal @ total) : (left : H.cells) @ immutable -> (right : H.cells) @ immutable ->
    {out : bool | out === (left === right)} = fun left right ->
  match left, right with
  | H.Empty, H.Empty -> true
  | H.Cell (a, rest), H.Cell (b, tail) -> value_equal a b && cells_equal rest tail
  | _ -> false
type view = {bytes : B.bytes; suffix : B.bytes}
let (check @ total) : (memory : B.bytes) @ immutable -> (base : B.u32) -> (cells : H.cells) @ immutable ->
    {out : view option | match out with None -> true | Some out -> Bytes.drop memory base === Some out.bytes
      && Wire.decode_cells (H.length cells) out.bytes === Some (cells, out.suffix)} @ immutable =
  fun memory base cells ->
    match Bytes.drop memory base with
    | None -> None
    | Some bytes -> match Wire.decode_cells (H.length cells) bytes with
      | None -> None
      | Some (actual, suffix) -> if cells_equal actual cells then Some {bytes; suffix} else None
