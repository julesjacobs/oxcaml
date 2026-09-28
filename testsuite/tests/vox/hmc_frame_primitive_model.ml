module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Simple = Hmc_heap_simple
let[@def] (successor @ total) (signature : G.signature @ immutable) (operation : D.word_operation @ immutable) =
  match signature.G.accumulator, signature.G.temporaries with
  | Some D.Word64, G.Value (context, D.Word64, schema) -> Some {G.locals = context; temporaries = schema; accumulator = Some (D.operation_type operation)}
  | _ -> None
let[@def] (reshape @ total) (signature : G.signature @ immutable) (operation : D.word_operation @ immutable)
    (cells : Heap.cells @ immutable) (padding : Heap.cells @ immutable) =
  match signature.G.temporaries, cells with
  | G.Value (context, D.Word64, schema), Heap.Cell (current, Heap.Cell (V.Word right, body)) ->
    (match Seg.drop (Codec.locals_size signature.G.locals) body with
    | Some (Heap.Cell (V.Word left, saved_cells)) ->
      (match Seg.take (Codec.locals_size context) saved_cells, Seg.drop (Codec.locals_size context) saved_cells with
      | Some saved, Some rest -> (match Seg.take (Codec.temporaries_size schema) rest with
        | Some old -> Some (Heap.Cell (current, Heap.Cell (Simple.primitive operation left right, Seg.append saved (Seg.append old padding))))
        | None -> None)
      | _ -> None)
    | _ -> None)
  | _ -> None
