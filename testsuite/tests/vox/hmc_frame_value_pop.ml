module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
let[@def] (successor @ total) (signature : G.signature @ immutable) (result_type : D.mono @ immutable) =
  match signature.G.temporaries with
  | G.Value (context, _, schema) -> Some {G.locals = context; temporaries = schema; accumulator = Some result_type}
  | _ -> None
let[@def] (reshape @ total) (signature : G.signature @ immutable) (value : V.value @ immutable)
    (cells : Heap.cells @ immutable) (padding : Heap.cells @ immutable) =
  match signature.G.temporaries, cells with
  | G.Value (context, _, schema), Heap.Cell (current, Heap.Cell (_, body)) ->
    (match Seg.drop (Codec.locals_size signature.G.locals) body with
    | Some (Heap.Cell (_, saved_cells)) ->
      (match Seg.take (Codec.locals_size context) saved_cells, Seg.drop (Codec.locals_size context) saved_cells with
      | Some saved, Some rest -> (match Seg.take (Codec.temporaries_size schema) rest with
        | Some old -> Some (Heap.Cell (current, Heap.Cell (value, Seg.append saved (Seg.append old padding))))
        | None -> None)
      | _ -> None)
    | _ -> None)
  | _ -> None
let[@def] (transition @ total) (activation : Frame.activation @ immutable) (next : D.index @ immutable) (value : V.value @ immutable) =
  match activation.Frame.temporaries with
  | Frame.Value (_, env, rest) -> Some {activation with Frame.pc = next; accumulator = value; env; temporaries = rest}
  | _ -> None
