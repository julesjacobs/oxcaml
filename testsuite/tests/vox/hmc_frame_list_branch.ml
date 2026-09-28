module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
let[@def] (successor @ total) (signature : G.signature @ immutable) (element : D.mono @ immutable) =
  {signature with G.locals = D.Binding (D.Forall (D.Z, element), D.Binding (D.Forall (D.Z, D.List_type element), signature.G.locals));
    temporaries = G.Environment (signature.G.locals, signature.G.temporaries)}
let[@def] (transition @ total) (activation : Frame.activation @ immutable) (next : D.index @ immutable)
    (head : V.value @ immutable) (tail : V.value @ immutable) =
  {activation with Frame.pc = next; env = Heap.Cell (head, Heap.Cell (tail, activation.Frame.env));
    temporaries = Frame.Environment (activation.Frame.env, activation.Frame.temporaries)}
let[@def] (reshape @ total) (signature : G.signature @ immutable) (head : V.value @ immutable) (tail : V.value @ immutable)
    (cells : Heap.cells @ immutable) (padding : Heap.cells @ immutable) =
  match cells with
  | Heap.Cell (current, Heap.Cell (accumulator, body)) ->
    (match Seg.take (Codec.locals_size signature.G.locals) body, Seg.drop (Codec.locals_size signature.G.locals) body with
    | Some env, Some temporaries ->
      (match Seg.take (Codec.temporaries_size signature.G.temporaries) temporaries with
      | None -> None
      | Some old -> Some (Heap.Cell (current, Heap.Cell (accumulator, Heap.Cell (head, Heap.Cell (tail,
          Seg.append env (Seg.append env (Seg.append old padding))))))))
    | _ -> None)
  | _ -> None
