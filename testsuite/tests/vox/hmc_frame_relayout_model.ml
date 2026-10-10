module D = Hm_declarative
module Heap = Hmc_heap_objects
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
let[@def] (successor @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable) = match instruction with
  | G.Save_environment _ -> Some {signature with G.temporaries = G.Environment (signature.G.locals, signature.G.temporaries)}
  | G.Save_value _ -> (match signature.G.temporaries, signature.G.accumulator with
    | G.Environment (context, rest), Some ty -> Some {G.locals = context; temporaries = G.Value (context, ty, rest); accumulator = None}
    | _ -> None)
  | G.Bind _ -> (match signature.G.temporaries, signature.G.accumulator with
    | G.Environment (context, _), Some ty -> Some {signature with G.locals = D.Binding (D.Forall (D.Z, ty), context); accumulator = None}
    | _ -> None)
  | G.Restore _ -> (match signature.G.temporaries with
    | G.Environment (context, rest) -> Some {signature with G.locals = context; temporaries = rest}
    | _ -> None)
  | _ -> None
let[@def] (reshape @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (cells : Heap.cells @ immutable) (padding : Heap.cells @ immutable) =
  match cells with
  | Heap.Cell (current, Heap.Cell (accumulator, body)) ->
    (match Seg.take (Codec.locals_size signature.G.locals) body, Seg.drop (Codec.locals_size signature.G.locals) body with
    | Some env, Some temporaries ->
      let after = match instruction with
      | G.Save_environment _ -> (match Seg.take (Codec.temporaries_size signature.G.temporaries) temporaries with
        | None -> None | Some old -> Some (Seg.append env (Seg.append env (Seg.append old padding))))
      | G.Save_value _ | G.Bind _ | G.Restore _ -> (match signature.G.temporaries with
        | G.Environment (context, rest) ->
          (match Seg.take (Codec.locals_size context) temporaries, Seg.drop (Codec.locals_size context) temporaries with
          | Some saved, Some more -> (match Seg.take (Codec.temporaries_size rest) more with
            | None -> None
            | Some old ->
              let after = Seg.append old padding in
              (match instruction with
              | G.Save_value _ -> Some (Seg.append saved (Heap.Cell (accumulator, Seg.append saved after)))
              | G.Bind _ -> Some (Heap.Cell (accumulator, Seg.append saved (Seg.append saved after)))
              | _ -> Some (Seg.append saved after)))
          | _ -> None)
        | _ -> None)
      | _ -> None in
      (match after with None -> None | Some after -> Some (Heap.Cell (current, Heap.Cell (accumulator, after))))
    | _ -> None)
  | _ -> None
