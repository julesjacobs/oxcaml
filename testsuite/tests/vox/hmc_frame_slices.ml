module D = Hm_declarative
module G = Hmc_cfg_ir
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Cap = Hmc_cell_capacity
type view = {body : Heap.cells; env : Heap.cells; temporaries : Heap.cells; old : Heap.cells}
let (decode @ total) : (signature : G.signature) @ immutable -> (activation : Frame.activation) @ immutable ->
    (cells : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | Codec.decode signature activation.Frame.pc cells === Some (activation, padding)} ->
    {out : view | cells === Heap.Cell (activation.Frame.current, Heap.Cell (activation.Frame.accumulator, out.body))
      && out.env === activation.Frame.env
      && Seg.take (Codec.locals_size signature.G.locals) out.body === Some out.env
      && Seg.drop (Codec.locals_size signature.G.locals) out.body === Some out.temporaries
      && Codec.decode_temporaries signature.G.temporaries out.temporaries === Some (activation.Frame.temporaries, padding)
      && Seg.take (Codec.temporaries_size signature.G.temporaries) out.temporaries === Some out.old
      && Seg.drop (Codec.temporaries_size signature.G.temporaries) out.temporaries === Some padding
      && Heap.length out.env === Codec.locals_size signature.G.locals
      && Heap.length out.old === Codec.temporaries_size signature.G.temporaries} @ immutable =
  fun signature activation cells padding premise ->
    ghost_ (Codec.decode_def signature activation.Frame.pc cells);
    match cells with
    | Heap.Cell (_, Heap.Cell (_, body)) -> (match Codec.decode_environment signature.G.locals body with
      | None -> unreachable_ ()
      | Some (env, temporaries) ->
        ghost_ (Seg.environment signature.G.locals body env temporaries ();
          Hmc_cell_slice.take_length (Codec.locals_size signature.G.locals) body env ();
          Seg.temporaries signature.G.temporaries temporaries activation.Frame.temporaries padding ());
        let old = Cap.cut (Codec.temporaries_size signature.G.temporaries) temporaries padding () in
        {body; env; temporaries; old})
    | _ -> unreachable_ ()
type saved_view = {saved : Heap.cells; more : Heap.cells; older : Heap.cells}
let (saved @ total) : (context : D.context) @ immutable -> (schema : G.temporaries) @ immutable ->
    (cells : Heap.cells) @ immutable -> (runtime : Frame.temporaries) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_temporaries (G.Environment (context, schema)) cells === Some (runtime, padding)} ->
    {out : saved_view | Seg.take (Codec.locals_size context) cells === Some out.saved
      && Seg.drop (Codec.locals_size context) cells === Some out.more
      && Seg.take (Codec.temporaries_size schema) out.more === Some out.older
      && Seg.drop (Codec.temporaries_size schema) out.more === Some padding
      && Heap.length out.saved === Codec.locals_size context && Heap.length out.older === Codec.temporaries_size schema} @ immutable =
  fun context schema cells runtime padding premise ->
    ghost_ (Codec.decode_temporaries_def (G.Environment (context, schema)) cells);
    match Codec.decode_environment context cells with
    | None -> unreachable_ ()
    | Some (saved, more) -> (match Codec.decode_temporaries schema more with
      | None -> unreachable_ ()
      | Some (remaining, _) ->
        ghost_ (Seg.environment context cells saved more ();
          Hmc_cell_slice.take_length (Codec.locals_size context) cells saved ();
          Seg.temporaries schema more remaining padding ());
        let older = Cap.cut (Codec.temporaries_size schema) more padding () in
        {saved; more; older})
