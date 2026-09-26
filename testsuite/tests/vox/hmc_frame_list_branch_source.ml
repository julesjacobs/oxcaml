module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Pad = Hmc_frame_repad
module Model = Hmc_frame_list_branch
let (correct @ total) : (signature : G.signature) @ immutable -> (element : D.mono) @ immutable ->
    (activation : Frame.activation) @ immutable -> (next : D.index) @ immutable -> (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)} ->
    {out : Heap.cells | Model.reshape signature head tail cells padding === Some out
      && Codec.decode (Model.successor signature element) next out === Some (Model.transition activation next head tail, padding)} @ immutable =
  fun signature element activation next head tail cells old_padding padding premise ->
    ghost_ (Codec.decode_def signature activation.Frame.pc cells;
      Model.successor_def signature element; Model.transition_def activation next head tail;
      Model.reshape_def signature head tail cells padding);
    match cells with
    | Heap.Cell (current, Heap.Cell (accumulator, body)) ->
      (match Codec.decode_environment signature.G.locals body with
      | None -> unreachable_ ()
      | Some (env, temporary_cells) ->
        ghost_ (Seg.environment signature.G.locals body env temporary_cells ());
        let temporary_tail = Pad.temporaries signature.G.temporaries temporary_cells activation.Frame.temporaries old_padding padding () in
        ghost_ (Pad.prefix (Codec.temporaries_size signature.G.temporaries) temporary_cells padding);
        let saved = Seg.append env temporary_tail in
        let both = Seg.append env saved in
        let after = Heap.Cell (head, Heap.Cell (tail, both)) in
        let next_signature = Model.successor signature element in
        ghost_ (Seg.join_environment signature.G.locals env temporary_tail ();
          Codec.decode_temporaries_def next_signature.G.temporaries saved;
          Seg.join_environment signature.G.locals env saved ();
          Codec.decode_environment_def next_signature.G.locals after;
          Codec.decode_environment_def (D.Binding (D.Forall (D.Z, D.List_type element), signature.G.locals)) (Heap.Cell (tail, both)));
        let out = Heap.Cell (current, Heap.Cell (accumulator, after)) in
        ghost_ (Codec.decode_def next_signature next out); out)
    | _ -> unreachable_ ()
