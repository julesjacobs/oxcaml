module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Seg = Hmc_frame_segments
module Codec = Hmc_pointer_frame_codec
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Model = Hmc_frame_value_pop
module Pad = Hmc_frame_repad
let (correct @ total) : (signature : G.signature) @ immutable -> (result_type : D.mono) @ immutable -> (value : V.value) @ immutable -> (next : D.index) @ immutable ->
    (activation : Frame.activation) @ immutable -> (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (next_signature : G.signature) @ immutable -> (next_activation : Frame.activation) @ immutable ->
    {u : unit | Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && Model.successor signature result_type === Some next_signature
      && Model.transition activation next value === Some next_activation} ->
    {out : Heap.cells | Model.reshape signature value cells padding === Some out
      && Codec.decode next_signature next_activation.Frame.pc out === Some (next_activation, padding)} @ immutable =
  fun signature result_type value next activation cells old_padding padding next_signature next_activation premise ->
    ghost_ (Codec.decode_def signature activation.Frame.pc cells; Model.successor_def signature result_type;
      Model.reshape_def signature value cells padding; Model.transition_def activation next value);
    match signature.G.temporaries, cells with
    | G.Value (context, _, schema), Heap.Cell (current, Heap.Cell (_, body)) ->
      (match Codec.decode_environment signature.G.locals body with
      | None -> unreachable_ ()
      | Some (env, temporaries) ->
        ghost_ (Seg.environment signature.G.locals body env temporaries ();
          Codec.decode_temporaries_def signature.G.temporaries temporaries);
        (match temporaries with
        | Heap.Cell (_, saved_cells) -> (match Codec.decode_environment context saved_cells with
          | None -> unreachable_ ()
          | Some (saved, remaining_cells) ->
            ghost_ (Seg.environment context saved_cells saved remaining_cells ());
            (match Codec.decode_temporaries schema remaining_cells with
            | None -> unreachable_ ()
            | Some (remaining, _) ->
              let tail = Pad.temporaries schema remaining_cells remaining old_padding padding () in
              ghost_ (Pad.prefix (Codec.temporaries_size schema) remaining_cells padding);
              let after = Seg.append saved tail in
              ghost_ (Seg.join_environment context saved tail ());
              let out = Heap.Cell (current, Heap.Cell (value, after)) in
              ghost_ (Codec.decode_def next_signature next_activation.Frame.pc out); out))
        | _ -> unreachable_ ()))
    | _ -> unreachable_ ()
