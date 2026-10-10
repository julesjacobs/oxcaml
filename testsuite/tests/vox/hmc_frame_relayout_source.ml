module D = Hm_declarative
module Heap = Hmc_heap_objects
module G = Hmc_cfg_ir
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Simple = Hmc_heap_simple
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module Pad = Hmc_frame_repad
module Model = Hmc_frame_relayout_model
let (correct @ total) : (signature : G.signature) @ immutable -> (instruction : G.instruction) @ immutable ->
    (activation : Frame.activation) @ immutable -> (frames : State.frames) @ immutable ->
    (cells : Heap.cells) @ immutable -> (old_padding : Heap.cells) @ immutable -> (padding : Heap.cells) @ immutable ->
    (next_signature : G.signature) @ immutable -> (next_activation : Frame.activation) @ immutable ->
    {u : unit | Codec.decode signature activation.Frame.pc cells === Some (activation, old_padding)
      && Model.successor signature instruction === Some next_signature
      && Simple.step instruction (State.Running (activation, frames)) === State.Running (next_activation, frames)} ->
    {out : Heap.cells | Model.reshape signature instruction cells padding === Some out
      && Codec.decode next_signature next_activation.Frame.pc out === Some (next_activation, padding)} @ immutable =
  fun signature instruction activation frames cells old_padding padding next_signature next_activation premise ->
    ghost_ (Codec.decode_def signature activation.Frame.pc cells;
      Model.successor_def signature instruction; Model.reshape_def signature instruction cells padding;
      Simple.step_def instruction (State.Running (activation, frames)));
    match cells with
    | Heap.Cell (current, Heap.Cell (accumulator, body)) ->
      (match Codec.decode_environment signature.G.locals body with
      | None -> unreachable_ ()
      | Some (env, temporary_cells) ->
        ghost_ (Seg.environment signature.G.locals body env temporary_cells ());
        let after = match instruction with
        | G.Save_environment _ ->
          let temporary_tail = Pad.temporaries signature.G.temporaries temporary_cells activation.Frame.temporaries old_padding padding () in
          ghost_ (Pad.prefix (Codec.temporaries_size signature.G.temporaries) temporary_cells padding);
          let saved = Seg.append env temporary_tail in
          let after = Seg.append env saved in
          ghost_ (Seg.join_environment signature.G.locals env temporary_tail ();
            Codec.decode_temporaries_def next_signature.G.temporaries saved;
            Seg.join_environment signature.G.locals env saved ());
          after
        | G.Save_value _ | G.Bind _ | G.Restore _ ->
          (match signature.G.temporaries with
          | G.Environment (context, rest_schema) ->
            ghost_ (Codec.decode_temporaries_def signature.G.temporaries temporary_cells);
            (match Codec.decode_environment context temporary_cells with
            | None -> unreachable_ ()
            | Some (saved, remaining_cells) ->
              ghost_ (Seg.environment context temporary_cells saved remaining_cells ());
              (match Codec.decode_temporaries rest_schema remaining_cells with
              | None -> unreachable_ ()
              | Some (remaining, _) ->
                let tail = Pad.temporaries rest_schema remaining_cells remaining old_padding padding () in
                ghost_ (Pad.prefix (Codec.temporaries_size rest_schema) remaining_cells padding);
                let saved_tail = Seg.append saved tail in
                ghost_ (Seg.join_environment context saved tail ());
                (match instruction with
                | G.Save_value _ ->
                  let temporary_tail = Heap.Cell (accumulator, saved_tail) in
                  let after = Seg.append saved temporary_tail in
                  ghost_ (Codec.decode_temporaries_def next_signature.G.temporaries temporary_tail;
                    Seg.join_environment context saved temporary_tail ()); after
                | G.Bind _ ->
                  let after_env = Seg.append saved saved_tail in
                  let after = Heap.Cell (accumulator, after_env) in
                  ghost_ (Codec.decode_temporaries_def signature.G.temporaries saved_tail;
                    Seg.join_environment context saved saved_tail ();
                    Codec.decode_environment_def next_signature.G.locals after); after
                | _ -> saved_tail)))
          | _ -> unreachable_ ())
        | _ -> unreachable_ () in
        let out = Heap.Cell (current, Heap.Cell (accumulator, after)) in
        ghost_ (Codec.decode_def next_signature next_activation.Frame.pc out);
        out)
    | _ -> unreachable_ ()
