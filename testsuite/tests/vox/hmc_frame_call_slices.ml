module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
module View = Hmc_frame_slices
module Cut = Hmc_cell_capacity
module Repad = Hmc_frame_repad
module Save = Hmc_frame_call_save
let rec (right_identity @ total) : (cells : H.cells) @ immutable -> {u : unit | Seg.append cells H.Empty === cells} @ ghost =
  fun cells -> ghost_ (Seg.append_def cells H.Empty; match cells with H.Empty -> () | H.Cell (_, rest) -> right_identity rest)
type result = {start : H.cells; remaining : H.cells; saved : F.activation; closure : V.value}
let (correct @ total) : (signature : G.signature) @ immutable -> (activation : F.activation) @ immutable ->
    (cells : H.cells) @ immutable -> (padding : H.cells) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (schema : G.temporaries) @ immutable -> (next : D.index) @ immutable ->
    {u : unit | signature.G.temporaries === G.Value (context, ty, schema)
      && Codec.decode signature activation.F.pc cells === Some (activation, padding)} ->
    {out : result | Seg.drop (D.S (D.S (D.S (Codec.locals_size signature.G.locals)))) cells === Some out.start
      && Seg.take (H.length out.remaining) out.start === Some out.remaining
      && H.length out.remaining === D.add (Codec.locals_size context) (Codec.temporaries_size schema)
      && activation.F.temporaries === F.Value (out.closure, out.saved.F.env, out.saved.F.temporaries)
      && out.saved.F.pc === next && out.saved.F.current === activation.F.current && out.saved.F.accumulator === activation.F.accumulator
      && Codec.decode (Save.signature context schema) next (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, out.remaining))) === Some (out.saved, H.Empty)} @ immutable =
  fun signature activation cells padding context ty schema next premise ->
    let view = View.decode signature activation cells padding () in
    ghost_ (Codec.decode_temporaries_def signature.G.temporaries view.View.temporaries;
      Seg.temporaries signature.G.temporaries view.View.temporaries activation.F.temporaries padding ();
      Codec.temporaries_size_def signature.G.temporaries);
    match view.View.temporaries with
    | H.Empty -> unreachable_ ()
    | H.Cell (closure, start) ->
      ghost_ (Seg.drop_def (Codec.temporaries_size signature.G.temporaries) view.View.temporaries);
      let length = D.add (Codec.locals_size context) (Codec.temporaries_size schema) in
      let remaining = Cut.cut length start padding () in
      (match Codec.decode_environment context start with
      | None -> unreachable_ ()
      | Some (env, after) ->
        (match Codec.decode_temporaries schema after with
        | None -> unreachable_ ()
        | Some (temporaries, _) ->
          let truncated = Repad.temporaries schema after temporaries padding H.Empty () in
          let compact = Repad.environment context start env after truncated () in
          let saved = {activation with F.pc = next; env; temporaries} in
          ghost_ (Seg.environment context start env after ();
            Repad.add (Codec.locals_size context) (Codec.temporaries_size schema) start after H.Empty truncated compact ();
            Repad.prefix length start H.Empty; right_identity remaining;
            Save.signature_def context schema;
            Codec.decode_def (Save.signature context schema) next (H.Cell (activation.F.current, H.Cell (activation.F.accumulator, remaining)));
            Save.skip (Codec.locals_size signature.G.locals) view.View.body;
            Seg.drop_def (D.S (D.S (D.S (Codec.locals_size signature.G.locals)))) cells;
            Seg.drop_def (D.S (D.S (Codec.locals_size signature.G.locals))) (H.Cell (activation.F.accumulator, view.View.body)));
          {start; remaining; saved; closure}))
