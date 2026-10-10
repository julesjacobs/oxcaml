module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module W = Hmc_wire_word_sequence
module Codec = Hmc_pointer_frame_codec
module Save = Hmc_wasm_call_save
module Memory = Hmc_wasm_call_save_memory
module Geometry = Hmc_wasm_relayout_geometry
module Split = Hmc_wasm_range_split
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Plan = Wasm_parallel_copy
module Cross = Wasm_cross_words
module Q = Wasm_word_sequence
let (correct @ total) : (fragment : Save.fragment) @ immutable -> (signature : G.signature) @ immutable ->
    (next : D.index) @ immutable -> (capacity : R.count) -> (context : D.context) @ immutable -> (ty : D.mono) @ immutable ->
    (schema : G.temporaries) @ immutable -> (env : R.count) -> (remaining : H.cells) @ immutable ->
    (pc : B.u32) -> (current : V.value) @ immutable -> (accumulator : V.value) @ immutable ->
    {u : unit | Save.matches signature next capacity fragment && signature.G.temporaries === G.Value (context, ty, schema)
      && Index.represents (Codec.locals_size signature.G.locals) env
      && H.length remaining === D.add (Codec.locals_size context) (Codec.temporaries_size schema)} ->
    {tail : Plan.plan | Cross.matches fragment.Save.copies (Wasm_scatter_memory.zero ()) (Wasm_scatter_memory.zero ())
      (W.words (Memory.header pc current accumulator)) tail && R.range_is tail (3 + env) 2 (H.length remaining) Plan.End} @ immutable =
  fun fragment signature next capacity context ty schema env remaining pc current accumulator premise ->
    ghost_ (Save.matches_def signature next capacity fragment;
      Geometry.size_represents (Codec.locals_size signature.G.locals) env ();
      match fragment.Save.copies with
      | Plan.Copy (_, _, Plan.Copy (_, _, cells)) ->
        Geometry.two_def cells 0 0 (D.S (D.S D.Z)) (3 + env) 2 (H.length remaining);
        Geometry.split_def cells 0 0 (D.S (D.S D.Z));
        (match cells with Plan.Copy (_, _, Plan.Copy (_, _, after)) ->
          Geometry.split_def after 1 1 (D.S D.Z) | _ -> ())
      | _ -> ());
    match fragment.Save.copies with
    | Plan.Copy (_, _, Plan.Copy (_, _, (Plan.Copy (_, _, Plan.Copy (_, _, Plan.Copy (_, _, Plan.Copy (_, _, tail)))) as cells))) ->
      ghost_ (Geometry.two_def cells 0 0 (D.S (D.S D.Z)) (3 + env) 2 (H.length remaining);
        Geometry.split_def cells 0 0 (D.S (D.S D.Z));
        (match cells with Plan.Copy (_, _, Plan.Copy (_, _, after)) ->
          Geometry.split_def after 1 1 (D.S D.Z); (match after with Plan.Copy (_, _, Plan.Copy (_, _, end_)) -> Geometry.split_def end_ 2 2 D.Z | _ -> ()) | _ -> ());
        Split.correct tail (3 + env) 2 (H.length remaining) Plan.End; Split.split_def tail (3 + env) 2 (H.length remaining);
        Wasm_scatter_memory.zero_def (); Memory.header_def pc current accumulator;
        W.words_def (Memory.header pc current accumulator);
        W.words_def (H.Cell (current, H.Cell (accumulator, H.Empty))); W.words_def (H.Cell (accumulator, H.Empty)); W.words_def H.Empty;
        let values = W.words (Memory.header pc current accumulator) in
        Cross.matches_def fragment.Save.copies 0 0 values tail;
        match fragment.Save.copies, values with Plan.Copy (_, _, p1), Q.Word (_, w1) ->
          Cross.matches_def p1 8 8 w1 tail; (match p1, w1 with Plan.Copy (_, _, p2), Q.Word (_, w2) ->
            Cross.matches_def p2 16 16 w2 tail; (match p2, w2 with Plan.Copy (_, _, p3), Q.Word (_, w3) ->
              Cross.matches_def p3 24 24 w3 tail; (match p3, w3 with Plan.Copy (_, _, p4), Q.Word (_, w4) ->
                Cross.matches_def p4 32 32 w4 tail; (match p4, w4 with Plan.Copy (_, _, p5), Q.Word (_, w5) ->
                  Cross.matches_def p5 40 40 w5 tail; Cross.matches_def tail 48 48 Q.End tail | _ -> ()) | _ -> ()) | _ -> ()) | _ -> ()) | _ -> ());
      tail
    | _ -> unreachable_ ()
