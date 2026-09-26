module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
let rec (environment @ total) : (context : D.context) @ immutable -> (cells : H.cells) @ immutable ->
    (env : H.cells) @ immutable -> (tail : H.cells) @ immutable -> (suffix : H.cells) @ immutable ->
    {u : unit | Codec.decode_environment context cells === Some (env, tail)} ->
    {u : unit | Codec.decode_environment context (Seg.append cells suffix) === Some (env, Seg.append tail suffix)} @ ghost =
  fun context cells env tail suffix premise -> ghost_ (
    Codec.decode_environment_def context cells; Seg.append_def cells suffix;
    Codec.decode_environment_def context (Seg.append cells suffix);
    match context, cells with
    | D.Empty_context, _ -> ()
    | D.Binding (_, rest), H.Cell (_, remaining) ->
      (match Codec.decode_environment rest remaining with None -> () | Some (more, tail) -> environment rest remaining more tail suffix ())
    | _ -> ())
let rec (temporaries @ total) : (schema : G.temporaries) @ immutable -> (cells : H.cells) @ immutable ->
    (runtime : F.temporaries) @ immutable -> (tail : H.cells) @ immutable -> (suffix : H.cells) @ immutable ->
    {u : unit | Codec.decode_temporaries schema cells === Some (runtime, tail)} ->
    {u : unit | Codec.decode_temporaries schema (Seg.append cells suffix) === Some (runtime, Seg.append tail suffix)} @ ghost =
  fun schema cells runtime tail suffix premise -> ghost_ (
    Codec.decode_temporaries_def schema cells; Codec.decode_temporaries_def schema (Seg.append cells suffix);
    match schema with
    | G.Empty_temporaries -> ()
    | G.Environment (context, rest) ->
      (match Codec.decode_environment context cells with None -> () | Some (env, after) ->
        environment context cells env after suffix ();
        (match Codec.decode_temporaries rest after with None -> () | Some (remaining, tail) -> temporaries rest after remaining tail suffix ()))
    | G.Value (context, _, rest) ->
      Seg.append_def cells suffix;
      (match cells with H.Empty -> () | H.Cell (_, more) ->
        (match Codec.decode_environment context more with None -> () | Some (env, after) ->
          environment context more env after suffix ();
          (match Codec.decode_temporaries rest after with None -> () | Some (remaining, tail) -> temporaries rest after remaining tail suffix ()))))
let (correct @ total) : (signature : G.signature) @ immutable -> (pc : D.index) @ immutable -> (cells : H.cells) @ immutable ->
    (activation : F.activation) @ immutable -> (tail : H.cells) @ immutable -> (suffix : H.cells) @ immutable ->
    {u : unit | Codec.decode signature pc cells === Some (activation, tail)} ->
    {u : unit | Codec.decode signature pc (Seg.append cells suffix) === Some (activation, Seg.append tail suffix)} @ ghost =
  fun signature pc cells activation tail suffix premise -> ghost_ (
    Codec.decode_def signature pc cells; Seg.append_def cells suffix;
    Codec.decode_def signature pc (Seg.append cells suffix);
    match cells with
    | H.Cell (_, (H.Cell (_, body) as more)) ->
      Seg.append_def more suffix;
      (match Codec.decode_environment signature.G.locals body with None -> () | Some (env, after) ->
        environment signature.G.locals body env after suffix ();
        (match Codec.decode_temporaries signature.G.temporaries after with None -> () | Some (runtime, tail) ->
          temporaries signature.G.temporaries after runtime tail suffix ()))
    | _ -> ())
