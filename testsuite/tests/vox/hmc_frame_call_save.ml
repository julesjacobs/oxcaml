module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
let rec (skip @ total) : (count : D.index) @ immutable -> (cells : H.cells) @ immutable ->
    {u : unit | Seg.drop (D.S count) cells === (match Seg.drop count cells with
      | Some (H.Cell (_, rest)) -> Some rest | _ -> None)} @ ghost =
  fun count cells -> ghost_ (
    Seg.drop_def count cells; Seg.drop_def (D.S count) cells;
    match count, cells with
    | D.Z, H.Cell (_, rest) -> Seg.drop_def D.Z rest
    | D.S n, H.Cell (_, rest) -> skip n rest
    | _ -> ())
let[@def] (signature @ total) (context : D.context @ immutable) (rest : G.temporaries @ immutable) =
  {G.locals = context; temporaries = rest; accumulator = None}
let[@def] (cells @ total) (current : V.value @ immutable) (accumulator : V.value @ immutable) (remaining : H.cells @ immutable) =
  H.Cell (current, H.Cell (accumulator, remaining))
let (correct @ total) : (before : G.signature) @ immutable -> (context : D.context) @ immutable ->
    (ty : D.mono) @ immutable -> (rest : G.temporaries) @ immutable -> (next : D.index) @ immutable ->
    (activation : F.activation) @ immutable -> (closure : V.value) @ immutable -> (env : H.cells) @ immutable ->
    (temporaries : F.temporaries) @ immutable -> (source : H.cells) @ immutable -> (remaining : H.cells) @ immutable -> (tail : H.cells) @ immutable ->
    {u : unit | before.G.temporaries === G.Value (context, ty, rest)
      && activation.F.temporaries === F.Value (closure, env, temporaries)
      && Codec.decode before activation.F.pc source === Some (activation, tail)
      && Seg.drop (D.S (D.S (D.S (Codec.locals_size before.G.locals)))) source === Some remaining} ->
    {u : unit | Codec.decode (signature context rest) next (cells activation.F.current activation.F.accumulator remaining) ===
      Some ({activation with F.pc = next; env; temporaries}, tail)} @ ghost =
  fun before context ty rest next activation closure env temporaries source remaining tail premise -> ghost_ (
    Codec.decode_def before activation.F.pc source;
    Seg.drop_def (D.S (D.S (D.S (Codec.locals_size before.G.locals)))) source;
    match source with
    | H.Cell (current, H.Cell (accumulator, body)) ->
      Seg.drop_def (D.S (D.S (Codec.locals_size before.G.locals))) (H.Cell (accumulator, body));
      Seg.drop_def (D.S (Codec.locals_size before.G.locals)) body;
      (match Codec.decode_environment before.G.locals body with
      | None -> ()
      | Some (locals, pending) ->
        Seg.environment before.G.locals body locals pending ();
        Codec.decode_temporaries_def before.G.temporaries pending;
        skip (Codec.locals_size before.G.locals) body;
        (match pending with
        | H.Empty -> ()
        | H.Cell (_, saved) ->
          Seg.drop_def (D.S D.Z) pending; Seg.drop_def D.Z saved;
          signature_def context rest; cells_def activation.F.current activation.F.accumulator remaining;
          Codec.decode_def (signature context rest) next (cells activation.F.current activation.F.accumulator remaining)))
    | _ -> ())
