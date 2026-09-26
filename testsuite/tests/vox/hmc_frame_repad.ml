module D = Hm_declarative
module Heap = Hmc_heap_objects
module G = Hmc_cfg_ir
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Seg = Hmc_frame_segments
let[@def] rec (splice @ total) (count : D.index @ immutable) (cells : Heap.cells @ immutable) (tail : Heap.cells @ immutable) =
  match count, cells with
  | D.Z, _ -> Some tail
  | D.S n, Heap.Cell (value, rest) -> (match splice n rest tail with None -> None | Some after -> Some (Heap.Cell (value, after)))
  | _ -> None
let rec (prefix @ total) : (count : D.index) @ immutable -> (cells : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | splice count cells tail === (match Seg.take count cells with None -> None | Some head -> Some (Seg.append head tail))} @ ghost =
  fun count cells tail -> ghost_ (
    splice_def count cells tail; Seg.take_def count cells;
    match count, cells with
    | D.Z, _ -> Seg.append_def Heap.Empty tail
    | D.S n, Heap.Cell (value, rest) ->
      prefix n rest tail;
      (match Seg.take n rest with None -> () | Some head -> Seg.append_def (Heap.Cell (value, head)) tail)
    | _ -> ())
let rec (add @ total) : (first : D.index) @ immutable -> (second : D.index) @ immutable ->
    (cells : Heap.cells) @ immutable -> (middle : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    (after : Heap.cells) @ immutable -> (out : Heap.cells) @ immutable ->
    {u : unit | Seg.drop first cells === Some middle && splice second middle tail === Some after && splice first cells after === Some out} ->
    {u : unit | splice (D.add first second) cells tail === Some out} @ ghost =
  fun first second cells middle tail after out premise -> ghost_ (
    D.add_def first second; Seg.drop_def first cells;
    splice_def first cells after; splice_def (D.add first second) cells tail;
    match first, cells with
    | D.S n, Heap.Cell (_, rest) -> (match splice n rest after with None -> () | Some remaining -> add n second rest middle tail after remaining ())
    | _ -> ())
let rec (environment @ total) : (context : D.context) @ immutable -> (cells : Heap.cells) @ immutable ->
    (env : Heap.cells) @ immutable -> (old_tail : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_environment context cells === Some (env, old_tail)} ->
    {out : Heap.cells | splice (Codec.locals_size context) cells tail === Some out
      && Codec.decode_environment context out === Some (env, tail)} @ immutable =
  fun context cells env old_tail tail premise ->
    ghost_ (Codec.decode_environment_def context cells; Codec.locals_size_def context; splice_def (Codec.locals_size context) cells tail);
    match context, cells, env with
    | D.Empty_context, _, _ -> ghost_ (Codec.decode_environment_def context tail); tail
    | D.Binding (_, rest), Heap.Cell (value, more), Heap.Cell (_, remaining) ->
      let after = environment rest more remaining old_tail tail () in
      let out = Heap.Cell (value, after) in
      ghost_ (Codec.decode_environment_def context out); out
    | _ -> unreachable_ ()
let rec (temporaries @ total) : (schema : G.temporaries) @ immutable -> (cells : Heap.cells) @ immutable ->
    (runtime : Frame.temporaries) @ immutable -> (old_tail : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_temporaries schema cells === Some (runtime, old_tail)} ->
    {out : Heap.cells | splice (Codec.temporaries_size schema) cells tail === Some out
      && Codec.decode_temporaries schema out === Some (runtime, tail)} @ immutable =
  fun schema cells runtime old_tail tail premise ->
    ghost_ (Codec.decode_temporaries_def schema cells; Codec.temporaries_size_def schema);
    match schema with
    | G.Empty_temporaries -> ghost_ (splice_def D.Z cells tail; Codec.decode_temporaries_def schema tail); tail
    | G.Environment (context, rest) ->
      (match Codec.decode_environment context cells with
      | None -> unreachable_ ()
      | Some (env, more) -> (match Codec.decode_temporaries rest more with
        | None -> unreachable_ ()
        | Some (remaining, _) ->
          let after = temporaries rest more remaining old_tail tail () in
          let out = environment context cells env more after () in
          ghost_ (Seg.environment context cells env more ();
            add (Codec.locals_size context) (Codec.temporaries_size rest) cells more tail after out ();
            Codec.decode_temporaries_def schema out); out))
    | G.Value (context, _, rest) -> (match cells with
      | Heap.Empty -> unreachable_ ()
      | Heap.Cell (value, body) -> (match Codec.decode_environment context body with
        | None -> unreachable_ ()
        | Some (env, more) -> (match Codec.decode_temporaries rest more with
          | None -> unreachable_ ()
          | Some (remaining, _) ->
            let after = temporaries rest more remaining old_tail tail () in
            let prefix = environment context body env more after () in
            let out = Heap.Cell (value, prefix) in
            ghost_ (Seg.environment context body env more ();
              add (Codec.locals_size context) (Codec.temporaries_size rest) body more tail after prefix ();
              splice_def (Codec.temporaries_size schema) cells tail; Codec.decode_temporaries_def schema out); out)))
