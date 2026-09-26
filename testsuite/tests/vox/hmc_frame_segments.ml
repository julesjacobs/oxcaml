module D = Hm_declarative
module Heap = Hmc_heap_objects
module G = Hmc_cfg_ir
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
let[@def] rec (append @ total) (left : Heap.cells @ immutable) (right : Heap.cells @ immutable) = match left with
  | Heap.Empty -> right | Heap.Cell (value, rest) -> Heap.Cell (value, append rest right)
let[@def] rec (take @ total) (count : D.index @ immutable) (cells : Heap.cells @ immutable) = match count, cells with
  | D.Z, _ -> Some Heap.Empty
  | D.S n, Heap.Cell (value, rest) -> (match take n rest with None -> None | Some prefix -> Some (Heap.Cell (value, prefix)))
  | _ -> None
let[@def] rec (drop @ total) (count : D.index @ immutable) (cells : Heap.cells @ immutable) = match count, cells with
  | D.Z, _ -> Some cells | D.S n, Heap.Cell (_, rest) -> drop n rest | _ -> None
let rec (drop_add @ total) : (first : D.index) @ immutable -> (second : D.index) @ immutable -> (cells : Heap.cells) @ immutable ->
    {u : unit | drop (D.add first second) cells === (match drop first cells with None -> None | Some rest -> drop second rest)} @ ghost =
  fun first second cells -> ghost_ (
    D.add_def first second; drop_def first cells; drop_def (D.add first second) cells;
    match first, cells with D.S n, Heap.Cell (_, rest) -> drop_add n second rest | _ -> ())
let rec (environment @ total) : (context : D.context) @ immutable -> (cells : Heap.cells) @ immutable ->
    (env : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_environment context cells === Some (env, tail)} ->
    {u : unit | take (Codec.locals_size context) cells === Some env && drop (Codec.locals_size context) cells === Some tail
      && append env tail === cells && Codec.environment context env} @ ghost = fun context cells env tail premise -> ghost_ (
    Codec.decode_environment_def context cells; Codec.locals_size_def context;
    take_def (Codec.locals_size context) cells; drop_def (Codec.locals_size context) cells; append_def env tail; Codec.environment_def context env;
    match context, cells, env with
    | D.Binding (_, rest), Heap.Cell (_, more), Heap.Cell (_, remaining) -> environment rest more remaining tail ()
    | _ -> ())
let rec (join_environment @ total) : (context : D.context) @ immutable -> (env : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.environment context env} ->
    {u : unit | Codec.decode_environment context (append env tail) === Some (env, tail)} @ ghost =
  fun context env tail premise -> ghost_ (
    Codec.environment_def context env; append_def env tail; Codec.decode_environment_def context (append env tail);
    match context, env with
    | D.Binding (_, rest), Heap.Cell (_, remaining) -> join_environment rest remaining tail ()
    | _ -> ())
let rec (temporaries @ total) : (schema : G.temporaries) @ immutable -> (cells : Heap.cells) @ immutable ->
    (runtime : Frame.temporaries) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_temporaries schema cells === Some (runtime, tail)} ->
    {u : unit | drop (Codec.temporaries_size schema) cells === Some tail} @ ghost = fun schema cells runtime tail premise -> ghost_ (
    Codec.decode_temporaries_def schema cells; Codec.temporaries_size_def schema;
    match schema with
    | G.Empty_temporaries -> drop_def D.Z cells
    | G.Environment (context, rest) -> (match Codec.decode_environment context cells with
      | None -> ()
      | Some (env, after) ->
        environment context cells env after ();
        (match Codec.decode_temporaries rest after with None -> () | Some (remaining, _) -> temporaries rest after remaining tail ());
        drop_add (Codec.locals_size context) (Codec.temporaries_size rest) cells)
    | G.Value (context, _, rest) -> (match cells with
      | Heap.Empty -> ()
      | Heap.Cell (_, more) ->
        drop_def (Codec.temporaries_size schema) cells;
        (match Codec.decode_environment context more with
        | None -> ()
        | Some (env, after) ->
          environment context more env after ();
          (match Codec.decode_temporaries rest after with None -> () | Some (remaining, _) -> temporaries rest after remaining tail ());
          drop_add (Codec.locals_size context) (Codec.temporaries_size rest) more)))
