module D = Hm_declarative
module Heap = Hmc_heap_objects
module G = Hmc_cfg_ir
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
let rec (environment @ total) : (context : D.context) @ immutable -> (left : Heap.cells) @ immutable -> (right : Heap.cells) @ immutable ->
    (env : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_environment context left === Some (env, tail) && Codec.decode_environment context right === Some (env, tail)} ->
    {u : unit | left === right} @ ghost = fun context left right env tail premise -> ghost_ (
    Codec.decode_environment_def context left; Codec.decode_environment_def context right;
    match context, left, right, env with
    | D.Binding (_, rest), Heap.Cell (_, a), Heap.Cell (_, b), Heap.Cell (_, remaining) -> environment rest a b remaining tail ()
    | _ -> ())
let rec (temporaries @ total) : (schema : G.temporaries) @ immutable -> (left : Heap.cells) @ immutable -> (right : Heap.cells) @ immutable ->
    (runtime : Frame.temporaries) @ immutable -> (tail : Heap.cells) @ immutable ->
    {u : unit | Codec.decode_temporaries schema left === Some (runtime, tail) && Codec.decode_temporaries schema right === Some (runtime, tail)} ->
    {u : unit | left === right} @ ghost = fun schema left right runtime tail premise -> ghost_ (
    Codec.decode_temporaries_def schema left; Codec.decode_temporaries_def schema right;
    match schema with
    | G.Empty_temporaries -> ()
    | G.Environment (context, rest) -> (match Codec.decode_environment context left, Codec.decode_environment context right with
      | Some (env, a), Some (_, b) -> (match Codec.decode_temporaries rest a with
        | None -> () | Some (remaining, _) -> temporaries rest a b remaining tail (); environment context left right env a ())
      | _ -> ())
    | G.Value (context, _, rest) -> (match left, right with
      | Heap.Cell (_, a), Heap.Cell (_, b) -> (match Codec.decode_environment context a, Codec.decode_environment context b with
        | Some (env, c), Some (_, d) -> (match Codec.decode_temporaries rest c with
          | None -> () | Some (remaining, _) -> temporaries rest c d remaining tail (); environment context a b env c ())
        | _ -> ())
      | _ -> ()))
let (frame @ total) : (signature : G.signature) @ immutable -> (pc : D.index) @ immutable ->
    (left : Heap.cells) @ immutable -> (right : Heap.cells) @ immutable -> (activation : Frame.activation) @ immutable -> (padding : Heap.cells) @ immutable ->
    {u : unit | Codec.decode signature pc left === Some (activation, padding) && Codec.decode signature pc right === Some (activation, padding)} ->
    {u : unit | left === right} @ ghost = fun signature pc left right activation padding premise -> ghost_ (
    Codec.decode_def signature pc left; Codec.decode_def signature pc right;
    match left, right with
    | Heap.Cell (_, Heap.Cell (_, a)), Heap.Cell (_, Heap.Cell (_, b)) ->
      (match Codec.decode_environment signature.G.locals a, Codec.decode_environment signature.G.locals b with
      | Some (_, c), Some (_, d) ->
        temporaries signature.G.temporaries c d activation.Frame.temporaries padding ();
        environment signature.G.locals a b activation.Frame.env c ()
      | _ -> ())
    | _ -> ())
