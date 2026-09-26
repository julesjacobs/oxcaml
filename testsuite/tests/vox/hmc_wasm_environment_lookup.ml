module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Simple = Hmc_heap_simple
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Index = Hmc_u32_index
module L = Hmc_linear_bytes
let rec (environment @ total) : (context : D.context) @ immutable -> (cells : Heap.cells) @ immutable ->
    (env : Heap.cells) @ immutable -> (tail : Heap.cells) @ immutable -> (index : D.index) @ immutable -> (value : V.value) @ immutable ->
    {u : unit | Codec.decode_environment context cells === Some (env, tail) && Simple.lookup env index === Some value} ->
    {u : unit | Simple.lookup cells index === Some value} @ ghost = fun context cells env tail index value premise -> ghost_ (
    Codec.decode_environment_def context cells; Simple.lookup_def env index; Simple.lookup_def cells index;
    match context, cells, env, index with
    | D.Binding (_, rest), Heap.Cell (_, more), Heap.Cell (_, remaining), D.S previous ->
      (match Codec.decode_environment rest more with None -> () | Some (_, after) -> environment rest more remaining after previous value ())
    | _ -> ())
let rec (find @ total) : (count : D.index) @ immutable -> (bytes : B.bytes) @ immutable -> (cells : Heap.cells) @ immutable ->
    (tail : B.bytes) @ immutable -> (index : D.index) @ immutable -> (number : W.limb) -> (offset : B.u32) -> (value : V.value) @ immutable ->
    {u : unit | Wire.decode_cells count bytes === Some (cells, tail) && Simple.lookup cells index === Some value
      && Index.represents index number && offset = 16 * number} ->
    {suffix : B.bytes | L.drop bytes offset === Some suffix && (match V.decode suffix with Some (actual, _) -> actual === value | None -> false)} @ immutable =
  fun count bytes cells tail index number offset value premise ->
    ghost_ (Wire.decode_cells_def count bytes; Simple.lookup_def cells index; Index.represents_def index number);
    match count, cells with
    | D.S rest, Heap.Cell (head, more) ->
      (match V.decode bytes with
      | None -> unreachable_ ()
      | Some (_, after) ->
        match index with
        | D.Z -> ghost_ (L.drop_def bytes offset); bytes
        | D.S previous ->
          let suffix = find rest after more tail previous (number - 1) (offset - 16) value () in
          ghost_ (L.drop_def bytes 0; Wasm_cell_suffix.suffix bytes 0 bytes head after 16 ();
            Wasm_cell.shift bytes 16 (offset - 16) offset after ()); suffix)
    | _ -> unreachable_ ()
