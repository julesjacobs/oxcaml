module B = Wasm_u32
module D = Hm_declarative
module H = Hmc_heap_objects
module V = Hmc_tagged_cell
module R = Hmc_wasm_relayout
module Index = Hmc_u32_index
module Plan = Wasm_parallel_copy
module Copy = Wasm_cross_copy
module Range = Hmc_wasm_range_copy
module Cross = Wasm_cross_words
module Scatter = Wasm_scatter_words
module Memory = Wasm_scatter_memory
module W = Hmc_wire_word_sequence
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Q = Wasm_word_sequence
module Seg = Hmc_frame_segments
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module Bytes = Hmc_linear_bytes
module Bounds = Hmc_linear_bounds
module P = Hmc_linear_preservation
let[@def] (matches @ total) (plan : Plan.plan @ immutable) (length : D.index @ immutable) = ghost_ (
  match plan with Plan.Copy (a, b, Plan.Copy (c, d, rest)) -> a = 0 && b = 0 && c = 8 && d = 8 && R.range_is rest 0 0 length Plan.End | _ -> false)
let (build @ total) : (length : D.index) @ immutable -> (count : R.count) ->
    {u : unit | Index.represents length count} -> {plan : Plan.plan | matches plan length} @ immutable =
  fun length count premise ->
    let rest = R.range 0 0 count Plan.End length () in
    let plan = Plan.Copy (0, 0, Plan.Copy (8, 8, rest)) in
    ghost_ (matches_def plan length); plan
let rec (take_all @ total) : (cells : H.cells) @ immutable -> {u : unit | Seg.take (H.length cells) cells === Some cells} @ ghost =
  fun cells -> ghost_ (H.length_def cells; Seg.take_def (H.length cells) cells;
    match cells with H.Empty -> () | H.Cell (_, rest) -> take_all rest)
let[@def] (width @ total) (count : R.count) : B.u32 = 16 + 16 * count
let (correct @ total) : (plan : Plan.plan) @ immutable -> (pc : B.u32) -> (payload : H.cells) @ immutable -> (count : R.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (base_local : B.u32) ->
    {u : unit | matches plan (H.length payload) && Index.represents (H.length payload) count
      && source + 16 + 16 * count <= 4294967296 && base + 16 + 16 * count <= limit && Bounds.covers state.X.memory limit
      && Bytes.drop state.X.memory source === Some source_bytes
      && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number pc), payload))) source_bytes === Some (H.Cell (V.Word (Header.number pc), payload), source_suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : Memory.result | X.run (Copy.emit plan source_local base_local) state === X.Done {X.memory = out.Memory.memory; machine = state.X.machine}
      && Bytes.drop out.Memory.memory base === Some out.Memory.bytes
      && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number pc), payload))) out.Memory.bytes === Some (H.Cell (V.Word (Header.number pc), payload), out.Memory.suffix)
      && P.equal_prefix base state.X.memory out.Memory.memory && V.length out.Memory.memory === V.length state.X.memory && Bounds.covers out.Memory.memory limit
      && Bytes.drop state.X.memory (S.add32 base (width count)) === Some out.Memory.suffix
      && Bytes.drop out.Memory.memory (S.add32 base (width count)) === Some out.Memory.suffix} @ immutable =
  fun plan pc payload count state source base limit source_bytes source_suffix source_local base_local premise ->
    let cells = H.Cell (V.Word (Header.number pc), payload) in
    ghost_ (matches_def plan (H.length payload);
      H.length_def cells; Index.represents_def (H.length cells) (count + 1);
      W.size cells (count + 1) (16 + 16 * count) ();
      Seg.drop_def (D.S D.Z) cells; Seg.drop_def D.Z payload; take_all payload;
      Index.represents_def D.Z 0;
      Hmc_wasm_range_read.correct payload 0 count D.Z cells payload state.X.memory source source_bytes source_suffix ();
      Hmc_heap_simple.lookup_def cells D.Z;
      Hmc_wasm_cells_read.correct state.X.memory source source_bytes (H.length cells) cells source_suffix D.Z 0 0 8 (V.Word (Header.number pc)) ());
    ghost_ (match plan with
    | Plan.Copy (_, _, (Plan.Copy (_, _, rest) as second)) ->
      Hmc_wasm_cross_range_words.correct payload rest Plan.End 0 0 count state.X.memory source ();
      Range.tag_def 0;
      Scatter.matches_def Plan.End (16 + 16 * count) Q.End state.X.memory source;
      W.size payload count (16 * count) ();
      Wasm_scatter_range.correct rest Plan.End 16 16 (W.words payload) Q.End state.X.memory source (16 * count) (16 + 16 * count) ();
      Wasm_scatter_range.right_identity (W.words payload);
      W.words_def cells;
      Scatter.matches_def plan 0 (W.words cells) state.X.memory source;
      Scatter.matches_def second 8 (Q.Word (V.payload (V.Word (Header.number pc)), W.words payload)) state.X.memory source
    | _ -> ());
    ghost_ (Memory.zero_def (); width_def count);
    let result = Memory.correct plan (W.words cells) (width count) state base limit source source_local base_local () in
    ghost_ (W.recover cells result.Memory.bytes result.Memory.suffix ());
    result
