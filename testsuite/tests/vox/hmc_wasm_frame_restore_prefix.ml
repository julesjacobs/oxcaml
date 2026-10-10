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
module C = Wasm_code
module Restore = Hmc_wasm_frame_restore
module Prefix = Wasm_instruction_prefix
module Copy_prefix = Wasm_cross_copy_prefix
module Range_prefix = Hmc_wasm_range_prefix
let (correct @ total) : (plan : Plan.plan) @ immutable -> (pc : B.u32) -> (payload : H.cells) @ immutable -> (count : R.count) ->
    (state : X.state) @ immutable -> (source : B.u32) -> (base : B.u32) -> (limit : B.u32) ->
    (source_bytes : B.bytes) @ immutable -> (source_suffix : B.bytes) @ immutable -> (source_local : B.u32) -> (base_local : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | Restore.matches plan (H.length payload) && Index.represents (H.length payload) count
      && source + 16 + 16 * count <= 4294967296 && base + 16 + 16 * count <= limit && Bounds.covers state.X.memory limit
      && Bytes.drop state.X.memory source === Some source_bytes
      && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number pc), payload))) source_bytes === Some (H.Cell (V.Word (Header.number pc), payload), source_suffix)
      && L.get state.X.machine.E.locals source_local === Some (S.I32 source)
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)} ->
    {out : Memory.result | X.run (Copy.emit plan source_local base_local) state === X.Done {X.memory = out.Memory.memory; machine = state.X.machine}
      && Bytes.drop out.Memory.memory base === Some out.Memory.bytes
      && Wire.decode_cells (H.length (H.Cell (V.Word (Header.number pc), payload))) out.Memory.bytes === Some (H.Cell (V.Word (Header.number pc), payload), out.Memory.suffix)
      && match X.run (Prefix.take fuel (Copy.emit plan source_local base_local)) state with
        | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
          && Copy_prefix.partial plan state.X.memory source base current.X.memory
          && P.equal_prefix base state.X.memory current.X.memory
          && Bytes.drop state.X.memory (S.add32 base (Restore.width count)) === Bytes.drop current.X.memory (S.add32 base (Restore.width count))
          && V.length current.X.memory === V.length state.X.memory && Bounds.covers current.X.memory limit
        | _ -> false} @ ghost =
  fun plan pc payload count state source base limit source_bytes source_suffix source_local base_local fuel premise -> ghost_ (
    let out = Restore.correct plan pc payload count state source base limit source_bytes source_suffix source_local base_local () in
    let stop : B.u32 = base + 16 + 16 * count in
    let _ = Bounds.suffix state.X.memory limit base () in
    Bounds.covers_def state.X.memory base;
    Restore.matches_def plan (H.length payload); Restore.width_def count; S.add32_def base (Restore.width count);
    (match plan with
    | Plan.Copy (_, _, (Plan.Copy (_, _, rest) as second)) ->
      Copy_prefix.bounded_def Plan.End base base stop;
      Range_prefix.bounded rest Plan.End 0 0 (H.length payload) count base base stop ();
      Copy_prefix.bounded_def second base base stop;
      Copy_prefix.bounded_def plan base base stop
    | _ -> ());
    Copy_prefix.reflect plan source_local base_local state source base {X.memory = out.Memory.memory; machine = state.X.machine} ();
    Copy_prefix.region plan source_local base_local state source base out.Memory.memory base stop fuel ();
    (match X.run (Prefix.take fuel (Copy.emit plan source_local base_local)) state with
    | X.Done current -> Bounds.same_length state.X.memory current.X.memory limit ()
    | _ -> ());
    out)
