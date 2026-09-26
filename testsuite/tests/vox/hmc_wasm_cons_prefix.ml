module B = Wasm_u32
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module K = Hmc_closure_ir
module Image = Hmc_heap_image
module Bounds = Hmc_linear_bounds
module Bytes = Hmc_linear_bytes
module P = Hmc_linear_preservation
module X = Wasm_memory_execution
module E = Wasm_execution
module S = Wasm_scalar
module L = Wasm_locals
module C = Wasm_code
module W = Wasm_frame_write
module Write = Hmc_wasm_cons_write
module Memory = Hmc_wasm_cons_memory
module Prefix = Wasm_frame_write_prefix
module Instruction_prefix = Wasm_instruction_prefix

let (correct @ total) : (table : K.table) @ immutable -> (heap : Heap.heap) @ immutable ->
    (head : V.value) @ immutable -> (tail : V.value) @ immutable ->
    (state : X.state) @ immutable -> (base : B.u32) -> (stop : B.u32) -> (limit : B.u32) ->
    (base_local : B.u32) -> (head_tag : B.u32) -> (head_payload : B.u32) ->
    (tail_tag : B.u32) -> (tail_payload : B.u32) -> (fuel : C.count) @ immutable ->
    {u : unit | Heap.valid table heap && Heap.used heap = base && stop = base + 32 && stop <= limit
      && Image.related state.X.memory heap && Bounds.covers state.X.memory limit
      && L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals head_tag === Some (S.I64 (V.tag head))
      && L.get state.X.machine.E.locals head_payload === Some (S.I64 (V.payload head))
      && L.get state.X.machine.E.locals tail_tag === Some (S.I64 (V.tag tail))
      && L.get state.X.machine.E.locals tail_payload === Some (S.I64 (V.payload tail))} ->
    {u : unit | match X.run (Instruction_prefix.take fuel (Write.emit base_local head_tag head_payload tail_tag tail_payload)) state with
      | X.Done current -> current.X.machine.E.locals === state.X.machine.E.locals
        && Prefix.partial (Write.writes head_tag head_payload tail_tag tail_payload)
          state.X.memory base state.X.machine.E.locals current.X.memory
        && Image.related current.X.memory heap && Bounds.covers current.X.memory limit
        && P.equal_prefix base state.X.memory current.X.memory
        && Bytes.drop state.X.memory stop === Bytes.drop current.X.memory stop
        && V.length current.X.memory === V.length state.X.memory
      | _ -> false} @ ghost =
  fun table heap head tail state base stop limit base_local head_tag head_payload tail_tag tail_payload fuel premise -> ghost_ (
    let written = Memory.correct head tail state base limit base_local head_tag head_payload tail_tag tail_payload () in
    let after = {X.memory = written.Memory.memory; machine = state.X.machine} in
    let writes = Write.writes head_tag head_payload tail_tag tail_payload in
    Write.emit_def base_local head_tag head_payload tail_tag tail_payload;
    Write.writes_def head_tag head_payload tail_tag tail_payload;
    Prefix.bounded_def writes base base stop;
    Prefix.bounded_def (W.Write (8, head_payload, W.Write (16, tail_tag, W.Write (24, tail_payload, W.End)))) base base stop;
    Prefix.bounded_def (W.Write (16, tail_tag, W.Write (24, tail_payload, W.End))) base base stop;
    Prefix.bounded_def (W.Write (24, tail_payload, W.End)) base base stop;
    Prefix.bounded_def W.End base base stop;
    let _ = Bounds.suffix state.X.memory limit base () in Bounds.covers_def state.X.memory base;
    Prefix.region writes base_local state base after base stop fuel ();
    match X.run (Instruction_prefix.take fuel (W.emit writes base_local)) state with
    | X.Done current ->
      Image.preserve table state.X.memory current.X.memory heap base ();
      Bounds.same_length state.X.memory current.X.memory limit ()
    | _ -> ())
