module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module Codec = Hmc_pointer_frame_codec
module Index = Hmc_u32_index
module Relayout = Hmc_wasm_relayout
module Geometry = Hmc_wasm_relayout_geometry
module Copy = Wasm_parallel_copy
module E = Wasm_execution
module Write = Hmc_wasm_primitive_write
type fragment = {operation : D.word_operation; left_offset : B.u32; copies : Copy.plan; pc : W.limb; required : Relayout.count}
let[@def] (matches @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) (fragment : fragment @ immutable) = ghost_ (
  match instruction, signature.G.accumulator, signature.G.temporaries with
  | G.Primitive (operation, next), Some D.Word64, G.Value (context, D.Word64, schema) ->
    let env_size = Codec.locals_size signature.G.locals in
    let saved_size = Codec.locals_size context in
    let rest_size = Codec.temporaries_size schema in
    let env = Geometry.size env_size in let saved = Geometry.size saved_size in let rest = Geometry.size rest_size in
    fragment.operation === operation && fragment.left_offset = 56 + 16 * env
    && Index.represents next fragment.pc && fragment.pc <= max_pc
    && Index.fits env_size capacity && Index.fits saved_size capacity && Index.fits rest_size capacity
    && 3 + env + saved + rest <= capacity && fragment.required = 2 + saved + rest && fragment.required <= capacity
    && Geometry.two fragment.copies (3 + env) 2 saved_size (3 + env + saved) (2 + saved) rest_size
  | _ -> false)
let[@def] (encodable @ total) (signature : G.signature @ immutable) (instruction : G.instruction @ immutable)
    (capacity : Relayout.count) (max_pc : W.limb) = ghost_ (
  match instruction, signature.G.accumulator, signature.G.temporaries with
  | G.Primitive (_, next), Some D.Word64, G.Value (_, D.Word64, _) -> Hmc_wasm_value_pop.encodable signature next capacity max_pc
  | _ -> false)
let (build @ total) : (signature : G.signature) @ immutable -> (instruction : G.instruction) @ immutable ->
    (capacity : Relayout.count) -> (max_pc : W.limb) ->
    {out : fragment option | match out with None -> not (encodable signature instruction capacity max_pc) | Some fragment -> encodable signature instruction capacity max_pc && matches signature instruction capacity max_pc fragment} @ immutable =
  fun signature instruction capacity max_pc ->
    ghost_ (encodable_def signature instruction capacity max_pc);
    match instruction, signature.G.accumulator, signature.G.temporaries with
    | G.Primitive (operation, next), Some D.Word64, G.Value (_, D.Word64, _) ->
      (match Hmc_wasm_value_pop.build signature next capacity max_pc with
      | None -> None
      | Some pop ->
        let fragment = {operation; left_offset = pop.Hmc_wasm_value_pop.head_payload; copies = pop.Hmc_wasm_value_pop.copies;
          pc = pop.Hmc_wasm_value_pop.pc; required = pop.Hmc_wasm_value_pop.required} in
        ghost_ (Hmc_wasm_value_pop.matches_def signature next capacity max_pc pop; matches_def signature instruction capacity max_pc fragment);
        Some fragment)
    | _ -> None
let[@def] (moves @ total) (fragment : fragment @ immutable) : Relayout.fragment @ immutable =
  {Relayout.copies = fragment.copies; pc = fragment.pc; required = fragment.required}
let[@def] (emit @ total) (fragment : fragment @ immutable) (base_local : B.u32) =
  E.append (Write.emit fragment.operation fragment.left_offset base_local) (Relayout.emit (moves fragment) base_local)
let (correct @ total) : (fragment : fragment) @ immutable -> (base_local : B.u32) ->
    (state : Wasm_memory_execution.state) @ immutable -> (base : B.u32) ->
    (computed : B.bytes) @ immutable -> (copied : B.bytes) @ immutable -> (after : B.bytes) @ immutable ->
    {u : unit | Wasm_locals.get state.Wasm_memory_execution.machine.E.locals base_local === Some (Wasm_scalar.I32 base)
      && Wasm_memory_execution.run (Write.emit fragment.operation fragment.left_offset base_local) state ===
        Wasm_memory_execution.Done {Wasm_memory_execution.memory = computed; machine = state.Wasm_memory_execution.machine}
      && Copy.apply fragment.copies computed base === Some copied && Relayout.finish copied base fragment.pc === Some after} ->
    {u : unit | Wasm_memory_execution.run (emit fragment base_local) state ===
      Wasm_memory_execution.Done {Wasm_memory_execution.memory = after; machine = state.Wasm_memory_execution.machine}} @ ghost =
  fun fragment base_local state base computed copied after premise -> ghost_ (
    moves_def fragment; emit_def fragment base_local;
    Relayout.correct (moves fragment) base_local {Wasm_memory_execution.memory = computed; machine = state.Wasm_memory_execution.machine} base copied after ();
    Wasm_memory_execution.append_correct (Write.emit fragment.operation fragment.left_offset base_local) (Relayout.emit (moves fragment) base_local) state)
