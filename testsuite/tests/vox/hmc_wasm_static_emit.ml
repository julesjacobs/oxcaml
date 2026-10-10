module B = Wasm_u32
module V = Wasm_static_types
module Check = Wasm_static_control
module Emit = Hmc_wasm_program_emit
module Block = Hmc_wasm_program_block
module Program = Hmc_wasm_program_lower
module Structured = Hmc_wasm_structured_block
module Simple = Hmc_wasm_static_structured
module Call = Hmc_wasm_static_call
module Return = Hmc_wasm_static_return
let[@def] (locals_typed @ total) (context : V.context @ immutable) (locals : Emit.locals @ immutable) = ghost_ (
  Simple.locals_typed context locals.Emit.structured
  && V.local context.V.locals locals.Emit.top === Some V.I32
  && V.local context.V.locals locals.Emit.stack_limit === Some V.I32
  && V.local context.V.locals locals.Emit.code === Some V.I32
  && V.local context.V.locals locals.Emit.address === Some V.I32
  && Hmc_wasm_static_call_data.descriptor_typed context locals.Emit.descriptor
  && V.local context.V.locals locals.Emit.status === Some V.I32
  && V.local context.V.locals locals.Emit.result_tag === Some V.I64
  && V.local context.V.locals locals.Emit.result_payload === Some V.I64)
let[@def] (padding_typed @ total) (context : V.context @ immutable) (fragment : Block.fragment @ immutable) = ghost_ (
  match fragment with Block.Call call -> Wasm_static_memory.mixed_sources context call.Block.padding | _ -> true)
let (emit @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable -> (program : Program.program) @ immutable ->
    (fragment : Block.fragment) @ immutable -> (locals : Emit.locals) @ immutable -> (table_base : B.u32) -> (stack_base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | locals_typed context locals && padding_typed context fragment} ->
    {u : unit | Check.check context labels (Emit.emit program fragment locals table_base stack_base) state === Some state} @ ghost =
  fun context labels program fragment locals table_base stack_base state premise -> ghost_ (
    locals_typed_def context locals; padding_typed_def context fragment;
    Simple.locals_typed_def context locals.Emit.structured;
    let base = locals.Emit.structured in
    let nested = Check.Label (Check.Label labels) in
    let empty = V.initial () in
    Check.label_def (Check.Label nested) 2; Check.label_def nested 1; Check.label_def (Check.Label labels) 0;
    Emit.emit_def program fragment locals table_base stack_base;
    match fragment with
    | Block.Structured body ->
      Simple.block context nested body base 2 empty ();
      Return.protected context labels locals.Emit.status 2 (Structured.emit body base 2) state ()
    | Block.Tail_call environment ->
      Call.loaded context nested environment base.Structured.scratch program.Program.calls table_base locals.Emit.code locals.Emit.address
        locals.Emit.descriptor base.Structured.object_ base.Structured.frame empty ();
      Call.finished program.Program.calls;
      Return.protected_finished context labels locals.Emit.status 0
        (Hmc_wasm_loaded_call.emit environment base.Structured.scratch program.Program.calls table_base locals.Emit.code
          locals.Emit.address locals.Emit.descriptor base.Structured.object_ base.Structured.frame)
        (Call.after program.Program.calls empty) state ()
    | Block.Call call ->
      Call.ordinary context nested call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Program.width
        locals.Emit.stack_limit 2 call.Block.environment base.Structured.scratch program.Program.calls table_base locals.Emit.code
        locals.Emit.address locals.Emit.descriptor base.Structured.object_ empty ();
      Call.finished program.Program.calls;
      Return.protected_finished context labels locals.Emit.status 3
        (Hmc_wasm_ordinary_call.emit call.Block.save call.Block.padding base.Structured.frame locals.Emit.top program.Program.width
          locals.Emit.stack_limit 2 call.Block.environment base.Structured.scratch program.Program.calls table_base locals.Emit.code
          locals.Emit.address locals.Emit.descriptor base.Structured.object_) (Call.after program.Program.calls empty) state ()
    | Block.Return -> Return.return context labels program locals table_base stack_base state ())
let (padding_from_correspondence @ total) : (context : V.context) @ immutable -> (globals : Hmc_heap_machine.globals) @ immutable ->
    (signature : Hmc_cfg_ir.signature) @ immutable -> (instruction : Hmc_tail_ir.instruction) @ immutable ->
    (capacity : Hmc_wasm_relayout.count) -> (max_pc : B.u32) -> (fragment : Block.fragment) @ immutable ->
    {u : unit | Block.corresponds globals signature instruction capacity max_pc fragment} ->
    {u : unit | padding_typed context fragment} @ ghost =
  fun context globals signature instruction capacity max_pc fragment premise -> ghost_ (
    padding_typed_def context fragment; Block.corresponds_def globals signature instruction capacity max_pc fragment;
    match fragment, instruction with
    | Block.Call call, Hmc_tail_ir.Keep (Hmc_cfg_ir.Call _) -> (match signature.Hmc_cfg_ir.temporaries with
      | Hmc_cfg_ir.Value _ -> Call.padding context call.Block.padding (3 + call.Block.saved) call.Block.padding_length ()
      | _ -> ())
    | _ -> ())
