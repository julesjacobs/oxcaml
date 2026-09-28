module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module M = Wasm_memory
module P = Wasm_local_preservation
module Read = Wasm_frame_snapshot
module Lower = Wasm_memory_lowering
module Pointer = Wasm_pointer_local
module Pointer_read = Wasm_pointer_read
module Capture = Hmc_wasm_cons_capture
module Slots = Hmc_wasm_simple_lower
module Operands = Hmc_wasm_call_operands
module Closure = Hmc_wasm_closure_code
module Target = Hmc_wasm_call_target
let (read @ total) : (width : M.width) @ immutable -> (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Lower.read_code width offset base destination) local} @ ghost =
  fun width offset base destination local premise -> ghost_ (
    Lower.read_code_def width offset base destination;
    Lower.load_instruction_def width offset;
    P.preserves_def C.Empty local;
    P.instruction_preserves_def (I.Local_set destination) local; P.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    P.instruction_preserves_def (Lower.load_instruction width offset) local; P.preserves_def (C.Next (Lower.load_instruction width offset, C.Next (I.Local_set destination, C.Empty))) local;
    P.instruction_preserves_def (I.Local_get base) local; P.preserves_def (C.Next (I.Local_get base, C.Next (Lower.load_instruction width offset, C.Next (I.Local_set destination, C.Empty)))) local)
let (pointer @ total) : (source : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Pointer.emit source destination) local} @ ghost =
  fun source destination local premise -> ghost_ (
    Pointer.emit_def source destination;
    P.preserves_def C.Empty local;
    P.instruction_preserves_def (I.Local_set destination) local; P.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    P.instruction_preserves_def (I.Plain I.I32_wrap_i64) local; P.preserves_def (C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Local_set destination, C.Empty))) local;
    P.instruction_preserves_def (I.Local_get source) local; P.preserves_def (C.Next (I.Local_get source, C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Local_set destination, C.Empty)))) local)
let (pointer_read @ total) : (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | P.preserves (Pointer_read.emit offset base destination) local} @ ghost =
  fun offset base destination local premise -> ghost_ (
    Pointer_read.emit_def offset base destination;
    P.preserves_def C.Empty local;
    P.instruction_preserves_def (I.Local_set destination) local; P.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    P.instruction_preserves_def (I.Plain I.I32_wrap_i64) local; P.preserves_def (C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Local_set destination, C.Empty))) local;
    P.instruction_preserves_def (I.I64_load (3, offset)) local; P.preserves_def (C.Next (I.I64_load (3, offset), C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Local_set destination, C.Empty)))) local;
    P.instruction_preserves_def (I.Local_get base) local; P.preserves_def (C.Next (I.Local_get base, C.Next (I.I64_load (3, offset), C.Next (I.Plain I.I32_wrap_i64, C.Next (I.Local_set destination, C.Empty))))) local)
let rec (snapshot @ total) : (reads : Read.reads) @ immutable -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | Read.separate reads local} -> {u : unit | P.preserves (Read.emit reads base) local} @ ghost =
  fun reads base local premise -> ghost_ (
    Read.separate_def reads local; Read.emit_def reads base;
    match reads with
    | Read.End -> P.preserves_def C.Empty local
    | Read.Read (offset, destination, rest) ->
      read M.W64 offset base destination local (); snapshot rest base local ();
      P.append (Lower.read_code M.W64 offset base destination) (Read.emit rest base) local ())
let (correct @ total) : (env_count : Slots.slot) -> (slots : Capture.slots) @ immutable -> (frame_local : B.u32) ->
    (object_local : B.u32) -> (code_local : B.u32) -> (before : X.state) @ immutable -> (after : X.state) @ immutable -> (local : B.u32) ->
    {u : unit | Capture.separate slots local && object_local <> local && code_local <> local
      && X.run (Target.emit env_count slots frame_local object_local code_local) before === X.Done after} ->
    {u : unit | L.get before.X.machine.E.locals local === L.get after.X.machine.E.locals local} @ ghost =
  fun env_count slots frame_local object_local code_local before after local premise -> ghost_ (
    Capture.layout (Slots.slot_tag env_count) (Slots.slot_payload env_count) slots local ();
    snapshot (Capture.reads (Slots.slot_tag env_count) (Slots.slot_payload env_count) slots) frame_local local ();
    Capture.emit_def (Slots.slot_tag env_count) (Slots.slot_payload env_count) slots frame_local;
    pointer slots.Capture.head_payload object_local local ();
    P.append (Capture.emit (Slots.slot_tag env_count) (Slots.slot_payload env_count) slots frame_local)
      (Pointer.emit slots.Capture.head_payload object_local) local ();
    Operands.emit_def env_count slots frame_local object_local;
    pointer_read (Hmc_wasm_closure_read.offset ()) object_local code_local local ();
    Closure.emit_def object_local code_local;
    P.append (Operands.emit env_count slots frame_local object_local) (Closure.emit object_local code_local) local ();
    Target.emit_def env_count slots frame_local object_local code_local;
    P.correct (Target.emit env_count slots frame_local object_local code_local) before after local ())
