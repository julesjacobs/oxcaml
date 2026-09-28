module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module M = Wasm_memory
module LP = Wasm_local_preservation
module Lower = Wasm_memory_lowering
module Read = Wasm_frame_snapshot
module Write = Wasm_frame_write
module Copy = Wasm_parallel_copy
module Immediate = Wasm_immediate_write
module Advance = Hmc_wasm_allocation_advance
module Capture = Hmc_wasm_cons_capture
module Stored = Hmc_wasm_cons_write
module Allocate = Hmc_wasm_cons_allocate
module Result = Hmc_wasm_cons_result
module Finish = Hmc_wasm_cons_finish
module Relayout = Hmc_wasm_relayout
module Pop = Hmc_wasm_value_pop
module Success = Hmc_wasm_cons_success
module PC = Hmc_wasm_pc_update
let (read @ total) : (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (local : B.u32) ->
    {u : unit | destination <> local} -> {u : unit | LP.preserves (Lower.read_code M.W64 offset base destination) local} @ ghost =
  fun offset base destination local premise -> ghost_ (
    Lower.read_code_def M.W64 offset base destination; Lower.load_instruction_def M.W64 offset;
    LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.Local_set destination) local; LP.preserves_def (C.Next (I.Local_set destination, C.Empty)) local;
    LP.instruction_preserves_def (I.I64_load (3, offset)) local; LP.preserves_def (C.Next (I.I64_load (3, offset), C.Next (I.Local_set destination, C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get base) local; LP.preserves_def (C.Next (I.Local_get base, C.Next (I.I64_load (3, offset), C.Next (I.Local_set destination, C.Empty)))) local)
let (write @ total) : (offset : B.u32) -> (base : B.u32) -> (source : B.u32) -> (local : B.u32) ->
    {u : unit | true} -> {u : unit | LP.preserves (Lower.write_code M.W64 offset base source) local} @ ghost =
  fun offset base source local premise -> ghost_ (
    Lower.write_code_def M.W64 offset base source; Lower.store_instruction_def M.W64 offset;
    LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.I64_store (3, offset)) local; LP.preserves_def (C.Next (I.I64_store (3, offset), C.Empty)) local;
    LP.instruction_preserves_def (I.Local_get source) local; LP.preserves_def (C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get base) local; LP.preserves_def (C.Next (I.Local_get base, C.Next (I.Local_get source, C.Next (I.I64_store (3, offset), C.Empty)))) local)
let (immediate @ total) : (offset : B.u32) -> (base : B.u32) -> (word : W.t) @ immutable -> (local : B.u32) ->
    {u : unit | true} -> {u : unit | LP.preserves (Immediate.emit offset base word) local} @ ghost =
  fun offset base word local premise -> ghost_ (
    Immediate.emit_def offset base word;
    LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.I64_store (3, offset)) local; LP.preserves_def (C.Next (I.I64_store (3, offset), C.Empty)) local;
    LP.instruction_preserves_def (I.I64_const word) local; LP.preserves_def (C.Next (I.I64_const word, C.Next (I.I64_store (3, offset), C.Empty))) local;
    LP.instruction_preserves_def (I.Local_get base) local; LP.preserves_def (C.Next (I.Local_get base, C.Next (I.I64_const word, C.Next (I.I64_store (3, offset), C.Empty)))) local)
let (advance @ total) : (bytes : B.u32) -> (heap : B.u32) -> (local : B.u32) ->
    {u : unit | heap <> local} -> {u : unit | LP.preserves (Advance.emit bytes heap) local} @ ghost =
  fun bytes heap local premise -> ghost_ (
    Advance.emit_def bytes heap;
    LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.Local_set heap) local; LP.preserves_def (C.Next (I.Local_set heap, C.Empty)) local;
    LP.instruction_preserves_def (I.Plain I.I32_add) local; LP.preserves_def (C.Next (I.Plain I.I32_add, C.Next (I.Local_set heap, C.Empty))) local;
    LP.instruction_preserves_def (I.I32_const bytes) local; LP.preserves_def (C.Next (I.I32_const bytes, C.Next (I.Plain I.I32_add, C.Next (I.Local_set heap, C.Empty)))) local;
    LP.instruction_preserves_def (I.Local_get heap) local; LP.preserves_def (C.Next (I.Local_get heap, C.Next (I.I32_const bytes, C.Next (I.Plain I.I32_add, C.Next (I.Local_set heap, C.Empty))))) local)
let (payload @ total) : (base : B.u32) -> (heap : B.u32) -> (local : B.u32) ->
    {u : unit | true} -> {u : unit | LP.preserves (Result.payload base heap) local} @ ghost =
  fun base heap local premise -> ghost_ (
    Result.payload_def base heap;
    LP.preserves_def C.Empty local;
    LP.instruction_preserves_def (I.I64_store (3, 40)) local; LP.preserves_def (C.Next (I.I64_store (3, 40), C.Empty)) local;
    LP.instruction_preserves_def (I.Plain I.I64_extend_i32_u) local; LP.preserves_def (C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, 40), C.Empty))) local;
    LP.instruction_preserves_def (I.Plain I.I32_sub) local; LP.preserves_def (C.Next (I.Plain I.I32_sub, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, 40), C.Empty)))) local;
    LP.instruction_preserves_def (I.I32_const 32) local; LP.preserves_def (C.Next (I.I32_const 32, C.Next (I.Plain I.I32_sub, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, 40), C.Empty))))) local;
    LP.instruction_preserves_def (I.Local_get heap) local; LP.preserves_def (C.Next (I.Local_get heap, C.Next (I.I32_const 32, C.Next (I.Plain I.I32_sub, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, 40), C.Empty)))))) local;
    LP.instruction_preserves_def (I.Local_get base) local; LP.preserves_def (C.Next (I.Local_get base, C.Next (I.Local_get heap, C.Next (I.I32_const 32, C.Next (I.Plain I.I32_sub, C.Next (I.Plain I.I64_extend_i32_u, C.Next (I.I64_store (3, 40), C.Empty))))))) local)
let rec (reads @ total) : (plan : Read.reads) @ immutable -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | Read.separate plan local} -> {u : unit | LP.preserves (Read.emit plan base) local} @ ghost =
  fun plan base local premise -> ghost_ (
    Read.emit_def plan base; Read.separate_def plan local;
    match plan with
    | Read.End -> LP.preserves_def C.Empty local
    | Read.Read (offset, destination, rest) ->
      read offset base destination local (); reads rest base local ();
      LP.append (Lower.read_code M.W64 offset base destination) (Read.emit rest base) local ())
let rec (writes @ total) : (plan : Write.writes) @ immutable -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Write.emit plan base) local} @ ghost = fun plan base local -> ghost_ (
    Write.emit_def plan base;
    match plan with
    | Write.End -> LP.preserves_def C.Empty local
    | Write.Write (offset, source, rest) ->
      write offset base source local (); writes rest base local;
      LP.append (Lower.write_code M.W64 offset base source) (Write.emit rest base) local ())
let rec (copies @ total) : (plan : Copy.plan) @ immutable -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | LP.preserves (Copy.emit plan base) local} @ ghost = fun plan base local -> ghost_ (
    Copy.emit_def plan base;
    match plan with
    | Copy.End -> LP.preserves_def C.Empty local
    | Copy.Copy (source, destination, rest) ->
      copies rest base local;
      let store = C.Next (I.I64_store (3, destination), C.Empty) in
      LP.preserves_def C.Empty local; LP.preserves_def store local; LP.instruction_preserves_def (I.I64_store (3, destination)) local;
      LP.append (Copy.emit rest base) store local ();
      let tail = E.append (Copy.emit rest base) store in
      LP.instruction_preserves_def (I.I64_load (3, source)) local; LP.instruction_preserves_def (I.Local_get base) local;
      LP.preserves_def (C.Next (I.I64_load (3, source), tail)) local;
      LP.preserves_def (C.Next (I.Local_get base, C.Next (I.I64_load (3, source), tail))) local;
      LP.preserves_def (Copy.emit plan base) local)
let (capture @ total) : (head_tag : B.u32) -> (head_payload : B.u32) -> (slots : Capture.slots) @ immutable -> (base : B.u32) -> (local : B.u32) ->
    {u : unit | Capture.separate slots local} ->
    {u : unit | LP.preserves (Capture.emit head_tag head_payload slots base) local} @ ghost = fun head_tag head_payload slots base local premise -> ghost_ (
    Capture.emit_def head_tag head_payload slots base; Capture.layout head_tag head_payload slots local ();
    reads (Capture.reads head_tag head_payload slots) base local ())
let (success @ total) : (plan : Pop.fragment) @ immutable -> (base : B.u32) -> (heap : B.u32) ->
    (head_tag : B.u32) -> (head_payload : B.u32) -> (tail_tag : B.u32) -> (tail_payload : B.u32) -> (local : B.u32) ->
    {u : unit | heap <> local} -> {u : unit | LP.preserves (Success.emit plan base heap head_tag head_payload tail_tag tail_payload) local} @ ghost =
  fun plan base heap head_tag head_payload tail_tag tail_payload local premise -> ghost_ (
    Stored.emit_def heap head_tag head_payload tail_tag tail_payload;
    writes (Stored.writes head_tag head_payload tail_tag tail_payload) heap local;
    advance (Wasm_four_words.width ()) heap local ();
    Allocate.emit_def heap head_tag head_payload tail_tag tail_payload;
    LP.append (Stored.emit heap head_tag head_payload tail_tag tail_payload) (Advance.emit (Wasm_four_words.width ()) heap) local ();
    payload base heap local ();
    immediate (Result.tag_offset ()) base (Result.tag ()) local ();
    Result.emit_def base heap;
    LP.append (Result.payload base heap) (Immediate.emit (Result.tag_offset ()) base (Result.tag ())) local ();
    let moves = Finish.moves plan in
    copies moves.Relayout.copies base local;
    PC.emit_def moves.Relayout.pc base; immediate (PC.offset ()) base (Hmc_wasm_header_update.number moves.Relayout.pc) local ();
    Relayout.emit_def moves base;
    LP.append (Copy.emit moves.Relayout.copies base) (PC.emit moves.Relayout.pc base) local ();
    Finish.emit_def plan base heap;
    LP.append (Result.emit base heap) (Relayout.emit moves base) local ();
    Success.emit_def plan base heap head_tag head_payload tail_tag tail_payload;
    LP.append (Allocate.emit heap head_tag head_payload tail_tag tail_payload) (Finish.emit plan base heap) local ())
