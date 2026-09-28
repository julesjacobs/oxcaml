module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Step = Wasm_static_steps
module Memory = Wasm_static_memory
module Cons = Hmc_wasm_static_cons
module Read = Wasm_frame_snapshot
module Frame = Wasm_frame_write
module Capture = Hmc_wasm_list_capture
module Finish = Hmc_wasm_list_finish
module Full = Hmc_wasm_list_full
module Entry = Hmc_wasm_list_full_entry
module Lower = Hmc_wasm_relayout
module Probe = Wasm_word_probe
let (pointer @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals destination === Some V.I32} ->
    {u : unit | Q.check context (Wasm_pointer_read.emit offset base destination) state === Some state} @ ghost =
  fun context offset base destination state premise -> ghost_ (
    Wasm_pointer_read.emit_def offset base destination;
    let a = V.push V.I32 state in let b = V.push V.I64 state in
    let c3 = C.Next (I.Local_set destination, C.Empty) in
    let c2 = C.Next (I.Plain I.I32_wrap_i64, c3) in
    let c1 = C.Next (I.I64_load (3, offset), c2) in
    Step.local_get context base V.I32 state (); Step.load64 context 3 offset state;
    Step.wrap64 context state; Step.local_set context destination V.I32 state ();
    Q.check_def context C.Empty state; Q.check_def context c3 a; Q.check_def context c2 b;
    Q.check_def context c1 a; Q.check_def context (Wasm_pointer_read.emit offset base destination) state)
let (probe @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (label : W.limb) -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Probe.emit offset label base) state === Some (V.push V.I32 state)} @ ghost =
  fun context offset label base state premise -> ghost_ (
    Probe.emit_def offset label base;
    let a = V.push V.I32 state in let b = V.push V.I64 state in let c = V.push V.I64 b in
    let c3 = C.Next (I.Plain I.I64_eq, C.Empty) in
    let c2 = C.Next (I.I64_const (Hmc_wasm_header_update.number label), c3) in
    let c1 = C.Next (I.I64_load (3, offset), c2) in
    Step.local_get context base V.I32 state (); Step.load64 context 3 offset state;
    Step.constant64 context (Hmc_wasm_header_update.number label) b;
    Step.take_push V.I64 b; Step.unary_push V.I64 V.I32 state;
    V.binary_def V.I64 V.I32 c; V.plain_def I.I64_eq c; V.instruction_def context (I.Plain I.I64_eq) c;
    Q.check_def context C.Empty a; Q.check_def context c3 c; Q.check_def context c2 b;
    Q.check_def context c1 a; Q.check_def context (Probe.emit offset label base) state)
let[@def] (slots_typed @ total) (context : V.context @ immutable) (slots : Capture.slots @ immutable) = ghost_ (
  V.local context.V.locals slots.Capture.head_tag === Some V.I64 && V.local context.V.locals slots.Capture.head_payload === Some V.I64
  && V.local context.V.locals slots.Capture.tail_tag === Some V.I64 && V.local context.V.locals slots.Capture.tail_payload === Some V.I64)
let (capture @ total) : (context : V.context) @ immutable -> (head_tag : B.u32) -> (head_payload : B.u32) ->
    (slots : Capture.slots) @ immutable -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Capture.emit head_tag head_payload slots base) state === Some state} @ ghost =
  fun context head_tag head_payload slots base state premise -> ghost_ (
    slots_typed_def context slots; Capture.reads_def head_tag head_payload slots;
    Cons.destinations_def context Read.End;
    Cons.destinations_def context (Read.Read (24, slots.Capture.tail_payload, Read.End));
    Cons.destinations_def context (Read.Read (16, slots.Capture.tail_tag, Read.Read (24, slots.Capture.tail_payload, Read.End)));
    Cons.destinations_def context (Read.Read (head_payload, slots.Capture.head_payload, Read.Read (16, slots.Capture.tail_tag, Read.Read (24, slots.Capture.tail_payload, Read.End))));
    Cons.destinations_def context (Capture.reads head_tag head_payload slots);
    Cons.snapshot context (Capture.reads head_tag head_payload slots) base state ();
    Capture.emit_def head_tag head_payload slots base)
let (finish @ total) : (context : V.context) @ immutable -> (fragment : Lower.fragment) @ immutable -> (base : B.u32) ->
    (slots : Capture.slots) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Finish.emit fragment base slots) state === Some state} @ ghost =
  fun context fragment base slots state premise -> ghost_ (
    slots_typed_def context slots; Finish.writes_def slots;
    Memory.frame_sources_def context Frame.End;
    Memory.frame_sources_def context (Frame.Write (72, slots.Capture.tail_payload, Frame.End));
    Memory.frame_sources_def context (Frame.Write (64, slots.Capture.tail_tag, Frame.Write (72, slots.Capture.tail_payload, Frame.End)));
    Memory.frame_sources_def context (Frame.Write (56, slots.Capture.head_payload, Frame.Write (64, slots.Capture.tail_tag, Frame.Write (72, slots.Capture.tail_payload, Frame.End))));
    Memory.frame_sources_def context (Finish.writes slots);
    Memory.frame context (Finish.writes slots) base state ();
    Wasm_static_copy.parallel context fragment.Lower.copies base state ();
    Hmc_wasm_static_simple.pc context fragment.Lower.pc base state ();
    Q.append context (Frame.emit (Finish.writes slots) base) (Hmc_wasm_pc_update.emit fragment.Lower.pc base) state;
    Q.append context (Wasm_parallel_copy.emit fragment.Lower.copies base)
      (E.append (Frame.emit (Finish.writes slots) base) (Hmc_wasm_pc_update.emit fragment.Lower.pc base)) state;
    Finish.emit_def fragment base slots)
let (full @ total) : (context : V.context) @ immutable -> (fragment : Lower.fragment) @ immutable -> (base : B.u32) ->
    (object_ : B.u32) -> (slots : Capture.slots) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals object_ === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Full.emit fragment base object_ slots) state === Some state} @ ghost =
  fun context fragment base object_ slots state premise -> ghost_ (
    capture context (Hmc_wasm_list_memory.zero ()) (Hmc_wasm_list_memory.eight ()) slots object_ state ();
    finish context fragment base slots state ();
    Q.append context (Capture.emit (Hmc_wasm_list_memory.zero ()) (Hmc_wasm_list_memory.eight ()) slots object_) (Finish.emit fragment base slots) state;
    Full.emit_def fragment base object_ slots)
let (entry @ total) : (context : V.context) @ immutable -> (fragment : Lower.fragment) @ immutable -> (base : B.u32) ->
    (object_ : B.u32) -> (slots : Capture.slots) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals object_ === Some V.I32 && slots_typed context slots} ->
    {u : unit | Q.check context (Entry.emit fragment base object_ slots) state === Some state} @ ghost =
  fun context fragment base object_ slots state premise -> ghost_ (
    pointer context (Hmc_wasm_list_pointer.offset ()) base object_ state ();
    full context fragment base object_ slots state ();
    Q.append context (Wasm_pointer_read.emit (Hmc_wasm_list_pointer.offset ()) base object_) (Full.emit fragment base object_ slots) state;
    Entry.emit_def fragment base object_ slots)
module T = Wasm_control
module Check = Wasm_static_control
module Conditional = Hmc_wasm_list_conditional
let (conditional @ total) : (context : V.context) @ immutable -> (labels : Check.labels) @ immutable ->
    (fragment : Lower.fragment) @ immutable -> (empty_pc : W.limb) -> (base : B.u32) -> (object_ : B.u32) ->
    (slots : Capture.slots) @ immutable -> (tail : T.code) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals object_ === Some V.I32 && slots_typed context slots} ->
    {u : unit | Check.check context labels (Conditional.emit fragment empty_pc base object_ slots tail) state === Check.check context labels tail state} @ ghost =
  fun context labels fragment empty_pc base object_ slots tail state premise -> ghost_ (
    let empty = V.initial () in
    let yes = Wasm_control_lift.embed (Hmc_wasm_pc_update.emit empty_pc base) T.Empty in
    let no = Wasm_control_lift.embed (Entry.emit fragment base object_ slots) T.Empty in
    Hmc_wasm_static_simple.pc context empty_pc base empty ();
    entry context fragment base object_ slots empty ();
    Q.embed_checked context (Check.Label labels) (Hmc_wasm_pc_update.emit empty_pc base) T.Empty empty empty ();
    Q.embed_checked context (Check.Label labels) (Entry.emit fragment base object_ slots) T.Empty empty empty ();
    Check.check_def context (Check.Label labels) T.Empty empty;
    V.initial_def (); V.consume_result_def Wasm_functions.Void empty; V.finish_def Wasm_functions.Void empty;
    Step.take_push V.I32 state;
    Check.check_def context labels (T.If (yes, no, tail)) (V.push V.I32 state);
    probe context (Hmc_wasm_list_probe.offset ()) (Hmc_wasm_list_probe.nil_tag ()) base state ();
    Hmc_wasm_list_probe.emit_def base;
    Q.embed_checked context labels (Hmc_wasm_list_probe.emit base) (T.If (yes, no, tail)) state (V.push V.I32 state) ();
    Wasm_control_select.emit_def (Hmc_wasm_list_probe.emit base) yes no tail;
    Conditional.emit_def fragment empty_pc base object_ slots tail)
