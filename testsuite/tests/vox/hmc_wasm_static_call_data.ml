module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Step = Wasm_static_steps
module Memory = Wasm_static_memory
module Mixed = Wasm_mixed_write
module Header = Hmc_wasm_dynamic_call_header
module Capture = Hmc_wasm_cons_capture
module Cons = Hmc_wasm_static_cons
module Slots = Hmc_wasm_descriptor_load
let (pointer_local @ total) : (context : V.context) @ immutable -> (source : B.u32) -> (destination : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals source === Some V.I64 && V.local context.V.locals destination === Some V.I32} ->
    {u : unit | Q.check context (Wasm_pointer_local.emit source destination) state === Some state} @ ghost =
  fun context source destination state premise -> ghost_ (
    let a = V.push V.I64 state in let b = V.push V.I32 state in
    let c2 = C.Next (I.Local_set destination, C.Empty) in
    let c1 = C.Next (I.Plain I.I32_wrap_i64, c2) in
    Step.local_get context source V.I64 state (); Step.wrap64 context state;
    Step.local_set context destination V.I32 state ();
    Q.check_def context C.Empty state; Q.check_def context c2 b; Q.check_def context c1 a;
    Wasm_pointer_local.emit_def source destination; Q.check_def context (Wasm_pointer_local.emit source destination) state)
let (target @ total) : (context : V.context) @ immutable -> (environment : Hmc_wasm_simple_lower.slot) -> (slots : Capture.slots) @ immutable ->
    (frame : B.u32) -> (object_ : B.u32) -> (code : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals frame === Some V.I32 && V.local context.V.locals object_ === Some V.I32
      && V.local context.V.locals code === Some V.I32 && Cons.slots_typed context slots} ->
    {u : unit | Q.check context (Hmc_wasm_call_target.emit environment slots frame object_ code) state === Some state} @ ghost =
  fun context environment slots frame object_ code state premise -> ghost_ (
    Cons.capture context (Hmc_wasm_simple_lower.slot_tag environment) (Hmc_wasm_simple_lower.slot_payload environment) slots frame state ();
    Cons.slots_typed_def context slots; pointer_local context slots.Capture.head_payload object_ state ();
    Q.append context (Capture.emit (Hmc_wasm_simple_lower.slot_tag environment) (Hmc_wasm_simple_lower.slot_payload environment) slots frame)
      (Wasm_pointer_local.emit slots.Capture.head_payload object_) state;
    Hmc_wasm_call_operands.emit_def environment slots frame object_;
    Hmc_wasm_static_list.pointer context (Hmc_wasm_closure_read.offset ()) object_ code state ();
    Hmc_wasm_closure_code.emit_def object_ code;
    Q.append context (Hmc_wasm_call_operands.emit environment slots frame object_) (Hmc_wasm_closure_code.emit object_ code) state;
    Hmc_wasm_call_target.emit_def environment slots frame object_ code)
let (header @ total) : (context : V.context) @ immutable -> (recursive : bool) -> (pc : B.u32) -> (frame : B.u32) ->
    (closure : B.u32) -> (tag : B.u32) -> (payload : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals pc === Some V.I32 && V.local context.V.locals frame === Some V.I32
      && V.local context.V.locals closure === Some V.I32 && V.local context.V.locals tag === Some V.I64 && V.local context.V.locals payload === Some V.I64} ->
    {u : unit | Q.check context (Header.emit recursive pc frame closure tag payload) state === Some state} @ ghost =
  fun context recursive pc frame closure tag payload state premise -> ghost_ (
    Hmc_wasm_call_header.writes_def recursive 0 closure tag payload;
    Header.writes_def recursive pc closure tag payload;
    Memory.mixed_source_def context (Mixed.Constant (Hmc_wasm_header_update.number 0));
    Memory.mixed_source_def context (Mixed.Constant (Hmc_wasm_header_update.number 1));
    Memory.mixed_source_def context (Mixed.Constant (Hmc_wasm_header_update.number 2));
    Memory.mixed_source_def context (Mixed.Constant (Hmc_wasm_header_update.number 4));
    Memory.mixed_source_def context (Mixed.Pointer_local closure);
    Memory.mixed_source_def context (Mixed.Pointer_local pc);
    Memory.mixed_source_def context (Mixed.Word_local payload);
    Memory.mixed_source_def context (Mixed.Word_local tag);
    Memory.mixed_sources_def context Mixed.End;
    Memory.mixed_sources_def context (Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End));
    Memory.mixed_sources_def context (Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)));
    Memory.mixed_sources_def context (Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End)));
    Memory.mixed_sources_def context (Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End))));
    Memory.mixed_sources_def context (Mixed.Write (40, Mixed.Constant (Hmc_wasm_header_update.number 0), Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End)))));
    Memory.mixed_sources_def context (Mixed.Write (32, Mixed.Constant (Hmc_wasm_header_update.number 2), Mixed.Write (40, Mixed.Constant (Hmc_wasm_header_update.number 0), Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End))))));
    Memory.mixed_sources_def context (Mixed.Write (24, Mixed.Pointer_local closure, Mixed.Write (32, Mixed.Constant (Hmc_wasm_header_update.number 2), Mixed.Write (40, Mixed.Constant (Hmc_wasm_header_update.number 0), Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End)))))));
    Memory.mixed_sources_def context (Mixed.Write (16, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (24, Mixed.Pointer_local closure, Mixed.Write (32, Mixed.Constant (Hmc_wasm_header_update.number 2), Mixed.Write (40, Mixed.Constant (Hmc_wasm_header_update.number 0), Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End))))))));
    Memory.mixed_sources_def context (Mixed.Write (8, Mixed.Pointer_local pc, Mixed.Write (16, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (24, Mixed.Pointer_local closure, Mixed.Write (32, Mixed.Constant (Hmc_wasm_header_update.number 2), Mixed.Write (40, Mixed.Constant (Hmc_wasm_header_update.number 0), Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End)))))))));
    Memory.mixed_sources_def context (Mixed.Write (0, Mixed.Constant (Hmc_wasm_header_update.number 1), Mixed.Write (8, Mixed.Pointer_local pc, Mixed.Write (16, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (24, Mixed.Pointer_local closure, Mixed.Write (32, Mixed.Constant (Hmc_wasm_header_update.number 2), Mixed.Write (40, Mixed.Constant (Hmc_wasm_header_update.number 0), Mixed.Write (48, Mixed.Word_local tag, Mixed.Write (56, Mixed.Word_local payload, (if recursive then Mixed.Write (64, Mixed.Constant (Hmc_wasm_header_update.number 4), Mixed.Write (72, Mixed.Pointer_local closure, Mixed.End)) else Mixed.End))))))))));
    Memory.mixed context (Header.writes recursive pc closure tag payload) frame state ();
    Header.emit_def recursive pc frame closure tag payload)
let (frame @ total) : (context : V.context) @ immutable -> (fragment : Hmc_wasm_call_captures.fragment) @ immutable ->
    (pc : B.u32) -> (object_ : B.u32) -> (frame : B.u32) -> (tag : B.u32) -> (payload : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals pc === Some V.I32 && V.local context.V.locals frame === Some V.I32
      && V.local context.V.locals object_ === Some V.I32 && V.local context.V.locals tag === Some V.I64 && V.local context.V.locals payload === Some V.I64} ->
    {u : unit | Q.check context (Hmc_wasm_dynamic_call_frame.emit fragment pc object_ frame tag payload) state === Some state} @ ghost =
  fun context fragment pc object_ frame tag payload state premise -> ghost_ (
    Wasm_static_copy.cross context fragment.Hmc_wasm_call_captures.copies object_ frame state ();
    Hmc_wasm_call_captures.emit_def fragment object_ frame;
    header context fragment.Hmc_wasm_call_captures.recursive pc frame object_ tag payload state ();
    Q.append context (Hmc_wasm_call_captures.emit fragment object_ frame)
      (Header.emit fragment.Hmc_wasm_call_captures.recursive pc frame object_ tag payload) state;
    Hmc_wasm_dynamic_call_frame.emit_def fragment pc object_ frame tag payload)
let (double @ total) : (context : V.context) @ immutable -> (source : B.u32) -> (destination : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals source === Some V.I32 && V.local context.V.locals destination === Some V.I32} ->
    {u : unit | Q.check context (Wasm_local_double.emit source destination) state === Some state} @ ghost =
  fun context source destination state premise -> ghost_ (
    let a = V.push V.I32 state in let b = V.push V.I32 a in
    let c3 = C.Next (I.Local_set destination, C.Empty) in
    let c2 = C.Next (I.Plain I.I32_add, c3) in let c1 = C.Next (I.Local_get source, c2) in
    Step.local_get context source V.I32 state (); Step.local_get context source V.I32 a ();
    Step.take_push V.I32 a; Step.unary_push V.I32 V.I32 state;
    V.binary_def V.I32 V.I32 b; V.plain_def I.I32_add b; V.instruction_def context (I.Plain I.I32_add) b;
    Step.local_set context destination V.I32 state ();
    Q.check_def context C.Empty state; Q.check_def context c3 a; Q.check_def context c2 b; Q.check_def context c1 a;
    Wasm_local_double.emit_def source destination; Q.check_def context (Wasm_local_double.emit source destination) state)
let (address @ total) : (context : V.context) @ immutable -> (base : B.u32) -> (source : B.u32) -> (destination : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals source === Some V.I32 && V.local context.V.locals destination === Some V.I32} ->
    {u : unit | Q.check context (Hmc_wasm_descriptor_address.emit base source destination) state === Some state} @ ghost =
  fun context base source destination state premise -> ghost_ (
    double context source destination state (); double context destination destination state ();
    Hmc_wasm_static_objects.advance context base destination state ();
    Q.append context (Wasm_local_double.emit destination destination) (Hmc_wasm_allocation_advance.emit base destination) state;
    Q.append context (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (Hmc_wasm_allocation_advance.emit base destination)) state;
    Q.append context (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (Hmc_wasm_allocation_advance.emit base destination))) state;
    Q.append context (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (Hmc_wasm_allocation_advance.emit base destination)))) state;
    Q.append context (Wasm_local_double.emit source destination) (E.append (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (E.append (Wasm_local_double.emit destination destination) (Hmc_wasm_allocation_advance.emit base destination))))) state;
    Hmc_wasm_descriptor_address.emit_def base source destination)
let (limb @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (base : B.u32) -> (destination : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals destination === Some V.I32} ->
    {u : unit | Q.check context (Wasm_limb_read.emit offset base destination) state === Some state} @ ghost =
  fun context offset base destination state premise -> ghost_ (
    let a = V.push V.I32 state in
    let c2 = C.Next (I.Local_set destination, C.Empty) in let c1 = C.Next (I.I32_load (2, offset), c2) in
    Step.local_get context base V.I32 state (); Step.load32 context 2 offset state; Step.local_set context destination V.I32 state ();
    Q.check_def context C.Empty state; Q.check_def context c2 a; Q.check_def context c1 a;
    Wasm_limb_read.emit_def offset base destination; Q.check_def context (Wasm_limb_read.emit offset base destination) state)
let[@def] (descriptor_typed @ total) (context : V.context @ immutable) (slots : Slots.slots @ immutable) = ghost_ (
  V.local context.V.locals slots.Slots.start === Some V.I32 && V.local context.V.locals slots.Slots.captures === Some V.I32
  && V.local context.V.locals slots.Slots.recursive === Some V.I32)
let (descriptor @ total) : (context : V.context) @ immutable -> (base : B.u32) -> (code : B.u32) -> (address_local : B.u32) ->
    (slots : Slots.slots) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals code === Some V.I32 && V.local context.V.locals address_local === Some V.I32 && descriptor_typed context slots} ->
    {u : unit | Q.check context (Hmc_wasm_descriptor_select.emit base code address_local slots) state === Some state} @ ghost =
  fun context base code address_local slots state premise -> ghost_ (
    descriptor_typed_def context slots;
    let start = Hmc_wasm_descriptor_reads.start_offset () in
    let captures = Hmc_wasm_descriptor_reads.captures_offset () in
    let recursive = Hmc_wasm_descriptor_reads.recursive_offset () in
    limb context start address_local slots.Slots.start state ();
    limb context captures address_local slots.Slots.captures state ();
    limb context recursive address_local slots.Slots.recursive state ();
    Q.append context (Wasm_limb_read.emit captures address_local slots.Slots.captures) (Wasm_limb_read.emit recursive address_local slots.Slots.recursive) state;
    Q.append context (Wasm_limb_read.emit start address_local slots.Slots.start)
      (E.append (Wasm_limb_read.emit captures address_local slots.Slots.captures) (Wasm_limb_read.emit recursive address_local slots.Slots.recursive)) state;
    Slots.emit_def address_local slots;
    address context base code address_local state ();
    Q.append context (Hmc_wasm_descriptor_address.emit base code address_local) (Slots.emit address_local slots) state;
    Hmc_wasm_descriptor_select.emit_def base code address_local slots)
