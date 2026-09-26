module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module M = Wasm_memory
module Lower = Wasm_memory_lowering
module V = Wasm_static_types
module Step = Wasm_static_steps
module Q = Wasm_static_sequence
module Immediate = Wasm_immediate_write
module Pointer = Wasm_pointer_store
module Frame = Wasm_frame_write
module Mixed = Wasm_mixed_write
let[@def] (ty @ total) (width : M.width @ immutable) = match width with M.W32 -> V.I32 | M.W64 -> V.I64
let (load @ total) : (context : V.context) @ immutable -> (width : M.width) @ immutable -> (offset : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (Lower.load_instruction width offset) (V.push V.I32 state) === Some (V.push (ty width) state)} @ ghost =
  fun context width offset state -> ghost_ (
    ty_def width; Lower.load_instruction_def width offset; Step.unary_push V.I32 (ty width) state;
    V.instruction_def context (Lower.load_instruction width offset) (V.push V.I32 state))
let (store @ total) : (context : V.context) @ immutable -> (width : M.width) @ immutable -> (offset : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.instruction context (Lower.store_instruction width offset) (V.push (ty width) (V.push V.I32 state)) === Some state} @ ghost =
  fun context width offset state -> ghost_ (
    ty_def width; Lower.store_instruction_def width offset;
    Step.take_push (ty width) (V.push V.I32 state); Step.take_push V.I32 state;
    V.store_def (ty width) (V.push (ty width) (V.push V.I32 state));
    V.instruction_def context (Lower.store_instruction width offset) (V.push (ty width) (V.push V.I32 state)))
let (read @ total) : (context : V.context) @ immutable -> (width : M.width) @ immutable -> (offset : B.u32) ->
    (base : B.u32) -> (destination : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals destination === Some (ty width)} ->
    {u : unit | Q.check context (Lower.read_code width offset base destination) state === Some state} @ ghost =
  fun context width offset base destination state premise -> ghost_ (
    Lower.read_code_def width offset base destination;
    let saved = C.Next (I.Local_set destination, C.Empty) in
    let loaded = C.Next (Lower.load_instruction width offset, saved) in
    Step.local_get context base V.I32 state (); load context width offset state;
    Step.local_set context destination (ty width) state ();
    Q.check_def context C.Empty state; Q.check_def context saved (V.push (ty width) state);
    Q.check_def context loaded (V.push V.I32 state); Q.check_def context (Lower.read_code width offset base destination) state)
let (write @ total) : (context : V.context) @ immutable -> (width : M.width) @ immutable -> (offset : B.u32) ->
    (base : B.u32) -> (source : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals source === Some (ty width)} ->
    {u : unit | Q.check context (Lower.write_code width offset base source) state === Some state} @ ghost =
  fun context width offset base source state premise -> ghost_ (
    Lower.write_code_def width offset base source;
    let saved = C.Next (Lower.store_instruction width offset, C.Empty) in
    let loaded = C.Next (I.Local_get source, saved) in
    Step.local_get context base V.I32 state (); Step.local_get context source (ty width) (V.push V.I32 state) ();
    store context width offset state; Q.check_def context C.Empty state;
    Q.check_def context saved (V.push (ty width) (V.push V.I32 state));
    Q.check_def context loaded (V.push V.I32 state); Q.check_def context (Lower.write_code width offset base source) state)
let (immediate @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (base : B.u32) ->
    (word : W.t) @ immutable -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Immediate.emit offset base word) state === Some state} @ ghost =
  fun context offset base word state premise -> ghost_ (
    Immediate.emit_def offset base word;
    let saved = C.Next (I.I64_store (3, offset), C.Empty) in
    let loaded = C.Next (I.I64_const word, saved) in
    Step.local_get context base V.I32 state (); Step.constant64 context word (V.push V.I32 state);
    Step.store64 context 3 offset state; Q.check_def context C.Empty state;
    Q.check_def context saved (V.push V.I64 (V.push V.I32 state));
    Q.check_def context loaded (V.push V.I32 state); Q.check_def context (Immediate.emit offset base word) state)
let (pointer @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (base : B.u32) ->
    (source : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32 && V.local context.V.locals source === Some V.I32} ->
    {u : unit | Q.check context (Pointer.emit offset base source) state === Some state} @ ghost =
  fun context offset base source state premise -> ghost_ (
    Pointer.emit_def offset base source;
    let saved = C.Next (I.I64_store (3, offset), C.Empty) in
    let extended = C.Next (I.Plain I.I64_extend_i32_u, saved) in
    let loaded = C.Next (I.Local_get source, extended) in
    let address = V.push V.I32 state in
    Step.local_get context base V.I32 state (); Step.local_get context source V.I32 address ();
    Step.extend32 context address; Step.store64 context 3 offset state;
    Q.check_def context C.Empty state; Q.check_def context saved (V.push V.I64 address);
    Q.check_def context extended (V.push V.I32 address); Q.check_def context loaded address;
    Q.check_def context (Pointer.emit offset base source) state)
let[@def] rec (frame_sources @ total) (context : V.context @ immutable) (writes : Frame.writes @ immutable) =
  match writes with Frame.End -> true | Frame.Write (_, source, rest) -> (match V.local context.V.locals source with Some V.I64 -> true | _ -> false) && frame_sources context rest
let rec (frame @ total) : (context : V.context) @ immutable -> (writes : Frame.writes) @ immutable -> (base : B.u32) ->
    (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32 && frame_sources context writes} ->
    {u : unit | Q.check context (Frame.emit writes base) state === Some state} @ ghost =
  fun context writes base state premise -> ghost_ (
    Frame.emit_def writes base; frame_sources_def context writes;
    match writes with Frame.End -> Q.check_def context C.Empty state
    | Frame.Write (offset, source, rest) ->
      ty_def M.W64; write context M.W64 offset base source state ();
      frame context rest base state (); Q.append context (Lower.write_code M.W64 offset base source) (Frame.emit rest base) state)
let[@def] (mixed_source @ total) (context : V.context @ immutable) (value : Mixed.value @ immutable) =
  match value with Mixed.Constant _ -> true | Mixed.Word_local source -> (match V.local context.V.locals source with Some V.I64 -> true | _ -> false)
  | Mixed.Pointer_local source -> (match V.local context.V.locals source with Some V.I32 -> true | _ -> false)
let[@def] rec (mixed_sources @ total) (context : V.context @ immutable) (writes : Mixed.writes @ immutable) =
  match writes with Mixed.End -> true | Mixed.Write (_, value, rest) -> mixed_source context value && mixed_sources context rest
let (one @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (value : Mixed.value) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32 && mixed_source context value} ->
    {u : unit | Q.check context (Mixed.one offset value base) state === Some state} @ ghost =
  fun context offset value base state premise -> ghost_ (
    Mixed.one_def offset value base; mixed_source_def context value;
    match value with
    | Mixed.Constant word -> immediate context offset base word state ()
    | Mixed.Pointer_local source -> pointer context offset base source state ()
    | Mixed.Word_local source ->
      frame_sources_def context Frame.End; frame_sources_def context (Frame.Write (offset, source, Frame.End));
      frame context (Frame.Write (offset, source, Frame.End)) base state ())
let rec (mixed @ total) : (context : V.context) @ immutable -> (writes : Mixed.writes) @ immutable -> (base : B.u32) ->
    (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32 && mixed_sources context writes} ->
    {u : unit | Q.check context (Mixed.emit writes base) state === Some state} @ ghost =
  fun context writes base state premise -> ghost_ (
    Mixed.emit_def writes base; mixed_sources_def context writes;
    match writes with Mixed.End -> Q.check_def context C.Empty state
    | Mixed.Write (offset, value, rest) -> one context offset value base state (); mixed context rest base state ();
      Q.append context (Mixed.one offset value base) (Mixed.emit rest base) state)
let rec (literals @ total) : (context : V.context) @ immutable -> (writes : Wasm_frame_literals.writes) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Wasm_frame_literals.emit writes base) state === Some state} @ ghost =
  fun context writes base state premise -> ghost_ (
    Wasm_frame_literals.emit_def writes base;
    match writes with Wasm_frame_literals.End -> Q.check_def context C.Empty state
    | Wasm_frame_literals.Write (offset, word, rest) ->
      immediate context offset base word state (); literals context rest base state ();
      Q.append context (Immediate.emit offset base word) (Wasm_frame_literals.emit rest base) state)
