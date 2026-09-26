module B = Wasm_u32
module D = Hm_declarative
module I = Wasm_instruction
module C = Wasm_code
module E = Wasm_execution
module V = Wasm_static_types
module Q = Wasm_static_sequence
module Step = Wasm_static_steps
module Simple = Hmc_wasm_static_simple
module Value = Hmc_wasm_primitive_value
module Payload = Hmc_wasm_primitive_payload
module Write = Hmc_wasm_primitive_write
module Primitive = Hmc_wasm_primitive_lower
module Global = Hmc_wasm_global_lower
module Block = Hmc_wasm_block_lower
let (value @ total) : (context : V.context) @ immutable -> (operation : D.word_operation) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | Q.check context (Value.emit operation) (V.push V.I64 (V.push V.I64 state)) === Some (V.push V.I64 state)} @ ghost =
  fun context operation state -> ghost_ (
    let input = V.push V.I64 (V.push V.I64 state) in
    Step.take_push V.I64 (V.push V.I64 state); Step.take_push V.I64 state;
    Step.unary_push V.I64 V.I64 state; Step.unary_push V.I64 V.I32 state;
    Value.emit_def operation; Value.opcode_def operation; Value.extension_def operation;
    V.instruction_def context (I.Plain (Value.opcode operation)) input;
    V.plain_def (Value.opcode operation) input;
    V.binary_def V.I64 V.I64 input; V.binary_def V.I64 V.I32 input;
    Q.check_def context (Value.emit operation) input;
    match operation with
    | D.Add | D.Subtract -> Q.check_def context C.Empty (V.push V.I64 state)
    | D.Equal_word | D.Unsigned_less ->
      Step.extend32 context state;
      Q.check_def context (Value.extension operation) (V.push V.I32 state);
      Q.check_def context C.Empty (V.push V.I64 state))
let (prefix @ total) : (context : V.context) @ immutable -> (offset : B.u32) -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Payload.prefix offset base) state === Some (V.push V.I64 (V.push V.I64 (V.push V.I32 state)))} @ ghost =
  fun context offset base state premise -> ghost_ (
    Payload.prefix_def offset base;
    let a = V.push V.I32 state in let b = V.push V.I32 a in let c = V.push V.I64 a in
    let d = V.push V.I32 c in let e = V.push V.I64 c in
    let c4 = C.Next (I.I64_load (3, 40), C.Empty) in
    let c3 = C.Next (I.Local_get base, c4) in
    let c2 = C.Next (I.I64_load (3, offset), c3) in
    let c1 = C.Next (I.Local_get base, c2) in
    Step.local_get context base V.I32 state (); Step.local_get context base V.I32 a ();
    Step.load64 context 3 offset a; Step.local_get context base V.I32 c (); Step.load64 context 3 40 c;
    Q.check_def context C.Empty e; Q.check_def context c4 d; Q.check_def context c3 c;
    Q.check_def context c2 b; Q.check_def context c1 a; Q.check_def context (Payload.prefix offset base) state)
let (payload @ total) : (context : V.context) @ immutable -> (operation : D.word_operation) @ immutable ->
    (offset : B.u32) -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Payload.emit operation offset base) state === Some state} @ ghost =
  fun context operation offset base state premise -> ghost_ (
    let a = V.push V.I32 state in let input = V.push V.I64 (V.push V.I64 a) in
    let store = C.Next (I.I64_store (3, 40), C.Empty) in
    prefix context offset base state (); value context operation a; Step.store64 context 3 40 state;
    Q.check_def context C.Empty state; Q.check_def context store (V.push V.I64 a);
    Q.append context (Value.emit operation) store input;
    Q.append context (Payload.prefix offset base) (E.append (Value.emit operation) store) state;
    Payload.emit_def operation offset base)
let (write @ total) : (context : V.context) @ immutable -> (operation : D.word_operation) @ immutable ->
    (offset : B.u32) -> (base : B.u32) -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Write.emit operation offset base) state === Some state} @ ghost =
  fun context operation offset base state premise -> ghost_ (
    payload context operation offset base state ();
    Wasm_static_memory.immediate context (Write.tag_offset ()) base (Value.tag operation) state ();
    Q.append context (Payload.emit operation offset base) (Wasm_immediate_write.emit (Write.tag_offset ()) base (Value.tag operation)) state;
    Write.emit_def operation offset base)
let (primitive @ total) : (context : V.context) @ immutable -> (fragment : Primitive.fragment) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Primitive.emit fragment base) state === Some state} @ ghost =
  fun context fragment base state premise -> ghost_ (
    write context fragment.Primitive.operation fragment.Primitive.left_offset base state ();
    Simple.relayout context (Primitive.moves fragment) base state ();
    Q.append context (Write.emit fragment.Primitive.operation fragment.Primitive.left_offset base)
      (Hmc_wasm_relayout.emit (Primitive.moves fragment) base) state;
    Primitive.emit_def fragment base)
let (block @ total) : (context : V.context) @ immutable -> (fragment : Block.fragment) @ immutable ->
    (base : B.u32) -> (state : V.state) @ immutable -> {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Q.check context (Block.emit fragment base) state === Some state} @ ghost =
  fun context fragment base state premise -> ghost_ (
    Block.emit_def fragment base;
    match fragment with
    | Block.Simple simple -> Simple.simple context simple base state ()
    | Block.Relayout relayout -> Simple.relayout context relayout base state ()
    | Block.Primitive fragment -> primitive context fragment base state ()
    | Block.Global global -> Global.emit_def global base; Simple.literal context global.Global.pc global.Global.value base state ())
let (embedded @ total) : (context : V.context) @ immutable -> (labels : Wasm_static_control.labels) @ immutable ->
    (fragment : Block.fragment) @ immutable -> (base : B.u32) -> (tail : Wasm_control.code) @ immutable -> (state : V.state) @ immutable ->
    {u : unit | V.local context.V.locals base === Some V.I32} ->
    {u : unit | Wasm_static_control.check context labels (Wasm_control_lift.embed (Block.emit fragment base) tail) state ===
      Wasm_static_control.check context labels tail state} @ ghost =
  fun context labels fragment base tail state premise -> ghost_ (
    block context fragment base state ();
    Q.embed_checked context labels (Block.emit fragment base) tail state state ())
