module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module C = Hmc_cfg_program
module O = Hmc_closure_program
module P = Hmc_tail_ir
module Machine = Hmc_heap_machine
module State = Hmc_heap_state
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module V = Hmc_tagged_cell
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Index = Hmc_u32_index
module Lower = Hmc_wasm_program_lower
module Table = Hmc_wasm_program_table
module Assembly = Hmc_wasm_program_functions
module Runtime = Hmc_wasm_program_runtime
module Descriptor = Hmc_runtime_closures
module F = Wasm_functions
module T = Wasm_control
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module WG = Wasm_globals
module Calls = Wasm_calls
module Code = Wasm_code
module Bound = Wasm_shallow_calls
module Checked = Wasm_shallow_module
let rec (zero_padding @ total) : (count : D.index) @ immutable -> {out : Heap.cells | Heap.length out === count} @ immutable =
  fun count -> match count with
  | D.Z -> ghost_ (Heap.length_def Heap.Empty); Heap.Empty
  | D.S rest ->
    let tail = zero_padding rest in
    let out = Heap.Cell (V.Boolean false, tail) in
    ghost_ (Heap.length_def out); out
let rec zeros n = if n = 0 then B.End else B.Byte (0, zeros (n - 1))
let rec overwrite memory at bytes = match memory, bytes with
  | _, B.End -> memory
  | B.End, _ -> failwith "initial image overflow"
  | B.Byte (head, rest), _ -> if at > 0 then B.Byte (head, overwrite rest (at - 1) bytes)
    else match bytes with B.End -> memory | B.Byte (byte, tail) -> B.Byte (byte, overwrite rest 0 tail)
let number (index : D.index @ immutable) : B.u32 = match Index.encode 10000 index with Some n -> n | None -> failwith "fixture index overflow"
let rec heap_image memory = function
  | Heap.Empty_heap _ -> memory
  | Heap.Allocate (allocation, rest) ->
    let memory = heap_image memory rest in
    match Wire.lower 10000 allocation.Heap.object_ with
    | None -> failwith "fixture object encoding"
    | Some object_ -> overwrite memory allocation.Heap.address (Wire.encode object_ B.End)
let rec descriptors memory = function
  | Descriptor.Empty -> memory
  | Descriptor.Add (descriptor, rest) ->
    let memory = descriptors memory rest in
    overwrite memory (32 * number (Descriptor.size rest))
      (Wire.encode_cells (Hmc_runtime_descriptor.cells descriptor) B.End)
let rec source_run program globals limit frames steps state : B.u32 * V.value option =
  if steps = 0 then failwith "source execution fuel" else
  match state.Machine.state with
  | State.Done value -> 1, Some value
  | State.Stuck -> failwith "source execution stuck"
  | State.Running _ -> match Machine.step program globals limit frames state with
    | Machine.Advanced state -> source_run program globals limit frames (steps - 1) state
    | Machine.Exhausted Hmc_heap_machine.Heap -> 2, None
    | Machine.Exhausted Hmc_heap_machine.Stack -> 3, None
let rec depth = function Calls.Root -> 0 | Calls.Caller (_, rest) -> 1 + depth rest
module Dispatch = Hmc_wasm_program_dispatch
module Registers = Hmc_wasm_program_registers
let audit_registers (globals : WG.t @ immutable) (memory : B.bytes @ immutable) =
  match Registers.read globals with
  | None -> failwith "runtime global image"
  | Some registers ->
    ghost_ (Registers.globals_def registers);
    let imported = Registers.import registers globals memory 0 2048 () in
    let config = Runtime.config 0 2048 in
    let initial = {GE.globals; execution = {X.memory; machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
    if Wasm_global_registers.load config.Assembly.loads initial <> Some {GE.globals; execution = imported}
      then failwith "runtime imported image"
let audit_dispatch (module_ : F.module_ @ immutable) (configuration : Calls.configuration @ immutable) =
  let globals = configuration.Calls.current.Wasm_instance_control.globals in
  let memory = configuration.Calls.current.Wasm_instance_control.body.T.state.X.memory in
  let capacity = configuration.Calls.capacity in
  if configuration = Dispatch.loop globals memory capacity || configuration = Dispatch.returned globals memory capacity then audit_registers globals memory;
  if configuration = Dispatch.loop globals memory capacity then
    (match WG.get globals 0 with
    | Some (S.I32 base) ->
      (match Wasm_memory.load memory base 8 Wasm_memory.W32 with
      | Some (S.I32 pc) ->
        ghost_ (Dispatch.frame_global_def (); Dispatch.pc_offset_def (); Dispatch.header module_ globals memory capacity base pc ());
        if Calls.run (Dispatch.two ()) module_ configuration <> Calls.Running (Dispatch.call globals memory pc capacity)
          then failwith "dispatcher header execution"
      | _ -> failwith "dispatcher header load")
    | _ -> failwith "dispatcher frame global")
  else if configuration = Dispatch.returned globals memory capacity then
    (match WG.get globals 5 with
    | Some (S.I32 status) ->
      ghost_ (Dispatch.status_global_def (); Dispatch.resume module_ globals memory capacity status ());
      let expected = if status = 0 then Calls.Running (Dispatch.loop globals memory capacity) else
        Calls.Finished {GE.globals; execution = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 status, S.Empty)}}} in
      if Calls.run (if status = 0 then Dispatch.three () else Dispatch.six ()) module_ configuration <> expected
        then failwith "dispatcher status execution"
    | _ -> failwith "dispatcher status global")
let rec execute : (module_ : F.module_) @ immutable -> (configuration : Calls.configuration) @ immutable -> int -> int ->
    {u : unit | Bound.table module_.F.functions module_.F.table && Bound.bounded configuration} -> GE.state * int =
  fun module_ configuration remaining peak premise ->
  if remaining = 0 then failwith "Wasm execution fuel" else
  audit_dispatch module_ configuration;
  let peak = max peak (depth configuration.Calls.callers) in
  ghost_ (Bound.available module_ configuration ());
  match Calls.step module_ configuration with
  | Calls.Running next -> ghost_ (Bound.step module_ configuration next ()); execute module_ next (remaining - 1) peak ()
  | Calls.Finished state -> state, peak
  | Calls.Type_error -> failwith "Wasm execution type error"
  | Calls.Trap -> failwith "Wasm execution trap"
  | Calls.Host_limit -> unreachable_ ()
  | Calls.Not_supported -> failwith "Wasm unsupported instruction"
let rec check_targets : (program : Lower.program) @ immutable -> (length : D.index) @ immutable -> (count : B.u32) ->
    (config : Assembly.config) @ immutable -> (dispatcher : F.function_) @ immutable -> (blocks : Table.table) @ immutable ->
    {u : unit | Assembly.ordered program.Lower.blocks count && Index.represents length count} -> unit =
  fun program length count config dispatcher blocks premise -> match blocks with
  | Table.Empty -> ()
  | Table.Add (label, _, rest) ->
    if label >= count then failwith "block label exceeds table" else (
      (match Table.lookup program.Lower.blocks label with
      | None -> failwith "block label missing"
      | Some fragment ->
        ghost_ (Assembly.dispatch_target program length count config dispatcher label fragment ());
        let module_ = Assembly.assemble program length count config dispatcher in
        match F.element module_.F.table label with
        | None -> failwith "element missing"
        | Some index -> if F.lookup module_.F.functions index <> Some (Assembly.function_ program fragment config) then
            failwith "function table correspondence");
      check_targets program length count config dispatcher rest ())
type fixture = {module_ : F.module_; memory : B.bytes; expected_memory : B.bytes; globals : WG.t; main : B.u32; expected_status : B.u32;
  expected_tag : Hmc_word64.t; expected_payload : Hmc_word64.t}
let build term (input : B.u32) (heap_limit : B.u32) (stack_frames : B.u32) =
  if heap_limit < 4096 || heap_limit > 8192 then failwith "fixture heap layout" else
  let program = Hmc_wasm_global_fixture.build term in
  let initialized = Hmc_heap_initialize.initialize program 4096 heap_limit (Header.number input) () in
  ghost_ (Hmc_heap_initialize.correct_def program 4096 heap_limit (Header.number input) initialized);
  match initialized with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "fixture initialization exhaustion"
  | Hmc_heap_initialize.Initialized start ->
    let globals = start.Hmc_heap_initialize.globals in
    let initial = start.Hmc_heap_initialize.configuration in
    match Lower.lower program globals 10000, Index.encode 10000 (G.size program.P.origin.C.blocks),
      Descriptor.lower 10000 program.P.origin.C.origin.O.table program.P.origin.C.functions with
    | Some lowered, Some count, Some runtime ->
      let config = Runtime.config 0 2048 in
      (match Assembly.assemble_checked lowered (G.size program.P.origin.C.blocks) count config (Runtime.dispatcher ()) with
      | None -> failwith "generated module violates host-stack policy"
      | Some checked ->
      let module_ = checked.Checked.module_ in
      ghost_ (Checked.valid_def checked);
      ghost_ (Lower.corresponds_def program globals 10000 lowered;
        Assembly.source_order globals program.P.origin.C.blocks program.P.code lowered.Lower.blocks lowered.Lower.capacity 10000 count ());
      ghost_ (Assembly.main_correct lowered (G.size program.P.origin.C.blocks) count config (Runtime.dispatcher ()) ();
        Assembly.element_reject (G.size program.P.origin.C.blocks) count count ());
      if F.lookup module_.F.functions count <> Some (Runtime.dispatcher ()) || F.element module_.F.table count <> None then
        failwith "dispatcher index or element boundary";
      check_targets lowered (G.size program.P.origin.C.blocks) count config (Runtime.dispatcher ()) lowered.Lower.blocks ();
      if lowered.Lower.width > 1024 || stack_frames > 16 || 2048 + stack_frames * lowered.Lower.width > 4096 then
        failwith "fixture stack layout";
      let limit = 2048 + stack_frames * lowered.Lower.width in
      if limit < 2048 || limit > 4096 then failwith "stack limit" else
      let stack_limit : B.u32 = limit in
      let memory = descriptors (heap_image (zeros 8192) initial.Machine.heap) runtime in
      (match initial.Machine.state with
      | State.Running (activation, State.Halt) ->
        (match G.lookup program.P.origin.C.blocks activation.Frame.pc with
        | None -> failwith "initial source label"
        | Some block ->
          if not (Codec.shape block.G.signature activation) then failwith "initial source shape" else
          let _ = ghost_ (Hmc_frame_capacity.lookup program.P.origin.C.blocks activation.Frame.pc block ();
            Hmc_pointer_frame_shape.size block.G.signature) in
          let padding_size = Hmc_frame_capacity.remaining (Hmc_frame_capacity.capacity program.P.origin.C.blocks) (Codec.size block.G.signature) () in
          let padding = zero_padding padding_size in
          let cells = Codec.encode block.G.signature activation padding () in
          let unframed = zeros 8192 in
          let memory = overwrite memory 512 (Wire.encode_cells (Heap.Cell (V.Word (Header.number (number activation.Frame.pc)), cells)) B.End) in
          let registers = {Hmc_wasm_program_registers.frame = 512; heap = Heap.used initial.Machine.heap; heap_limit;
            top = 2048; stack_limit; status = 0; tag = Header.number 0; payload = Header.number 0} in
          (match Index.encode 10000 (Hmc_closure_ir.size program.P.origin.C.origin.O.table),
            Index.encode 10000 activation.Frame.pc, Index.encode 10000 (D.S (Heap.length cells)), Index.encode 134217727 (Descriptor.size runtime) with
          | Some _, Some pc, Some cell_count, Some table_count ->
            let frame_end = 512 + 16 * cell_count in
            if frame_end > 2048 || frame_end <> 512 + lowered.Lower.width || table_count > 16 then failwith "initial memory layout" else
            (match Hmc_linear_bytes.drop unframed heap_limit, Hmc_linear_bytes.drop unframed stack_limit, Hmc_linear_bytes.drop unframed frame_end with
            | Some _, Some _, Some _ ->
              ghost_ (Hmc_linear_bounds.covers_def unframed heap_limit; Hmc_linear_bounds.covers_def unframed stack_limit;
                Hmc_linear_bounds.covers_def unframed frame_end);
              let verified = Hmc_wasm_program_memory_initialize.initialize program globals lowered.Lower.width 2048 frame_end 4096 10000
                (Hmc_tail_semantics.initial program (Header.number input)) initial.Machine.heap activation registers unframed block.G.signature padding pc cell_count () in
              let installed = Hmc_wasm_program_descriptors.install program globals lowered.Lower.width 2048 frame_end
                (Hmc_tail_semantics.initial program (Header.number input)) initial.Machine.heap activation State.Halt registers verified
                block.G.signature padding pc cell_count runtime 0 table_count () in
              if installed.Hmc_wasm_program_frame_store.memory <> memory then failwith "verified initial memory differs"
            | _ -> failwith "initial memory coverage")
          | _ -> failwith "initial frame encoding");
          let wasm_globals = Hmc_wasm_program_registers.globals registers in
          let before = Checked.start checked memory wasm_globals in
          let verified_entry = Hmc_wasm_program_entry.start module_ count wasm_globals memory (Code.Succ Code.Zero) () in
          if verified_entry <> before then failwith "verified module entry differs";
          (match Calls.run (Code.Succ Code.Zero) module_ before with
          | Calls.Running loop ->
            if loop <> Dispatch.loop wasm_globals memory (Code.Succ Code.Zero) then failwith "dispatcher entry differs"
          | _ -> failwith "dispatcher entry stopped");
          let two = Code.Succ (Code.Succ Code.Zero) in
          let four = Code.Succ (Code.Succ two) in
          (match Calls.run four module_ {before with Calls.capacity = Code.Zero} with
          | Calls.Host_limit -> () | _ -> failwith "missing Wasm call slot accepted");
          let invalid = {before with Calls.current = {before.Calls.current with Wasm_instance_control.body =
            {before.Calls.current.Wasm_instance_control.body with T.code =
              T.Instruction (Wasm_instruction.I32_const count,
                T.Instruction (Wasm_instruction.Call_indirect 0, T.Empty))}}} in
          (match Calls.run two module_ invalid with
          | Calls.Trap -> () | _ -> failwith "out-of-range indirect slot accepted");
          let actual, peak = execute module_ before 200000 0 () in
          let expected_status, expected = source_run program globals heap_limit (Hmc_wasm_control_fixture.index stack_frames) 10000 initial in
          let status = Wasm_locals.get actual.GE.globals.WG.values 5 in
          if status <> Some (S.I32 expected_status) || actual.GE.execution.X.machine.E.stack <> S.Push (S.I32 expected_status, S.Empty) then
            failwith "program status disagrees with source";
          if peak <> 1 then failwith "dispatcher host stack depth";
          let expected_tag, expected_payload = match expected with
            | None -> Header.number 0, Header.number 0
            | Some value -> V.tag value, V.payload value in
          if Wasm_locals.get actual.GE.globals.WG.values 6 <> Some (S.I64 expected_tag)
            || Wasm_locals.get actual.GE.globals.WG.values 7 <> Some (S.I64 expected_payload) then failwith "program result disagrees with source";
          {module_; memory; expected_memory = actual.GE.execution.X.memory; globals = wasm_globals; main = count; expected_status; expected_tag; expected_payload})
      | _ -> failwith "initial source state"))
    | _ -> failwith "whole program lowering"
let fixtures () =
  let word n = D.Word (Header.number n) in
  let captured = D.Lambda (D.Let (D.Apply (D.Lambda (D.Bound (D.S D.Z)), word 7), D.Bound D.Z)) in
  let countdown = D.Recursive (D.If (D.Primitive (D.Equal_word, D.Bound D.Z, word 0), word 17,
    D.Apply (D.Bound (D.S D.Z), D.Primitive (D.Subtract, D.Bound D.Z, word 1)))) in
  let lists = D.Lambda (D.CaseList (D.Cons (D.Bound D.Z, D.Nil), word 0, D.Bound D.Z)) in
  let identity = D.Let (D.Lambda (D.Bound D.Z), D.Lambda (D.If (
    D.Apply (D.Bound (D.S D.Z), D.Truth), D.Apply (D.Bound (D.S D.Z), D.Bound D.Z), word 0))) in
  let var n = D.Bound (Hmc_wasm_control_fixture.index n) in
  let map = D.Lambda (D.Recursive (D.CaseList (var 0, D.Nil,
    D.Cons (D.Apply (var 4, var 0), D.Apply (var 3, var 1))))) in
  let sum = D.Recursive (D.CaseList (var 0, word 0,
    D.Primitive (D.Add, var 0, D.Apply (var 3, var 1)))) in
  let mapped_sum = D.Let (map, D.Let (sum, D.Lambda (D.Apply (var 1,
    D.Apply (D.Apply (var 2, D.Lambda (D.Primitive (D.Add, var 0, word 1))),
      D.Cons (var 0, D.Cons (word 2, D.Nil))))))) in
  let ordinary = D.Recursive (D.If (D.Primitive (D.Equal_word, var 0, word 0), word 17,
    D.Primitive (D.Add, word 1, D.Apply (var 1, D.Primitive (D.Subtract, var 0, word 1))))) in
  let expect status value fixture =
    if fixture.expected_status <> status then failwith "fixture did not reach intended outcome";
    (match value with None -> () | Some n ->
      if fixture.expected_tag <> Header.number 1 || fixture.expected_payload.Hmc_word64.lo <> n
        || fixture.expected_payload.Hmc_word64.hi <> 0 then failwith "fixture arithmetic result"); fixture in
  [expect 1 (Some 7) (build captured 7 8192 2); expect 3 None (build captured 7 8192 0);
   expect 1 (Some 17) (build countdown 25 8192 0); expect 1 (Some 42) (build lists 42 8192 0);
   expect 2 None (build lists 42 4112 0); expect 1 (Some 42) (build identity 42 8192 2);
   expect 1 (Some 11) (build mapped_sum 7 8192 5); expect 1 (Some 21) (build ordinary 4 8192 4);
   expect 3 None (build ordinary 4 8192 2)]
