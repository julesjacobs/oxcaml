module B = Wasm_u32
module D = Hm_declarative
module G = Hmc_cfg_ir
module H = Hmc_heap_objects
module F = Hmc_heap_frame
module Q = Hmc_heap_state
module V = Hmc_tagged_cell
module Program = Hmc_tail_ir
module Machine = Hmc_heap_machine
module Header = Hmc_wasm_header_update
module Cells = Hmc_wasm_call_save_memory
module Wire = Hmc_heap_wire
module Words = Hmc_wire_word_sequence
module Bytes = Hmc_linear_bytes
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module I = Wasm_instruction
module C = Wasm_code
module T = Wasm_control
module Root = Hmc_wasm_root_return
module New = Hmc_wasm_program_root
module Lower = Hmc_wasm_program_lower
module Emit = Hmc_wasm_program_emit
module Block = Hmc_wasm_structured_block
module Assembly = Hmc_wasm_program_functions
module Round = Hmc_wasm_program_roundtrip
module R = Wasm_global_registers
module WG = Wasm_globals
module GE = Wasm_global_execution
module Func = Wasm_functions
module Calls = Wasm_calls
module P = Wasm_instance_control
module Stack = Hmc_memory_stack
module Pad = Hmc_wasm_frame_padding
let rec fill n tail = if n = 0 then tail else B.Byte (173, fill (n - 1) tail)
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
type fixture = {memory : B.bytes; block : T.code; globals : WG.t; value : V.value}
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable ->
    (base : B.u32) -> D.index @ immutable -> V.value @ immutable ->
    {u : unit | Program.lookup program.Program.code activation.F.pc === Some (Program.Keep G.Return)
      && activation.F.temporaries === F.Empty} -> fixture =
  fun program globals heap activation base padding value premise ->
    if base > 7 then failwith "root frame base" else
    match Lower.lower program globals 1000 with
    | None -> failwith "root program lowering"
    | Some lowered ->
    if lowered.Lower.width = 0 then failwith "root stack width" else
    let activation = {activation with F.accumulator = value} in
    let rest = Pad.cells padding in
    match Hmc_u32_index.encode 1000 activation.F.pc with
    | None -> failwith "root PC"
    | Some pc ->
    let cells = Cells.cells pc activation.F.current value rest in
    let suffix = fill 32 B.End in
    let bytes = Wire.encode_cells cells suffix in
    let memory = fill base bytes in
    match Bytes.drop memory base with
    | None -> failwith "root bytes"
    | Some bytes ->
      match Wasm_word_sequence.decode (Words.words cells) bytes with
      | None -> failwith "root wire"
      | Some suffix ->
      let top = S.Push (S.I32 512, S.Push (S.I32 99, S.Empty)) in
      let payload = S.Push (S.I64 (Header.number 999), top) in
      let tag = S.Push (S.I64 (Header.number 999), payload) in
      let locals = S.Push (S.I32 base, tag) in
      if Hmc_wasm_program_table.lookup lowered.Lower.blocks pc <> Some Hmc_wasm_program_block.Return then failwith "root lowered fragment";
      let runtime = {Emit.structured = {Block.frame = 0; heap = 0; limit = 0; object_ = 0;
        scratch = {Hmc_wasm_cons_capture.head_tag = 1; head_payload = 2; tail_tag = 1; tail_payload = 2}};
        top = 3; stack_limit = 0; code = 0; address = 0; descriptor = {Hmc_wasm_descriptor_load.start = 0; captures = 0; recursive = 0};
        status = 4; result_tag = 1; result_payload = 2} in
      let config = {Assembly.locals = runtime; local_types = Func.Local32 (Func.Local64 (Func.Local64 (Func.Local32 (Func.Local32 Func.No_locals))));
        loads = R.Binding (0, 0, R.Binding (1, 1, R.Binding (2, 2, R.Binding (3, 3, R.Binding (4, 4, R.End)))));
        stores = R.Binding (1, 1, R.Binding (2, 2, R.Binding (4, 4, R.End))); table_base = 0; stack_base = 512} in
      let wasm_globals = {WG.values = locals; permissions = WG.Global (false, WG.Global (true, WG.Global (true, WG.Global (false, WG.Global (true, WG.Empty)))))} in
      let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = Func.zero_locals config.Assembly.local_types; stack = S.Empty}}} in
      (match R.load config.Assembly.loads initial with
      | None -> failwith "root import"
      | Some imported ->
        let state = imported.GE.execution in
        let locals = state.X.machine.E.locals in
        (match L.get locals 0, L.get locals 3 with
        | Some (S.I32 actual_base), Some (S.I32 actual_top) ->
          if actual_base <> base || actual_top <> 512
            || not (L.can_set locals 1 (S.I64 (V.tag value)) && L.can_set locals 2 (S.I64 (V.payload value)) && L.can_set locals 4 (S.I32 0))
            || not (Wasm_local_preservation.preserves (Wasm_frame_snapshot.emit (Root.reads 1 2) 0) 4) then failwith "root imported layout" else
          let _ = ghost_ (Words.recover cells bytes suffix ();
            Stack.related_def program.Program.origin.Hmc_cfg_program.blocks lowered.Lower.width memory 512 512 Q.Halt;
            Hmc_wasm_program_status.zero_def ()) in
          let result = New.correct lowered runtime 0 program globals heap 8192 (D.S D.Z) activation pc rest state base bytes suffix 0 1 2 3 512 lowered.Lower.width (Round.labels config) () in
          let target = Assembly.function_ lowered Hmc_wasm_program_block.Return config in
          let module_ = {Func.functions = Func.Function (target, Func.No_functions); signatures = Func.Signature (Func.Void, Func.No_signatures); table = Func.Element (Some 0, Func.No_elements)} in
          let call = {Calls.current = {P.globals = wasm_globals; body = {T.code = T.Instruction (I.Call_indirect 0, T.Empty); labels = T.No_labels;
            state = {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 0, S.Empty)}}}};
            result = Func.Void; callers = Calls.Root; capacity = C.Succ C.Zero} in
          (match R.store config.Assembly.stores {GE.globals = wasm_globals; execution = result.New.state} with
          | None -> failwith "root export"
          | Some exported ->
            ghost_ (Func.signature_def module_.Func.signatures 0; Func.element_def module_.Func.table 0; Func.lookup_def module_.Func.functions 0;
              Round.correct lowered Hmc_wasm_program_block.Return config module_ call 0 0 0 T.Empty S.Empty C.Zero state result.New.state exported result.New.fuel ());
            (match Calls.run (Round.cost config result.New.fuel) module_ call with
            | Calls.Running actual ->
              if actual.Calls.current.P.body.T.state.X.memory <> memory || actual.Calls.callers <> Calls.Root || actual.Calls.capacity <> call.Calls.capacity
                || WG.get actual.Calls.current.P.globals 4 <> Some (S.I32 1)
                || WG.get actual.Calls.current.P.globals 1 <> Some (S.I64 (V.tag value))
                || WG.get actual.Calls.current.P.globals 2 <> Some (S.I64 (V.payload value)) then failwith "root indirect result"
            | _ -> failwith "root indirect execution");
            {memory; block = target.Func.code; globals = wasm_globals; value})
        | _ -> failwith "root imported types"))

let rec reach (program : Program.program @ immutable) (globals : Machine.globals @ immutable)
    (configuration : Machine.configuration @ immutable) remaining (base : B.u32) (padding : D.index @ immutable) (value : V.value @ immutable) =
  if remaining = 0 then failwith "root source fuel" else
  match configuration.Machine.state with
  | Q.Running (activation, frames) ->
    (match frames, activation.F.temporaries, Program.lookup program.Program.code activation.F.pc with
    | Q.Halt, F.Empty, Some (Program.Keep G.Return) -> fixture program globals configuration.Machine.heap activation base padding value ()
    | _ -> match Machine.step program globals 8192 (D.S (D.S D.Z)) configuration with
      | Machine.Advanced next -> reach program globals next (remaining - 1) base padding value
      | Machine.Exhausted _ -> failwith "root source exhaustion")
  | _ -> failwith "root source state"
let fixtures () =
  let program = Hmc_wasm_global_fixture.build (D.Lambda (D.Bound D.Z)) in
  match Hmc_heap_initialize.initialize program 1024 8192 (Header.number 42) () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "root initialization"
  | Hmc_heap_initialize.Initialized initial ->
    let values = [V.Word {Hmc_word64.lo = 4294967295; hi = 4294967295}; V.Boolean false; V.Boolean true;
      V.Nil; V.Cons_pointer 2048; V.Closure_pointer 4096] in
    List.concat_map (fun padding -> List.concat_map (fun base ->
      List.map (reach program initial.Hmc_heap_initialize.globals initial.Hmc_heap_initialize.configuration 100 base padding) values)
      [0; 7]) [D.Z; D.S (D.S (D.S D.Z))]
