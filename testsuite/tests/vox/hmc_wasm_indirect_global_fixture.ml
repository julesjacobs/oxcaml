module B = Wasm_u32
module W = Hmc_word64
module D = Hm_declarative
module G = Hmc_cfg_ir
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Frame = Hmc_heap_frame
module State = Hmc_heap_state
module Machine = Hmc_heap_machine
module Program = Hmc_tail_ir
module Codec = Hmc_pointer_frame_codec
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Lower = Hmc_wasm_global_lower
module Block = Hmc_wasm_block_lower
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module New = Hmc_wasm_indirect_global
module Registers = Wasm_global_registers
module Transfer = Wasm_global_local_transfer
module WG = Wasm_globals
module GE = Wasm_global_execution
module T = Wasm_control
module P = Wasm_instance_control
module Func = Wasm_functions
module Calls = Wasm_calls
module WI = Wasm_instruction
module WC = Wasm_code
module WL = Wasm_locals
type fixture = {memory : B.bytes; expected : B.bytes; block : T.code; base : B.u32}
let build : D.term @ immutable -> Program.program @ immutable = fun source -> match Hmc_specialization.compile source with
  | Hmc_specialization.Compiled p -> Program.build (Hmc_cfg_program.build (Hmc_closure_program.build p))
  | _ -> failwith "global fixture source rejected"
let rec collect (program : Program.program) globals configuration (base : B.u32) fuel =
  if base > 7 || fuel = 0 then failwith "global fixture bounds" else
  match configuration.Machine.state with
  | State.Done _ -> []
  | State.Stuck -> failwith "global fixture stuck"
  | State.Running (activation, frames) ->
    let next_result = Machine.step program globals 1048576 (D.S (D.S (D.S D.Z))) configuration in
    let rest () = match next_result with
      | Machine.Exhausted _ -> failwith "global fixture exhausted"
      | Machine.Advanced next -> collect program globals next base (fuel - 1) in
    match Program.lookup program.Program.code activation.Frame.pc with
    | Some (Program.Keep (G.Load (G.Global index, ty, derivation, next) as instruction)) ->
      (match G.lookup program.Program.origin.Hmc_cfg_program.blocks activation.Frame.pc with
      | None -> failwith "global fixture signature"
      | Some block ->
        let signature = block.G.signature in
        if not (Codec.shape signature activation) then failwith "global fixture frame shape" else
        let padding = Heap.Cell (V.Nil, Heap.Cell (V.Nil, Heap.Empty)) in
        let cells = Codec.encode signature activation padding () in
        match Hmc_u32_index.encode 1024 activation.Frame.pc, Block.lower globals signature instruction 1024 1024 with
        | Some old_pc, Some (Block.Global fragment as lowered) ->
          if Lower.lower Machine.Empty_globals instruction 1024 <> None then failwith "missing global accepted";
          let tail = B.Byte (42, B.End) in
          let full = Heap.Cell (V.Word (Header.number old_pc), cells) in
          let before_frame = Wire.encode_cells full tail in
          let memory = Hmc_wasm_frame_fixture.prefix base before_frame in
          let locals = S.Push (S.I32 base, S.Empty) in
          let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
          let next_signature = {signature with G.accumulator = Some ty} in
          ghost_ (Heap.length_def full; Wasm_locals.get_def locals 0;
            Block.corresponds_def globals signature instruction 1024 1024 lowered);
          let loads = Registers.Binding (0, 0, Registers.End) in
          let wasm_globals = {WG.values = S.Push (S.I32 base, S.Empty); permissions = WG.Global (false, WG.Empty)} in
          let target = {Func.result = Func.Void; locals = Func.Local32 Func.No_locals; code = New.emit loads fragment 0} in
          let initial = {GE.globals = wasm_globals; execution = {X.memory; machine = {E.locals = Func.zero_locals target.Func.locals; stack = S.Empty}}} in
          (match Transfer.load 0 0 initial with
          | None -> failwith "source block register import"
          | Some imported ->
          ghost_ (WG.get_def wasm_globals 0; WL.get_def wasm_globals.WG.values 0;
            Func.zero_locals_def target.Func.locals; Func.zero_locals_def Func.No_locals;
            WL.replaced_def initial.GE.execution.X.machine.E.locals 0 (S.I32 base) imported.GE.execution.X.machine.E.locals;
            Registers.load_code_def loads; Registers.load_code_def Registers.End; Transfer.load_code_def 0 0);
          let module_ = {Func.functions = Func.Function (target, Func.No_functions);
            signatures = Func.Signature (Func.Void, Func.No_signatures); table = Func.Element (Some 0, Func.No_elements)} in
          let call = {Calls.current = {P.globals = wasm_globals; body = {T.code = T.Instruction (WI.Call_indirect 0, T.Empty); labels = T.No_labels;
            state = {initial.GE.execution with X.machine = {initial.GE.execution.X.machine with E.stack = S.Push (S.I32 0, S.Empty)}}}};
            result = Func.Void; callers = Calls.Root; capacity = WC.Succ WC.Zero} in
          ghost_ (Func.signature_def module_.Func.signatures 0; Func.element_def module_.Func.table 0; Func.lookup_def module_.Func.functions 0);
          let wrapped = New.correct loads module_ target call 0 0 0 T.Empty S.Empty WC.Zero
            program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
            signature next_signature activation frames index ty derivation next fragment old_pc cells padding state 0 base before_frame tail () in
          let result = wrapped.New.source in
          (match Calls.run wrapped.New.fuel module_ call with
          | Calls.Running returned ->
            if returned.Calls.current.P.body.T.state.X.memory <> result.Hmc_wasm_global_invariant.memory
              || returned.Calls.capacity <> call.Calls.capacity || returned.Calls.callers <> Calls.Root
              then failwith "source block indirect return"
          | _ -> failwith "source block indirect execution");
          if not (Codec.shape next_signature result.Hmc_wasm_global_invariant.activation) then failwith "global successor shape" else
          let expected_cells = Codec.encode next_signature result.Hmc_wasm_global_invariant.activation padding () in
          ghost_ (Hmc_frame_decode_unique.frame next_signature next result.Hmc_wasm_global_invariant.cells expected_cells result.Hmc_wasm_global_invariant.activation padding ();
            Block.emit_def lowered 0);
          let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = result.Hmc_wasm_global_invariant.memory; machine = state.X.machine}} @ immutable = Block.emit lowered 0 in
          (match X.run code state with
          | X.Done actual -> if actual.X.memory <> result.Hmc_wasm_global_invariant.memory then failwith "global Wasm execution"
          | _ -> failwith "global Wasm trap");
          let expected = Hmc_wasm_frame_fixture.prefix base
            (Wire.encode_cells (Heap.Cell (V.Word (Header.number fragment.Lower.pc), expected_cells)) tail) in
          if expected <> result.Hmc_wasm_global_invariant.memory then failwith "independent source block encoding";
          let fixture = {memory; base; block = target.Func.code; expected} in
          fixture :: rest ())
        | _ -> failwith "global fixture lowering")
    | _ -> rest ()
let fixtures () =
  let zero = D.Z in
  let one = D.S zero in
  let source = D.Let (D.Lambda (D.Bound zero), D.Lambda (D.If (
    D.Apply (D.Bound one, D.Truth), D.Apply (D.Bound one, D.Bound zero), D.Word (Header.number 0)))) in
  let program = build source in
  match Hmc_heap_initialize.initialize program 64 1048576 (Header.number 42) () with
  | Hmc_heap_initialize.Heap_exhausted _ -> failwith "global initialization"
  | Hmc_heap_initialize.Initialized start ->
    let result = collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration 0 200
      @ collect program start.Hmc_heap_initialize.globals start.Hmc_heap_initialize.configuration 7 200 in
    if List.length result <> 4 then failwith "global fixture coverage" else result
