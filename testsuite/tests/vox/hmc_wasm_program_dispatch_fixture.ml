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
module Dispatch = Hmc_wasm_program_dispatch
module Runtime = Hmc_wasm_program_runtime
module Registers = Hmc_wasm_program_registers
module Source_root = Hmc_wasm_program_source_root
module Fixture = Hmc_wasm_program_run_fixture
type fixture = Fixture.fixture
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
      if Hmc_wasm_program_table.lookup lowered.Lower.blocks pc <> Some Hmc_wasm_program_block.Return then failwith "root lowered fragment";
      let config = Runtime.config 0 512 in
      let registers = {Registers.frame = base; heap = H.used heap; heap_limit = 8192; top = 512; stack_limit = 512;
        status = 99; tag = Header.number 999; payload = Header.number 999} in
      let wasm_globals = Registers.globals registers in
      ghost_ (Words.recover cells bytes suffix ());
          (match Hmc_u32_index.encode 1000 (G.size program.Program.origin.Hmc_cfg_program.blocks) with
          | None -> failwith "dispatcher block count"
          | Some count ->
          if pc >= count then failwith "dispatcher PC range" else
          (match Hmc_wasm_program_table.lookup lowered.Lower.blocks pc with
          | Some Hmc_wasm_program_block.Return ->
          ghost_ (Lower.corresponds_def program globals 1000 lowered;
            Assembly.source_order globals program.Program.origin.Hmc_cfg_program.blocks program.Program.code lowered.Lower.blocks lowered.Lower.capacity 1000 count ();
            Assembly.dispatch_target lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) count config (Runtime.dispatcher ()) pc Hmc_wasm_program_block.Return ());
          let module_ = Assembly.assemble lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) count config (Runtime.dispatcher ()) in
          ghost_ (Dispatch.void_signature_def ();
            Assembly.assemble_def lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) count config (Runtime.dispatcher ());
            Func.signature_def module_.Func.signatures 0);
          let result = Source_root.correct lowered module_ program globals heap activation (D.S D.Z) registers memory bytes suffix pc rest 0 512 C.Zero
            (Assembly.reverse_index count pc) () in
          let fuel = result.Source_root.fuel in
            let loop = Dispatch.loop wasm_globals memory (C.Succ C.Zero) in
            (match Calls.run fuel module_ loop with
            | Calls.Finished actual ->
              if actual.GE.execution.X.memory <> memory || actual.GE.execution.X.machine.E.stack <> S.Push (S.I32 1, S.Empty)
                || WG.get actual.GE.globals 6 <> Some (S.I64 (V.tag value))
                || WG.get actual.GE.globals 7 <> Some (S.I64 (V.payload value)) then failwith "dispatcher root result"
            | _ -> failwith "dispatcher root execution");
            ghost_ (Assembly.main_correct lowered (G.size program.Program.origin.Hmc_cfg_program.blocks) count config (Runtime.dispatcher ()) ();
              Dispatch.enter module_ wasm_globals memory (C.Succ C.Zero));
            (match Calls.start module_ count memory wasm_globals (C.Succ C.Zero) with
            | Calls.Running start ->
              (match Calls.run (C.Succ fuel) module_ start with
              | Calls.Finished actual -> if actual.GE.execution <> {X.memory; machine = {E.locals = S.Empty; stack = S.Push (S.I32 1, S.Empty)}} then failwith "dispatcher entry result"
              | _ -> failwith "dispatcher entry execution")
            | _ -> failwith "dispatcher start");
            {Fixture.module_; memory; expected_memory = memory; globals = wasm_globals; main = count; expected_status = 1;
              expected_tag = V.tag value; expected_payload = V.payload value}
          | _ -> failwith "dispatcher root fragment"))

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
