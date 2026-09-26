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
module R = Hmc_wasm_moves_fixture
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
          let locals = S.Push (S.I32 base, S.Push (S.I64 (Header.number 37), S.Push (S.I64 (Header.number 99), S.Empty))) in
          let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
          let next_signature = {signature with G.accumulator = Some ty} in
          ghost_ (Heap.length_def full; Wasm_locals.get_def locals 0;
            Block.corresponds_def globals signature instruction 1024 1024 lowered);
          let table = Hmc_wasm_table_lower.Add (old_pc, lowered, Hmc_wasm_table_lower.Empty) in
          ghost_ (Hmc_wasm_table_lower.lookup_def table old_pc);
          let result = Hmc_wasm_dispatch_global_source.correct program globals configuration.Machine.heap 1048576 (D.S (D.S (D.S D.Z)))
            signature next_signature activation frames index ty derivation next table Wasm_control.No_labels fragment old_pc cells padding state 0 base before_frame tail () in
          let expected_state = {state with X.memory = result.Hmc_wasm_global_invariant.memory} in
          (match Wasm_control.run (Hmc_wasm_dispatch_loop.cost table old_pc 0 lowered)
              (Hmc_wasm_dispatch_loop.configuration table 0 Wasm_control.No_labels state) with
          | Wasm_control.Running actual ->
            if actual <> Hmc_wasm_dispatch_loop.configuration table 0 Wasm_control.No_labels expected_state then failwith "global dispatcher iteration"
          | _ -> failwith "global dispatcher stopped");
          if not (Codec.shape next_signature result.Hmc_wasm_global_invariant.activation) then failwith "global successor shape" else
          let expected_cells = Codec.encode next_signature result.Hmc_wasm_global_invariant.activation padding () in
          ghost_ (Hmc_frame_decode_unique.frame next_signature next result.Hmc_wasm_global_invariant.cells expected_cells result.Hmc_wasm_global_invariant.activation padding ();
            Block.emit_def lowered 0);
          let code : {code : Wasm_code.t | X.run code state === X.Done {X.memory = result.Hmc_wasm_global_invariant.memory; machine = state.X.machine}} @ immutable = Block.emit lowered 0 in
          (match X.run code state with
          | X.Done actual -> if actual.X.memory <> result.Hmc_wasm_global_invariant.memory then failwith "global Wasm execution"
          | _ -> failwith "global Wasm trap");
          let fixture = {R.memory; base; first = Header.number 37; second = Header.number 99; code;
            expected = Hmc_wasm_range_fixture.fields 0 (Heap.Cell (V.Word (Header.number fragment.Lower.pc), expected_cells))} in
          fixture :: rest ()
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
