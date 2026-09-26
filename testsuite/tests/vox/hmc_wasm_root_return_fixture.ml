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
module Stack = Hmc_memory_stack
module Pad = Hmc_wasm_frame_padding
let rec fill n tail = if n = 0 then tail else B.Byte (173, fill (n - 1) tail)
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
type fixture = {memory : B.bytes; code : C.t; prefix : I.t list; value : V.value}
let fixture : (program : Program.program) @ immutable -> (globals : Machine.globals) @ immutable ->
    (heap : H.heap) @ immutable -> (activation : F.activation) @ immutable ->
    (base : B.u32) -> D.index @ immutable -> V.value @ immutable ->
    {u : unit | Program.lookup program.Program.code activation.F.pc === Some (Program.Keep G.Return)
      && activation.F.temporaries === F.Empty} -> fixture =
  fun program globals heap activation base padding value premise ->
    if base > 7 then failwith "root frame base" else
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
      let top = S.Push (S.I32 512, S.Empty) in
      let payload = S.Push (S.I64 (Header.number 999), top) in
      let tag = S.Push (S.I64 (Header.number 999), payload) in
      let locals = S.Push (S.I32 base, tag) in
      let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
      let labels = T.Label ({T.restart = None; continuation = T.Empty; saved = S.Empty}, T.No_labels) in
      let caller = T.Instruction (I.Plain I.Unreachable, T.Empty) in
      ghost_ (Words.recover cells bytes suffix ();
        L.get_def locals 0; L.get_def locals 1; L.get_def locals 2; L.get_def locals 3;
        L.get_def tag 0; L.get_def tag 1; L.get_def tag 2;
        L.get_def payload 0; L.get_def payload 1; L.get_def top 0;
        L.can_set_def locals 1 (S.I64 (V.tag value)); L.can_set_def locals 2 (S.I64 (V.payload value));
        S.same_type_def (S.I64 (Header.number 999)) (S.I64 (V.tag value));
        S.same_type_def (S.I64 (Header.number 999)) (S.I64 (V.payload value));
        Stack.related_def program.Program.origin.Hmc_cfg_program.blocks 48 memory 512 512 Q.Halt);
      let out = Root.selected_correct program globals heap 8192 (D.S D.Z) activation pc rest state base bytes suffix 0 1 2 3 512 48 1 labels caller () in
      let code = T.Block (Root.selected_emit 0 1 2 3 512 1 caller, T.Empty) in
      if T.run (fuel 1000) {T.code; labels = T.No_labels; state} <>
        T.Finished {X.memory; machine = {E.locals = out; stack = S.Empty}} then failwith "root control";
      if L.get out 1 <> Some (S.I64 (V.tag value)) || L.get out 2 <> Some (S.I64 (V.payload value)) then failwith "root value";
      {memory; code = T.flatten code C.Empty; value;
        prefix = [I.I32_const base; I.Local_set 0; I.I64_const (Header.number 999); I.Local_set 1;
          I.I64_const (Header.number 999); I.Local_set 2; I.I32_const 512; I.Local_set 3]}
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
