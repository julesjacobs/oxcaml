module B = Wasm_u32
module D = Hm_declarative
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Lower = Hmc_wasm_closure_write
module Copy = Wasm_cross_copy
module M = Wasm_memory
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module I = Wasm_instruction
module C = Wasm_code
module Bytes = Hmc_linear_bytes
module G = Hmc_cfg_ir
module Frame = Hmc_heap_frame
module Codec = Hmc_pointer_frame_codec
module Source = Hmc_wasm_closure_memory_source
module Memory = Hmc_wasm_closure_memory
type fixture = {memory : B.bytes; expected : B.bytes; prefix : I.t list; code : C.t; cursor : B.u32}
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let fixture (base : B.u32) count =
  if base > 7 then failwith "closure frame base" else
  let target : B.u32 = 512 + base in
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context = if count = 0 then D.Empty_context else if count = 1 then context1 else D.Binding (D.Forall (D.Z, D.Word64), D.Binding (D.Forall (D.Z, D.Word64), context1)) in
  let word = V.Word {Hmc_word64.lo = 42; hi = 37} in
  let captures = if count = 0 then Heap.Empty else if count = 1 then Heap.Cell (word, Heap.Empty)
    else Heap.Cell (word, Heap.Cell (V.Closure_pointer 128, Heap.Cell (V.Cons_pointer 256, Heap.Empty))) in
  let signature = {G.locals = context; temporaries = G.Empty_temporaries; accumulator = None} in
  let activation = {Frame.pc = Hmc_wasm_control_fixture.index 7; current = V.Closure_pointer 64; accumulator = V.Nil; env = captures; temporaries = Frame.Empty} in
  if not (Codec.shape signature activation) then failwith "closure source frame" else
  let source_cells = Codec.encode signature activation Heap.Empty () in
  let cells = Heap.Cell (V.Word (Header.number 7), source_cells) in
  let trailer = filler 1024 in
  let frame = Wire.encode_cells cells trailer in
  let memory = Hmc_wasm_frame_fixture.prefix base frame in
  let id = Hmc_wasm_control_fixture.index 13 in
  match Lower.build context id 16 100 with
  | None -> failwith "closure write build"
  | Some fragment ->
    if fragment.Lower.bytes <> 16 * (count + 1) then failwith "closure allocation width";
    if fragment.Lower.bytes > 64 then failwith "closure fixture width" else
    (match Bytes.drop memory target, Bytes.drop memory (target + fragment.Lower.bytes) with
    | Some before, Some suffix ->
      (match Bytes.drop memory 1024 with
      | None -> failwith "closure fixture coverage"
      | Some _ ->
      let object_bytes = Wire.encode (Wire.Closure (13, captures)) suffix in
      let expected = Wasm_memory_splice.replace memory target before object_bytes () in
      let locals = S.Push (S.I32 base, S.Push (S.I32 target, S.Empty)) in
      let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 91, S.Empty)}} in
      ghost_ (Wasm_locals.get_def locals 0; Wasm_locals.get_def locals 1; Wasm_locals.get_def (S.Push (S.I32 target, S.Empty)) 0);
      ghost_ (Hmc_linear_bounds.covers_def memory 1024; Heap.length_def cells);
      let proved = Source.correct signature activation source_cells Heap.Empty 7 id 16 100 fragment state base target 1024 0 1 frame trailer () in
      if proved.Memory.memory <> expected then failwith "closure decoder-derived memory";
      (match Copy.apply fragment.Lower.copies memory base target with
      | None -> failwith "closure capture copy"
      | Some copied -> (match M.store copied target (Lower.zero ()) (S.I64 (Hmc_wasm_header_words.tag ())) with
        | None -> failwith "closure header tag"
        | Some tagged -> (match M.store tagged target (Hmc_wasm_pc_update.offset ()) (S.I64 (Header.number fragment.Lower.code)) with
          | None -> failwith "closure header code"
          | Some after ->
            ghost_ (Lower.correct fragment 0 1 state base target copied tagged after ());
            if after <> expected then failwith "closure encoding mismatch";
            let code = Lower.emit fragment 0 1 in
            (match X.run code state with
            | X.Done out -> if out.X.memory <> expected || out.X.machine <> state.X.machine then failwith "closure write execution"
            | _ -> failwith "closure write trap");
            {memory; expected; prefix = [I.I32_const base; I.Local_set 0; I.I32_const target; I.Local_set 1; I.I32_const 91];
              code = E.append code (C.Next (I.Plain I.Drop, C.Empty)); cursor = target}))))
    | _ -> failwith "closure memory coverage")
let fixtures () =
  let context = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  if Lower.build context D.Z 2 0 <> None || Lower.build context (D.S D.Z) 3 0 <> None then failwith "invalid closure plan accepted";
  [fixture 0 0; fixture 7 0; fixture 0 1; fixture 7 1; fixture 0 3; fixture 7 3]
let allocation_fixture (base : B.u32) count enough =
  let written = fixture base count in
  let context1 = D.Binding (D.Forall (D.Z, D.Word64), D.Empty_context) in
  let context = if count = 0 then D.Empty_context else if count = 1 then context1 else D.Binding (D.Forall (D.Z, D.Word64), D.Binding (D.Forall (D.Z, D.Word64), context1)) in
  match Lower.build context (Hmc_wasm_control_fixture.index 13) 16 100 with
  | None -> failwith "closure allocation build"
  | Some fragment ->
    if written.cursor > 519 || fragment.Lower.bytes < 16 || fragment.Lower.bytes > 64 then failwith "closure allocation fixture bounds" else
    let limit : B.u32 = written.cursor + fragment.Lower.bytes - (if enough then 0 else 1) in
    let cursor : B.u32 = if enough then written.cursor + fragment.Lower.bytes else written.cursor in
    let expected = if enough then written.expected else written.memory in
    let locals = S.Push (S.I32 base, S.Push (S.I32 written.cursor, S.Push (S.I32 limit, S.Empty))) in
    let state = {X.memory = written.memory; machine = {E.locals; stack = S.Empty}} in
    let code = Hmc_wasm_closure_conditional.emit fragment 0 1 2 in
    (match Wasm_control.run (Hmc_wasm_closure_conditional.cost written.cursor limit fragment 0 1 2)
        {Wasm_control.code; labels = Wasm_control.No_labels; state} with
    | Wasm_control.Finished out ->
      if out.X.memory <> expected || out.X.machine.E.stack <> S.Empty
        || Wasm_locals.get out.X.machine.E.locals 1 <> Some (S.I32 cursor)
        || Wasm_locals.get out.X.machine.E.locals 0 <> Some (S.I32 base)
        || Wasm_locals.get out.X.machine.E.locals 2 <> Some (S.I32 limit)
      then failwith "closure allocation result"
    | _ -> failwith "closure allocation control");
    {memory = written.memory; expected; cursor; code = Wasm_control.flatten code C.Empty;
      prefix = [I.I32_const base; I.Local_set 0; I.I32_const written.cursor; I.Local_set 1; I.I32_const limit; I.Local_set 2]}
let allocation_fixtures () =
  List.concat_map (fun count -> [allocation_fixture 0 count true; allocation_fixture 7 count true;
    allocation_fixture 0 count false; allocation_fixture 7 count false]) [0; 1; 3]
