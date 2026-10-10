module B = Wasm_u32
module V = Hmc_tagged_cell
module Heap = Hmc_heap_objects
module Wire = Hmc_heap_wire
module Header = Hmc_wasm_header_update
module Write = Hmc_wasm_call_header
module Model = Wasm_mixed_write
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module I = Wasm_instruction
module C = Wasm_code
type fixture = {memory : B.bytes; expected : B.bytes; prefix : I.t list; code : C.t; cursor : B.u32}
let rec filler n = if n = 0 then B.End else B.Byte (173, filler (n - 1))
let fixture (base : B.u32) recursive argument =
  if base > 7 then failwith "header frame base" else
  let memory = filler 256 in
  let closure = V.Closure_pointer 4096 in
  let self = if recursive then Heap.Cell (closure, Heap.Empty) else Heap.Empty in
  let cells = Heap.Cell (V.Word (Header.number 13), Heap.Cell (closure, Heap.Cell (V.Nil, Heap.Cell (argument, self)))) in
  let width : B.u32 = if recursive then 80 else 64 in
  match Hmc_linear_bytes.drop memory base, Hmc_linear_bytes.drop memory (base + width) with
  | Some before, Some suffix ->
    let expected_bytes = Wire.encode_cells cells suffix in
    let expected = Wasm_memory_splice.replace memory base before expected_bytes () in
    let l3 = S.Push (S.I64 (V.payload argument), S.Empty) in
    let l2 = S.Push (S.I64 (V.tag argument), l3) in
    let l1 = S.Push (S.I32 4096, l2) in
    let locals = S.Push (S.I32 base, l1) in
    let state = {X.memory; machine = {E.locals; stack = S.Empty}} in
    ghost_ (Hmc_linear_bounds.covers_def memory (base + width); Hmc_wasm_call_header_layout.width_def recursive;
      Wasm_locals.get_def locals 0;
      Wasm_locals.get_def locals 1;
      Wasm_locals.get_def locals 2;
      Wasm_locals.get_def locals 3;
      Wasm_locals.get_def l1 0;
      Wasm_locals.get_def l1 1;
      Wasm_locals.get_def l1 2;
      Wasm_locals.get_def l2 0;
      Wasm_locals.get_def l2 1;
      Wasm_locals.get_def l3 0);
    let proved = Hmc_wasm_call_header_memory.correct recursive 13 4096 argument state base (base + width) 0 1 2 3 () in
    if proved.Wasm_mixed_memory.memory <> expected then failwith "decoded header memory";
    let writes = Write.writes recursive 13 1 2 3 in
    (match Model.apply writes memory base locals with
    | None -> failwith "header writes failed"
    | Some after ->
      ghost_ (Wasm_locals.get_def locals 0; Model.correct writes 0 state base after ());
      if after <> expected then failwith "header encoding mismatch";
      let code = Write.emit recursive 13 0 1 2 3 in
      (match X.run code state with
      | X.Done out -> if out.X.memory <> expected || out.X.machine <> state.X.machine then failwith "header target state"
      | _ -> failwith "header target trap");
      {memory; expected; code; cursor = base;
        prefix = [I.I32_const base; I.Local_set 0; I.I32_const 4096; I.Local_set 1;
          I.I64_const (V.tag argument); I.Local_set 2; I.I64_const (V.payload argument); I.Local_set 3]})
  | _ -> failwith "header memory coverage"
let fixtures () =
  List.concat_map (fun argument -> [fixture 0 false argument; fixture 7 false argument; fixture 0 true argument; fixture 7 true argument])
    [V.Word {Hmc_word64.lo = 42; hi = 37}; V.Boolean true; V.Cons_pointer 512; V.Closure_pointer 1024]
