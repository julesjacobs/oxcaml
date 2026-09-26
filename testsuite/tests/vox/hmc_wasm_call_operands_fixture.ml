module B = Wasm_u32
module V = Hmc_tagged_cell
module H = Hmc_heap_objects
module W = Hmc_heap_wire
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module Capture = Hmc_wasm_cons_capture
module Call = Hmc_wasm_call_operands
module Header = Hmc_wasm_header_update
let rec cells = function [] -> H.Empty | value :: rest -> H.Cell (value, cells rest)
let rec prefix n tail = if n = 0 then tail else B.Byte (173, prefix (n - 1) tail)
let rec stack = function [] -> S.Empty | value :: rest -> S.Push (value, stack rest)
let check (base : B.u32) (closure : B.u32) argument env =
  let env_count = List.length env in
  if env_count < 0 || env_count > 2 then failwith "operand environment" else
  let zero = S.I64 (Header.number 0) in
  let slots = {Capture.head_tag = 3; head_payload = 4; tail_tag = 5; tail_payload = 6} in
  let memory = prefix base (W.encode_cells
    (cells ([V.Word (Header.number 9); V.Closure_pointer 37; argument] @ env @ [V.Closure_pointer closure; V.Nil])) B.End) in
  let operand_stack = S.Push (S.I64 {Hmc_word64.lo = 19; hi = 43}, S.Empty) in
  let initial = {X.memory; machine = {E.locals = stack [S.I32 base; S.I32 11; S.I32 65536; zero; zero; zero; zero]; stack = operand_stack}} in
  let expected = {X.memory; machine = {E.locals = stack [S.I32 base; S.I32 closure; S.I32 65536;
    S.I64 (V.tag (V.Closure_pointer closure)); S.I64 (V.payload (V.Closure_pointer closure));
    S.I64 (V.tag argument); S.I64 (V.payload argument)]; stack = operand_stack}} in
  if X.run (Call.emit env_count slots 0 1) initial <> X.Done expected then failwith "call operand capture"
let fixtures () =
  List.iter (fun base -> List.iter (fun closure -> List.iter (fun argument ->
    List.iter (fun env -> check base closure argument env)
      [[]; [V.Word (Header.number 12); V.Boolean true]])
    [V.Boolean false; V.Word {Hmc_word64.lo = 4294967295; hi = 2147483648}; V.Nil; V.Cons_pointer 512; V.Closure_pointer 2048])
    [0; 2048; 4294967295]) [0; 7]
