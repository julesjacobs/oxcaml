module B = Wasm_u32
module Table = Hmc_runtime_descriptor_table
module Address = Hmc_wasm_descriptor_address
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module F = Hmc_wasm_descriptor_load_fixture
let check (base : B.u32) (code : Table.count) same_local =
  if base + 32 * code > 4294967295 then failwith "address overflow fixture" else
  let destination : B.u32 = if same_local then 0 else 1 in
  let locals = F.locals [code; 17; 4294967295] in
  let memory = B.Byte (173, B.End) in
  let stack = S.Push (S.I64 {Hmc_word64.lo = 7; hi = 2147483648}, S.Empty) in
  let state = {X.memory; machine = {E.locals; stack}} in
  match L.get locals 0, L.get locals destination with
  | Some (S.I32 actual), Some (S.I32 _) when actual = code ->
    let out = Address.correct base code 0 destination state () in
    let expected = if same_local then F.locals [base + 32 * code; 17; 4294967295]
      else F.locals [code; base + 32 * code; 4294967295] in
    if out <> expected || X.run (Address.emit base 0 destination) state <> X.Done {X.memory; machine = {E.locals = expected; stack}}
    then failwith "descriptor address execution"
  | _ -> failwith "address locals"
let fixtures () = List.iter (fun ((base, code) : B.u32 * Table.count) -> List.iter (check base code) [false; true])
  [0, 0; 7, 0; 0, 1; 7, 3; 0, 134217727; 31, 134217727; 4294967295, 0]
