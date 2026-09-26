module B = Wasm_u32
module W = Hmc_word64
module S = Wasm_scalar
module L = Wasm_locals
module G = Wasm_globals
module E = Wasm_execution
module X = Wasm_memory_execution
module GE = Wasm_global_execution
module F = Wasm_functions
module R = Wasm_global_registers
module Runtime = Hmc_wasm_program_runtime
module Assembly = Hmc_wasm_program_functions
module Registers = Hmc_wasm_program_registers
module Replace = Wasm_local_replace
let set locals (index : B.u32) value = match L.set locals index value with
  | None -> failwith "register fixture local type"
  | Some after -> after
let fixture (base : B.u32) (status : B.u32) =
  let maximum = {W.lo = 4294967295; hi = 4294967295} in
  let before = {Registers.frame = base; heap = 1024; heap_limit = 8192; top = 8192; stack_limit = 16384;
    status = 99; tag = maximum; payload = {W.lo = 0; hi = 2147483648}} in
  let globals = Registers.globals before in
  let memory = B.Byte (173, B.Byte (0, B.Byte (255, B.End))) in
  let config = Runtime.config 32 8192 in
  ghost_ (Registers.globals_def before);
  let imported = Registers.import before globals memory 32 8192 () in
  if R.load config.Assembly.loads {GE.globals; execution = {X.memory; machine = {E.locals = F.zero_locals config.Assembly.local_types; stack = S.Empty}}}
    <> Some {GE.globals; execution = imported} then failwith "canonical register import";
  let after = {before with Registers.heap = 4096; top = 9216; status; tag = {W.lo = 3; hi = 0}; payload = maximum} in
  ghost_ (Registers.local_values after);
  let locals = Registers.locals after in
  let locals = List.fold_left (fun locals index -> set locals index (S.I32 4294967295)) locals [0; 2; 3; 5; 6; 7; 8; 9; 10] in
  let locals = List.fold_left (fun locals index -> set locals index (S.I64 maximum)) locals [14; 15; 16; 17] in
  let state = {X.memory; machine = {E.locals; stack = S.Push (S.I32 123, S.Empty)}} in
  match Registers.read_exports before locals with
  | None -> failwith "valid export registers rejected"
  | Some decoded ->
    let exported = Registers.export before decoded state 32 8192 () in
    if exported <> Registers.globals after || R.store config.Assembly.stores {GE.globals; execution = state} <> Some {GE.globals = exported; execution = state}
      then failwith "canonical register export";
    if Registers.read exported <> Some after then failwith "register image decoding";
    if Registers.read {exported with G.permissions = G.Empty} <> None then failwith "invalid global permissions accepted";
    if Registers.read {exported with G.values = Replace.replace exported.G.values 7 (S.I32 0)} <> None then failwith "invalid global type accepted";
    if Registers.read_exports before (Replace.replace locals 12 (S.I32 0)) <> None then failwith "invalid export type accepted"
let fixtures () = List.iter (fun base -> List.iter (fixture base) [0; 1; 2; 3]) [0; 7]
