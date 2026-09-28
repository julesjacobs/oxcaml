module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module M = Wasm_memory
module X = Wasm_memory_execution

let[@def] (load_instruction @ total) (width : M.width @ immutable) (offset : B.u32) =
  match width with M.W32 -> I.I32_load (2, offset) | M.W64 -> I.I64_load (3, offset)
let[@def] (store_instruction @ total) (width : M.width @ immutable) (offset : B.u32) =
  match width with M.W32 -> I.I32_store (2, offset) | M.W64 -> I.I64_store (3, offset)

let[@def] (read_code @ total) (width : M.width @ immutable) (offset : B.u32)
    (base_local : B.u32) (destination : B.u32) : C.t @ immutable =
  C.Next (Wasm_instruction.Local_get base_local,
    C.Next (load_instruction width offset, C.Next (Wasm_instruction.Local_set destination, C.Empty)))

let (load @ total) : (width : M.width) @ immutable -> (offset : B.u32) ->
    (base_local : B.u32) -> (destination : B.u32) ->
    (state : X.state) @ ghost -> (base : B.u32) @ ghost -> (value : S.value) @ ghost ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && M.load state.X.memory base offset width === Some value
      && L.can_set state.X.machine.E.locals destination value} ->
    {code : C.t | code === read_code width offset base_local destination && (match X.run code state with
      | X.Done after -> after.X.memory === state.X.memory
        && after.X.machine.E.stack === state.X.machine.E.stack
        && L.get after.X.machine.E.locals destination === Some value
        && L.same_types state.X.machine.E.locals after.X.machine.E.locals
        && L.replaced state.X.machine.E.locals destination value after.X.machine.E.locals
      | _ -> false)} @ immutable = fun width offset base_local destination state base value premise ->
  let instruction = load_instruction width offset in
  let save = C.Next (I.Local_set destination, C.Empty) in
  let read = C.Next (instruction, save) in
  let code = C.Next (I.Local_get base_local, read) in
  ghost_ (
    let locals = state.X.machine.E.locals in
    let stack = state.X.machine.E.stack in
    let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 base, stack)}} in
    let loaded = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (value, stack)}} in
    X.run_def code state; X.step_def (I.Local_get base_local) state;
    E.step_def (I.Local_get base_local) state.X.machine;
    read_code_def width offset base_local destination; load_instruction_def width offset;
    X.run_def read addressed; X.step_def instruction addressed; X.read_def width offset addressed;
    X.run_def save loaded; X.step_def (I.Local_set destination) loaded;
    E.step_def (I.Local_set destination) loaded.X.machine;
    match L.set locals destination value with
    | None -> ()
    | Some locals -> X.run_def C.Empty {X.memory = state.X.memory; machine = {E.locals; stack}});
  code

let[@def] (write_code @ total) (width : M.width @ immutable) (offset : B.u32)
    (base_local : B.u32) (source : B.u32) : C.t @ immutable =
  C.Next (I.Local_get base_local, C.Next (I.Local_get source, C.Next (store_instruction width offset, C.Empty)))

let (store @ total) : (width : M.width) @ immutable -> (offset : B.u32) ->
    (base_local : B.u32) -> (source : B.u32) ->
    (state : X.state) @ ghost -> (base : B.u32) @ ghost -> (value : S.value) @ ghost ->
    {u : unit | L.get state.X.machine.E.locals base_local === Some (S.I32 base)
      && L.get state.X.machine.E.locals source === Some value
      && M.width value === width && M.can_store state.X.memory base offset value} ->
    {code : C.t | code === write_code width offset base_local source && (match X.run code state with
      | X.Done after -> after.X.machine === state.X.machine
        && M.load after.X.memory base offset width === Some value
        && Hmc_tagged_cell.length after.X.memory === Hmc_tagged_cell.length state.X.memory
        && (match M.address base offset width with None -> false
          | Some address -> Hmc_linear_bytes.updated state.X.memory address (M.encode value) after.X.memory)
      | _ -> false)} @ immutable = fun width offset base_local source state base value premise ->
  let instruction = store_instruction width offset in
  let write = C.Next (instruction, C.Empty) in
  let fetch = C.Next (I.Local_get source, write) in
  let code = C.Next (I.Local_get base_local, fetch) in
  ghost_ (
    let locals = state.X.machine.E.locals in
    let stack = state.X.machine.E.stack in
    let addressed = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (S.I32 base, stack)}} in
    let operands = {X.memory = state.X.memory; machine = {E.locals; stack = S.Push (value, addressed.X.machine.E.stack)}} in
    X.run_def code state; X.step_def (I.Local_get base_local) state;
    E.step_def (I.Local_get base_local) state.X.machine;
    X.run_def fetch addressed; X.step_def (I.Local_get source) addressed;
    E.step_def (I.Local_get source) addressed.X.machine;
    write_code_def width offset base_local source; store_instruction_def width offset;
    X.run_def write operands; X.step_def instruction operands; X.write_def width offset operands; X.compatible_def value width;
    match M.store state.X.memory base offset value with
    | None -> ()
    | Some memory -> X.run_def C.Empty {X.memory; machine = state.X.machine});
  code
