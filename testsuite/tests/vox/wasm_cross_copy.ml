module B = Wasm_u32
module C = Wasm_code
module I = Wasm_instruction
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module M = Wasm_memory
module L = Wasm_locals
module Plan = Wasm_parallel_copy
type plan = Plan.plan
let[@def] rec (apply @ total) (plan : plan @ immutable) (memory : B.bytes @ immutable) (source_base : B.u32) (target_base : B.u32) : B.bytes option @ immutable =
  match plan with
  | Plan.End -> Some memory
  | Plan.Copy (source, destination, rest) -> (match M.load memory source_base source M.W64 with
    | Some (S.I64 word) -> (match apply rest memory source_base target_base with None -> None | Some after -> M.store after target_base destination (S.I64 word))
    | _ -> None)
let[@def] rec (emit @ total) (plan : plan @ immutable) (source_local : B.u32) (target_local : B.u32) : C.t @ immutable =
  match plan with
  | Plan.End -> C.Empty
  | Plan.Copy (source, destination, rest) ->
    C.Next (I.Local_get target_local, C.Next (I.Local_get source_local, C.Next (I.I64_load (3, source),
      E.append (emit rest source_local target_local) (C.Next (I.I64_store (3, destination), C.Empty)))))
let rec (correct @ total) : (plan : plan) @ immutable -> (source_local : B.u32) -> (target_local : B.u32) -> (state : X.state) @ immutable ->
    (source_base : B.u32) -> (target_base : B.u32) -> (after : B.bytes) @ immutable ->
    {u : unit | L.get state.X.machine.E.locals source_local === Some (S.I32 source_base)
      && L.get state.X.machine.E.locals target_local === Some (S.I32 target_base) && apply plan state.X.memory source_base target_base === Some after} ->
    {u : unit | X.run (emit plan source_local target_local) state === X.Done {X.memory = after; machine = state.X.machine}
      && Hmc_tagged_cell.length after === Hmc_tagged_cell.length state.X.memory} @ ghost =
  fun plan source_local target_local state source_base target_base after premise -> ghost_ (
    emit_def plan source_local target_local; apply_def plan state.X.memory source_base target_base;
    match plan with
    | Plan.End -> X.run_def C.Empty state
    | Plan.Copy (source, destination, rest) -> (match M.load state.X.memory source_base source M.W64 with
      | Some (S.I64 word) -> (match apply rest state.X.memory source_base target_base with
        | None -> ()
        | Some memory ->
          let addressed = {X.memory = state.X.memory; machine = {E.locals = state.X.machine.E.locals;
            stack = S.Push (S.I32 target_base, state.X.machine.E.stack)}} in
          let reading = {addressed with X.machine = {E.locals = state.X.machine.E.locals;
            stack = S.Push (S.I32 source_base, addressed.X.machine.E.stack)}} in
          let captured = {addressed with X.machine = {E.locals = state.X.machine.E.locals;
            stack = S.Push (S.I64 word, addressed.X.machine.E.stack)}} in
          let restored = {X.memory; machine = captured.X.machine} in
          let write = C.Next (I.I64_store (3, destination), C.Empty) in
          let tail = E.append (emit rest source_local target_local) write in
          let read = C.Next (I.I64_load (3, source), tail) in
          let address = C.Next (I.Local_get source_local, read) in
          X.run_def (emit plan source_local target_local) state; X.step_def (I.Local_get target_local) state;
          E.step_def (I.Local_get target_local) state.X.machine;
          X.run_def address addressed; X.step_def (I.Local_get source_local) addressed;
          E.step_def (I.Local_get source_local) addressed.X.machine;
          X.run_def read reading; X.step_def (I.I64_load (3, source)) reading; X.read_def M.W64 source reading;
          correct rest source_local target_local captured source_base target_base memory ();
          X.append_correct (emit rest source_local target_local) write captured;
          X.run_def write restored; X.step_def (I.I64_store (3, destination)) restored;
          M.width_def (S.I64 word); X.compatible_def (S.I64 word) M.W64; X.write_def M.W64 destination restored;
          (match M.store memory target_base destination (S.I64 word) with
          | None -> ()
          | Some stored -> X.run_def C.Empty {X.memory = stored; machine = state.X.machine}))
      | _ -> ()))
