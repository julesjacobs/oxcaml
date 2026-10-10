module B = Wasm_u32
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module L = Wasm_locals
module Advance = Hmc_wasm_allocation_advance
module Prefix = Wasm_instruction_prefix

let (correct @ total) : (bytes : B.u32) -> (cursor_local : B.u32) -> (cursor : B.u32) -> (limit : B.u32) ->
    (state : X.state) @ immutable -> (fuel : C.count) @ immutable ->
    {u : unit | cursor + bytes <= limit && L.get state.X.machine.E.locals cursor_local === Some (S.I32 cursor)} ->
    {u : unit | match X.run (Prefix.take fuel (Advance.emit bytes cursor_local)) state with
      | X.Done current -> current.X.memory === state.X.memory
        && (current.X.machine.E.locals === state.X.machine.E.locals
          || (L.replaced state.X.machine.E.locals cursor_local (S.I32 (S.add32 cursor bytes)) current.X.machine.E.locals
            && L.get current.X.machine.E.locals cursor_local === Some (S.I32 (S.add32 cursor bytes))))
      | _ -> false} @ ghost = fun bytes cursor_local cursor limit state fuel premise -> ghost_ (
    let out = Advance.correct bytes cursor_local cursor limit state () in
    let s1 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 cursor, state.X.machine.E.stack)}} in
    let s2 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 bytes, s1.X.machine.E.stack)}} in
    let s3 = {state with X.machine = {state.X.machine with E.stack = S.Push (S.I32 (S.add32 cursor bytes), state.X.machine.E.stack)}} in
    let c3 = C.Next (I.Local_set cursor_local, C.Empty) in
    let c2 = C.Next (I.Plain I.I32_add, c3) in
    let c1 = C.Next (I.I32_const bytes, c2) in
    Advance.emit_def bytes cursor_local; Prefix.take_def fuel (Advance.emit bytes cursor_local);
    match fuel with
    | C.Zero -> X.run_def C.Empty state
    | C.Succ fuel1 ->
      X.run_def (Prefix.take fuel (Advance.emit bytes cursor_local)) state;
      X.step_def (I.Local_get cursor_local) state; E.step_def (I.Local_get cursor_local) state.X.machine;
      Prefix.take_def fuel1 c1;
      match fuel1 with
      | C.Zero -> X.run_def C.Empty s1
      | C.Succ fuel2 ->
        X.run_def (Prefix.take fuel1 c1) s1; X.step_def (I.I32_const bytes) s1;
        E.step_def (I.I32_const bytes) s1.X.machine; S.step_def (I.I32_const bytes) s1.X.machine.E.stack;
        Prefix.take_def fuel2 c2;
        match fuel2 with
        | C.Zero -> X.run_def C.Empty s2
        | C.Succ fuel3 ->
          X.run_def (Prefix.take fuel2 c2) s2; X.step_def (I.Plain I.I32_add) s2;
          E.step_def (I.Plain I.I32_add) s2.X.machine; S.step_def (I.Plain I.I32_add) s2.X.machine.E.stack;
          Prefix.take_def fuel3 c3;
          match fuel3 with
          | C.Zero -> X.run_def C.Empty s3
          | C.Succ fuel4 ->
            Prefix.take_def fuel4 C.Empty;
            X.run_def c3 s3; X.step_def (I.Local_set cursor_local) s3; E.step_def (I.Local_set cursor_local) s3.X.machine;
            L.replaced_def state.X.machine.E.locals cursor_local (S.I32 (S.add32 cursor bytes)) out;
            match L.set state.X.machine.E.locals cursor_local (S.I32 (S.add32 cursor bytes)) with
            | None -> ()
            | Some locals -> X.run_def C.Empty {X.memory = state.X.memory; machine = {E.locals; stack = state.X.machine.E.stack}})
