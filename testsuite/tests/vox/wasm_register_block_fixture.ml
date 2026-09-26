module B = Wasm_u32
module W = Hmc_word64
module I = Wasm_instruction
module C = Wasm_code
module S = Wasm_scalar
module L = Wasm_locals
module E = Wasm_execution
module X = Wasm_memory_execution
module G = Wasm_globals
module GE = Wasm_global_execution
module T = Wasm_control
module P = Wasm_instance_control
module F = Wasm_functions
module Calls = Wasm_calls
module Indirect = Wasm_indirect_block
module Registers = Wasm_global_registers
module Transfer = Wasm_global_local_transfer
module Block = Wasm_register_block
module Lift = Wasm_control_lift
module Continue = Wasm_control_branch_continue
let rec code = function [] -> C.Empty | head :: rest -> C.Next (head, code rest)
let rec fuel n = if n = 0 then C.Zero else C.Succ (fuel (n - 1))
let loads = Registers.Binding (2, 0, Registers.Binding (0, 1, Registers.Binding (1, 2, Registers.End)))
let stores = Registers.Binding (0, 1, Registers.Binding (1, 2, Registers.End))
let local_types = F.Local32 (F.Local64 (F.Local32 F.No_locals))
let body = code [I.Local_get 2; I.I32_const 1; I.Plain I.I32_add; I.Local_set 2;
  I.Local_get 1; I.I64_const {W.lo = 1; hi = 0}; I.Plain I.I64_add; I.Local_set 1]
type fixture = {initial : GE.state; final : GE.state; block : T.code; main : T.code; counter : B.u32; word : W.t}
let fixture (counter : B.u32) (word : W.t @ immutable) =
  let globals = {G.values = S.Push (S.I64 word, S.Push (S.I32 counter, S.Push (S.I32 512, S.Empty)));
    permissions = G.Global (true, G.Global (true, G.Global (false, G.Empty)))} in
  let initial = {GE.globals; execution = {X.memory = B.Byte (42, B.Byte (173, B.End));
    machine = {E.locals = F.zero_locals local_types; stack = S.Empty}}} in
  let fragment = Lift.embed body T.Empty in
  let emitted = Block.emit loads fragment stores T.Empty in
  (match Registers.load loads initial with
  | None -> failwith "register import"
  | Some imported ->
    match X.run body imported.GE.execution with
    | X.Done after ->
      (match Registers.store stores {GE.globals = imported.GE.globals; execution = after} with
      | None -> failwith "register export"
      | Some exported ->
        let labels = Continue.labels (Block.epilogue stores T.Empty) T.No_labels in
        ghost_ (Wasm_control_success.straight body imported.GE.execution after ();
          Lift.correct body T.Empty labels imported.GE.execution after ());
        (match imported.GE.execution.X.machine.E.stack, after.X.machine.E.stack with
        | S.Empty, S.Empty ->
        ghost_ (Block.correct loads fragment stores T.Empty T.No_labels initial imported after exported (C.length body) ());
        let function_ = {F.result = F.Void; locals = local_types; code = emitted} in
        let single = {F.functions = F.Function (function_, F.No_functions);
          signatures = F.Signature (F.Void, F.No_signatures); table = F.Element (Some 0, F.No_elements)} in
        let call = {Calls.current = {P.globals; body = {T.code = T.Instruction (I.Call_indirect 0, T.Empty); labels = T.No_labels;
          state = {initial.GE.execution with X.machine = {initial.GE.execution.X.machine with E.stack = S.Push (S.I32 0, S.Empty)}}}};
          result = F.Void; callers = Calls.Root; capacity = C.Succ C.Zero} in
        ghost_ (F.signature_def single.F.signatures 0; F.element_def single.F.table 0; F.lookup_def single.F.functions 0;
          Indirect.entry_def function_ call.Calls.current;
          Indirect.correct single 0 0 0 function_ T.Empty call S.Empty C.Zero exported
            (Block.cost loads (C.length body) stores) ());
        let resumed = {call with Calls.current = {P.globals = exported.GE.globals;
          body = {T.code = T.Empty; labels = T.No_labels; state = {X.memory = exported.GE.execution.X.memory;
            machine = {E.locals = initial.GE.execution.X.machine.E.locals; stack = S.Empty}}}}} in
        if Calls.run (Indirect.cost (Block.cost loads (C.length body) stores)) single call <> Calls.Running resumed
        then failwith "indirect theorem round trip";
        if P.run (fuel 1000) {P.body = {T.code = emitted; labels = T.No_labels; state = initial.GE.execution}; globals} <> P.Finished exported
        then failwith "register block instance execution"
        | _ -> failwith "register operand stack"))
    | _ -> failwith "register block execution");
  let main = Lift.embed (code [I.I32_const 0; I.Call_indirect 0; I.I32_const 0; I.Call_indirect 0;
    I.I32_const 0; I.Call_indirect 0; I.Global_get 1]) T.Empty in
  let module_ = {F.functions = F.Function ({F.result = F.I32; locals = F.No_locals; code = main},
      F.Function ({F.result = F.Void; locals = local_types; code = emitted}, F.No_functions));
    signatures = F.Signature (F.Void, F.No_signatures); table = F.Element (Some 1, F.No_elements)} in
  let counter = S.add32 (S.add32 (S.add32 counter 1) 1) 1 in
  let one = {W.lo = 1; hi = 0} in
  let word = W.add (W.add (W.add word one) one) one in
  match Calls.start module_ 0 initial.GE.execution.X.memory globals (C.Succ C.Zero) with
  | Calls.Running start ->
    if Calls.run (fuel 1000) module_ {start with Calls.capacity = C.Zero} <> Calls.Host_limit then failwith "missing call capacity";
    (match Calls.run (fuel 1000) module_ start with
    | Calls.Finished final ->
      if final.GE.execution.X.memory <> initial.GE.execution.X.memory || final.GE.execution.X.machine.E.stack <> S.Push (S.I32 counter, S.Empty)
        || G.get final.GE.globals 0 <> Some (S.I64 word) || G.get final.GE.globals 1 <> Some (S.I32 counter)
        || G.get final.GE.globals 2 <> Some (S.I32 512) then failwith "indirect register persistence";
      {initial; final; block = emitted; main; counter; word}
    | _ -> failwith "indirect block sequence")
  | _ -> failwith "indirect block start"
let rejected () =
  let state = {GE.globals = {G.values = S.Push (S.I32 7, S.Empty); permissions = G.Global (false, G.Empty)};
    execution = {X.memory = B.End; machine = {E.locals = S.Push (S.I64 {W.lo = 0; hi = 0}, S.Empty); stack = S.Empty}}} in
  if Transfer.load 0 0 state <> None || Transfer.load 1 0 state <> None || Transfer.store 0 0 state <> None
    || Transfer.store 0 1 state <> None then failwith "invalid register transfer accepted";
  let mutable_ = {state with GE.globals = {state.GE.globals with G.permissions = G.Global (true, G.Empty)}} in
  if Transfer.store 0 0 mutable_ <> None then failwith "mismatched global store accepted";
  let same_type = {state with GE.execution = {state.GE.execution with X.machine = {E.locals = S.Push (S.I32 11, S.Empty); stack = S.Empty}}} in
  if Transfer.store 0 0 same_type <> None then failwith "immutable global store accepted"
let fixtures () =
  rejected ();
  [fixture 7 {W.lo = 42; hi = 37}; fixture 4294967295 {W.lo = 4294967295; hi = 4294967295}]
