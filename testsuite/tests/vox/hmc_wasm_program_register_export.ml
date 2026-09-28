module B = Wasm_u32
module C = Wasm_code
module W = Hmc_word64
module S = Wasm_scalar
module E = Wasm_execution
module X = Wasm_memory_execution
module T = Wasm_control
module L = Wasm_locals
module Types = Wasm_control_local_type
module Registers = Hmc_wasm_program_registers
let (read @ total) : (before : Registers.registers) @ immutable -> (fuel : C.count) @ immutable ->
    (initial : T.configuration) @ immutable -> (final : T.configuration) @ immutable ->
    {u : unit | initial.T.state.X.machine.E.locals === Registers.locals before && T.run fuel initial === T.Running final} ->
    {after : Registers.registers | after.Registers.frame = before.Registers.frame && after.Registers.heap_limit = before.Registers.heap_limit
      && after.Registers.stack_limit = before.Registers.stack_limit && Registers.exports final.T.state.X.machine.E.locals after} @ immutable =
  fun before fuel initial final premise ->
    ghost_ (Registers.local_values before; Registers.matches_def (Registers.locals before) before;
      L.can_set_def (Registers.locals before) 1 (S.I32 0); S.same_type_def (S.I32 before.Registers.heap) (S.I32 0);
      L.can_set_def (Registers.locals before) 4 (S.I32 0); S.same_type_def (S.I32 before.Registers.top) (S.I32 0);
      L.can_set_def (Registers.locals before) 11 (S.I32 0); S.same_type_def (S.I32 before.Registers.status) (S.I32 0);
      L.can_set_def (Registers.locals before) 12 (S.I64 before.Registers.tag); S.same_type_def (S.I64 before.Registers.tag) (S.I64 before.Registers.tag);
      L.can_set_def (Registers.locals before) 13 (S.I64 before.Registers.payload); S.same_type_def (S.I64 before.Registers.payload) (S.I64 before.Registers.payload);
      Types.run fuel initial final 1 (S.I32 0) (); Types.run fuel initial final 4 (S.I32 0) (); Types.run fuel initial final 11 (S.I32 0) ();
      Types.run fuel initial final 12 (S.I64 before.Registers.tag) (); Types.run fuel initial final 13 (S.I64 before.Registers.payload) ();
      L.can_set_def final.T.state.X.machine.E.locals 1 (S.I32 0); L.can_set_def final.T.state.X.machine.E.locals 4 (S.I32 0);
      L.can_set_def final.T.state.X.machine.E.locals 11 (S.I32 0); L.can_set_def final.T.state.X.machine.E.locals 12 (S.I64 before.Registers.tag);
      L.can_set_def final.T.state.X.machine.E.locals 13 (S.I64 before.Registers.payload));
    let actual = final.T.state.X.machine.E.locals in
    match L.get actual 1, L.get actual 4, L.get actual 11, L.get actual 12, L.get actual 13 with
    | Some (S.I32 heap), Some (S.I32 top), Some (S.I32 status), Some (S.I64 tag), Some (S.I64 payload) ->
      let after = {before with Registers.heap; top; status; tag; payload} in
      ghost_ (Registers.exports_def actual after); after
    | heap, top, status, tag, payload ->
      ghost_ (
        (match heap with Some value -> S.same_type_def value (S.I32 0) | None -> ());
        (match top with Some value -> S.same_type_def value (S.I32 0) | None -> ());
        (match status with Some value -> S.same_type_def value (S.I32 0) | None -> ());
        (match tag with Some value -> S.same_type_def value (S.I64 before.Registers.tag) | None -> ());
        (match payload with Some value -> S.same_type_def value (S.I64 before.Registers.payload) | None -> ()));
      unreachable_ ()
